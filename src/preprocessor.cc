#include <ctime>
#include <stack>
#include <sstream>
#include <algorithm>
#include <cassert>

#include "preprocessor.hh"
#include "scanner.hh"
#include "error.hh"

namespace axcc {

class Preprocessor::Macro {
public:
    // For obj-like macro
    Macro(const std::string& ident, const std::list<Token>& repl)
        : ident_{ident}, repl_{repl}, is_obj_like_{true} {}
    // For funcion-like macro, use empty vector as argument for macros with zero
    // parameter.
    Macro(const std::string& ident, const std::list<Token>& repl,
          const std::vector<std::string>& params, bool is_variadic)
        : ident_{ident}, repl_{repl}, params_{params}, is_obj_like_{false},
          is_variadic_{is_variadic} {
        // For variadic functions, always add a parameter named __VA_ARGS__.
        if (is_variadic_)
            params_.push_back("__VA_ARGS__");
    }
    Macro(const Macro&) = default;
    Macro(Macro&&) = default;
    Macro& operator=(const Macro&) = delete;
    Macro& operator=(Macro&&) = delete;
    virtual ~Macro() = default;

    bool IsObjLike() const { return is_obj_like_; }
    bool IsFuncLike() const { return !is_obj_like_; }
    std::string Ident() const { return ident_; }
    std::list<Token> Repl() const { return repl_; }
    // For function-like macro
    const std::vector<std::string>& Params() const { return params_; }
    bool IsVariadic() const { return is_variadic_; }

private:
    const bool is_obj_like_;
    const std::string ident_;
    const std::list<Token> repl_;
    std::vector<std::string> params_{};
    const bool is_variadic_{false};
};

class Preprocessor::PPConditions {
public:
    PPConditions() = default;
    PPConditions(const PPConditions&) = default;
    PPConditions(PPConditions&&) = default;
    PPConditions& operator=(const PPConditions&) = default;
    PPConditions& operator=(PPConditions&&) = default;
    virtual ~PPConditions() = default;

    void CondBegin(bool state, const SourceLoc& loc) {
        conditions_.push({state, true, loc}); }
    void CondEnd() { conditions_.pop(); }
    bool HasNextElse() const { return conditions_.top().has_next_else; }
    void EncounterElse() {
        conditions_.top().has_next_else = false;
        conditions_.top().state = !conditions_.top().state;
    }
    bool CurState() const {
        return conditions_.empty() || conditions_.top().state; }
    void SetCurState(bool state) { conditions_.top().state = state; }
    bool IsEmpty() const { return conditions_.empty(); }
    SourceLoc BeginLoc() const { return conditions_.top().begin_loc; }

private:
    struct PPCondition {
        // Indicate whether current conditional preprocessing block need to be
        // processed.
        bool state;
        bool has_next_else;
        const SourceLoc begin_loc;
    };
    std::stack<PPCondition> conditions_{};
};

// These two functions has to be defined here due to the incomplete type of
// Macro class.
Preprocessor::Preprocessor(TokenSequence& ts,
                           std::map<std::string, std::string>& files,
                           const std::list<std::string>& header_paths)
    : ts_{ts}, files_{files}, header_paths_{header_paths}, macros_{},
      conditionsp_{std::make_unique<PPConditions>()} {
    for (auto& dir : header_paths_) {
        if (dir.back() != '/')
            dir.push_back('/');
    }
    AddInitMacros();
    AddInitHeaderPaths();
}

Preprocessor::~Preprocessor() = default;

void Preprocessor::Preprocess() {
    // The count of tokens waiting to be deleted.
    // Use this variable so that we can delete a block of tokens at a time
    // instead of delete them one by one.
    int wait_del = 0;
    bool follow_newline = true;
    Token* tp = Begin();

    while (!IsEndToken(*tp)) {
        if (follow_newline && tp->Tag() == TokenType::SHARP) {
            tp = Next();
            switch (tp->Tag()) {
                case TokenType::IF: ParsePPIf(); break;
                case TokenType::ELSE: ParsePPElse(wait_del); break;
                case TokenType::IDENTIFIER: {
                    std::string token_str{tp->TokenStr()};
                    if (token_str == "ifdef") ParsePPIfdef();
                    else if (token_str == "ifndef") ParsePPIfndef();
                    else if (token_str == "elif") ParsePPElif(wait_del);
                    else if (token_str == "endif") ParsePPEndif(wait_del);
                    else if (token_str == "define") ParsePPDefine();
                    else if (token_str == "undef") ParsePPUndef();
                    else if (token_str == "include") ParsePPInclude();
                    else if (token_str == "error") ParsePPError();
                    else if (token_str == "line") ParsePPLine();
                    break;
                }
                case TokenType::NEWLINE:
                case TokenType::END:
                    ts_.ErasePrevN(2); break;
                default:
                    Error("invalid preprocessing directive", tp->Loc());
                    EraseUntilNextLine(1);
            }
            // Now Next() should return the first token of next line or the
            // END token. CurToken() is meaningless after the process of
            // erasing.
            tp = Next();
            follow_newline = true;
            continue;
        }
        if (IsNewlineToken(*tp))
            follow_newline = true;
        else
            follow_newline = false;
        if (conditionsp_->CurState()) {
            // Note that not only identifiers but also keywords could be text
            // macros.
            if (HasMacro(tp->TokenStr()))
                ExpandCurMacro();
            else if (IsNewlineToken(*tp))
                ts_.ErasePrevN(1);
        } else {
            ++wait_del;
        }
        // It is safe to call Next() here, even if the token sequence has
        // already reached the end.
        tp = Next();
    }
    // Check unterminated conditional directive
    while (!conditionsp_->IsEmpty()) {
        Error("unterminated conditional directive", conditionsp_->BeginLoc());
        conditionsp_->CondEnd();
    }
}

bool Preprocessor::AddMacro(const Macro& macro) {
    bool is_redef = false;
    std::string ident{macro.Ident()};
    auto iter = macros_.find(ident);
    if (iter != macros_.cend()) {
        // If the new definition is effectively the same, the redefinition is
        // silently ignored.
        const Macro& orig_macro = *iter->second;
        auto orig_repl = orig_macro.Repl();
        auto new_repl = macro.Repl();
        if (std::equal(orig_repl.cbegin(), orig_repl.cend(),
                       new_repl.cbegin(), new_repl.cend()) &&
            (orig_macro.IsFuncLike() == macro.IsFuncLike()) &&
            // If it is a function-like macro, the parameters should also be
            // the same.
            (!orig_macro.IsFuncLike() ||
             std::equal(orig_macro.Params().cbegin(), orig_macro.Params().cend(),
                        macro.Params().cbegin(), macro.Params().cend()))) {
            return is_redef;
        }
        macros_.erase(iter);
        is_redef = true;
    }
    macros_.emplace(ident, std::make_unique<Macro>(macro));
    return is_redef;
}

void Preprocessor::RmMacro(const std::string& ident) {
    auto iter = macros_.find(ident);
    if (iter != macros_.cend())
        macros_.erase(iter);
}

const Preprocessor::Macro* Preprocessor::GetMacro(
          const std::string& ident) const {
    auto iter = macros_.find(ident);
    if (iter != macros_.cend())
        return &*iter->second;
    else
        return nullptr;
}

namespace {

std::string GetDirFromPath(const std::string& path) {
    auto index = path.rfind('/');
    if (index == -1)
        return {};
    else
        return path.substr(0, index + 1);
}

// Remove extra "./" and "../" and continuously repeated '/' in path if
// possible.
std::string SimplifyPath(const std::string& path) {
    if (path.empty())
        return {};
    std::string ret;
    std::string cur_dir;
    std::vector<std::string> stk;
    std::stringstream ss{path};
    if (path.front() == '/')
        stk.push_back("/");
    while (std::getline(ss, cur_dir, '/')) {
        if (cur_dir.empty())
            continue;
        if (cur_dir == ".") {
            if (stk.empty())
                stk.push_back(".");
            else
                continue;
        } else if (cur_dir != "..") {
            stk.push_back(cur_dir);
        } else if (!stk.empty() && stk.back() != "." &&
                   stk.back() != ".." && stk.back() != "/") {
            stk.pop_back();
        } else {
            stk.push_back(cur_dir);
        }
    }
    for (const auto& dir : stk)
        ret += dir + "/";
    if (path.front() == '/' && path.back() == '/' && stk.size() == 1)
        return "/";
    if (path.front() == '/')
        ret = ret.substr(1);
    if (path.back() != '/' && path.back() != '.')
        ret.pop_back();
    return ret;
}

} // unnamed namespace

std::string Preprocessor::FindHeader(const std::string& fname,
                                     bool include_cur_path,
                                     const std::string& cur_path) {
    if (fname.empty())
        return {};
    if (fname.front() == '/') {
        if (FileExist(fname))
            return fname;
        return {};
    }
    if (include_cur_path)
        header_paths_.push_front(GetDirFromPath(cur_path));
    std::string found_path;
    for (const auto& dir : header_paths_) {
        if (FileExist(dir + fname)) {
            found_path = dir + fname;
            break;
        }
    }
    if (include_cur_path)
        header_paths_.pop_front();
    return SimplifyPath(found_path);
}

void Preprocessor::AddInitMacros() {
    // Macro __FILE__ and __LINE__ will be handled seperately.
    AddMacro({"__FILE__", {}});
    AddMacro({"__LINE__", {}});
    // Add time related predefined macro
    char time_buf[100];
    std::time_t now_t = std::time(nullptr);
    std::tm* now = std::localtime(&now_t);
    std::strftime(time_buf, sizeof(time_buf), "b d Y", now);
    AddMacro({"__DATE__", {{TokenType::STRING, time_buf}}});
    std::strftime(time_buf, sizeof(time_buf), "T", now);
    AddMacro({"__TIME__", {{TokenType::STRING, time_buf}}});
    // Other predefined macros
    AddMacro({"__STDC__", {{TokenType::LONG, "1"}}});
    AddMacro({"__STDC_VERSION__", {{TokenType::INT, "1"}}});
    AddMacro({"__STDC_HOSTED__", {{TokenType::INT, "1"}}});
}

void Preprocessor::AddInitHeaderPaths() {
    AddHeaderPath("/usr/local/include/");
    AddHeaderPath("/usr/include/x86_64-linux-gnu/");
    AddHeaderPath("/usr/include/");
}

void Preprocessor::ParsePPIf() {
    // TODO: Support #if directive.
    if (!conditionsp_->CurState()) {
        EraseUntilNextLine(1);
        return;
    }
    Error("#if directive is not support yet", CurToken()->Loc());
    EraseUntilNextLine(1);
}

void Preprocessor::ParsePPIfdef() {
    if (!conditionsp_->CurState()) {
        EraseUntilNextLine(1);
        return;
    }
    Token* tp = Next();
    if (IsNewlineToken(*tp) || IsEndToken(*tp)) {
        Error("macro name missing", tp->Loc());
        conditionsp_->CondBegin(false, tp->Loc());
        ts_.ErasePrevN(3);
        return;
    }
    if (!IsIdentOrKeyword(*tp)) {
        Error("macro name must be an identifier", tp->Loc());
        conditionsp_->CondBegin(false, tp->Loc());
    } else if (HasMacro(tp->TokenStr())) {
        conditionsp_->CondBegin(true, tp->Loc());
    } else {
        conditionsp_->CondBegin(false, tp->Loc());
    }
    EraseExtraTokensIfHas(2);
}

void Preprocessor::ParsePPIfndef() {
    // Note that if there is no identifier following #ifndef, the subsequent
    // conditional block will be processed. This behavior is different from
    // clang/gcc.
    ParsePPIfdef();
    conditionsp_->SetCurState(!conditionsp_->CurState());
}

void Preprocessor::ParsePPElse(int& wait_del) {
    Token* tp = CurToken();
    if (conditionsp_->IsEmpty()) {
        Error("#else without #if", tp->Loc());
        EraseUntilNextLine(1);
        return;
    }
    if (!conditionsp_->HasNextElse()) {
        Error("#else after #else", tp->Loc());
        conditionsp_->SetCurState(false);
        EraseUntilNextLine(1);
        return;
    }
    conditionsp_->EncounterElse();
    // Delete the #else directive together with the tokens that were waiting to
    // be deleted.
    EraseExtraTokensIfHas(wait_del + 1);
    // Reset the count.
    wait_del = 0;
}

void Preprocessor::ParsePPElif(int& wait_del) {
    // TODO: Support #elif directive.
    Token* tp = CurToken();
    if (conditionsp_->IsEmpty()) {
        Error("#elif without #if", tp->Loc());
        EraseUntilNextLine(1);
        return;
    }
    if (!conditionsp_->HasNextElse()) {
        Error("#elif after #else", tp->Loc());
        conditionsp_->SetCurState(false);
        EraseUntilNextLine(1);
        return;
    }
    Error("#elif is not supported yet", tp->Loc());
    EraseUntilNextLine(wait_del + 1);
    wait_del = 0;
}

void Preprocessor::ParsePPEndif(int& wait_del) {
    Token* tp = CurToken();
    if (conditionsp_->IsEmpty()) {
        Error("#endif without #if", tp->Loc());
        EraseUntilNextLine(1);
        return;
    }
    conditionsp_->CondEnd();
    EraseExtraTokensIfHas(wait_del + 1);
    wait_del = 0;
}

void Preprocessor::ParsePPDefine() {
    if (!conditionsp_->CurState()) {
        EraseUntilNextLine(1);
        return;
    }
    Token* tp = Next();
    const SourceLoc& macro_loc = tp->Loc();
    if (!IsIdentOrKeyword(*tp)) {
        Error("macro name must be an identifier", macro_loc);
        EraseUntilNextLine(2);
        return;
    }
    std::string macro_name = tp->TokenStr();
    if (macro_name == "defined") {
        Error("'defined' cannot be used as a macro name", macro_loc);
        EraseUntilNextLine(2);
        return;
    }
    if (macro_name == "__FILE__" ||
        macro_name == "__LINE__" ||
        macro_name == "__DATE__" ||
        macro_name == "__TIME__" ||
        macro_name == "__STDC__" ||
        macro_name == "__STDC_VERSION__" ||
        macro_name == "__STDC_HOSTED__") {
        Error("redefining builtin macro", macro_loc);
        EraseUntilNextLine(2);
        return;
    }
    tp = Next();
    bool is_redef = false;
    // For object-like macros
    // We need to check whether there are whitespaces between the macro name
    // and the left parenthesis.
    if (tp->Tag() != TokenType::LPAR || HasPreWhiteSpace(tp->Loc())) {
        std::list<Token> macro_repl;
        while (!IsNewlineToken(*tp) && !IsEndToken(*tp)) {
            macro_repl.push_back(*tp);
            tp = Next();
        }
        is_redef = AddMacro({macro_name, macro_repl});
        ts_.ErasePrevN(4 + macro_repl.size());
    } else {
        // For function-like macros
        // Parse parameters
        std::vector<std::string> params;
        bool is_variadic = false;
        tp = Next();
        if (tp->Tag() != TokenType::RPAR) {
            while (true) {
                if (IsNewlineToken(*tp) || IsEndToken(*tp)) {
                    Error("missing ')' in macro parameter list", tp->Loc());
                    EraseUntilNextLine(4 + 2 * params.size());
                    return;
                }
                if (tp->Tag() == TokenType::ELLIP) {
                    is_variadic = true;
                    tp = Next();
                    if (tp->Tag() != TokenType::RPAR) {
                        Error("missing ')' in macro parameter list", tp->Loc());
                        EraseUntilNextLine(5 + 2 * params.size());
                        return;
                    }
                    break;
                }
                if (!IsIdentOrKeyword(*tp)) {
                    Error("expected identifier in macro parameter list",
                          tp->Loc());
                    EraseUntilNextLine(4 + 2 * params.size());
                    return;
                }
                params.push_back(tp->TokenStr());
                tp = Next();
                if (tp->Tag() == TokenType::RPAR)
                    break;
                if (tp->Tag() != TokenType::COMMA) {
                    Error("expected comma in macro parameter list", tp->Loc());
                    EraseUntilNextLine(3 + 2 * params.size());
                    return;
                }
                tp = Next();
            }
        }
        // Parse replacement tokens
        std::list<Token> macro_repl;
        tp = Next();
        while (!IsNewlineToken(*tp) && !IsEndToken(*tp)) {
            macro_repl.push_back(*tp);
            tp = Next();
        }
        is_redef = AddMacro({macro_name, macro_repl, params, is_variadic});
        int params_size = params.size() + (is_variadic ? 1 : 0);
        ts_.ErasePrevN(5 + (params_size == 0 ? 1 : 2 * params_size) +
                       macro_repl.size());
    }
    if (is_redef)
        Warning("'" + macro_name + "' macro redefined", macro_loc);
}

void Preprocessor::ParsePPUndef() {
    if (!conditionsp_->CurState()) {
        EraseUntilNextLine(1);
        return;
    }
    Token* tp = Next();
    if (IsNewlineToken(*tp) || IsEndToken(*tp)) {
        Error("macro name missing", tp->Loc());
        ts_.ErasePrevN(3);
        return;
    }
    if (!IsIdentOrKeyword(*tp))
        Error("macro name must be an identifier", tp->Loc());
    else
        RmMacro(tp->TokenStr());
    EraseExtraTokensIfHas(2);
}

namespace {

std::string WrapStr(const std::string& str) {
    std::string ret;
    ret.push_back('\"');
    for (auto c : str) {
        if (c == '\"' || c == '\\')
            ret.push_back('\\');
        ret.push_back(c);
    }
    ret.push_back('\"');
    return ret;
}

std::string UnwrapStr(const std::string& str) {
    std::string ret;
    std::string str_content = str.substr(1, str.size() - 2);
    bool follow_bslash = false;
    for (auto iter = str_content.cbegin(); iter != str_content.cend(); ++iter) {
        if (*iter == '\\' && !follow_bslash) {
            follow_bslash = true;
            continue;
        } else {
            follow_bslash = false;
            ret.push_back(*iter);
        }
    }
    return ret;
}

} // unnamed namespace

void Preprocessor::ParsePPInclude() {
    if (!conditionsp_->CurState()) {
        EraseUntilNextLine(1);
        return;
    }
    ExpandCurLine();
    Token* tp = Next();
    std::string fname;
    bool include_cur_path = false;
    bool err_unexpect = false;
    const SourceLoc* err_locp = &tp->Loc();
    int prevn = 2;
    if (tp->Tag() == TokenType::STRING) {
        fname = tp->TokenStr();
        include_cur_path = true;
        // Encoding prefix is not permitted.
        if (fname[0] != '"')
            err_unexpect = true;
        // Remove the leading and trailing double quotes directly. Do not call
        // UnwrapStr() here.
        fname.pop_back();
        fname = fname.substr(1);
    } else if (tp->Tag() == TokenType::LABRACKET) {
        do {
            tp = Next();
            ++prevn;
            if (tp->Tag() == TokenType::RABRACKET)
                break;
            fname += tp->TokenStr();
        } while (!IsNewlineToken(*tp) && !IsEndToken(*tp));
        err_unexpect = true;
        err_locp = &tp->Loc();
    } else {
        err_unexpect = true;
    }
    if (err_unexpect) {
        Error("expected \"FILENAME\" or <FILENAME>", *err_locp);
        EraseUntilNextLine(prevn);
        return;
    }
    if (fname.empty()) {
        Error("empty filename", *err_locp);
        EraseUntilNextLine(prevn);
        return;
    }
    // TODO: Deal with recursive include?
    std::string found_path{
                    FindHeader(fname, include_cur_path, *err_locp->fnamep)};
    if (found_path.empty()) {
        Error("'" + fname + "' file not found", *err_locp);
        EraseUntilNextLine(prevn);
        return;
    }
    // Check whether the file has already been read before.
    auto iter = files_.find(found_path);
    if (iter == files_.cend())
        iter = files_.emplace(found_path, ReadFile(found_path)).first;
    Scanner scanner{iter->first, iter->second};
    auto tsp = scanner.Scan();
    EraseExtraTokensIfHas();
    ts_.ReplacePrevN(prevn, std::move(*tsp));
}

void Preprocessor::ParsePPError() {
    if (!conditionsp_->CurState()) {
        EraseUntilNextLine(1);
        return;
    }
    const SourceLoc& loc = LookAhead()->Loc();
    std::string err_str{std::next(loc.linep, loc.column - 1),
                        std::next(loc.linep, loc.line_len)};
    Error(err_str, CurToken()->Loc());
    EraseUntilNextLine(1);
}

void Preprocessor::ParsePPLine() {
    if (!conditionsp_->CurState()) {
        EraseUntilNextLine(1);
        return;
    }
    // Save the original location information before expand tokens in current
    // line.
    Token* tp = ts_.LookAhead();
    unsigned int orig_row = tp->Loc().row;
    const std::string* orig_fnamep = tp->Loc().fnamep;
    ExpandCurLine();
    // Deal with the current preprocessor line number.
    tp = Next();
    if (tp->Tag() != TokenType::I_CONSTANT) {
        Error("#line directive requires a positive integer argument", tp->Loc());
        EraseUntilNextLine(2);
        return;
    }
    std::string line_str{tp->TokenStr()};
    for (auto c : line_str) {
        if (c > '9' || c < '0') {
            Error("#line directive requires a simple digit sequence", tp->Loc());
            EraseUntilNextLine(2);
            return;
        }
    }
    int line_corr = 0;
    try {
        line_corr = std::stoi(line_str) - orig_row;
    } catch (const std::out_of_range& e) {
        Error("#line directive requires a positive integer argument", tp->Loc());
        EraseUntilNextLine(2);
        return;
    }
    // Deal with the current preprocessor file name.
    const std::string* fnamep_corr{nullptr};
    auto iter = line_corr_map_.find(*orig_fnamep);
    if (iter != line_corr_map_.cend())
        fnamep_corr = iter->second.second;
    tp = LookAhead();
    // Encoding prefix is not permitted.
    if (tp->Tag() == TokenType::STRING && tp->TokenStr()[0] == '"') {
        // Save this file name. Note that it is the address of the string
        // constructed in the files_ map that should be taked.
        std::string new_fname{UnwrapStr(tp->TokenStr())};
        fnamep_corr = &(files_.emplace(new_fname, "").first->first);
        Next();
        EraseExtraTokensIfHas(3);
    } else if (!IsNewlineToken(*tp) && !IsEndToken(*tp)) {
        Error("invalid filename for #line directive", tp->Loc());
        EraseUntilNextLine(2);
        return;
    } else {
        EraseExtraTokensIfHas(2);
    }
    // Update line_corr_map_
    if (iter != line_corr_map_.cend())
        iter->second = {line_corr, fnamep_corr};
    else
        line_corr_map_.emplace(*orig_fnamep,
                               std::make_pair(line_corr, fnamep_corr));
}

namespace {

// Get the tokens of current macro from the list of tokens. NEWLINE tokens will
// not be copyed into the returned list.
// Return a list with only one token, which should be the macro name token,
// if no left parenthesis is following the macro name. Return an empty list if
// other errors occur.
std::list<Token> GetFuncMacroInst(const std::list<Token>& is,
                                  std::list<Token>::const_iterator& cur_iter) {
    std::list<Token> minst{*cur_iter};
    const SourceLoc& loc = cur_iter->Loc();
    // Jump over these possible NEWLINE token.
    while (true) {
        auto next_iter = std::next(cur_iter, 1);
        if (next_iter == is.cend())
            return minst;
        if (!IsNewlineToken(*next_iter)) {
            if (next_iter->Tag() != TokenType::LPAR)
                return minst;
            break;
        }
        ++cur_iter;
    }
    int unmatch_lpar = 0;
    do {
        ++cur_iter;
        if (cur_iter == is.cend()) {
            --cur_iter;
            Error("unterminated function-like macro invocation", loc);
            return {};
        } else if (cur_iter->Tag() == TokenType::LPAR) {
            ++unmatch_lpar;
        } else if (cur_iter->Tag() == TokenType::RPAR) {
            --unmatch_lpar;
        } else if (IsNewlineToken(*cur_iter)) {
            auto next_iter = std::next(cur_iter, 1);
            if (next_iter != is.cend() && next_iter->Tag() == TokenType::SHARP) {
                // Error then jump to next NEWLINE
                Error("embedding directives within macro arguments is "
                      "not supported", next_iter->Loc());
                do {
                    ++cur_iter;
                } while (cur_iter != is.cend() && !IsNewlineToken(*cur_iter));
                // Move back so two consecutive embedding directives can be
                // reported correctly.
                --cur_iter;
            }
            // Do not copy NEWLINE tokens into the returned list.
            continue;
        }
        minst.push_back(*cur_iter);
    } while (unmatch_lpar != 0);
    return minst;
}

// Parse arguments of specified function-like macro. The instance of
// function-like macro represented by parameter minst should be guaranteed
// to be complete in form.
// Return true if it succeed.
bool ParseMacroArgs(const std::list<Token>& minst,
                    std::vector<std::list<Token>>& ap,
                    int arg_count, bool is_variadic) {
    int arg_index = 0;
    int unmatch_lpar = 0;
    ap.emplace_back();
    if (is_variadic)
        --arg_count;
    auto t_iter = minst.cbegin();
    auto end_iter = std::prev(minst.cend(), 1);
    std::advance(t_iter, 2);
    for (; t_iter != end_iter; ++t_iter) {
        if (t_iter->Tag() != TokenType::COMMA || unmatch_lpar != 0) {
            if (t_iter->Tag() == TokenType::LPAR)
                ++unmatch_lpar;
            else if (t_iter->Tag() == TokenType::RPAR)
                --unmatch_lpar;
            ap[arg_index].push_back(*t_iter);
        } else if (arg_index + 1 >= arg_count && !is_variadic) {
            Error("too many arguments provided to function-like macro"
                  "invocation", t_iter->Loc());
            return false;
        } else if (arg_index + 1 < arg_count ||
                   (arg_index + 1 == arg_count && is_variadic)) {
            ++arg_index;
            ap.emplace_back();
        } else {
            ap[arg_index].push_back(*t_iter);
        }
    }
    if (arg_index + 1 < arg_count) {
        Error("too few arguments provided to function-like macro invocation",
              minst.cbegin()->Loc());
        return false;
    }
    return true;
}

Token Stringize(const std::list<Token>& tl) {
    std::string token_str;
    auto iter = tl.cbegin();
    if (iter == tl.cend())
        return {TokenType::STRING, ""};
    token_str += iter->TokenStr();
    ++iter;
    for (; iter != tl.cend(); ++iter) {
        // C11 6.10.3.2: Each occurrence of white space between the argument’s
        // preprocessing tokens becomes a single space character in the
        // character string literal.
        if (HasPreWhiteSpace(iter->Loc()))
            token_str += ' ';
        token_str += iter->TokenStr();
    }
    token_str = WrapStr(token_str);
    return {TokenType::STRING, token_str};
}

void Glue(std::list<Token>& lhs, const std::list<Token>& rhs,
          const SourceLoc& err_loc) {
    auto l_iter = lhs.rbegin();
    auto r_iter = rhs.cbegin();
    assert(l_iter != lhs.rend() && r_iter != rhs.cend());
    std::string glue_str{l_iter->TokenStr() + r_iter->TokenStr()};
    Scanner scanner{"", glue_str};
    auto tsp = scanner.Scan();
    Token* tp = tsp->Begin();
    lhs.pop_back();
    lhs.push_back(*tp);
    tp = tsp->Next();
    if (!IsEndToken(*tp)) {
        Error("pasting formed '"+ glue_str + "', an invalid preprocessing token",
              err_loc);
        // Still keep these tokens.
        do {
            lhs.push_back(*tp);
            tp = tsp->Next();
        } while (!IsEndToken(*tp));
    }
    lhs.insert(lhs.cend(), std::next(rhs.cbegin(), 1), rhs.cend());
}

template<typename T>
bool VectorHas(std::vector<T> vec, const T& value, int& pos) {
    auto iter = std::find(vec.cbegin(), vec.cend(), value);
    if (iter == vec.cend())
        return false;
    pos = std::distance(vec.cbegin(), iter);
    return true;
}

} // unnamed namespace

// The input token list will never contain the END token but may contain
// NEWLINE token which appear only in a instance of function-like macro.
std::list<Token> Preprocessor::Expand(const std::list<Token>& is) {
    std::list<Token> os;
    for (auto t_iter = is.cbegin(); t_iter != is.cend(); ++t_iter) {
        const Token& cur_t = *t_iter;
        if (!IsIdentOrKeyword(cur_t) || cur_t.HSHas(cur_t.TokenStr()) ||
            !HasMacro(cur_t.TokenStr())) {
            os.push_back(cur_t);
        } else {
            // Deal with __FILE__ and __LINE__ macro.
            if (cur_t.TokenStr() == "__FILE__") {
                os.push_back({TokenType::STRING,
                              WrapStr(*cur_t.Loc().fnamep),
                              cur_t.LocPtr()});
            } else if (cur_t.TokenStr() == "__LINE__") {
                os.push_back({TokenType::I_CONSTANT,
                              std::to_string(cur_t.Loc().row),
                              cur_t.LocPtr()});
            } else {
                // Deal with other macros.
                std::list<Token> subst_os;
                Token::HideSet hs{cur_t.GetHideSet()};
                const Macro* mp = GetMacro(cur_t.TokenStr());
                auto locp = cur_t.LocPtr();
                if (mp->IsObjLike()) {
                    Subst(mp->Repl(), subst_os,
                          HSUnion(hs, {cur_t.TokenStr()}), {}, {}, locp);
                } else {
                    std::list<Token> minst = GetFuncMacroInst(is, t_iter);
                    if (minst.empty())
                        continue;
                    if (minst.size() == 1) {
                        os.push_back(*minst.cbegin());
                        continue;
                    }
                    const std::vector<std::string>& fp{mp->Params()};
                    std::vector<std::list<Token>> ap;
                    if (ParseMacroArgs(minst, ap, fp.size(), mp->IsVariadic())) {
                        // After calling GetFuncMacroInst, t_iter should point
                        // to the end of current function-like macro, which
                        // should be right parenthesis.
                        hs = HSIntersect(hs, t_iter->GetHideSet());
                        hs = HSUnion(hs, {cur_t.TokenStr()});
                        Subst(mp->Repl(), subst_os, hs, fp, ap, locp);
                    } else {
                        os.splice(os.cend(), minst);
                        continue;
                    }
                }
                os.splice(os.cend(), Expand(subst_os));
            }
        }
    }
    return os;
}

void Preprocessor::Subst(const std::list<Token>& is, std::list<Token>& os,
                         const Token::HideSet& hideset,
                         const std::vector<std::string>& fp,
                         const std::vector<std::list<Token>>& ap,
                         std::shared_ptr<SourceLoc> locp) {
    int pos;
    for (auto t_iter = is.cbegin(); t_iter != is.cend(); ++t_iter) {
        const Token& cur_t = *t_iter;
        auto next_iter = std::next(t_iter, 1);
        if (t_iter != is.cend()) {
            const Token& next_t = *next_iter;
            ++t_iter;
            if (cur_t.Tag() == TokenType::SHARP &&
                VectorHas(fp, next_t.TokenStr(), pos)) {
                os.push_back(Stringize(ap[pos]));
                continue;
            } else if (cur_t.Tag() == TokenType::DSHARP &&
                       VectorHas(fp, next_t.TokenStr(), pos)) {
                if (!ap[pos].empty())
                    Glue(os, ap[pos], *locp);
                continue;
            } else if (cur_t.Tag() == TokenType::DSHARP) {
                Glue(os, {next_t}, *locp);
                continue;
            } else if (VectorHas(fp, cur_t.TokenStr(), pos) &&
                       next_t.Tag() == TokenType::DSHARP) {
                if (ap[pos].empty()) {
                    auto next2_iter = std::next(next_iter, 1);
                    if (next2_iter == is.cend())
                        break;
                    const Token& next2_t = *next2_iter;
                    if (VectorHas(fp, next2_t.TokenStr(), pos)) {
                        os.insert(os.cend(), ap[pos].cbegin(), ap[pos].cend());
                        ++t_iter;
                    }
                } else {
                    --t_iter;
                    os.insert(os.cend(), ap[pos].cbegin(), ap[pos].cend());
                }
                continue;
            }
            --t_iter;
        }
        if (VectorHas(fp, cur_t.TokenStr(), pos))
            os.splice(os.cend(), Expand(ap[pos]));
        else
            os.push_back(cur_t);
    }
    // Add hideset to the tokens in os and set location pointer
    for (auto& t : os) {
        t.HSAdd(hideset);
        t.SetLocPtr(locp);
    }
}

// The cursor of token sequence will be moved to the last token of current
// macro. NEWLINE tokens interspersed in the macro will be copyed into the
// returned list.
std::list<Token> Preprocessor::GetCurMacroInst() {
    const Macro* mp = GetMacro(CurToken()->TokenStr());
    if (mp == nullptr)
        return {};
    std::list<Token> minst{*CurToken()};
    if (mp->IsObjLike())
        return minst;
    // Deal with function-like macro
    // Jump over these possible NEWLINE token.
    Token* tp = LookAhead();
    while (IsNewlineToken(*tp)) {
        minst.push_back(*tp);
        Next();
        tp = LookAhead();
    }
    if (tp->Tag() != TokenType::LPAR)
        return minst;
    int unmatch_lpar = 0;
    do {
        tp = Next();
        if (IsEndToken(*tp))
            break;
        if (tp->Tag() == TokenType::LPAR)
            ++unmatch_lpar;
        if (tp->Tag() == TokenType::RPAR)
            --unmatch_lpar;
        minst.push_back(*CurToken());
    } while (unmatch_lpar != 0);
    return minst;
}

void Preprocessor::ExpandCurMacro() {
    std::list<Token> cur_macro = GetCurMacroInst();
    int macro_size = cur_macro.size();
    if (IsEndToken(*CurToken()))
        ++macro_size;
    // Do not need to re-traversal these expanded tokens.
    if (macro_size != 0)
        ts_.ReplacePrevN(macro_size, Expand(cur_macro), false);
}

// The NEWLINE token at the end of current line should be kept.
void Preprocessor::ExpandCurLine() {
    Token* tp = LookAhead();
    std::list<Token> cur_line;
    while (!IsNewlineToken(*tp) && !IsEndToken(*tp)) {
        cur_line.push_back(*tp);
        Next();
        tp = LookAhead();
    }
    ts_.ReplacePrevN(cur_line.size(), Expand(cur_line));
}

void Preprocessor::EraseUntilNextLine(int prevn) {
    int extra_count = prevn + 1;
    Token* tp = CurToken();
    while (!IsNewlineToken(*tp) && !IsEndToken(*tp)) {
        ++extra_count;
        tp = Next();
    }
    ts_.ErasePrevN(extra_count);
}

void Preprocessor::EraseExtraTokensIfHas(int prevn) {
    Token* tp = Next();
    if (!IsNewlineToken(*tp) && !IsEndToken(*tp))
        Warning("extra tokens at end of directive", tp->Loc());
    EraseUntilNextLine(prevn + 1);
}

Token* Preprocessor::PPLineCorrect(Token* tp) {
    SourceLoc& loc = *tp->LocPtr();
    // Each token should be processed only once.
    if (!loc.ppline_corrected) {
        auto iter = line_corr_map_.find(*loc.fnamep);
        if (iter != line_corr_map_.cend()) {
            loc.row += iter->second.first;
            if (iter->second.second)
                loc.fnamep = iter->second.second;
            loc.ppline_corrected = true;
        }
    }
    return tp;
}

}
