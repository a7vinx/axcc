#include <ctime>
#include <stack>
#include <sstream>

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

    void CondBegin(bool state, const SourceLocation& loc) {
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
    SourceLocation BeginLoc() const { return conditions_.top().begin_loc; }

private:
    struct PPCondition {
        // Indicate whether current conditional preprocessing block need to be
        // processed.
        bool state;
        bool has_next_else;
        const SourceLocation begin_loc;
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
    const SourceLocation& macro_loc = tp->Loc();
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
    SourceLocation& loc = *tp->LocPtr();
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
