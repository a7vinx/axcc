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
    // TODO
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

}
