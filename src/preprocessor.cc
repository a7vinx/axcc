#include <ctime>
#include <stack>

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
    : ts_{ts}, files_{files}, header_paths_{header_paths}, macros_{} {
    AddInitMacros();
    AddInitHeaderPaths();
}

Preprocessor::~Preprocessor() = default;

void Preprocessor::Preprocess() {
    // TODO
}

bool Preprocessor::AddMacro(const Macro& macro) {
    bool has_added = true;
    std::string ident{macro.Ident()};
    auto iter = macros_.find(ident);
    if (iter == macros_.cend())
        has_added = false;
    else
        macros_.erase(iter);
    macros_.emplace(ident, std::make_unique<Macro>(macro));
    return has_added;
}

void Preprocessor::RmMacro(const std::string& ident) {
    auto iter = macros_.find(ident);
    if (iter != macros_.cend())
        macros_.erase(iter);
}

const Macro* Preprocessor::GetMacro(const std::string& ident) {
    auto iter = macros_.find(ident);
    if (iter != macros_.cend())
        return &iter->second;
    else
        return nullptr;
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
