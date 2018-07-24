#ifndef _AXCC_PREPROCESSER_HH_
#define _AXCC_PREPROCESSER_HH_

#include <string>
#include <vector>
#include <list>
#include <map>
#include <memory>
#include <utility>

#include "token.hh"

namespace axcc {

class Preprocessor {
public:
    Preprocessor(TokenSequence& ts,
                 std::map<std::string, std::string>& files,
                 const std::list<std::string>& header_paths);
    Preprocessor(const Preprocessor&) = delete;
    Preprocessor(Preprocessor&&) = delete;
    Preprocessor& operator=(const Preprocessor&) = delete;
    Preprocessor& operator=(Preprocessor&&) = delete;
    virtual ~Preprocessor();

    void Preprocess();

private:
    class Macro;
    class PPConditions;

    // The adding action will always be performed unless the new definition is
    // effectively the same as the old one.
    // Return true if it is a redefinition. Note that if the new definition is
    // effectively the same, it will return false.
    bool AddMacro(const Macro& macro);
    void RmMacro(const std::string& ident);
    bool HasMacro(const std::string& ident) const {
        return macros_.find(ident) != macros_.cend(); }
    const Macro* GetMacro(const std::string& ident) const;
    void AddHeaderPath(const std::string& path) {
        header_paths_.push_back(path); }
    // Return the path of the found file and return an empty string if not found.
    std::string FindHeader(const std::string& fname,
                           bool include_cur_path,
                           const std::string& cur_path);
    void AddInitMacros();
    void AddInitHeaderPaths();

    // Directives' parser and handler
    void ParsePPIf();
    void ParsePPElse(int& wait_del);
    void ParsePPIfdef();
    void ParsePPIfndef();
    void ParsePPElif(int& wait_del);
    void ParsePPEndif(int& wait_del);
    void ParsePPDefine();
    void ParsePPUndef();
    void ParsePPInclude();
    void ParsePPError();
    void ParsePPLine();

    // Other helper functions
    // Dave Prosser's C macro expanding algorithm
    std::list<Token> Expand(const std::list<Token>& is);
    void Subst(const std::list<Token>& is, std::list<Token>& os,
               const Token::HideSet& hideset,
               const std::vector<std::string>& fp,
               const std::vector<std::list<Token>>& ap,
               std::shared_ptr<SourceLocation> locp);
    // Get the tokens of current macro from the token sequence.
    std::list<Token> GetCurMacroInst();
    // Expand current object-like macro or function-like macro.
    void ExpandCurMacro();
    // Expand tokens from the next one to the next NEWLINE token.
    void ExpandCurLine();
    // Delete tokens from the current position to the next NEWLINE or END token,
    // plus the number of previous tokens specified by parameter prevn (does not
    // include the current token).
    void EraseUntilNextLine(int prevn = 0);
    void EraseExtraTokensIfHas(int prevn = 0);
    // Wrapper functions of iteration functions of TokenSequence class.
    Token* Begin() { return ts_.Begin(); }
    Token* Next() { return PPLineCorrect(ts_.Next()); }
    Token* CurToken() { return ts_.CurToken(); }
    Token* LookAhead() { return LookAheadN(1); }
    Token* LookAheadN(int n) { return PPLineCorrect(ts_.LookAheadN(n)); }
    Token* PPLineCorrect(Token* tp);

    TokenSequence& ts_;
    std::map<std::string, std::string>& files_;
    std::list<std::string> header_paths_;
    // Due to the incomplete type of Macro class, this map can neither be
    // specialized with Macro class nor be initialized using member initializer.
    std::map<std::string, std::unique_ptr<Macro>> macros_;
    std::unique_ptr<PPConditions> conditionsp_;
    // Store the correction of the current line number and file name pointer
    // caused by #line directive. Int is enough here.
    // Each #line directive should only be used to correct the location
    // information of tokens which are in the same file with it.
    std::map<std::string, std::pair<int, const std::string*>> line_corr_map_{};
};

}
#endif
