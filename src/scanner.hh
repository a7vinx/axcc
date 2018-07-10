#ifndef _AXCC_SCANNER_HH_
#define _AXCC_SCANNER_HH_

#include <string>
#include <memory>
#include <cstdlib>
#include <unordered_map>

#include "token.hh"

namespace axcc {

class Scanner {
public:
    Scanner(const std::string& fname, const std::string& fcontent)
        : fname_{fname},
          fcontent_{fcontent},
          cur_linep_{fcontent_.cbegin()},
          cur_charp_{fcontent_.cbegin()},
          cur_line_len_{FindNextNewline()},
          tsp_{std::make_unique<TokenSequence>()} {}
    Scanner(const Scanner&) = delete;
    Scanner(Scanner&&) = delete;
    Scanner& operator=(const Scanner&) = delete;
    Scanner& operator=(Scanner&&) = delete;
    virtual ~Scanner() = default;

    std::unique_ptr<TokenSequence> Scan();

private:
    friend class ScannerTest;
    // Return 0 if it reaches the end.
    char Begin();
    char CurChar() const;
    // Next() can not be called if it has already reached the end.
    char Next();
    char NextN(int n);
    char LookAhead() const { return LookAheadN(1); }
    char LookAheadN(int n) const;
    bool NextIs(char c) const { return LookAhead() == c; }
    bool Try(char c);
    bool Try(const std::string& chars);
    // Return the distance from current position to next newline.
    unsigned int FindNextNewline() const;
    void MakeTokenInTS(const TokenType& tag, const std::string& token_str,
                       const SourceLocation& loc);
    void MakeTokenInTS(const TokenType& tag, const SourceLocation& loc);
    void MakeTokenInTS(const TokenType& tag, const std::string& token_str);
    void MakeTokenInTS(const TokenType& tag);
    SourceLocation SaveCurLoc() {
        return {&fname_, cur_row_, cur_column_,
                cur_linep_, cur_line_len_, false}; }

    void SkipComment();
    void ScanNumConstant();
    void ScanCharConstant();
    void ScanStrLiteral();
    void ScanIdent();

    // Use reference for file name and content string because SourceLocation
    // object which we have to create when scanning has pointer and iterator
    // members related to these string.
    const std::string& fname_;
    const std::string& fcontent_;
    unsigned int cur_row_{1};
    unsigned int cur_column_{1};
    std::string::const_iterator cur_linep_;
    std::string::const_iterator cur_charp_;
    std::size_t cur_line_len_;
    std::unique_ptr<TokenSequence> tsp_;
    static const std::unordered_map<std::string, TokenType> kKeyToType_;
};

// Check whether the specified file exists.
bool FileExist(const std::string& fname);
// Maybe we can return a smart pointer?
std::string ReadFile(const std::string& fname);
// void SubstTrigraph(const std::string& fcontent);

}
#endif
