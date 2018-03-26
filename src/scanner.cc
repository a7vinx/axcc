#include <iterator>
#include <sstream>
#include <fstream>

#include "scanner.hh"
#include "error.hh"

namespace axcc {

std::unique_ptr<TokenSequence> Scanner::Scan() {
}

char Scanner::Begin() const {
    if (cur_charp_ == fcontent_.cend())
        return 0;
    return *cur_charp_;
}

char Scanner::CurChar() const {
    if (cur_charp_ != fcontent_.cend())
        return *cur_charp_;
    return 0;
}

char Scanner::Next() {
    if (*cur_charp_ == '\n') {
        std::advance(cur_charp_, 1);
        ++cur_row_;
        cur_column_ = 1;
        cur_linep_ = cur_charp_;
        cur_line_len_ = FindNext('\n');
        if (cur_line_len_ == 0)
            cur_line_len_ = std::distance(cur_linep_, fcontent_.cend());
    } else {
        std::advance(cur_charp_, 1);
        ++cur_column_;
    }
    if (cur_charp_ == fcontent_.cend())
        return 0;
    return *cur_charp_;
}

char Scanner::NextN(int n) {
    for (int i = 0; i < n - 1; i++)
        Next();
    return Next();
}

char Scanner::LookAheadN(int n) const {
    if (std::distance(cur_charp_, fcontent_.cend()) <= n)
        return 0;
    return *std::next(cur_charp_, n);
}

bool Scanner::Try(char c) {
    if (LookAhead() == c) {
        Next();
        return true;
    }
    return false;
}

bool Scanner::Try(const std::string& chars) {
    int n = chars.size();
    for (int i = 1; i <= n; i++) {
        if (LookAheadN(i) != chars[i - 1])
            return false;
    }
    NextN(n);
    return true;
}

unsigned int Scanner::FindNext(char c) const {
    unsigned int distance = 0;
    auto cur_charp = cur_charp_;
    while (++distance) {
        std::advance(cur_charp, 1);
        if (cur_charp == fcontent_.cend())
           return 0;
        if (*cur_charp == c)
           break;
    }
    return distance;
}

void Scanner::MakeTokenInTS(const TokenType& tag, const std::string& token_str) {
    tsp_->EmplaceBack(tag, token_str,
                      std::make_shared<const SourceLocation>(
                               SourceLocation{
                                   &fname_, cur_row_, cur_column_,
                                   cur_linep_, cur_line_len_}));
}

void Scanner::MakeTokenInTS(const TokenType& tag) {
    tsp_->EmplaceBack(tag, std::make_shared<const SourceLocation>(
                                    SourceLocation{
                                        &fname_, cur_row_, cur_column_,
                                        cur_linep_, cur_line_len_}));
}

std::string ReadFile(const std::string& fname) {
    std::ifstream infile{fname};
    if (!infile.is_open()) {
        Error("No such file or directory: '" + fname + "'");
        return {};
    }
    std::stringstream buffer;
    buffer << infile.rdbuf();
    return buffer.str();
}

}
