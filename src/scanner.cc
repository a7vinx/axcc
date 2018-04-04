#include <iterator>
#include <sstream>
#include <fstream>
#include <cctype>
#include <cassert>

#include "scanner.hh"
#include "error.hh"

namespace axcc {

std::unique_ptr<TokenSequence> Scanner::Scan() {
    char curc = Begin();
    while (curc != 0) {
        switch(curc) {
            case '(': MakeTokenInTS(TokenType::LPAR); break;
            case ')': MakeTokenInTS(TokenType::RPAR); break;
            case '{': MakeTokenInTS(TokenType::LBRACE); break;
            case '}': MakeTokenInTS(TokenType::RBRACE); break;
            case '[': MakeTokenInTS(TokenType::LSBRACKET); break;
            case ']': MakeTokenInTS(TokenType::RSBRACKET); break;
            case '?': MakeTokenInTS(TokenType::QUES); break;
            case '~': MakeTokenInTS(TokenType::TILDE); break;
            case ',': MakeTokenInTS(TokenType::COMMA); break;
            case ';': MakeTokenInTS(TokenType::SEMI); break;
            case '.':
                if (isdigit(LookAhead())) ScanNumConstant();
                else if (Try("..")) MakeTokenInTS(TokenType::ELLIP);
                else MakeTokenInTS(TokenType::DOT);
                break;
            case ':':
                if (Try('>')) MakeTokenInTS(TokenType::LSBRACKET);
                else MakeTokenInTS(TokenType::COLON);
                break;
            case '!':
                if (Try('=')) MakeTokenInTS(TokenType::NEQ);
                else MakeTokenInTS(TokenType::EXCL);
                break;
            case '*':
                if (Try('=')) MakeTokenInTS(TokenType::MUL_ASGN);
                else MakeTokenInTS(TokenType::AST);
                break;
            case '%':
                if (Try('=')) MakeTokenInTS(TokenType::MOD_ASGN);
                else if (Try('>')) MakeTokenInTS(TokenType::RBRACE);
                else if (Try(":%:")) MakeTokenInTS(TokenType::DSHARP);
                else if (Try(':')) MakeTokenInTS(TokenType::SHARP);
                else MakeTokenInTS(TokenType::PCT);
                break;
            case '&':
                if (Try('&')) MakeTokenInTS(TokenType::LOGICAL_AND);
                else if (Try('=')) MakeTokenInTS(TokenType::AND_ASGN);
                else MakeTokenInTS(TokenType::AMP);
                break;
            case '|':
                if (Try('|')) MakeTokenInTS(TokenType::LOGICAL_OR);
                else if (Try('=')) MakeTokenInTS(TokenType::OR_ASGN);
                else MakeTokenInTS(TokenType::VBAR);
                break;
            case '^':
                if (Try('=')) MakeTokenInTS(TokenType::XOR_ASGN);
                else MakeTokenInTS(TokenType::CARET);
                break;
            case '=':
                if (Try('=')) MakeTokenInTS(TokenType::EQ);
                else MakeTokenInTS(TokenType::ASGN);
                break;
            case '+':
                if (Try('+')) MakeTokenInTS(TokenType::INC);
                else if (Try('=')) MakeTokenInTS(TokenType::ADD_ASGN);
                else MakeTokenInTS(TokenType::PLUS);
                break;
            case '-':
                if (Try('-')) MakeTokenInTS(TokenType::DEC);
                else if (Try('>')) MakeTokenInTS(TokenType::ARROW);
                else if (Try('=')) MakeTokenInTS(TokenType::SUB_ASGN);
                else MakeTokenInTS(TokenType::MINUS);
                break;
            case '#':
                if (Try('#')) MakeTokenInTS(TokenType::DSHARP);
                else MakeTokenInTS(TokenType::SHARP);
                break;
            case '<':
                if (Try("<=")) MakeTokenInTS(TokenType::SHL_ASGN);
                else if (Try('<')) MakeTokenInTS(TokenType::SHL);
                else if (Try('=')) MakeTokenInTS(TokenType::LESS_EQ);
                else if (Try(':')) MakeTokenInTS(TokenType::LSBRACKET);
                else if (Try('%')) MakeTokenInTS(TokenType::LBRACE);
                else MakeTokenInTS(TokenType::LABRACKET);
                break;
            case '>':
                if (Try(">=")) MakeTokenInTS(TokenType::SHR_ASGN);
                else if (Try('>')) MakeTokenInTS(TokenType::SHR);
                else if (Try('=')) MakeTokenInTS(TokenType::GREATER_EQ);
                else MakeTokenInTS(TokenType::RABRACKET);
                break;
            case '/':
                if (NextIs('/') || NextIs('*')) SkipComment();
                else if (Try('=')) MakeTokenInTS(TokenType::DIV_ASGN);
                else MakeTokenInTS(TokenType::SLASH);
                break;
            case '\\':
                if (NextIs('u') || NextIs('U')) ScanIdent();
                else MakeTokenInTS(TokenType::INVALID);
                break;
            case '\'':
                ScanCharConstant(); break;
            case '\"':
                ScanStrLiteral(); break;
            case '\n':
                MakeTokenInTS(TokenType::NEWLINE);
                break;
            case 'u':
                if (NextIs('8') &&
                    LookAheadN(2) == '\"') { ScanStrLiteral(); break; }
            case 'U':
            case 'L':
                if (NextIs('\'')) ScanCharConstant();
                else if (NextIs('\"')) ScanStrLiteral();
                break;
            default:
                if (isspace(curc))
                    break;
                else if (curc >= '0' && curc <= '9')
                    ScanNumConstant();
                else if ((curc >= 'a' && curc <= 'z') ||
                         (curc >= 'A' && curc <= 'Z') ||
                         curc == '_' || curc == '$')
                    ScanIdent();
                else
                    MakeTokenInTS(TokenType::INVALID);
        }
        curc = Next();
    }
    return std::move(tsp_);
}

char Scanner::Begin() {
    if (cur_charp_ == fcontent_.cend())
        return 0;
    // Deal with backslash-newline
    if (*cur_charp_ == '\\' && *std::next(cur_charp_, 1) == '\n') {
        std::advance(cur_charp_, 1);
        return Next();
    }
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
    // Deal with backslash-newline
    if (*cur_charp_ == '\\' && *std::next(cur_charp_, 1) == '\n') {
        std::advance(cur_charp_, 1);
        ++cur_column_;
        return Next();
    }
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
    auto iter = cur_charp_;
    auto end_iter = std::next(iter, n);
    do {
        std::advance(iter, 1);
        // Deal with backslash-newline
        if (*iter == '\\' && *std::next(iter, 1) == '\n') {
            if (std::distance(end_iter, fcontent_.cend()) <= 2)
                return 0;
            std::advance(end_iter, 2);
            std::advance(iter, 1);
        }
    } while (iter != end_iter);
    return *end_iter;
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
    do {
        if (cur_charp == fcontent_.cend())
           return 0;
        if (*cur_charp == c && cur_charp != cur_charp_)
           break;
        std::advance(cur_charp, 1);
    } while (++distance);
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

void Scanner::SkipComment() {
}

void Scanner::ScanNumConstant() {
}

void Scanner::ScanCharConstant() {
}

void Scanner::ScanStrLiteral() {
}

void Scanner::ScanIdent() {
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
