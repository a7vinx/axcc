#include <iterator>
#include <sstream>
#include <fstream>
#include <cctype>
#include <cassert>

#include "scanner.hh"
#include "error.hh"

namespace axcc {

const std::unordered_map<std::string, TokenType> Scanner::kKeyToType_{
    {"void", TokenType::VOID},
    {"char", TokenType::CHAR},
    {"short", TokenType::SHORT},
    {"int", TokenType::INT},
    {"long", TokenType::LONG},
    {"float", TokenType::FLOAT},
    {"double", TokenType::DOUBLE},
    {"_Bool", TokenType::BOOL},
    {"unsigned", TokenType::UNSIGNED},
    {"signed", TokenType::SIGNED},
    {"struct", TokenType::STRUCT},
    {"union", TokenType::UNION},
    {"enum", TokenType::ENUM},
    {"const", TokenType::CONST},
    {"volatile", TokenType::VOLATILE},
    {"restrict", TokenType::RESTRICT},
    {"_Atomic", TokenType::ATOMIC},
    {"_Complex", TokenType::COMPLEX},
    {"_Imaginary", TokenType::IMAGINARY},
    {"static", TokenType::STATIC},
    {"extern", TokenType::EXTERN},
    {"auto", TokenType::AUTO},
    {"register", TokenType::REGISTER},
    {"_Thread_local", TokenType::THREAD_LOCAL},
    {"inline", TokenType::INLINE},
    {"_Noreturn", TokenType::NO_RETURN},
    {"if", TokenType::IF},
    {"else", TokenType::ELSE},
    {"for", TokenType::FOR},
    {"while", TokenType::WHILE},
    {"do", TokenType::DO},
    {"break", TokenType::BREAK},
    {"switch", TokenType::SWITCH},
    {"case", TokenType::CASE},
    {"default", TokenType::DEFAULT},
    {"goto", TokenType::GOTO},
    {"continue", TokenType::CONTINUE},
    {"typedef", TokenType::TYPEDEF},
    {"return", TokenType::RETURN},
    {"_Static_assert", TokenType::STATIC_ASSERT},
    {"_Generic", TokenType::GENERIC},
    {"sizeof", TokenType::SIZEOF},
    {"_Alignas", TokenType::ALIGNAS},
    {"_Alignof", TokenType::ALIGNOF},
};

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
                if (NextIs('\'')) { ScanCharConstant(); break; }
                else if (NextIs('\"')) { ScanStrLiteral(); break; }
            default:
                if (isspace(curc))
                    break;
                else if (curc >= '0' && curc <= '9')
                    ScanNumConstant();
                else if ((curc >= 'a' && curc <= 'z') ||
                         (curc >= 'A' && curc <= 'Z') || curc == '_')
                    ScanIdent();
                else
                    MakeTokenInTS(TokenType::INVALID);
        }
        curc = Next();
    }
    tsp_->SetEndLoc(SaveCurLoc());
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
        if (cur_charp_ == fcontent_.cend())
            return 0;
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

void Scanner::MakeTokenInTS(const TokenType& tag, const std::string& token_str,
                            const SourceLocation& loc) {
    tsp_->EmplaceBack(tag, token_str,
                      std::make_shared<const SourceLocation>(loc));
}

void Scanner::MakeTokenInTS(const TokenType& tag, const SourceLocation& loc) {
    tsp_->EmplaceBack(tag, std::make_shared<const SourceLocation>(loc));
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
    assert(CurChar() == '/' && (NextIs('/') || NextIs('*')));
    SourceLocation loc{SaveCurLoc()};
    if (Try('/')) {
        // The '\n' should not be "eaten" because we need to generate the
        // NEWLINE token after the comment.
        while (!NextIs(0) && !NextIs('\n')) { Next(); }
    } else if (Try('*')) {
        // Next() will be called immediately after this function so we should
        // use LookAhead() instead of Next() to check if the end of the file
        // has been reached.
        while (!NextIs(0)) {
            if (Try("*/"))
                return;
            Next();
        }
        Error("unterminated /* comment", loc);
    }
}

// Do as little work as possible to distinguish between I_CONSTANT and
// F_CONSTANT.
void Scanner::ScanNumConstant() {
    char curc = CurChar();
    SourceLocation loc{SaveCurLoc()};
    TokenType tag = TokenType::I_CONSTANT;
    auto begin_charp = cur_charp_;
    bool has_invalid = false;
    bool has_hex_prefix = false;

    if (curc == '.')
        tag = TokenType::F_CONSTANT;
    char peekc = LookAheadN(2);
    if ((isxdigit(peekc) || peekc == '.') &&
        (curc = '0' && (Try('x') || Try('X'))))
        has_hex_prefix = true;
    while ((peekc = LookAhead()) != 0) {
        if (isdigit(peekc) || isalpha(peekc) || peekc == '.' || peekc == '_') {
            if (peekc == 'e' || peekc == 'E' || peekc == 'p' || peekc == 'P') {
                if (LookAheadN(2) == '+' || LookAheadN(2) == '-')
                    Next();
                if (has_hex_prefix && !has_invalid &&
                    (peekc == 'p' || peekc == 'P'))
                    tag = TokenType::F_CONSTANT;
                if (!has_hex_prefix && !has_invalid &&
                    (peekc == 'e' || peekc == 'E'))
                    tag = TokenType::F_CONSTANT;
            }
            if ((peekc == '.') && !has_invalid)
                tag = TokenType::F_CONSTANT;
            if (!isdigit(peekc) && !(has_hex_prefix && isxdigit(peekc)))
                has_invalid = true;
            Next();
            continue;
        }
        break;
    }
    MakeTokenInTS(tag, {begin_charp, std::next(cur_charp_, 1)}, loc);
}

void Scanner::ScanCharConstant() {
    char curc = CurChar();
    SourceLocation loc{SaveCurLoc()};
    auto begin_charp = cur_charp_;

    assert(((curc == 'u' || curc == 'U' || curc == 'L') && NextIs('\'')) ||
           curc == '\'');
    if (curc != '\'') Next();
    while (!Try('\'')) {
        if (NextIs(0)) {
            MakeTokenInTS(TokenType::INVALID, loc);
            Error("missing terminating ' character", loc);
            return;
        }
        if (LookAheadN(2) != 0)
            Try('\\');
        Next();
    }
    MakeTokenInTS(TokenType::C_CONSTANT,
                  {begin_charp, std::next(cur_charp_, 1)}, loc);
}

void Scanner::ScanStrLiteral() {
    char curc = CurChar();
    SourceLocation loc{SaveCurLoc()};
    auto begin_charp = cur_charp_;

    assert((curc == 'u' && NextIs('8') && LookAheadN(2) == '\"') ||
           ((curc == 'u' || curc == 'U' || curc == 'L') && NextIs('\"')) ||
           curc == '\"');
    if (curc != '\"') Next();
    if (CurChar() != '\"') Next();
    while (!Try('\"')) {
        if (NextIs(0)) {
            MakeTokenInTS(TokenType::INVALID, loc);
            Error("missing terminating '\"' character", loc);
            return;
        }
        if (LookAheadN(2) != 0)
            Try('\\');
        Next();
    }
    MakeTokenInTS(TokenType::STRING,
                  {begin_charp, std::next(cur_charp_, 1)}, loc);
}

void Scanner::ScanIdent() {
    auto begin_charp = cur_charp_;
    SourceLocation loc{SaveCurLoc()};
    char peekc;

    while ((peekc = LookAhead()) != 0) {
        if (Try("\\u") || Try("\\U"))
            continue;
        if (!isalpha(peekc) && !isdigit(peekc) && peekc != '_')
            break;
        Next();
    }
    std::string token_str{begin_charp, std::next(cur_charp_, 1)};
    auto iter = kKeyToType_.find(token_str);
    if (iter != kKeyToType_.cend()) {
        MakeTokenInTS(iter->second, loc);
        return;
    }
    MakeTokenInTS(TokenType::IDENTIFIER, token_str, loc);
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
