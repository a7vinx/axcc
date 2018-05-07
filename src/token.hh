#ifndef _AXCC_TOKEN_HH_
#define _AXCC_TOKEN_HH_

#include <string>
#include <list>
#include <vector>
#include <iterator>
#include <memory>
#include <cstdlib>
#include <utility>

namespace axcc {

struct SourceLocation {
    const std::string* fnamep;
    unsigned int row;
    unsigned int column;
    std::string::const_iterator linep;
    std::size_t line_len;
};

// Return string like "token.hh:19:36: source code"
std::string LocStr(const SourceLocation& loc);

enum class TokenType {
    // Punctuators
    DOT, COMMA, COLON, SEMI,
    EXCL, QUES, AST, PCT,
    AMP, VBAR, TILDE, CARET,
    ASGN, PLUS, MINUS, SHARP,
    SLASH, LPAR, RPAR,
    LBRACE, RBRACE,
    LSBRACKET, RSBRACKET,
    LABRACKET, RABRACKET,
    // multi-punctuator operators
    LOGICAL_AND, LOGICAL_OR,
    SHL, SHR, INC, DEC, EQ, NEQ,
    LESS_EQ, GREATER_EQ,
    ADD_ASGN, SUB_ASGN, MUL_ASGN,
    DIV_ASGN, AND_ASGN, OR_ASGN,
    XOR_ASGN, SHL_ASGN, SHR_ASGN,
    MOD_ASGN, ARROW, ELLIP, DSHARP,
    // Type specifiers
    VOID, CHAR, SHORT, INT,
    LONG, FLOAT, DOUBLE, BOOL,
    UNSIGNED, SIGNED,
    STRUCT, UNION, ENUM,
    // Type qualifiers
    // _Atomic can also be type specifier
    CONST, VOLATILE, RESTRICT,
    ATOMIC, COMPLEX, IMAGINARY,
    // Storage-class specifiers
    STATIC, EXTERN, AUTO, REGISTER, THREAD_LOCAL,
    // Function specifiers
    INLINE, NO_RETURN,
    // Other keywords
    IF, ELSE, FOR, WHILE, DO, BREAK,
    SWITCH, CASE, DEFAULT, GOTO,
    CONTINUE, TYPEDEF, RETURN,
    STATIC_ASSERT, GENERIC,
    SIZEOF, ALIGNAS, ALIGNOF,
    // Preprocessor directives
    ELIF, ENDIF, INCLUDE, LINE, ERROR,
    DEF, IFDEF, IFNDEF, UNDEF,
    // Users' tokens
    IDENTIFIER, COMMENT, STRING,
    I_CONSTANT, F_CONSTANT, C_CONSTANT,
    // Others
    NEWLINE, INVALID
};

class Token {
public:
    Token(const TokenType& tag, const std::string& token_str,
          const std::shared_ptr<const SourceLocation>& locp)
        : tag_{tag}, token_str_{token_str}, locp_{locp} {}
    Token(const TokenType& tag,
          const std::shared_ptr<const SourceLocation>& locp)
        : tag_{tag}, locp_{locp} {}
    // No loction pointer version for constructing the token whose location
    // pointer is meaningless.
    Token(const TokenType& tag, const std::string& token_str)
        : tag_{tag}, token_str_{token_str} {}
    Token(const TokenType& tag) : tag_{tag} {}
    Token(const Token&) = default;
    Token(Token&&) = default;
    Token& operator=(const Token&) = delete;
    Token& operator=(Token&&) = delete;
    virtual ~Token() = default;

    TokenType Tag() const { return tag_; }
    std::string TokenStr() const { return token_str_; }
    const SourceLocation& Loc() const { return *locp_; }
    std::shared_ptr<const SourceLocation> LocPtr() const { return locp_; }
    void SetLocPtr(const std::shared_ptr<const SourceLocation>& locp) {
        locp_ = locp;
    }

private:
    const TokenType tag_;
    // Integer and float constant is stored as string for now.
    const std::string token_str_{};
    std::shared_ptr<const SourceLocation> locp_{};
};

inline bool operator==(const Token& lhs, const Token& rhs) {
    return (lhs.Tag() == rhs.Tag()) && (lhs.TokenStr() == rhs.TokenStr());
}
inline bool operator!=(const Token& lhs, const Token& rhs) {
    return !(lhs == rhs);
}

class TokenSequence {
public:
    TokenSequence() = default;
    // Deep copy
    TokenSequence(const TokenSequence& other);
    TokenSequence(TokenSequence&&) = default;
    TokenSequence& operator=(const TokenSequence&) = delete;
    TokenSequence& operator=(TokenSequence&&) = delete;
    virtual ~TokenSequence() = default;

    // Simple wrapper functions for adding tokens from scratch one by one.
    template<typename... Args>
    void EmplaceBack(Args&&... args) {
        token_list_.emplace_back(
                        std::make_unique<Token>(std::forward<Args>(args)...));
    }
    void PushBack(const Token& t) {
        token_list_.push_back(std::make_unique<Token>(t));
    }

    // Helper functions for iterating token sequence.
    // All the following functions can only be used after Begin() has been
    // called and the parameter n, if it has, should be positive (no check
    // is performed inside).
    // (Maybe we can add a Token::END type so that we can return reference
    // type here?)
    Token* Begin();
    Token* Next();
    Token* CurToken();
    Token* LookAhead() const { return LookAheadN(1); }
    Token* LookAheadN(int n) const;
    // Wrapper functions for adjusting the token list.
    // The previous n tokens indicated by the parameter n include the current
    // token (i.e., if n = 1 only the current token will be erased).
    void ErasePrevN(int n) {
        token_list_.erase(std::prev(token_list_iter_, n), token_list_iter_);
    }
    void ReplacePrevN(int n, TokenSequence&& ts);
    void ReplacePrevN(int n, const std::vector<Token>& tv);

private:
    // Use pointers here to speed up the merging process of two token
    // sequence (Is this really useful?).
    std::list<std::unique_ptr<Token>> token_list_{};
    // This iterator points to the next token in the list, not the current one.
    std::list<std::unique_ptr<Token>>::iterator token_list_iter_{};
};

}
#endif
