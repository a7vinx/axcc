#ifndef _AXCC_TOKEN_HH_
#define _AXCC_TOKEN_HH_

#include <string>
#include <list>
#include <iterator>
#include <memory>
#include <cstdlib>
#include <unordered_map>
#include <set>

namespace axcc {

struct SourceLoc {
    const std::string* fnamep;
    unsigned int row;
    unsigned int column;
    std::string::const_iterator linep;
    std::size_t line_len;
    bool ppline_corrected;
};

using SourceLocPtr = std::shared_ptr<SourceLoc>;

// Return string like "token.hh:19:36: source code"
std::string LocStr(const SourceLoc& loc);
// Check whether there is a white space in front of the specified location. Note
// that the specified location can not be the first non-newline token's.
bool HasPreWhiteSpace(const SourceLoc& loc);

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
    NEWLINE, INVALID, END
};

class Token {
public:
    // Type alias
    using HideSet = std::set<std::string>;

    Token(const TokenType& tag, const std::string& token_str,
          const SourceLocPtr& locp)
        : tag_{tag}, token_str_{token_str}, locp_{locp} {}
    Token(const TokenType& tag,
          const SourceLocPtr& locp)
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
    std::string TokenStr() const;
    const SourceLoc& Loc() const { return *locp_; }
    SourceLocPtr LocPtr() const { return locp_; }
    void SetLocPtr(const SourceLocPtr& locp) { locp_ = locp; }
    void HSAdd(const std::string& token_str) {
        hs_.insert(token_str_); }
    void HSAdd(const HideSet& hs) {
        hs_.insert(hs.cbegin(), hs.cend()); }
    bool HSHas(const std::string& token_str) const {
        return hs_.find(token_str_) != hs_.cend(); }
    const HideSet& GetHideSet() const { return hs_; }
    void SetHideSet(const HideSet& hs) { hs_ = hs; }
    static std::string TypeToStr(const TokenType& tag);

private:
    const TokenType tag_;
    // Integer and float constant is stored as string for now.
    const std::string token_str_{};
    SourceLocPtr locp_{};
    HideSet hs_{};
    static const std::unordered_map<TokenType, std::string> kTypeToStr_;
};

Token::HideSet HSIntersect(const Token::HideSet& lhs, const Token::HideSet& rhs);
Token::HideSet HSUnion(const Token::HideSet& lhs, const Token::HideSet& rhs);

inline bool operator==(const Token& lhs, const Token& rhs) {
    return (lhs.Tag() == rhs.Tag()) && (lhs.TokenStr() == rhs.TokenStr());
}
inline bool operator!=(const Token& lhs, const Token& rhs) {
    return !(lhs == rhs);
}
inline bool IsEndToken(const Token& t) {
    return t.Tag() == TokenType::END;
}
inline bool IsNewlineToken(const Token& t) {
    return t.Tag() == TokenType::NEWLINE;
}
inline bool IsIdentToken(const Token& t) {
    return t.Tag() == TokenType::IDENTIFIER;
}
bool IsIdentOrKeyword(const Token& t);

class TokenSequence {
public:
    TokenSequence() = default;
    // Deep copy
    TokenSequence(const TokenSequence& other);
    TokenSequence(TokenSequence&&);
    TokenSequence& operator=(const TokenSequence&) = delete;
    TokenSequence& operator=(TokenSequence&&) = delete;
    virtual ~TokenSequence() = default;

    // Simple wrapper functions for adding tokens from scratch one by one.
    template<typename... Args>
    void EmplaceBack(Args&&... args) {
        token_list_.emplace_back(
                        std::make_unique<Token>(std::forward<Args>(args)...)); }
    void PushBack(const Token& t) {
        token_list_.push_back(std::make_unique<Token>(t)); }
    void SetEndLoc(const SourceLoc& loc) {
        end_token_.SetLocPtr(std::make_shared<SourceLoc>(loc)); }

    // Helper functions for iterating token sequence.
    // All the following functions can only be used after Begin() has been
    // called and the parameter n, if it has, should be positive (no check
    // is performed inside).
    Token* Begin();
    Token* Next();
    Token* CurToken();
    Token* LookAhead() { return LookAheadN(1); }
    Token* LookAheadN(int n);
    // Wrapper functions for adjusting the token list.
    // The previous n tokens indicated by the parameter n include the current
    // token (i.e., if n = 1 only the current token will be erased).
    void ErasePrevN(int n);
    void ReplacePrevN(int n, TokenSequence&& ts, bool move_back = true);
    void ReplacePrevN(int n, const std::list<Token>& tl, bool move_back = true);

private:
    // Use pointers here to speed up the merging process of two token
    // sequence (Is this really useful?).
    std::list<std::unique_ptr<Token>> token_list_{};
    // This iterator points to the next token in the list, not the current one.
    std::list<std::unique_ptr<Token>>::iterator token_list_iter_{};
    // Make this end token behave as if it were in the list of tokens and it
    // will never be erased. ErasePrevN() has been modified to match this
    // behavior.
    Token end_token_{TokenType::END};
    bool reach_end_{true};
};

}
#endif
