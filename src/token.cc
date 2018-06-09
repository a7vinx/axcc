#include <algorithm>

#include "token.hh"

namespace axcc {

const std::unordered_map<TokenType, std::string> Token::kTypeToStr_{
    {TokenType::DOT, "."},
    {TokenType::COMMA, ","},
    {TokenType::COLON, ":"},
    {TokenType::SEMI, ";"},
    {TokenType::EXCL, "!"},
    {TokenType::QUES, "?"},
    {TokenType::AST, "*"},
    {TokenType::PCT, "%"},
    {TokenType::AMP, "&"},
    {TokenType::VBAR, "|"},
    {TokenType::TILDE, "~"},
    {TokenType::CARET, "^"},
    {TokenType::ASGN, "="},
    {TokenType::PLUS, "+"},
    {TokenType::MINUS, "-"},
    {TokenType::SHARP, "#"},
    {TokenType::SLASH, "/"},
    {TokenType::LPAR, "("},
    {TokenType::RPAR, ")"},
    {TokenType::LBRACE, "{"},
    {TokenType::RBRACE, "}"},
    {TokenType::LSBRACKET, "["},
    {TokenType::RSBRACKET, "]"},
    {TokenType::LABRACKET, "<"},
    {TokenType::RABRACKET, ">"},
    {TokenType::LOGICAL_AND, "&&"},
    {TokenType::LOGICAL_OR, "||"},
    {TokenType::SHL, "<<"},
    {TokenType::SHR, ">>"},
    {TokenType::INC, "++"},
    {TokenType::DEC, "--"},
    {TokenType::EQ, "=="},
    {TokenType::NEQ, "!="},
    {TokenType::LESS_EQ, "<="},
    {TokenType::GREATER_EQ, ">="},
    {TokenType::ADD_ASGN, "+="},
    {TokenType::SUB_ASGN, "-="},
    {TokenType::MUL_ASGN, "*="},
    {TokenType::DIV_ASGN, "/="},
    {TokenType::AND_ASGN, "&="},
    {TokenType::OR_ASGN, "|="},
    {TokenType::XOR_ASGN, "^="},
    {TokenType::SHL_ASGN, "<<="},
    {TokenType::SHR_ASGN, ">>="},
    {TokenType::MOD_ASGN, "%="},
    {TokenType::ARROW, "->"},
    {TokenType::ELLIP, "..."},
    {TokenType::DSHARP, "##"},
    {TokenType::VOID, "void"},
    {TokenType::CHAR, "char"},
    {TokenType::SHORT, "short"},
    {TokenType::INT, "int"},
    {TokenType::LONG, "long"},
    {TokenType::FLOAT, "float"},
    {TokenType::DOUBLE, "double"},
    {TokenType::BOOL, "_Bool"},
    {TokenType::UNSIGNED, "unsigned"},
    {TokenType::SIGNED, "signed"},
    {TokenType::STRUCT, "struct"},
    {TokenType::UNION, "union"},
    {TokenType::ENUM, "enum"},
    {TokenType::CONST, "const"},
    {TokenType::VOLATILE, "volatile"},
    {TokenType::RESTRICT, "restrict"},
    {TokenType::ATOMIC, "_Atomic"},
    {TokenType::COMPLEX, "_Complex"},
    {TokenType::IMAGINARY, "_Imaginary"},
    {TokenType::STATIC, "static"},
    {TokenType::EXTERN, "extern"},
    {TokenType::AUTO, "auto"},
    {TokenType::REGISTER, "register"},
    {TokenType::THREAD_LOCAL, "_Thread_local"},
    {TokenType::INLINE, "inline"},
    {TokenType::NO_RETURN, "_Noreturn"},
    {TokenType::IF, "if"},
    {TokenType::ELSE, "else"},
    {TokenType::FOR, "for"},
    {TokenType::WHILE, "while"},
    {TokenType::DO, "do"},
    {TokenType::BREAK, "break"},
    {TokenType::SWITCH, "switch"},
    {TokenType::CASE, "case"},
    {TokenType::DEFAULT, "default"},
    {TokenType::GOTO, "goto"},
    {TokenType::CONTINUE, "continue"},
    {TokenType::TYPEDEF, "typedef"},
    {TokenType::RETURN, "return"},
    {TokenType::STATIC_ASSERT, "_Static_assert"},
    {TokenType::GENERIC, "_Generic"},
    {TokenType::SIZEOF, "sizeof"},
    {TokenType::ALIGNAS, "_Alignas"},
    {TokenType::ALIGNOF, "_Alignof"}
};

std::string Token::TypeToStr(const TokenType& tag) {
    auto iter = kTypeToStr_.find(tag);
    if (iter != kTypeToStr_.cend())
        return {iter->second};
    return {};
}

std::string LocStr(const SourceLocation& loc) {
    std::string locstr;
    locstr += *loc.fnamep;
    locstr += ":";
    locstr += std::to_string(loc.row);
    locstr += ":";
    locstr += std::to_string(loc.column);
    locstr += ": ";
    int loc_prefix_len = locstr.size();
    std::string linestr{loc.linep, loc.linep + loc.line_len};
    linestr += '\n';
    // Format by trimming
    int skipn = linestr.find_first_not_of(" \t\n\r\f\v");
    linestr.erase(0, skipn);
    locstr += linestr;
    // Add '^' in the next line to point to target token
    locstr += std::string(loc_prefix_len + loc.column - skipn - 1, ' ');
    locstr += '^';
    locstr += '\n';
    return locstr;
}

std::string TokenStr(const Token& t) {
    if (t.TokenStr().empty())
        return Token::TypeToStr(t.Tag());
    else
        return t.TokenStr();
}

Token::HideSet HSIntersect(const Token::HideSet& lhs,
                           const Token::HideSet& rhs) {
    Token::HideSet hs;
    std::set_intersection(lhs.cbegin(), lhs.cend(), rhs.cbegin(), rhs.cend(),
                          std::inserter(hs, hs.cbegin()));
    return hs;
}

Token::HideSet HSUnion(const Token::HideSet& lhs, const Token::HideSet& rhs) {
    Token::HideSet hs;
    std::set_union(lhs.cbegin(), lhs.cend(), rhs.cbegin(), rhs.cend(),
                   std::inserter(hs, hs.cbegin()));
    return hs;
}

TokenSequence::TokenSequence(const TokenSequence& other)
    : end_token_{other.end_token_} {
    for (auto const &tp : other.token_list_) {
        token_list_.push_back(std::make_unique<Token>(*tp));
    }
}

TokenSequence::TokenSequence(TokenSequence&& other)
    : token_list_{std::move(other.token_list_)},
      end_token_{std::move(other.end_token_)} {
    other.token_list_.clear();
}

Token* TokenSequence::Begin() {
    reach_end_ = false;
    token_list_iter_ = token_list_.begin();
    return Next();
}

Token* TokenSequence::Next() {
    if (reach_end_ || token_list_iter_ == token_list_.end()) {
        reach_end_ = true;
        return &end_token_;
    }
    auto ret = token_list_iter_;
    std::advance(token_list_iter_, 1);
    return &(**ret);
}

Token* TokenSequence::CurToken() {
    if (reach_end_ || token_list_iter_ == token_list_.begin())
        return &end_token_;
    return &(**std::prev(token_list_iter_, 1));
}

Token* TokenSequence::LookAheadN(int n) {
    if (reach_end_ || std::distance({token_list_iter_}, token_list_.end()) < n)
        return &end_token_;
    return &(**std::next(token_list_iter_, n - 1));
}

void TokenSequence::ErasePrevN(int n) {
    if (reach_end_) n -= 1;
    token_list_.erase(std::prev(token_list_iter_, n), token_list_iter_);
}

void TokenSequence::ReplacePrevN(int n, TokenSequence&& ts) {
    ErasePrevN(n);
    std::move(ts.token_list_.begin(), ts.token_list_.end(),
              std::inserter(token_list_, token_list_iter_));
    // Set the iterator pointing to the first token in the inserted token list.
    std::advance(token_list_iter_, -ts.token_list_.size());
}

void TokenSequence::ReplacePrevN(int n, const std::vector<Token>& tv) {
    ErasePrevN(n);
    for (auto const& t : tv) {
        token_list_.insert(token_list_iter_, std::make_unique<Token>(t));
    }
    std::advance(token_list_iter_, -tv.size());
}

}
