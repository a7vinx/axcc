#include "token.hh"

namespace axcc {

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

TokenSequence::TokenSequence(const TokenSequence& other)
    : end_token_{other.end_token_} {
    for (auto const &tp : other.token_list_) {
        token_list_.push_back(std::make_unique<Token>(*tp));
    }
}

Token* TokenSequence::Begin() {
    token_list_iter_ = token_list_.begin();
    return Next();
}

Token* TokenSequence::Next() {
    if (token_list_iter_ == token_list_.end())
        return &end_token_;
    auto ret = token_list_iter_;
    std::advance(token_list_iter_, 1);
    return &(**ret);
}

Token* TokenSequence::CurToken() {
    if (std::distance(token_list_.cbegin(), {token_list_iter_}) < 1)
        return &end_token_;
    return &(**std::prev(token_list_iter_, 1));
}

Token* TokenSequence::LookAheadN(int n) {
    if (std::distance({token_list_iter_}, token_list_.cend()) < n)
        return &end_token_;
    return &(**std::next(token_list_iter_, n - 1));
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

