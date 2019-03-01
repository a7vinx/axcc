#include <algorithm>
#include <exception>

#include "parser.hh"

namespace axcc {

enum class ScopeKind {
    kFile,
    kProto,
    // C11 6.2.1p3: A label name is the only kind of identifier that has
    // function scope.
    kFunc,
    kBlock
};

class Parser::Scope {
public:
    Scope(const ScopeKind& kind) : kind_{kind} {}
    ScopeKind Kind() const { return kind_; }
    bool HasTag(const std::string& name) const {
        return tags_.find(name) != tags_.cend(); }
    bool HasOrdIdent(const std::string& name) const {
        return ord_idents_.find(name) != ord_idents_.cend(); }
    TagPtr GetTagp(const std::string& name) const {
        auto iter = tags_.find(name);
        return iter != tags_.cend() ? iter->second : TagPtr{};
    }
    IdentPtr GetOrdIdentp(const std::string& name) const {
        auto iter = ord_idents_.find(name);
        return iter != ord_idents_.cend() ? iter->second : IdentPtr{};
    }
    // No check inside.
    void AddTag(const TagPtr& tagp) {
        tags_.emplace(tagp->Name(), tagp); }
    void AddOrdIdent(const IdentPtr& identp) {
        ord_idents_.emplace(identp->Name(), identp); }
private:
    ScopeKind kind_;
    std::map<std::string, TagPtr> tags_{};
    std::map<std::string, IdentPtr> ord_idents_{};
};

class Parser::Scopes {
public:
    Scopes() { scopes_.emplace_back(ScopeKind::kFile); }
    ScopeKind CurKind() const { return scopes_.back().Kind(); }
    bool HasTagInCurScope(const std::string& name) const {
        return scopes_.back().HasTag(name); }
    TagPtr GetTagpInCurScope(const std::string& name) const {
        return scopes_.back().GetTagp(name); }
    bool HasTagInAllScope(const std::string& name) const {
        return GetTagpInAllScope(name).get() != nullptr; }
    TagPtr GetTagpInAllScope(const std::string& name) const {
        auto has_tag = [&name](const Scope& s) { return s.HasTag(name); };
        // In reverse order.
        auto iter = std::find_if(scopes_.crbegin(), scopes_.crend(), has_tag);
        return iter == scopes_.crend() ? TagPtr{} : iter->GetTagp(name);
    }
    bool HasOrdIdentInCurScope(const std::string& name) const {
        return scopes_.back().HasOrdIdent(name); }
    IdentPtr GetOrdIdentpInCurScope(const std::string& name) const {
        return scopes_.back().GetOrdIdentp(name); }
    bool HasOrdIdentInFileScope(const std::string& name) const {
        return scopes_.front().HasOrdIdent(name); }
    IdentPtr GetOrdIdentpInFileScope(const std::string& name) const {
        return scopes_.front().GetOrdIdentp(name); }
    bool HasOrdIdentInAllScope(const std::string& name) const {
        return GetOrdIdentpInAllScope(name).get() != nullptr; }
    IdentPtr GetOrdIdentpInAllScope(const std::string& name) const {
        auto has_ident = [&name](const Scope& s) { return s.HasOrdIdent(name); };
        auto iter = std::find_if(scopes_.crbegin(), scopes_.crend(), has_ident);
        return iter == scopes_.crend() ? IdentPtr{} : iter->GetOrdIdentp(name);
    }
    bool HasLabel(const std::string& name) const {
        return labels_.find(name) != labels_.cend(); }
    LabelPtr GetLabelp(const std::string& name) const {
        auto iter = labels_.find(name);
        return iter != labels_.cend() ? iter->second : LabelPtr{};
    }
    void AddTag(const TagPtr& tagp) { scopes_.back().AddTag(tagp); }
    void AddOrdIdent(const IdentPtr& identp) {
        scopes_.back().AddOrdIdent(identp); }
    void AddLabel(const LabelPtr& labelp) {
        labels_.emplace(labelp->Name(), labelp); }
    void EnterProto() { scopes_.emplace_back(ScopeKind::kProto); }
    void EnterBlock() { scopes_.emplace_back(ScopeKind::kBlock); }
    void ExitProto() { scopes_.pop_back(); }
    void ExitBlock() { scopes_.pop_back(); }
private:
    std::vector<Scope> scopes_{};
    // Special treatment for labels.
    std::map<std::string, LabelPtr> labels_{};
};

namespace {

class ParseError : public std::exception {
public:
    ParseError(const std::string& msg, const SourceLocPtr& locp)
        : msg_{msg}, locp_{locp} {}
    virtual const char* what() const noexcept { return msg_.c_str(); }
    const SourceLoc& Loc() const noexcept {
        assert(locp_.get() != nullptr); return *locp_; }
private:
    std::string msg_;
    SourceLocPtr locp_;
};

} // unnamed namespace

}
