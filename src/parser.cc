#include <algorithm>
#include <exception>
#include <type_traits>

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

Parser::Parser(TokenSequence& ts)
    : ts_{ts}, scopesp_{std::make_unique<Scopes>()} {}

Parser::~Parser() = default;

AstRoot Parser::Parse() {
    ParseTranslationUnit();
    return ast_;
}

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

template<typename F>
class FinalObj {
public:
    FinalObj(F&& cleaner) : cleaner_{std::forward<F>(cleaner)} {}
    FinalObj(const FinalObj&) = delete;
    FinalObj(FinalObj&& other) : cleaner_{other.cleaner_} {
        other.Cancel(); }
    FinalObj& operator=(const FinalObj&) = delete;
    FinalObj& operator=(FinalObj&&) = delete;
    ~FinalObj() noexcept { if (do_it_) cleaner_(); }
    void Cancel() { do_it_ = false; }
private:
    typename std::remove_reference<F>::type cleaner_;
    bool do_it_{true};
};

template<typename F>
FinalObj<F> Finally(F&& cleaner) {
    return FinalObj<F>{std::forward<F>(cleaner)};
}

} // unnamed namespace

enum class Parser::DeclPos {
    kGlobal,
    kLocal,
    kForLoop,
    kParam,
    kRecord,
    kTypeName
};

enum class Parser::StorSpec {
    kNone,
    kAuto,
    kExtern,
    kStatic,
    kRegister,
    kTypedef
};

enum class Parser::TypeSpec {
    kNone,
    kVoid,
    kArith,
    kEnum,
    kRecord,
    kTypeAlias
};

struct Parser::DeclSpecInfo {
    QualType base_qty;
    StorSpec stor;
    long long align;
    unsigned char func_specs;
};

struct Parser::DeclaratorInfo {
    QualType qtype;
    std::string name;
    SourceLocPtr locp;
};

void Parser::ParseTranslationUnit() {
    Token* tp = ts_.Begin();
    for (; !IsEndToken(*tp); tp = ts_.Next()) {
        try {
            ParseExtDecl();
        } catch (const ParseError& e) {
            Error(e.what(), e.Loc());
            SkipToSyncToken();
        }
    }
    TryGenTentativeDefs();
}

void Parser::ParseExtDecl() {
    if (ts_.CurToken()->Tag() == TokenType::STATIC_ASSERT) {
        ParseStaticAssert();
        return;
    }
    bool has_prev_ident = false;
    DeclSpecInfo spec_info = ParseDeclSpec(DeclPos::kGlobal);
    while (true) {
        IdentPtr identp = ParseDeclarator(DeclPos::kGlobal, spec_info);
        // Unless this identifier represents nothing, we will always insert it
        // into current scope.
        auto finally = Finally([&](){ TryAddToScope(identp); });
        // A rudimentary approach to empty declaration checking.
        if (identp->Name().empty() && !IsRecordTy(identp->QType())) {
            // Warning but continue the subsequent processing.
            Warning("empty declaration does not declare anything", identp->Loc());
            finally.Cancel();
        }
        if (!has_prev_ident && CurIsFuncDef(*identp, spec_info.base_qty)) {
            // Deal with function definition.
            ast_.AddNode(ParseFuncDef(identp));
        } else {
            if (identp->Name().empty()) {
                // Do nothing.
            } else if (IsTypedefName(*identp)) {
                // Do nothing more with typedef.
            } else if (IsFuncName(*identp)) {
                // Do nothing more with function declaration.
            } else if (ts_.CurIs(TokenType::ASGN)) {
                // Deal with normal definition.
                ts_.Next();
                if (spec_info.stor == StorSpec::kExtern)
                    Warning("'extern' variable has an initializer",
                            identp->Loc());
                ObjectPtr objp = NodepConv<Object>(identp);
                objp->EncounterDef();
                ast_.AddNode(ParseStaticInitializer(objp));
                // ParseStaticInitializer() will complete the array type.
                if (!identp->QType()->IsComplete()) {
                    Error("variable has incomplete type", identp->Loc());
                    identp->SetErrFlags();
                }
                // Leave other checks to TryAddToScope().
                RmTentativeDefIfHas(identp->Name());
            } else if (IsVoidTy(identp->QType())) {
                // Whether it is a tentative definition or declaration, void
                // type should be checked immediately. If it is a tentative
                // definition, do not add it to the record.
                Error("variable has incomplete type", identp->Loc());
                identp->SetErrFlags();
            } else if (spec_info.stor == StorSpec::kNone ||
                       spec_info.stor == StorSpec::kStatic) {
                // Deal with tentative definition.
                AddTentativeDefIfNeed(NodepConv<Object>(identp));
            }  // For other declarations, no more actions need to be performed.
            if (ts_.CurIs(TokenType::COMMA)) {
                has_prev_ident = true;
                ts_.Next();
                continue;
            }
            ExpectCur(TokenType::SCLN);
        }
        return;
    }
}

void Parser::ParseStaticAssert() {
    const SourceLoc& loc = ts_.CurToken()->Loc();
    ExpectNext(TokenType::LPAR);
    ts_.Next();
    ConstantPtr constantp = ParseIntConstantExpr();
    ExpectCur(TokenType::COMMA);
    ts_.Next();
    StrLiteralPtr literalp = ParseStrLiterals();
    if (!constantp->IntVal())
        Error(literalp->Str(), loc);
    ExpectNext(TokenType::RPAR);
    ExpectNext(TokenType::SCLN);
}

namespace {

// Compatibility of arithmetic type specifiers.
const unsigned int kASBoolCmpt = 0;
const unsigned int kASCharCmpt = ArithType::kASSigned | ArithType::kASUnsigned;
const unsigned int kASIntCmpt = ArithType::kASSigned | ArithType::kASUnsigned |
                                ArithType::kASShort | ArithType::kASLong |
                                ArithType::kASLLong;
const unsigned int kASFloatCmpt = 0;
const unsigned int kASDoubleCmpt = ArithType::kASLong;
const unsigned int kASShortCmpt = ArithType::kASInt | ArithType::kASSigned |
                                  ArithType::kASUnsigned;
const unsigned int kASLongCmpt = ArithType::kASInt | ArithType::kASLong |
                                 ArithType::kASDouble | ArithType::kASSigned |
                                 ArithType::kASUnsigned;
const unsigned int kASSignedCmpt = ArithType::kASChar | ArithType::kASInt |
                                   ArithType::kASShort | ArithType::kASLong |
                                   ArithType::kASLLong;
const unsigned int kASUnsignedCmpt = kASSignedCmpt;

} // unnamed namespace

Parser::DeclSpecInfo Parser::ParseDeclSpec(const DeclPos& decl_pos) {
    Token* tp = ts_.CurToken();
    DeclSpecInfo info{{}, StorSpec::kNone, -1, 0};
    TypeSpec type_spec = TypeSpec::kNone;
    unsigned int arith_kind = 0;
    unsigned char qualifiers = 0;
    bool spec_end = false;
    while (true) {
        switch (tp->Tag()) {
            // Storage Class Specifiers
            case TokenType::TYPEDEF:
                TrySetStorSpec(decl_pos, info.stor, StorSpec::kTypedef, tp->Loc());
                break;
            case TokenType::EXTERN:
                TrySetStorSpec(decl_pos, info.stor, StorSpec::kExtern, tp->Loc());
                break;
            case TokenType::STATIC:
                TrySetStorSpec(decl_pos, info.stor, StorSpec::kStatic, tp->Loc());
                break;
            case TokenType::AUTO:
                TrySetStorSpec(decl_pos, info.stor, StorSpec::kAuto, tp->Loc());
                break;
            case TokenType::REGISTER:
                TrySetStorSpec(
                    decl_pos, info.stor, StorSpec::kRegister, tp->Loc());
                break;
            case TokenType::THREAD_LOCAL:
                Error("_Thread_local is not supported yet", tp->Loc());
                break;
            // Type Qualifiers
            case TokenType::CONST:
                TrySetQual(qualifiers, QualType::kQualConst, tp->Loc());
                break;
            case TokenType::RESTRICT:
                TrySetQual(qualifiers, QualType::kQualRestrict, tp->Loc());
                break;
            case TokenType::VOLATILE:
                TrySetQual(qualifiers, QualType::kQualVolatile, tp->Loc());
                break;
            case TokenType::ATOMIC:
                Error("_Atomic is not supported yet", tp->Loc());
                break;
            // Function Specifiers
            case TokenType::INLINE:
                TrySetFuncSpec(info.func_specs, FuncType::kFSInline, tp->Loc());
                break;
            case TokenType::NO_RETURN:
                TrySetFuncSpec(info.func_specs, FuncType::kFSNoreturn, tp->Loc());
                break;
            // Alignment Specifiers
            case TokenType::ALIGNAS: {
                const SourceLoc& loc = tp->Loc();
                TrySetAlign(decl_pos, info.align, ParseAlignAs(), loc);
                break;
            }
            // Type Specifiers
            case TokenType::VOID:
                TrySetTypeSpec(type_spec, TypeSpec::kVoid, tp->Loc());
                break;
            case TokenType::BOOL:
                TrySetArithKind(type_spec, arith_kind, ArithType::kASBool,
                                kASBoolCmpt, tp->Loc());
                break;
            case TokenType::CHAR:
                TrySetArithKind(type_spec, arith_kind, ArithType::kASChar,
                                kASCharCmpt, tp->Loc());
                break;
            case TokenType::INT:
                TrySetArithKind(type_spec, arith_kind, ArithType::kASInt,
                                kASIntCmpt, tp->Loc());
                break;
            case TokenType::FLOAT:
                TrySetArithKind(type_spec, arith_kind, ArithType::kASFloat,
                                kASFloatCmpt, tp->Loc());
                break;
            case TokenType::DOUBLE:
                TrySetArithKind(type_spec, arith_kind, ArithType::kASDouble,
                                kASDoubleCmpt, tp->Loc());
                break;
            case TokenType::SHORT:
                TrySetArithKind(type_spec, arith_kind, ArithType::kASShort,
                                kASShortCmpt, tp->Loc());
                break;
            case TokenType::LONG:
                TrySetArithKind(type_spec, arith_kind, ArithType::kASLong,
                                kASLongCmpt, tp->Loc());
                break;
            case TokenType::SIGNED:
                TrySetArithKind(type_spec, arith_kind, ArithType::kASSigned,
                                kASSignedCmpt, tp->Loc());
                break;
            case TokenType::UNSIGNED:
                TrySetArithKind(type_spec, arith_kind, ArithType::kASUnsigned,
                                kASUnsignedCmpt, tp->Loc());
                break;
            case TokenType::ENUM:
                // Enumeration will be treated as int.
                TrySetTypeSpec(type_spec, TypeSpec::kEnum, tp->Loc());
                // Continue parsing the enum specifier even if it conflicts
                // with previous type specifiers.
                ParseEnumSpec();
                break;
            case TokenType::STRUCT:
            case TokenType::UNION:
                TrySetTypeSpec(type_spec, TypeSpec::kRecord, tp->Loc());
                // Continue parsing.
                info.base_qty.SetRawType(ParseRecordSpec());
                break;
            case TokenType::COMPLEX:
                Error("complex floating types is not supported yet", tp->Loc());
                break;
            case TokenType::IMAGINARY:
                Error("imaginary floating types is not supported yet", tp->Loc());
                break;
            default:
                if (IsTypeNameToken(*tp)) {
                    // Now it should be a typedef name.
                    TrySetTypeSpec(type_spec, TypeSpec::kTypeAlias, tp->Loc());
                    IdentPtr identp =
                        scopesp_->GetOrdIdentpInAllScope(tp->TokenStr());
                    QualType alias_qty = NodeConv<TypedefName>(*identp).QType();
                    info.base_qty.SetRawType(alias_qty.RawTypep());
                    info.base_qty.MergeQuals(alias_qty);
                } else {
                    spec_end = true;
                }
        }
        if (spec_end)
            break;
        tp = ts_.Next();
    }
    switch (type_spec) {
        case TypeSpec::kNone:
            Warning("type specifier missing, defaults to 'int'", tp->Loc());
            info.base_qty = MakeQType<ArithType>(ArithType::kASInt);
            break;
        case TypeSpec::kVoid:
            info.base_qty = MakeQType<VoidType>();
            break;
        case TypeSpec::kArith:
            info.base_qty = MakeQType<ArithType>(arith_kind);
            break;
        case TypeSpec::kEnum:
            info.base_qty = MakeQType<ArithType>(ArithType::kASInt);
            break;
        case TypeSpec::kRecord:
        case TypeSpec::kTypeAlias:
            break;
        default:
            assert(false);
    }
    return info;
}

void Parser::TrySetStorSpec(const DeclPos& decl_pos, StorSpec& stor,
                            const StorSpec& spec, const SourceLoc& loc) {
    if (stor != StorSpec::kNone) {
        Error("cannot combine with previous storage class specifier", loc);
    } else if (decl_pos == DeclPos::kRecord || decl_pos == DeclPos::kTypeName) {
        Error("type name does not allow storage class to be specified", loc);
    } else if (decl_pos == DeclPos::kGlobal &&
               (spec == StorSpec::kRegister || spec == StorSpec::kAuto)) {
        Error("illegal storage class on file-scoped variable", loc);
    } else if (decl_pos == DeclPos::kParam && spec != StorSpec::kRegister) {
        Error("invalid storage class specifier in function declarator", loc);
    } else if (decl_pos == DeclPos::kForLoop &&
               spec != StorSpec::kAuto && spec != StorSpec::kRegister) {
        Error("illegal storage class specifier in 'for' loop", loc);
    } else {
        stor = spec;
    }
}

void Parser::TrySetQual(unsigned char& qualifiers, QualType::Qualifier qual,
                        const SourceLoc& loc) {
    if (qualifiers & qual)
        Warning("duplicate qualifier", loc);
    else
        qualifiers |= qual;
}

void Parser::TrySetFuncSpec(unsigned char& func_specs, FuncType::FuncSpec spec,
                            const SourceLoc& loc) {
    if (func_specs & spec)
        Warning("duplicate function specifier", loc);
    else
        func_specs |= spec;
}

void Parser::TrySetAlign(const DeclPos& decl_pos, long long& align,
                         long long val, const SourceLoc& loc) {
    if (decl_pos == DeclPos::kParam) {
        Error("alignas specifier cannot be applied to a function parameter", loc);
    } else if (decl_pos == DeclPos::kTypeName) {
        Error("illegal alignas specifier, type name expected", loc);
    } else if (val > align) {
        // C11 6.7.5p6: An alignment specification of zero has no effect. When
        // multiple alignment specifiers occur in a declaration, the effective
        // alignment requirement is the strictest specified alignment.
        align = val;
    }
}

void Parser::TrySetTypeSpec(TypeSpec& type_spec, const TypeSpec& spec,
                            const SourceLoc& loc) {
    if (type_spec != TypeSpec::kNone)
        Error("cannot combine with previous type specifier", loc);
    else
        type_spec = spec;
}

void Parser::TrySetArithKind(TypeSpec& type_spec, unsigned int& arith_kind,
                             ArithType::ArithSpec spec, unsigned int cmpt,
                             const SourceLoc& loc) {
    if (type_spec == TypeSpec::kNone ||
        (type_spec == TypeSpec::kArith && !(arith_kind & ~cmpt))) {
        type_spec = TypeSpec::kArith;
        // Special treatment for "long long".
        if ((arith_kind & ArithType::kASLong) &&
            (spec == ArithType::kASLong)) {
            arith_kind &= ~ArithType::kASLong;
            arith_kind |= ArithType::kASLLong;
        } else {
            arith_kind |= spec;
        }
    } else {
        Error("cannot combine with previous type specifier", loc);
    }
}

void Parser::ParseEnumSpec() {
    std::string tag_name{};
    if (ts_.Try(TokenType::IDENTIFIER))
        tag_name = ts_.CurToken()->TokenStr();
    SourceLocPtr tag_locp = ts_.CurToken()->LocPtr();
    if (ts_.Try(TokenType::LBRACE)) {
        // Now it should be a definition.
        if (!tag_name.empty())
            TryAddToScope(MakeNodePtr<Tag>(
                              tag_locp, MakeQType<ArithType>(ArithType::kASInt),
                              tag_name));
        ParseEnumDef();
    } else if (!tag_name.empty()) {
        TagPtr tagp = scopesp_->GetTagpInAllScope(tag_name);
        if (tagp) {
            if (IsRecordTy(tagp->QType()))
                Error("use of '" + tag_name + "' with tag type that "
                      "does not match previous declaration", *tag_locp);
            // Do nothing if the tag is used correctly.
        } else if (ts_.NextIs(TokenType::SCLN)) {
            // Before we report the undefined tag, give this forward declaration
            // special treatment.
            // C11 6.7.2.3p7: A similar construction with enum does not exist.
            Error("no forward-declarations for enums in C", *tag_locp);
        } else {
            Error("undefined enum tag '" + tag_name + "'", *tag_locp);
        }
    } else {
        throw ParseError{"identifier or '{' expected", ts_.LookAhead()->LocPtr()};
    }
}

void Parser::ParseEnumDef() {
    ts_.Next();
    if (ts_.CurIs(TokenType::RBRACE))
        Error("use of empty enum", ts_.CurToken()->Loc());
    int val = 0;
    while (!ts_.CurIs(TokenType::RBRACE)) {
        try {
            val = ParseEnumDefItem(val) + 1;
        } catch (const ParseError& e) {
            // Try to handle the error, i.e., Skip to the synchronizing token.
            // If the token we skip to is the right brace, we assume that we
            // stop at the end of the current definition and then we can end
            // normally. If not, throw a new exception to the upper handler.
            Error(e.what(), e.Loc());
            SkipToSyncToken();
            if (!ts_.CurIs(TokenType::RBRACE))
                throw ParseError{"'}' expected", ts_.CurToken()->LocPtr()};
            break;
        }
    }
}

int Parser::ParseEnumDefItem(int val) {
    ExpectCur(TokenType::IDENTIFIER);
    std::string name = ts_.CurToken()->TokenStr();
    SourceLocPtr locp = ts_.CurToken()->LocPtr();
    ts_.Next();
    // Make sure this enumerator will still be inserted into current scope when
    // an error occurs.
    auto finally =
        Finally([&](){ TryAddToScope(MakeNodePtr<Enumerator>(locp, name, val)); });
    if (ts_.CurIs(TokenType::ASGN)) {
        ts_.Next();
        val = ParseIntConstantExpr()->IntVal();
    }
    if (ts_.CurIs(TokenType::COMMA)) {
        ts_.Next();
    } else {
        ExpectCur(TokenType::RBRACE);
    }
    return val;
}

}
