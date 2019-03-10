#include <algorithm>
#include <exception>
#include <type_traits>

#include "parser.hh"
#include "encoding.hh"
#include "evaluator.hh"

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

TypePtr Parser::ParseRecordSpec() {
    bool is_struct = ts_.CurIs(TokenType::STRUCT);
    std::string tag_name{};
    if (ts_.Try(TokenType::IDENTIFIER))
        tag_name = ts_.CurToken()->TokenStr();
    // Only when it is a named tag shall we use this location pointer.
    SourceLocPtr tag_locp = ts_.CurToken()->LocPtr();
    TagPtr tagp{};
    bool is_def = false;
    std::vector<ObjectPtr> members{};
    if (ts_.Try(TokenType::LBRACE)) {
        // Now it should be a struct/union definition.
        members = ParseRecordDef();
        is_def = true;
        tagp = scopesp_->GetTagpInCurScope(tag_name);
    } else if (!tag_name.empty() && ts_.NextIs(TokenType::SCLN)) {
        // Forward declaration introduces a new tag if there is no such tag in
        // current scope.
        tagp = scopesp_->GetTagpInCurScope(tag_name);
    } else if (!tag_name.empty()) {
        // Otherwise, it introduces a new tag only when no such tag is visible.
        tagp = scopesp_->GetTagpInAllScope(tag_name);
    } else {
        throw ParseError{"identifier or '{' expected", ts_.LookAhead()->LocPtr()};
    }
    // Handle this record specifier based on the tag we found.
    // Make sure that the places where this tag is used referred to the same
    // entity.
    if (tagp) {
        if ((is_struct && !IsStructTy(tagp->QType())) ||
            (!is_struct && !IsUnionTy(tagp->QType()))) {
            Error("use of '" + tag_name + "' with tag type that does not "
                  "match previous declaration", *tag_locp);
            if (is_def) {
                // We still try to suppress those related errors by returning
                // the type that should not exist.
                return MakeQType<RecordType>(
                           is_struct, members, tag_name).RawTypep();
            }
            // Otherwise let it return the type of the tag we found.
        } else if (is_def) {
            if (tagp->QType()->IsComplete())
                Error("redefinition of '" + tag_name + "'", *tag_locp);
            else
                TypeConv<RecordType>(tagp->QType()).EncounterDef(members);
        }
    } else {
        if (is_def) {
            // Do not add it into the scope if it is unnamed.
            if (tag_name.empty())
                return MakeQType<RecordType>(is_struct, members).RawTypep();
            tagp = MakeNodePtr<Tag>(
                       tag_locp,
                       MakeQType<RecordType>(is_struct, members, tag_name),
                       tag_name);
        } else {
            tagp = MakeNodePtr<Tag>(
                       tag_locp, MakeQType<RecordType>(is_struct, tag_name),
                       tag_name);
        }
        TryAddToScope(tagp);
    }
    return tagp->QType().RawTypep();
}

std::vector<ObjectPtr> Parser::ParseRecordDef() {
    std::vector<ObjectPtr> members{};
    for (ts_.Next(); !ts_.CurIs(TokenType::RBRACE); ts_.Next()) {
        try {
            if (ts_.CurIs(TokenType::STATIC_ASSERT))
                ParseStaticAssert();
            else
                ParseRecordDefItem(members);
        } catch (const ParseError& e) {
            // Like what we do in ParseEnumDef(), try to handle this error but
            // throw a new exception only if we reach the end token.
            Error(e.what(), e.Loc());
            SkipToSyncToken();
            if (IsEndToken(*ts_.CurToken()))
                throw ParseError{"'}' expected", ts_.CurToken()->LocPtr()};
            if (ts_.CurIs(TokenType::RBRACE))
                break;
        }
    }
    if (members.empty())
        Warning("use of empty struct/union", ts_.CurToken()->Loc());
    return members;
}

void Parser::ParseRecordDefItem(std::vector<ObjectPtr>& members) {
    DeclSpecInfo spec_info = ParseDeclSpec(DeclPos::kRecord);
    while (true) {
        IdentPtr identp = ParseDeclarator(DeclPos::kRecord, spec_info);
        if (IsFuncTy(identp->QType())) {
            Error("field declared as a function", identp->Loc());
        } else {
            ObjectPtr objp = NodepConv<Object>(identp);
            // Make sure this object will still be added into the members list
            // when an error occurs.
            auto finally = Finally([&](){ members.push_back(objp); });
            if (ts_.CurIs(TokenType::COLON)) {
                ts_.Next();
                objp = ParseBitField(objp);
            }
            if (objp->IsAnonymous()) {
                QualType obj_qtype = objp->QType();
                if (!IsRecordTy(obj_qtype) && !IsBitField(*objp)) {
                    Warning("empty declaration does not declare anything",
                            objp->Loc());
                    // Then this anonymous object do not need to be counted as a
                    // member.
                    finally.Cancel();
                } else if (IsRecordTy(obj_qtype) &&
                           !TypeConv<RecordType>(obj_qtype).TagName().empty()) {
                    // It is a nested struct/union declaration.
                    finally.Cancel();
                }
            }
        }
        if (!ts_.CurIs(TokenType::COMMA)) {
            ExpectCur(TokenType::SCLN);
            return;
        }
    }
}

ObjectPtr Parser::ParseBitField(const ObjectPtr& orig_objp) {
    const SourceLoc& loc = orig_objp->Loc();
    ConstantPtr constantp = ParseIntConstantExpr();
    std::size_t bit_width = constantp->UIntVal();
    std::size_t bitty_size =
        IsBoolTy(orig_objp->QType()) ? 1 : orig_objp->QType()->Size() * 8;
    if (!IsIntegerTy(orig_objp->QType())) {
        Error("bit-field has non-integral type", loc);
        return orig_objp;
    }
    if (constantp->IsNegInt()) {
        Error("bit-field has negative width", loc);
        bit_width = 1;
    } else if (bit_width == 0 && !orig_objp->IsAnonymous()) {
        Error("named bit-field '" + orig_objp->Name() + "' has zero width",
              loc);
        bit_width = 1;
    } else if (bit_width > bitty_size) {
        Error("width of bit-field (" + std::to_string(bit_width) + " bits) "
              "exceeds width of its type (" + std::to_string(bitty_size) +
              " bit)", loc);
        bit_width = bitty_size;
    }
    return MakeNodePtr<BitField>(orig_objp->Locp(), orig_objp->QType(),
                                 orig_objp->Name(), bit_width);
}

namespace {

// C11 6.7.6.3p1: A function declarator shall not specify a return type
// that is a function type or an array type.
// The completeness of the return type do not need to be checked here.
void CheckFuncRetQType(const QualType& ret_qtype, const SourceLoc& loc) {
    if (IsFuncTy(ret_qtype)) {
        Error("function cannot return function type", loc);
    } else if (IsArrayTy(ret_qtype)) {
        Error("function cannot return array type", loc);
    }
}

void CheckArrElemQType(const QualType& elem_qtype, const SourceLoc& loc) {
    if (IsFuncTy(elem_qtype)) {
        Error("array of functions", loc);
    } else if (!elem_qtype->IsComplete()) {
        Error("array has incomplete element type", loc);
    }
}

// Replace the basic type of the type specified by parameter typep with the
// type real_base.
void CorrectBaseQType(TypePtr& typep, const QualType& real_base,
                      const SourceLoc& loc) {
    if (IsPointerTy(*typep)) {
        TypeConv<PointerType>(*typep).ResetPointeeQTy(real_base);
    } else if (IsArrayTy(*typep)) {
        TypeConv<ArrayType>(*typep).ResetElemQType(real_base);
        CheckArrElemQType(real_base, loc);
    } else if (IsFuncTy(*typep)) {
        TypeConv<FuncType>(*typep).ResetRetQType(real_base);
        CheckFuncRetQType(real_base, loc);
    } else {
        assert(false);
    }
}

} // unnamed namespace

// Return the parsed identifier including Object, FuncName, TypedefName and it
// may be an abstract declarator. If some errors occured during parsing, it
// will not throw an exception and will try to return the identifier that has
// been parsed while leaving the situation to the caller to deal with.
IdentPtr Parser::ParseDeclarator(const DeclPos& decl_pos,
                                 const DeclSpecInfo& spec_info,
                                 bool check_err,
                                 TypePtr* prev_hookp) {
    Token* tp = ts_.CurToken();
    std::string name{};
    SourceLocPtr name_locp{};
    // '(' may be the beginning of function parameter list or of a parentheses
    // group.
    if (tp->Tag() == TokenType::LPAR && !IsTypeNameToken(*ts_.LookAhead())) {
        // Now we are dealing with a parentheses group. We need the information
        // behind the parentheses group to know the real type so we use a
        // temporary wrong basic type to do the recursive parsing and leave a
        // hook pointer so that when we get the real basic type we can correct it.
        ts_.Next();
        TypePtr hook{};
        IdentPtr tmp_identp = ParseDeclarator(decl_pos, spec_info, false, &hook);
        try {
            ExpectCur(TokenType::RPAR);
        } catch (const ParseError& e) {
            // Do not skip to the synchronizing token. Just pack up the work
            // we have done and leave the mess to the caller to deal with.
            Error(e.what(), e.Loc());
            return tmp_identp;
        }
        ts_.Next();
        QualType real_base = ParseDeclaratorTail(spec_info.base_qty);
        // Set previous hook pointer if has. The order can not be reversed.
        if (prev_hookp && hook.get() != nullptr)
            *prev_hookp = hook;
        if (prev_hookp && !real_base.IsCompatible(spec_info.base_qty))
            *prev_hookp = real_base.RawTypep();
        // Now get the real type.
        QualType real_qtype = real_base;
        if (hook.get() != nullptr) {
            CorrectBaseQType(hook, real_base, tmp_identp->Loc());
            real_qtype = tmp_identp->QType();
        }
        DeclaratorInfo declarator_info{real_qtype, tmp_identp->Name(),
                                       tmp_identp->Locp()};
        return MakeDeclarator(decl_pos, spec_info, declarator_info, check_err);
    } else if (tp->Tag() == TokenType::AST) {
        ts_.Next();
        unsigned char qualifiers = ParseQuals();
        QualType new_base = MakeQType<PointerType>(spec_info.base_qty);
        new_base.MergeQuals(qualifiers);
        if (prev_hookp)
            *prev_hookp = new_base.RawTypep();
        DeclSpecInfo new_spec_info = spec_info;
        new_spec_info.base_qty = new_base;
        return ParseDeclarator(decl_pos, new_spec_info, check_err, nullptr);
    } else if (IsIdentToken(*tp)) {
        name = tp->TokenStr();
        name_locp = tp->LocPtr();
        tp = ts_.Next();
    } else {
        name_locp = tp->LocPtr();
    }
    QualType real_qtype = ParseDeclaratorTail(spec_info.base_qty);
    if (prev_hookp && !spec_info.base_qty.IsCompatible(real_qtype))
        *prev_hookp = real_qtype.RawTypep();
    DeclaratorInfo declarator_info{real_qtype, name, name_locp};
    return MakeDeclarator(decl_pos, spec_info, declarator_info, check_err);
}

unsigned char Parser::ParseQuals() {
    unsigned char qualifiers = 0;
    Token* tp = ts_.CurToken();
    while (true) {
        switch (tp->Tag()) {
            case TokenType::CONST:
                TrySetQual(qualifiers, QualType::kQualConst, tp->Loc());
                break;
            case TokenType::VOLATILE:
                TrySetQual(qualifiers, QualType::kQualVolatile, tp->Loc());
                break;
            case TokenType::RESTRICT:
                TrySetQual(qualifiers, QualType::kQualRestrict, tp->Loc());
                break;
            case TokenType::ATOMIC:
                Error("_Atomic is not supported yet", tp->Loc());
                break;
            default:
                return qualifiers;
        }
        tp = ts_.Next();
    }
}

QualType Parser::ParseDeclaratorTail(const QualType& base_qty) {
    try {
        if (ts_.CurIs(TokenType::LPAR)) {
            return ParseFuncDeclTail(base_qty);
        } else if (ts_.CurIs(TokenType::LSBRACKET)) {
            return ParseArrayDeclTail(base_qty);
        }
    } catch (const ParseError& e) {
        // Same as what we do in ParseDeclarator(), do not skip to the
        // synchronizing token.
        Error(e.what(), e.Loc());
    }
    return base_qty;
}

QualType Parser::ParseFuncDeclTail(const QualType& base_qty) {
    std::vector<ObjectPtr> params{};
    bool is_old_style = false;
    const SourceLoc& func_loc = ts_.CurToken()->Loc();
    scopesp_->EnterProto();
    try {
        // The scope should end when we finish parsing the parameters.
        auto finally = Finally([&](){ scopesp_->ExitProto(); });
        if (IsTypeNameToken(*ts_.Next())) {
            params = ParseFuncParamsList();
        } else {
            params = ParseKRFuncParamsList();
            is_old_style = true;
        }
    } catch (const ParseError& e) {
        // Try to handle the error.
        Error(e.what(), e.Loc());
        SkipToSyncToken();
        if (!ts_.CurIs(TokenType::RPAR))
            throw ParseError{"')' expected", ts_.CurToken()->LocPtr()};
    }
    ts_.Next();
    // Though extra tail means it must be wrong, continue parsing and use the
    // result as return type.
    QualType ret_qtype = ParseDeclaratorTail(base_qty);
    // Let CheckFuncRetQType() print error messages.
    CheckFuncRetQType(ret_qtype, func_loc);
    return MakeQType<FuncType>(ret_qtype, params, is_old_style);
}

namespace {

// Convert IdentPtr to ObjectPtr and make type adjustments.
ObjectPtr AdjustFuncParam(const IdentPtr& identp) {
    assert(IsFuncName(*identp) || IsObject(*identp));
    if (IsFuncName(*identp)) {
        // C11 6.7.6.3p8: A declaration of a parameter as "function returning
        // type" shall be adjusted to "pointer to function returning type"
        return MakeNodePtr<Object>(
                   identp->Locp(), ValueTrans(identp->QType()), identp->Name(),
                   LinkKind::kNoLink, StorKind::kAuto);
    } else if (IsArrayTy(identp->QType())) {
        // C11 6.7.6.3p7: A declaration of a parameter as "array of type"
        // shall be adjusted to "qualified pointer to type".
        ObjectPtr objp = NodepConv<Object>(identp);
        objp->UpdateQType(ValueTrans(objp->QType()));
        return objp;
    } else {
        return NodepConv<Object>(identp);
    }
}

void HandleParamRedefIfHas(const std::vector<ObjectPtr>& params,
                           ObjectPtr& objp) {
    auto is_redef = [&](const ObjectPtr& paramp) {
        return !objp->IsAnonymous() && !paramp->IsAnonymous() &&
               objp->Name() == paramp->Name(); };
    if (std::any_of(params.cbegin(), params.cend(), is_redef)) {
        Error("redefinition of parameter '" + objp->Name() + "'",
              objp->Loc());
        // Treat it as anonymous.
        objp = MakeNodePtr<Object>(objp->Locp(), objp->QType(), "",
                                   LinkKind::kNoLink, StorKind::kAuto);
    }
}

} // unnamed namespace

std::vector<ObjectPtr> Parser::ParseFuncParamsList() {
    std::vector<ObjectPtr> params{};
    if (ts_.CurIs(TokenType::VOID) && ts_.Try(TokenType::RPAR))
        return params;
    while (true) {
        DeclSpecInfo spec_info = ParseDeclSpec(DeclPos::kParam);
        IdentPtr identp = ParseDeclarator(DeclPos::kParam, spec_info);
        ObjectPtr objp = AdjustFuncParam(identp);
        // Though parameters may have incomplete type, void type is not
        // permitted.
        // C11 6.7.6.3p12: If the function declarator is not part of a
        // definition of that function, parameters may have incomplete
        // type.
        if (IsVoidTy(objp->QType())) {
            Error("argument may not have 'void' type", objp->Loc());
        } else {
            HandleParamRedefIfHas(params, objp);
            params.push_back(objp);
        }
        if (!ts_.CurIs(TokenType::COMMA))
            break;
        ts_.Next();
    }
    ExpectCur(TokenType::RPAR);
    return params;
}

std::vector<ObjectPtr> Parser::ParseKRFuncParamsList() {
    std::vector<ObjectPtr> params{};
    while (ts_.CurIs(TokenType::IDENTIFIER)) {
        ObjectPtr objp = MakeNodePtr<Object>(ts_.CurToken()->LocPtr(),
                                             QualType{},
                                             ts_.CurToken()->TokenStr());
        HandleParamRedefIfHas(params, objp);
        params.push_back(objp);
        ts_.Next();
        if (!ts_.CurIs(TokenType::COMMA))
            break;
        ts_.Next();
    }
    ExpectCur(TokenType::RPAR);
    return params;
}

QualType Parser::ParseArrayDeclTail(const QualType& base_qty) {
    const SourceLoc& arr_loc = ts_.CurToken()->Loc();
    ts_.Next();
    bool has_static = false;
    // C11 6.7.6.2p1: The optional type qualifiers and the keyword static shall
    // appear only in a declaration of a function parameter with an array type,
    // and then only in the outermost array type derivation.
    // TODO: Check if it is outermost.
    auto try_static = [&]() {
        if (ts_.CurIs(TokenType::STATIC)) {
            has_static = true;
            if (scopesp_->CurKind() != ScopeKind::kProto)
                Error("'static' used in array declarator outside of function "
                      "prototype", ts_.CurToken()->Loc());
            ts_.Next();
        }
    };
    try_static();
    // TODO: Apply these qualifiers when convert this array type to a pointer
    // type.
    unsigned char qualifiers = ParseQuals();
    std::size_t arr_size = 0;
    if (!has_static)
        try_static();
    if (ts_.CurIs(TokenType::AST)) {
        if (scopesp_->CurKind() != ScopeKind::kProto)
            Error("star modifier used outside of function prototype",
                  ts_.CurToken()->Loc());
        if (has_static)
            Error("'static' may not be used with an unspecified variable "
                  "length array size", ts_.CurToken()->Loc());
        arr_size = 1;
    } else if (!ts_.CurIs(TokenType::RSBRACKET)) {
        ConstantPtr constantp = ParseIntConstantExpr();
        if (constantp->IsNegInt())
            Error("declare an array with a negative size", constantp->Loc());
        arr_size = constantp->UIntVal();
    }
    ExpectCur(TokenType::RSBRACKET);
    ts_.Next();
    QualType elem_qty = ParseDeclaratorTail(base_qty);
    CheckArrElemQType(elem_qty, arr_loc);
    if (arr_size == 0)
        return MakeQType<ArrayType>(elem_qty);
    return MakeQType<ArrayType>(elem_qty, arr_size);
}

IdentPtr Parser::MakeDeclarator(const DeclPos& decl_pos,
                                const DeclSpecInfo& spec_info,
                                const DeclaratorInfo& d_info,
                                bool check_err) {
    auto err = [check_err, &loc = *d_info.locp](const std::string& msg) {
                   if (check_err) Error(msg, loc); };
    QualType qtype = d_info.qtype;
    if (spec_info.align != -1) {
        if (IsFuncTy(qtype))
            err("'_Alignas' attribute only applies to variables and fields");
        else
            qtype->SetAlign(spec_info.align);
    }
    if (spec_info.func_specs != 0) {
        if (!IsFuncTy(qtype)) {
            err("'inline' or '_Noreturn' can only appear on functions");
        } else {
            auto& func_type = TypeConv<FuncType>(qtype);
            if (spec_info.func_specs & FuncType::kFSInline)
                func_type.AddInline();
            if (spec_info.func_specs & FuncType::kFSNoreturn)
                func_type.AddNoreturn();
        }
    }
    if (spec_info.stor == StorSpec::kTypedef) {
        return MakeNodePtr<TypedefName>(d_info.locp, qtype, d_info.name);
    } else if (IsFuncTy(qtype)) {
        return MakeNodePtr<FuncName>(d_info.locp, qtype, d_info.name,
                                     GetLinkKind(decl_pos, spec_info, false));
    } else {
        bool has_def = (decl_pos == DeclPos::kGlobal ? false : true);
        return MakeNodePtr<Object>(d_info.locp, qtype, d_info.name,
                                   GetLinkKind(decl_pos, spec_info, true),
                                   GetStorKind(decl_pos, spec_info), has_def);
    }
}

LinkKind Parser::GetLinkKind(const DeclPos& decl_pos,
                             const DeclSpecInfo& spec_info,
                             bool is_obj) {
    if (decl_pos == DeclPos::kRecord || decl_pos == DeclPos::kTypeName)
        return LinkKind::kInvalid;
    if (spec_info.stor == StorSpec::kExtern ||
        (spec_info.stor == StorSpec::kNone && decl_pos == DeclPos::kGlobal) ||
        (spec_info.stor == StorSpec::kNone && !is_obj))
        return LinkKind::kExtern;
    if (spec_info.stor == StorSpec::kStatic && decl_pos == DeclPos::kGlobal)
        return LinkKind::kIntern;
    return LinkKind::kNoLink;
}

StorKind Parser::GetStorKind(const DeclPos& decl_pos,
                             const DeclSpecInfo& spec_info) {
    if (decl_pos == DeclPos::kRecord || decl_pos == DeclPos::kTypeName)
        return StorKind::kInvalid;
    if (spec_info.stor == StorSpec::kRegister)
        return StorKind::kRegister;
    if (spec_info.stor == StorSpec::kExtern ||
        spec_info.stor == StorSpec::kStatic ||
        (spec_info.stor == StorSpec::kNone && decl_pos == DeclPos::kGlobal))
        return StorKind::kStatic;
    return StorKind::kAuto;
}

bool Parser::CurIsFuncDef(const Ident& ident, const QualType& base_qty) {
    Token* tp = ts_.CurToken();
    if (!IsFuncName(ident) || IsTypedefName(ident))
        return false;
    const auto& func_type = TypeConv<FuncType>(ident.QType());
    // C11 6.9.1p2: The identifier declared in a function definition (which is
    // the name of the function) shall have a function type, as specified by
    // the declarator portion of the function definition.
    if (IsFuncTy(base_qty) && func_type.IsCompatible(base_qty))
        return false;
    if (tp->Tag() == TokenType::LBRACE ||
        (!func_type.HasProto() && IsTypeNameToken(*tp)))
        return true;
    return false;
}

namespace {

void FinishKRFuncParams(FuncType& func_type,
                        const std::vector<ObjDefStmtPtr>& decl_list) {
    std::vector<ObjectPtr> params = func_type.Params();
    // It contains name-index pairs because we have to keep the original order.
    std::map<std::string, int> params_map{};
    for (int i = 0; i < params.size(); ++i)
        params_map[params[i]->Name()] = i;
    // Redefinition should already has been handled inside ParseDeclStmt();
    // C11 6.9.1p6: If the declarator includes an identifier list, each
    // declaration in the declaration list shall have at least one declarator,
    // those declarators shall declare only identifiers from the identifier
    // list, and every identifier in the identifier list shall be declared.
    for (const auto& declp : decl_list) {
        const auto& objp = declp->Objp();
        if (objp->IsAnonymous()) {
            // Do nothing here. Empty declaration error has already been
            // reported inside ParseDeclStmt().
        } else if (params_map.find(objp->Name()) == params_map.cend()) {
            Error("parameter named '" + objp->Name() + "' is missing",
                  objp->Loc());
        } else {
            if (!objp->QType()->IsComplete()) {
                Error("variable has incomplete type", objp->Loc());
                objp->SetErrFlags();
            }
            // In order to maintain consistence with the processing of
            // redefinition, use the first definition.
            if (!params[params_map[objp->Name()]]->QType().HasRawType())
                params[params_map[objp->Name()]] = objp;
        }
    }
    for (int i = 0; i < params.size(); ++i) {
        if (!params[i]->QType().HasRawType()) {
            Warning("type of '" + params[i]->Name() + "' defaults to 'int'",
                    params[i]->Loc());
            params[i]->UpdateQType(MakeQType<ArithType>(ArithType::kASInt));
        }
    }
    func_type.UpdateParams(params);
}

} // unnamed namespace

FuncDefPtr Parser::ParseFuncDef(const IdentPtr& identp) {
    auto& func_type = TypeConv<FuncType>(identp->QType());
    if (!func_type.HasProto()) {
        // It is an old style function definition and we need to complete its
        // parameters.
        FinishKRFuncParams(func_type, TryParseKRFuncDeclList());
    } else {
        // Also check the parameters of new style functions here. Now they must
        // have complete type and can not be anonymous.
        for (const auto& paramp : func_type.Params()) {
            // Void type has already been diagnosed as an error. Do not repeat.
            if (!paramp->QType()->IsComplete() && !IsVoidTy(paramp->QType()))
                Error("variable has incomplete type", paramp->Loc());
            if (paramp->IsAnonymous())
                Error("parameter name omitted", paramp->Loc());
        }
    }
    ExpectCur(TokenType::LBRACE);
    func_type.EncounterDef();
    cur_ret_qty_ = func_type.RetQType();
    CmpdStmtPtr bodyp = ParseFuncBody(func_type);
    // We need to make sure each identifier corresponds to only one entity.
    // Related errors will be reported by TryAddToScope();
    IdentPtr used_identp = scopesp_->GetOrdIdentpInFileScope(identp->Name());
    if (!used_identp)
        used_identp = identp;
    FuncDefPtr defp = MakeNodePtr<FuncDef>(bodyp, used_identp, cur_local_vars_);
    cur_local_vars_.clear();
    return defp;
}

std::vector<ObjDefStmtPtr> Parser::TryParseKRFuncDeclList() {
    // Now we should be in the file scope and we need to enter a temporary
    // block scope in order to make TryAddToScope() works fine.
    scopesp_->EnterBlock();
    // In case something goes wrong during parsing declarations.
    auto finally = Finally([&](){ scopesp_->ExitBlock(); });
    std::vector<ObjDefStmtPtr> all_decls{};
    while (IsTypeNameToken(*ts_.CurToken())) {
        std::vector<ObjDefStmtPtr> decls = ParseDeclStmt(DeclPos::kParam);
        all_decls.insert(all_decls.end(), decls.begin(), decls.end());
        ts_.Next();
    }
    return all_decls;
}

CmpdStmtPtr Parser::ParseFuncBody(const FuncType& func_type) {
    // Leave the change operations of scope to ParseCmpdStmt() and all
    // parameters will merged into that block scope there.
    CmpdStmtPtr bodyp = ParseCmpdStmt(func_type.Params());
    // Report all undeclared labels.
    for (const auto& labelp : undecl_labels_)
        Error("use of undeclared label '" + labelp->Name() + "'", labelp->Loc());
    undecl_labels_.clear();
    return bodyp;
}

ObjDefStmtPtr Parser::ParseStaticInitializer(const ObjectPtr& objp) {
    ObjDefStmtPtr defp = ParseInitializer(objp);
    for (auto& init : defp->Inits())
        init.ResetExprp(Evaluator{}.EvalStaticInitializer(init.Exprp()));
    // We need to make sure each identifier corresponds to only one entity.
    IdentPtr identp = scopesp_->GetOrdIdentpInFileScope(objp->Name());
    // We only need to make sure that the identifier we got from scopes is an
    // object so that it can be used to create ObjDefStmt node. Other errors
    // will be reported by TryAddToScope().
    ObjectPtr used_objp = objp;
    if (identp && IsObject(*identp))
        used_objp = NodepConv<Object>(identp);
    return MakeNodePtr<ObjDefStmt>(used_objp, defp->Inits());
}

void Parser::AddTentativeDefIfNeed(const ObjectPtr& objp) {
    IdentPtr exist_identp = scopesp_->GetOrdIdentpInFileScope(objp->Name());
    if (exist_identp && NodeConv<Object>(*exist_identp).HasDef())
        return;
    // C11 6.9.2p3: If the declaration of an identifier for an object is a
    // tentative definition and has internal linkage, the declared type shall
    // not be an incomplete type.
    if (objp->Linkage() == LinkKind::kIntern && !objp->QType()->IsComplete())
        Warning("tentative definition of variable with internal linkage has "
                "incomplete type", objp->Loc());
    if (tentative_defs_.find(objp->Name()) == tentative_defs_.cend())
        tentative_defs_.emplace(objp->Name(), objp);
}

void Parser::RmTentativeDefIfHas(const std::string& name) {
    auto pos = tentative_defs_.find(name);
    if (pos != tentative_defs_.cend())
        tentative_defs_.erase(pos);
}

// Try to generate definitions for identifiers which has tentative definitions
// but no external definition in the translation unit.
void Parser::TryGenTentativeDefs() {
    for (const auto& pair : tentative_defs_) {
        const auto& objp = pair.second;
        if (!objp->QType()->IsComplete() && IsArrayTy(objp->QType())) {
            Warning("tentative array definition assumed to have one element",
                    objp->Loc());
            TypeConv<ArrayType>(objp->QType()).SetArrSize(1);
        }
        if (!objp->QType()->IsComplete()) {
            Error("tentative definition has type that is never completed",
                  objp->Loc());
        } else {
            objp->EncounterDef();
            ast_.AddNode(MakeNodePtr<ObjDefStmt>(objp));
        }
    }
}

void Parser::TryAddToScope(const IdentPtr& identp) {
    if (IsTag(*identp)) {
        TryAddTagToScope(NodepConv<Tag>(identp));
    } else if (IsLabel(*identp)) {
        TryAddLabelToScope(NodepConv<Label>(identp));
    } else {
        TryAddOrdIdentToScope(identp);
    }
}

void Parser::TryAddTagToScope(const TagPtr& tagp) {
    // Checks has already been performed in ParseRecordSpec().
    scopesp_->AddTag(tagp);
}

void Parser::TryAddLabelToScope(const LabelPtr& labelp) {
    if (scopesp_->HasLabel(labelp->Name())) {
        Error("redefinition of label '" + labelp->Name() + "'", labelp->Loc());
    } else {
        scopesp_->AddLabel(labelp);
    }
}

namespace {

void TryAddExistFuncName(const FuncNamePtr& exist_funcp,
                         const FuncNamePtr& funcp) {
    auto& exist_functy = TypeConv<FuncType>(exist_funcp->QType());
    const auto& new_functy = TypeConv<FuncType>(funcp->QType());
    if (exist_functy.HasDef() && new_functy.HasDef()) {
        Error("redefinition of '" + funcp->Name() + "'", funcp->Loc());
    } else if (new_functy.HasDef()) {
        exist_functy.UpdateParams(new_functy.Params());
        exist_functy.EncounterDef();
    }
}

void TryAddExistObject(const ObjectPtr& exist_objp, const ObjectPtr& objp) {
    if (exist_objp->HasDef() && objp->HasDef()) {
        Error("redefinition of '" + objp->Name() + "'", objp->Loc());
    } else if (objp->HasDef()) {
        exist_objp->EncounterDef();
    }
}

} // unnamed namespace

void Parser::TryAddOrdIdentToScope(const IdentPtr& identp) {
    IdentPtr exist_identp = scopesp_->GetOrdIdentpInCurScope(identp->Name());
    if (!exist_identp) {
        scopesp_->AddOrdIdent(identp);
    } else if (exist_identp->Kind() != identp->Kind()) {
        Error("redefinition of '" + identp->Name() + "' as different kind of "
              "symbol", identp->Loc());
    } else if (!exist_identp->QType().IsCompatible(identp->QType())) {
        // Do not need to check error flags of these two identifiers here.
        Error("conflicting types for '" + identp->Name() + "'", identp->Loc());
    } else if (exist_identp->Linkage() == LinkKind::kExtern &&
               identp->Linkage() != LinkKind::kExtern) {
        Error("non-extern declaration of '" + identp->Name() + "' follows "
              "extern declaration", identp->Loc());
    } else {
        switch (identp->Kind()) {
            case AstNodeKind::kTypedefName:
                // Nothing need to be done.
                break;
            case AstNodeKind::kEnumerator:
                Error("redefinition of enumerator '" + identp->Name() + "'",
                      identp->Loc());
                break;
            case AstNodeKind::kObject:
                TryAddExistObject(NodepConv<Object>(exist_identp),
                                  NodepConv<Object>(identp));
                break;
            case AstNodeKind::kFuncName:
                TryAddExistFuncName(NodepConv<FuncName>(exist_identp),
                                    NodepConv<FuncName>(identp));
                break;
            default:
                assert(false);
        }
    }
}

CmpdStmtPtr Parser::ParseCmpdStmt(const std::vector<ObjectPtr>& mereged_objs) {
    scopesp_->EnterBlock();
    // Unexpected end token will cause an exception.
    auto finally = Finally([&](){ return scopesp_->ExitBlock(); });
    for (const auto& objp : mereged_objs)
        scopesp_->AddOrdIdent(objp);
    std::vector<StmtPtr> stmts{};
    for (ts_.Next(); !ts_.CurIs(TokenType::RBRACE); ts_.Next()) {
        try {
            if (ts_.CurIs(TokenType::STATIC_ASSERT)) {
                ParseStaticAssert();
            } else if (IsTypeNameToken(*ts_.CurToken())) {
                std::vector<ObjDefStmtPtr> decls = ParseDeclStmt(DeclPos::kLocal);
                stmts.insert(stmts.end(), decls.begin(), decls.end());
            } else {
                stmts.push_back(ParseStmt());
            }
        } catch (const ParseError& e) {
            // Try to handle the error.
            Error(e.what(), e.Loc());
            SkipToSyncToken();
            if (IsEndToken(*ts_.CurToken()))
                throw ParseError{"'}' expected", ts_.CurToken()->LocPtr()};
            if (ts_.CurIs(TokenType::RBRACE))
                break;
        }
    }
    ExpectCur(TokenType::RBRACE);
    return MakeNodePtr<CmpdStmt>(stmts);
}

StmtPtr Parser::ParseStmt() {
    Token* tp = ts_.CurToken();
    try {
        switch (tp->Tag()) {
            case TokenType::LBRACE: return ParseCmpdStmt();
            case TokenType::IF: return ParseIfStmt();
            case TokenType::WHILE: return ParseWhileStmt();
            case TokenType::DO: return ParseDoStmt();
            case TokenType::FOR: return ParseForStmt();
            case TokenType::SWITCH: return ParseSwitchStmt();
            case TokenType::GOTO: return ParseGotoStmt();
            case TokenType::CONTINUE: return ParseContinueStmt();
            case TokenType::BREAK: return ParseBreakStmt();
            case TokenType::RETURN: return ParseReturnStmt();
            case TokenType::CASE: return ParseCaseStmt();
            case TokenType::DEFAULT: return ParseDefaultStmt();
            default:
                if (IsIdentToken(*tp) &&
                    ts_.LookAhead()->Tag() == TokenType::COLON) {
                    return ParseLabelStmt();
                } else {
                    return ParseExprStmt();
                }
        }
    } catch (const ParseError& e) {
        // Try to handle the error.
        Error(e.what(), e.Loc());
        SkipToSyncToken();
        if (!ts_.CurIs(TokenType::SCLN))
            throw ParseError{"';' expected", ts_.CurToken()->LocPtr()};
        // We do not want to return a null pointer here.
        return MakeNodePtr<NullStmt>();
    }
}

// The parameter declpos could be DeclPos::kLocal, DeclPos::kForLoop or
// DeclPos::kParam.
std::vector<ObjDefStmtPtr> Parser::ParseDeclStmt(const DeclPos& declpos) {
    std::vector<ObjDefStmtPtr> decls{};
    DeclSpecInfo spec_info = ParseDeclSpec(declpos);
    // Record local non-static variables before we leave.
    auto record_vars = [&]() {
        if (declpos != DeclPos::kParam && spec_info.stor != StorSpec::kStatic) {
            auto add_local_var = [&](const ObjDefStmtPtr& defp){
                cur_local_vars_.push_back(defp->Objp()); };
            std::for_each(decls.cbegin(), decls.cend(), add_local_var);
        }
    };
    auto finally = Finally(record_vars);
    while (true) {
        IdentPtr identp = ParseDeclarator(declpos, spec_info);
        if (identp->Name().empty()) {
            if (!IsRecordTy(identp->QType()))
                Warning("empty declaration does not declare anything",
                        identp->Loc());
            ExpectCur(TokenType::SCLN);
            return decls;
        } else if (IsTypedefName(*identp)) {
            // Only DeclPos::kLocal permit typedef.
            TryAddToScope(identp);
        } else if (IsFuncName(*identp)) {
            if (declpos == DeclPos::kForLoop)
                Error("declaration of non-local variable in 'for' loop",
                      identp->Loc());
            // Only DeclPos::kLocal permit static and extern specifier.
            if (identp->Linkage() == LinkKind::kNoLink) {
                Error("function declared in block scope cannot have 'static' "
                      "storage class", identp->Loc());
                TryAddToScope(identp);
            } else {
                HandleLocalExternDecl(identp);
            }
        } else {
            ObjectPtr objp = NodepConv<Object>(identp);
            if (ts_.CurIs(TokenType::ASGN)) {
                if (declpos == DeclPos::kParam)
                    Error("C does not support default arguments",
                          ts_.CurToken()->Loc());
                if (spec_info.stor == StorSpec::kExtern) {
                    Error("'extern' variable cannot have an initializer",
                          objp->Loc());
                    HandleLocalExternDecl(identp);
                } else {
                    TryAddToScope(identp);
                }
                ts_.Next();
                decls.push_back(ParseInitializer(objp));
            } else if (spec_info.stor == StorSpec::kExtern) {
                // Only DeclPos::kLocal permit extern.
                HandleLocalExternDecl(identp);
            } else {
                // No additional actions will be performed on local static
                // variables.
                TryAddToScope(identp);
                decls.push_back(MakeNodePtr<ObjDefStmt>(objp));
            }
            if (!objp->QType()->IsComplete()) {
                Error("variable has incomplete type", objp->Loc());
                objp->SetErrFlags();
            }
        }
        if (!ts_.CurIs(TokenType::COMMA)) {
            ExpectCur(TokenType::SCLN);
            return decls;
        }
        ts_.Next();
    }
}

// Only DeclPos::kLocal permit extern specifier.
void Parser::HandleLocalExternDecl(const IdentPtr& identp) {
    // We have to check three identifiers, one newly introduced, one this extern
    // declaration want to refer to and one existing in the current scope.
    IdentPtr exist_identp = scopesp_->GetOrdIdentpInCurScope(identp->Name());
    // leave other checks to TryAddToScope().
    if (exist_identp.get() != nullptr &&
        exist_identp->Linkage() != LinkKind::kExtern)
        // Error but still perform TryAddToScope() which will only play the
        // role of completing other checks.
        Error("extern declaration of '" + identp->Name() + "' follows "
              "non-extern declaration", identp->Loc());
    IdentPtr ext_identp = scopesp_->GetOrdIdentpInFileScope(identp->Name());
    // Insert this newly introduced identifier if there is any error.
    if (ext_identp.get() != nullptr) {
        if (IsTypedefName(*ext_identp)) {
            // Do nothing.
        } else if (ext_identp->Kind() != identp->Kind()) {
            Error("redefinition of '" + identp->Name() + "' as "
                  "different kind of symbol", identp->Loc());
        } else if (!ext_identp->QType().IsCompatible(identp->QType())) {
            Error("conflicting types for '" + identp->Name() + "'",
                  identp->Loc());
        } else {
            TryAddToScope(ext_identp);
            return;
        }
    }
    TryAddToScope(identp);
}

// C11 6.7.9 Initialization
ObjDefStmtPtr Parser::ParseInitializer(const ObjectPtr& objp) {
    // It is troublesome to detect this error in ParseArrayInitializer().
    if (IsCharArrayTy(objp->QType())) {
        if (!ts_.CurIs(TokenType::LBRACE) && !ts_.CurIs(TokenType::STRING))
            Error("array initializer must be an initializer list or string "
                  "literal", ts_.CurToken()->Loc());
    } else if (IsArrayTy(objp->QType()) && !ts_.CurIs(TokenType::LBRACE)) {
        Error("array initializer must be an initializer list",
              ts_.CurToken()->Loc());
    }
    // Special treatment for top-level struct or union initializer without braces.
    if (IsRecordTy(objp->QType()) && !ts_.CurIs(TokenType::LBRACE)) {
        ExprPtr initp = ParseAssignExpr();
        ConvAsIfByAsgn(initp, objp->QType());
        return MakeNodePtr<ObjDefStmt>(
                   objp, std::vector<Initializer>{{0, objp->QType().RawTypep(),
                                                   initp}});
    }
    auto inits = ParseInitializer(objp->QType(), 0, false, false);
    // Initializers need to be sorted by offset for facilitating generating the
    // code.
    auto cmp_off = [](const Initializer& lhs, const Initializer& rhs) {
        return lhs.Off() < rhs.Off();
    };
    std::sort(inits.begin(), inits.end(), cmp_off);
    return MakeNodePtr<ObjDefStmt>(objp, inits);
}

std::vector<Initializer> Parser::ParseInitializer(const QualType& qtype,
                                                  long long off,
                                                  bool designated,
                                                  bool follow_asgn) {
    try {
        if (designated &&
            !ts_.CurIs(TokenType::DOT) && !ts_.CurIs(TokenType::LSBRACKET)) {
            ExpectCur(TokenType::ASGN);
            ts_.Next();
            follow_asgn = true;
            designated = false;
        }
        if (IsRecordTy(qtype)) {
            return ParseRecordInitializer(TypeConv<RecordType>(qtype), off,
                                          designated, follow_asgn);
        } else if (IsArrayTy(qtype)) {
            return ParseArrayInitializer(TypeConv<ArrayType>(qtype), off,
                                         designated, follow_asgn);
        } else {
            return {ParseScalarInitializer(qtype, off)};
        }
    } catch (const ParseError& e) {
        SkipToSyncToken();
        // A rudimentary approach to handle this error.
        if (!ts_.CurIs(TokenType::RBRACE))
            throw;
        Error(e.what(), e.Loc());
        ts_.Next();
        return {};
    }
}

namespace {

void AddInitializers(std::vector<Initializer>& inits,
                     const std::vector<Initializer>& new_inits) {
    for (const auto& new_init : new_inits) {
        auto init_same_member = [&](const Initializer& init) {
            return init.Off() == new_init.Off();
        };
        auto dup_init_iter = std::find_if(inits.begin(), inits.end(),
                                          init_same_member);
        if (dup_init_iter != inits.end()) {
            Warning("initializer overrides prior initialization of this "
                    "subobject", new_init.Exprp()->Loc());
            *dup_init_iter = new_init;
        } else {
            inits.push_back(new_init);
        }
    }
}

} // unnamed namespace

std::vector<Initializer> Parser::ParseRecordInitializer(
                                     const RecordType& rec_type, long long off,
                                     bool designated, bool follow_asgn) {
    if (!rec_type.IsComplete())
        throw ParseError{"Try to initialize object with incomplete type",
                         ts_.CurToken()->LocPtr()};
    std::vector<Initializer> inits{};
    bool has_brace = ts_.CurIs(TokenType::LBRACE);
    if (has_brace)
        ts_.Next();
    // C11 6.7.9p13: The initializer for a structure or union object that has
    // automatic storage duration shall be either an initializer list as
    // described below, or a single expression that has compatible structure or
    // union type.
    if (RecordInitTrySingleExpr(inits, rec_type, off, designated, has_brace))
        return inits;
    const auto& members = rec_type.Members();
    auto iter = members.cbegin();
    while (true) {
        if (ts_.CurIs(TokenType::RBRACE)) {
            break;
        } else if (follow_asgn && !has_brace) {
            // This case means that an assignment expression is expected.
        } else if (ts_.CurIs(TokenType::LSBRACKET)) {
            throw ParseError{"array designator cannot initialize non-array "
                             "type", ts_.CurToken()->LocPtr()};
        } else if (ts_.CurIs(TokenType::DOT)) {
            ExpectNext(TokenType::IDENTIFIER);
            std::string member_name = ts_.CurToken()->TokenStr();
            ObjectPtr memberp = rec_type.GetMember(member_name);
            if (memberp.get() == nullptr)
                throw ParseError{"field designator '" + member_name + "' does "
                                 "not refer to any field",
                                 ts_.CurToken()->LocPtr()};
            // The iterator need to be updated.
            auto has_same_name = [&](const ObjectPtr& objp) {
                return objp->Name() == memberp->Name();
            };
            iter = std::find_if(members.cbegin(), members.cend(), has_same_name);
            designated = true;
            follow_asgn = false;
            ts_.Next();
        }
        auto cur_member_inits = ParseInitializer((*iter)->QType(),
                                                 off + (*iter)->Off(),
                                                 designated, follow_asgn);
        AddInitializers(inits, cur_member_inits);
        designated = false;
        follow_asgn = false;
        ++iter;
        bool all_init = IsUnionTy(rec_type) || iter == members.cend();
        if (CurObjInitIsEnd(all_init, designated, has_brace))
            break;
    }
    if (has_brace) {
        ExpectCur(TokenType::RBRACE);
        ts_.Next();
    }
    return inits;
}

bool Parser::RecordInitTrySingleExpr(std::vector<Initializer>& inits,
                                     const RecordType& rec_type,
                                     long long off,
                                     bool designated,
                                     bool has_brace) {
    if (!designated && !has_brace &&
        !ts_.CurIs(TokenType::DOT) && !ts_.CurIs(TokenType::LSBRACKET)) {
        auto cursor = ts_.SaveCursor();
        ExprPtr initp = ParseAssignExpr();
        if (initp->QType()->IsCompatible(rec_type)) {
            // Do not need to step forward now. Maintain consistency with
            // ParseScalarInitializer().
            inits.emplace_back(off, initp->QType().RawTypep(), initp);
            return true;
        } else {
            ts_.ResetCursor(cursor);
        }
    }
    return false;
}

std::vector<Initializer> Parser::ParseArrayInitializer(ArrayType& arr_type,
                                                       long long off,
                                                       bool designated,
                                                       bool follow_asgn) {
    std::vector<Initializer> inits{};
    bool has_brace = ts_.CurIs(TokenType::LBRACE);
    if (has_brace)
        ts_.Next();
    QualType elem_qtype = arr_type.ElemQType();
    if (ArrayInitTryStrLiteral(inits, arr_type, off, designated, has_brace))
        return inits;
    bool is_flexible = arr_type.IsComplete() && arr_type.ArrSize() == 0;
    std::size_t i = 0;
    std::size_t largest_index = 0;
    while (true) {
        if (ts_.CurIs(TokenType::RBRACE)) {
            break;
        } else if (follow_asgn && !has_brace) {
            // This case means that an assignment expression is expected.
        } else if (ts_.CurIs(TokenType::DOT)) {
            throw ParseError{"field designator cannot initialize a non-struct, "
                             "non-union type", ts_.CurToken()->LocPtr()};
        } else if (ts_.CurIs(TokenType::LSBRACKET)) {
            ts_.Next();
            ConstantPtr designatorp = ParseIntConstantExpr();
            if (designatorp->IsNegInt())
                Error("array designator value is negative", designatorp->Loc());
            i = designatorp->UIntVal();
            if (arr_type.IsComplete() && i >= arr_type.ArrSize())
                Error("array designator index exceeds array bounds",
                      designatorp->Loc());
            ExpectCur(TokenType::RSBRACKET);
            ts_.Next();
            designated = true;
            follow_asgn = false;
        }
        auto cur_elem_inits = ParseInitializer(elem_qtype,
                                               off + i * elem_qtype->Size(),
                                               designated, follow_asgn);
        AddInitializers(inits, cur_elem_inits);
        designated = false;
        follow_asgn = false;
        largest_index = i > largest_index ? i : largest_index;
        ++i;
        bool all_init = !is_flexible && arr_type.IsComplete() &&
                        i >= arr_type.ArrSize();
        if (CurObjInitIsEnd(all_init, designated, has_brace))
            break;
    }
    // C11 6.7.9p22: If an array of unknown size is initialized, its size is
    // determined by the largest indexed element with an explicit initializer.
    if (!arr_type.IsComplete())
        arr_type.SetArrSize(largest_index + 1);
    if (has_brace) {
        ExpectCur(TokenType::RBRACE);
        ts_.Next();
    }
    return inits;
}

bool Parser::ArrayInitTryStrLiteral(std::vector<Initializer>& inits,
                                    ArrayType& arr_type,
                                    long long off,
                                    bool designated,
                                    bool has_brace) {
    if (ts_.CurIs(TokenType::STRING) && IsCharArrayTy(arr_type)) {
        StrLiteralPtr strp = ParseStrLiterals();
        QualType char_qtype = TypeConv<ArrayType>(strp->QType()).ElemQType();
        if (char_qtype->Size() != arr_type.ElemQType()->Size()) {
            Error("initializing char array with incompatible string literal",
                  strp->Loc());
        }
        if (!arr_type.IsComplete())
            arr_type.SetArrSize(strp->StrSize() + 1);
        if (has_brace) {
            ts_.Try(TokenType::COMMA);
            ExpectNext(TokenType::RBRACE);
        }
        ts_.Next();
        inits.emplace_back(off, strp->QType().RawTypep(), strp);
        return true;
    }
    return false;
}

// Now we should meet comma or right brace. If we are stopping at the comma,
// we need to check whether we can "eat" it. Note that an extra comma is
// permitted.
bool Parser::CurObjInitIsEnd(bool all_init, bool designated, bool has_brace) {
    if (!ts_.CurIs(TokenType::COMMA)) {
        return true;
    } else if (all_init) {
        if (has_brace && ts_.NextIs(TokenType::RBRACE))
            ts_.Next();
        return true;
    } else if ((ts_.NextIs(TokenType::DOT) || ts_.NextIs(TokenType::LSBRACKET)) &&
               !designated && !has_brace) {
        // This designator does not belong to us.
        return true;
    } else {
        ts_.Next();
        return false;
    }
}

Initializer Parser::ParseScalarInitializer(const QualType& qtype, long long off) {
    // C11 6.7.9p11: The initializer for a scalar shall be a single expression,
    // optionally enclosed in braces.
    bool has_brace = ts_.CurIs(TokenType::LBRACE);
    if (has_brace)
        ts_.Next();
    ExprPtr initp = ParseAssignExpr();
    if (has_brace) {
        ExpectCur(TokenType::RBRACE);
        ts_.Next();
    }
    // Note that due to our error handing, the void type may appear here.
    ConvAsIfByAsgn(initp, qtype);
    return {off, qtype.RawTypep(), initp};
}

IfStmtPtr Parser::ParseIfStmt() {
    // Until C99, selection and iteration statements did not establish their
    // own block scopes
    scopesp_->EnterBlock();
    // An exception may be thrown in the following parsing.
    auto finally = Finally([&](){ scopesp_->ExitBlock(); });
    ts_.Next();
    ExprPtr condp = ParseParenExpr();
    StmtPtr thenp = ParseStmt();
    if (!ts_.Try(TokenType::ELSE))
        return MakeNodePtr<IfStmt>(condp, thenp);
    ts_.Next();
    StmtPtr elsep = ParseStmt();
    return MakeNodePtr<IfStmt>(condp, thenp, elsep);
}

// Construct while-loop:
// cond_label:
//     if (cond) {
//         loop_body
//         jump cond_label
//     }
// end_label:
CmpdStmtPtr Parser::ParseWhileStmt() {
    scopesp_->EnterBlock();
    LabelPtr cond_labelp = MakeNodePtr<Label>();
    LabelPtr end_labelp = MakeNodePtr<Label>();
    continue_dsts_.push(cond_labelp);
    break_dsts_.push(end_labelp);
    // An exception may be thrown in the following parsing.
    auto finally = Finally([&](){ scopesp_->ExitBlock();
                                  continue_dsts_.pop();
                                  break_dsts_.pop(); });
    // Parse.
    ts_.Next();
    ExprPtr condp = ParseParenExpr();
    StmtPtr loop_bodyp = ParseStmt();
    // Then construct the corresponding ast node.
    std::vector<StmtPtr> stmts{};
    stmts.push_back(MakeNodePtr<LabelStmt>(cond_labelp));
    CmpdStmtPtr if_truep =
        MakeNodePtr<CmpdStmt>(
            std::vector<StmtPtr>{loop_bodyp, MakeNodePtr<JumpStmt>(cond_labelp)});
    stmts.push_back(MakeNodePtr<IfStmt>(condp, if_truep));
    stmts.push_back(MakeNodePtr<LabelStmt>(end_labelp));
    return MakeNodePtr<CmpdStmt>(stmts);
}

// Construct do-while-loop:
// begin_label:
//     loop_body
// cond_label:
//     if (cond)
//         jump begin_label
// end_label:
CmpdStmtPtr Parser::ParseDoStmt() {
    scopesp_->EnterBlock();
    LabelPtr begin_labelp = MakeNodePtr<Label>();
    LabelPtr cond_labelp = MakeNodePtr<Label>();
    LabelPtr end_labelp = MakeNodePtr<Label>();
    continue_dsts_.push(cond_labelp);
    break_dsts_.push(end_labelp);
    // An exception may be thrown in the following parsing.
    auto finally = Finally([&](){ scopesp_->ExitBlock();
                                  continue_dsts_.pop();
                                  break_dsts_.pop(); });
    // Parse.
    ts_.Next();
    StmtPtr loop_bodyp = ParseStmt();
    ExpectNext(TokenType::WHILE);
    ts_.Next();
    ExprPtr condp = ParseParenExpr();
    ExpectCur(TokenType::SCLN);
    // Then construct the corresponding ast node.
    std::vector<StmtPtr> stmts{};
    stmts.push_back(MakeNodePtr<LabelStmt>(begin_labelp));
    stmts.push_back(loop_bodyp);
    stmts.push_back(MakeNodePtr<LabelStmt>(cond_labelp));
    JumpStmtPtr jump_beginp = MakeNodePtr<JumpStmt>(begin_labelp);
    stmts.push_back(MakeNodePtr<IfStmt>(condp, jump_beginp));
    stmts.push_back(MakeNodePtr<LabelStmt>(end_labelp));
    return MakeNodePtr<CmpdStmt>(stmts);
}

// Construct for-loop:
//     init_clause
// cond_label:
//     if (cond) {
//         loop_body
// iter_label:
//         iter
//         jump cond_label
//     }
// end_label:
CmpdStmtPtr Parser::ParseForStmt() {
    scopesp_->EnterBlock();
    LabelPtr cond_labelp = MakeNodePtr<Label>();
    LabelPtr iter_labelp = MakeNodePtr<Label>();
    LabelPtr end_labelp = MakeNodePtr<Label>();
    continue_dsts_.push(iter_labelp);
    break_dsts_.push(end_labelp);
    // An exception may be thrown in the following parsing.
    auto finally = Finally([&](){ continue_dsts_.pop();
                                  break_dsts_.pop();
                                  scopesp_->ExitBlock(); });
    // Parse.
    std::vector<StmtPtr> stmts{};
    ExpectNext(TokenType::LPAR);
    ts_.Next();
    if (IsTypeNameToken(*ts_.CurToken())) {
        std::vector<ObjDefStmtPtr> defs = ParseDeclStmt(DeclPos::kForLoop);
        stmts.insert(stmts.end(), defs.cbegin(), defs.cend());
    } else {
        stmts.push_back(ParseExprStmt());
    }
    ts_.Next();
    // Parse condition expression.
    ExprPtr condp{};
    if (ts_.CurIs(TokenType::SCLN)) {
        // C11 6.8.5.3p2: An omitted expression-2 is replaced by a nonzero
        // constant.
        condp = MakeNodePtr<Constant>(ts_.CurToken()->LocPtr(),
                                      MakeQType<ArithType>(ArithType::kASInt),
                                      1ull);
    } else {
        condp = NodeConv<ExprStmt>(*ParseExprStmt()).GetExprp();
    }
    ts_.Next();
    // Parse iteration expression.
    StmtPtr iterp{};
    if (ts_.CurIs(TokenType::RPAR))
        iterp = MakeNodePtr<NullStmt>();
    else
        iterp = MakeNodePtr<ExprStmt>(ParseExpr());
    ts_.Next();
    StmtPtr loop_bodyp = ParseStmt();
    // Then construct the corresponding ast node.
    stmts.push_back(MakeNodePtr<LabelStmt>(cond_labelp));
    CmpdStmtPtr if_truep = MakeNodePtr<CmpdStmt>(
                               std::vector<StmtPtr>{
                                   loop_bodyp,
                                   MakeNodePtr<LabelStmt>(iter_labelp),
                                   iterp,
                                   MakeNodePtr<JumpStmt>(cond_labelp)});
    stmts.push_back(MakeNodePtr<IfStmt>(condp, if_truep));
    stmts.push_back(MakeNodePtr<LabelStmt>(end_labelp));
    return MakeNodePtr<CmpdStmt>(stmts);
}

CmpdStmtPtr Parser::ParseSwitchStmt() {
    scopesp_->EnterBlock();
    LabelPtr end_labelp = MakeNodePtr<Label>();
    break_dsts_.push(end_labelp);
    cases_.emplace();
    default_labels_.emplace();
    // An exception may be thrown in the following parsing.
    auto finally = Finally([&](){ scopesp_->ExitBlock();
                                  break_dsts_.pop();
                                  cases_.pop();
                                  default_labels_.pop(); });
    // Parse.
    ts_.Next();
    SourceLocPtr ctrl_locp = ts_.CurToken()->LocPtr();
    ExprPtr ctrlp = ParseParenExpr();
    // C11 6.8.4.2p5: The integer promotions are performed on the
    // controlling expression. The constant expression in each case label
    // is converted to the promoted type of the controlling expression.
    QualType ctrl_qtype = TryIntPromote(ctrlp);
    if (!IsIntegerTy(ctrl_qtype)) {
        Error("statement requires expression of integer type", *ctrl_locp);
        ctrl_qtype =
            MakeQType<ArithType>(ArithType::kASLLong | ArithType::kASUnsigned);
    }
    StmtPtr switch_bodyp = ParseStmt();
    // Then construct the corresponding ast node.
    std::vector<StmtPtr> stmts{};
    // Construct a temporary variable.
    Initializer tmp_var_initp{0, ctrl_qtype.RawTypep(), ctrlp};
    TempObjPtr tmp_varp = MakeNodePtr<TempObj>(
                              ctrl_locp, ctrl_qtype,
                              std::vector<Initializer>{tmp_var_initp});
    cur_local_vars_.push_back(tmp_varp);
    stmts.push_back(MakeNodePtr<ExprStmt>(tmp_varp));
    GenSwitchJumpStmts(stmts, ctrl_qtype, tmp_varp, end_labelp);
    stmts.push_back(switch_bodyp);
    stmts.push_back(MakeNodePtr<LabelStmt>(end_labelp));
    return MakeNodePtr<CmpdStmt>(stmts);
}

void Parser::GenSwitchJumpStmts(std::vector<StmtPtr>& stmts,
                                const QualType& ctrl_qtype,
                                const ObjectPtr& tmp_varp,
                                const LabelPtr& end_labelp) {
    std::vector<unsigned long long> cases_val{};
    for (auto iter = cases_.top().cbegin(); iter != cases_.top().cend(); ++iter) {
        ExprPtr cur_case_exprp = iter->first;
        cur_case_exprp = MakeNodePtr<UnaryExpr>(
                             cur_case_exprp->Locp(), ctrl_qtype, cur_case_exprp);
        // Evaluate to get the correct value.
        ConstantPtr constantp = Evaluator{}.EvalIntConstantExpr(cur_case_exprp);
        if (std::find(cases_val.cbegin(), cases_val.cend(),
                      constantp->UIntVal()) != cases_val.cend()) {
            Error("duplicate case value", constantp->Loc());
        } else {
            cases_val.push_back(constantp->UIntVal());
        }
        ExprPtr cmp_condp =
            MakeNodePtr<BinaryExpr>(tmp_varp->Locp(), BinaryOpKind::kEqual,
                                    tmp_varp, constantp);
        JumpStmtPtr jump_casep = MakeNodePtr<JumpStmt>(iter->second);
        stmts.push_back(MakeNodePtr<IfStmt>(cmp_condp, jump_casep));
    }
    // Generate a default jump statement.
    if (default_labels_.top().get() != nullptr) {
        stmts.push_back(MakeNodePtr<JumpStmt>(default_labels_.top()));
    } else {
        stmts.push_back(MakeNodePtr<JumpStmt>(end_labelp));
    }
}

JumpStmtPtr Parser::ParseGotoStmt() {
    ts_.Next();
    std::string label_name = ts_.CurToken()->TokenStr();
    LabelPtr labelp = scopesp_->GetLabelp(label_name);
    auto iter = std::find_if(undecl_labels_.begin(), undecl_labels_.end(),
                             [&](const LabelPtr& lp){
                                 return lp->Name() == label_name; });
    // Guarantee that there is only one entity per label.
    if (iter != undecl_labels_.end())
        labelp = *iter;
    if (labelp.get() == nullptr) {
        labelp = MakeNodePtr<Label>(ts_.CurToken()->LocPtr(), label_name);
        undecl_labels_.push_back(labelp);
    }
    ExpectNext(TokenType::SCLN);
    return MakeNodePtr<JumpStmt>(labelp);
}

StmtPtr Parser::ParseContinueStmt() {
    const SourceLoc& loc = ts_.CurToken()->Loc();
    ExpectNext(TokenType::SCLN);
    if (continue_dsts_.empty()) {
        Error("'continue' statement not in loop statement", loc);
        return MakeNodePtr<NullStmt>();
    }
    return MakeNodePtr<JumpStmt>(continue_dsts_.top());
}

StmtPtr Parser::ParseBreakStmt() {
    const SourceLoc& loc = ts_.CurToken()->Loc();
    ExpectNext(TokenType::SCLN);
    if (break_dsts_.empty()) {
        Error("'break' statement not in loop or switch statement", loc);
        return MakeNodePtr<NullStmt>();
    }
    return MakeNodePtr<JumpStmt>(break_dsts_.top());
}

ReturnStmtPtr Parser::ParseReturnStmt() {
    ts_.Next();
    ExprPtr retp{};
    const SourceLoc& ret_loc = ts_.CurToken()->Loc();
    if (!ts_.CurIs(TokenType::SCLN))
        retp = ParseExpr();
    if (IsVoidTy(cur_ret_qty_)) {
        if (retp.get() != nullptr)
            Error("void function should not return a value", ret_loc);
    } else if (retp.get() == nullptr) {
        Error("non-void function should return a value", ret_loc);
    } else {
        ConvAsIfByAsgn(retp, cur_ret_qty_);
    }
    ExpectCur(TokenType::SCLN);
    return MakeNodePtr<ReturnStmt>(retp);
}

CmpdStmtPtr Parser::ParseCaseStmt() {
    LabelPtr case_labelp = MakeNodePtr<Label>();
    const SourceLoc& case_loc = ts_.CurToken()->Loc();
    ts_.Next();
    ConstantPtr constantp = ParseIntConstantExpr();
    if (cases_.empty()) {
        Error("'case' statement not in switch statement", case_loc);
    } else {
        cases_.top().emplace_back(constantp, case_labelp);
    }
    ExpectCur(TokenType::COLON);
    ts_.Next();
    StmtPtr stmtp = ParseStmt();
    return MakeNodePtr<CmpdStmt>(
               std::vector<StmtPtr>{MakeNodePtr<LabelStmt>(case_labelp), stmtp});
}

CmpdStmtPtr Parser::ParseDefaultStmt() {
    LabelPtr default_labelp = MakeNodePtr<Label>();
    if (default_labels_.empty()) {
        Error("'default' statement not in switch statement",
              ts_.CurToken()->Loc());
    } else if (default_labels_.top().get() != nullptr) {
        Error("multiple default labels in one switch", ts_.CurToken()->Loc());
    } else {
        default_labels_.top() = default_labelp;
    }
    ExpectNext(TokenType::COLON);
    ts_.Next();
    StmtPtr stmtp = ParseStmt();
    return MakeNodePtr<CmpdStmt>(
               std::vector<StmtPtr>{MakeNodePtr<LabelStmt>(default_labelp),
                                    stmtp});
}

// We can't just check if the label is followed by a statement and then return
// a LabelStmtPtr only. Consider the case where a label follows if which can
// not be parsed correctly if we only return a LabelStmtPtr.
CmpdStmtPtr Parser::ParseLabelStmt() {
    std::string label_name = ts_.CurToken()->TokenStr();
    LabelPtr labelp{};
    // Update undecl_labels_.
    auto iter = std::find_if(undecl_labels_.begin(), undecl_labels_.end(),
                             [&](const LabelPtr& lp){
                                 return lp->Name() == label_name; });
    if (iter != undecl_labels_.end()) {
        labelp = *iter;
        undecl_labels_.erase(iter);
    } else {
        labelp = MakeNodePtr<Label>(ts_.CurToken()->LocPtr(), label_name);
    }
    TryAddToScope(labelp);
    ExpectNext(TokenType::COLON);
    ts_.Next();
    StmtPtr stmtp = ParseStmt();
    return MakeNodePtr<CmpdStmt>(
               std::vector<StmtPtr>{MakeNodePtr<LabelStmt>(labelp), stmtp});
}

StmtPtr Parser::ParseExprStmt() {
    if (ts_.CurIs(TokenType::SCLN))
        return MakeNodePtr<NullStmt>();
    ExprPtr exprp = ParseExpr();
    ExpectCur(TokenType::SCLN);
    return MakeNodePtr<ExprStmt>(exprp);
}

namespace {

unsigned int CheckIntSuffix(const std::string& token_str, std::size_t pos,
                            const SourceLoc& loc) {
    unsigned int arith_kind = 0;
    for (; pos < token_str.size(); ++pos) {
        if (token_str[pos] == 'u' || token_str[pos] == 'U') {
            if (!(arith_kind & ArithType::kASUnsigned)) {
                arith_kind |= ArithType::kASUnsigned;
                continue;
            }
        } else if (token_str[pos] == 'l' || token_str[pos] == 'L') {
            if (!(arith_kind & ArithType::kASLong) &&
                !(arith_kind & ArithType::kASLLong)) {
                if (pos + 1 < token_str.size() &&
                    (token_str[pos + 1] == 'l' || token_str[pos + 1] == 'L')) {
                    arith_kind |= ArithType::kASLLong;
                    ++pos;
                } else {
                    arith_kind |= ArithType::kASLong;
                }
                continue;
            }
        }
        Error("invalid suffix on integer constant", loc);
        break;
    }
    return arith_kind;
}

QualType DetermineIntKind(unsigned long long val, unsigned int arith_kind) {
    if ((arith_kind & ArithType::kASLong) || (arith_kind & ArithType::kASLLong)) {
        if (val > ArithType::kSignedLongMax)
            arith_kind |= ArithType::kASUnsigned;
    } else if (arith_kind & ArithType::kASUnsigned) {
        if (val <= ArithType::kUnsignedIntMax) {
            arith_kind |= ArithType::kASInt;
        } else {
            arith_kind |= ArithType::kASLong;
        }
    } else {
        if (val <= ArithType::kSignedIntMax) {
            arith_kind |= ArithType::kASInt;
        } else if (val <= ArithType::kSignedLongMax) {
            arith_kind |= ArithType::kASLong;
        } else {
            arith_kind |= ArithType::kASLong;
            arith_kind |= ArithType::kASUnsigned;
        }
    }
    return MakeQType<ArithType>(arith_kind);
}

unsigned int CheckFloatSuffix(const std::string& token_str, std::size_t pos,
                              const SourceLoc& loc) {
    unsigned int arith_kind = 0;
    if (pos < token_str.size()) {
        if (token_str[pos] == 'f' || token_str[pos] == 'F') {
            arith_kind |= ArithType::kASFloat;
            ++pos;
        } else if (token_str[pos] == 'l' || token_str[pos] == 'L') {
            arith_kind |= ArithType::kASLong;
            arith_kind |= ArithType::kASDouble;
            ++pos;
        }
        if (pos < token_str.size())
            Error("invalid suffix on floating constant", loc);
    }
    return arith_kind;
}

bool IsValidUcn(unsigned int ucn) {
    // C11 6.4.3p2: A universal character name shall not specify a character
    // whose short identifier is less than 00A0 other than 0024 ($), 0040 (@),
    // or 0060 (‘), nor one in the range D800 through DFFF inclusive.
    return (ucn > 0xA0 || ucn == 0x24 || ucn == 0x40 || ucn == 0x60) &&
           (ucn < 0xD800 || ucn > 0xDFFF);
}

unsigned int HexCharToVal(char c) {
    if (c >= 'a' && c <= 'f') {
        return c - 'a' + 10;
    } else if (c >= 'A' && c <= 'F') {
        return c - 'A' + 10;
    } else {
        return c - '0';
    }
}

unsigned int ParseUcn(const std::string& token_str, std::size_t& pos,
                      const SourceLoc& loc, int len) {
    unsigned int val = 0;
    for (int i = 0; i < len; ++i) {
        if (!std::isxdigit(token_str[pos + 1])) {
            Error("incomplete universal character name", loc);
            return 0x24;
        }
        val = (val << 4) | HexCharToVal(token_str[++pos]);
    }
    if (!IsValidUcn(val)) {
        Error("invalid universal character", loc);
        return 0x24;    // Any valid value is alright.
    }
    return val;
}

unsigned int ParseHexChar(const std::string& token_str, std::size_t& pos,
                          const SourceLoc& loc) {
    unsigned int val = 0;
    // No need to check if it is out of bound.
    if (!std::isxdigit(token_str[pos + 1]))
        Error("\\x used with no following hex digits", loc);
    while (std::isxdigit(token_str[pos + 1]))
        val = (val << 4) | HexCharToVal(token_str[++pos]);
    return val;
}

unsigned int ParseOctalChar(const std::string& token_str, std::size_t& pos,
                            const SourceLoc& loc) {
    unsigned int val = token_str[pos] - '0';
    for (auto i : {1, 2}) {
        if (token_str[pos + 1] >= '0' && token_str[pos + 1] <= '7')
            val = (val << 3) | (token_str[++pos] - '0');
        else
            break;
    }
    return val;
}

unsigned int ParseEscapeSequence(const std::string& token_str, std::size_t& pos,
                                 const SourceLoc& loc) {
    switch (token_str[pos]) {
        case '\'':
        case '"':
        case '?':
        case '\\':
            return token_str[pos];
        case 'a': return '\a';
        case 'b': return '\b';
        case 'f': return '\f';
        case 'n': return '\n';
        case 'r': return '\r';
        case 't': return '\t';
        case 'v': return '\v';
        case 'x': return ParseHexChar(token_str, pos, loc);
        case 'u': return ParseUcn(token_str, pos, loc, 4);
        case 'U': return ParseUcn(token_str, pos, loc, 8);
        default:
            if (token_str[pos] >= '0' && token_str[pos] <= '7') {
                return ParseOctalChar(token_str, pos, loc);
            } else {
                Error("unknown escape sequence", loc);
                return 0;
            }
    }
}

} // unnamed namespace

ConstantPtr Parser::ParseIConstant() {
    Token* tp = ts_.CurToken();
    std::string token_str = tp->TokenStr();
    std::size_t pos = 0;
    unsigned long long val = 0;
    try {
        val = std::stoull(token_str, &pos, 0);
    } catch (const std::out_of_range& e) {
        Error("integer literal is too large to be represented in any integer "
              "type", tp->Loc());
        val = ArithType::kUnsignedLongMax;
    }
    unsigned int arith_kind = CheckIntSuffix(token_str, pos, tp->Loc());
    QualType integer_qty = DetermineIntKind(val, arith_kind);
    return MakeNodePtr<Constant>(tp->LocPtr(), integer_qty, val);
}

ConstantPtr Parser::ParseFConstant() {
    Token* tp = ts_.CurToken();
    std::string token_str = tp->TokenStr();
    std::size_t pos = 0;
    long double val = 0;
    try {
        val = std::stold(token_str, &pos);
    } catch (const std::out_of_range& e) {
        Error("floating-point constant is too large to be represented in any "
              "floating-point type", tp->Loc());
        val = ArithType::kDoubleMax;
    }
    unsigned int arith_kind = CheckFloatSuffix(token_str, pos, tp->Loc());
    // C11 6.4.4.2p4: An unsuffixed floating constant has type double.
    if (arith_kind == 0)
        arith_kind |= ArithType::kASDouble;
    return MakeNodePtr<Constant>(
               tp->LocPtr(), MakeQType<ArithType>(arith_kind), val);
}

ConstantPtr Parser::ParseCConstant() {
    Token* tp = ts_.CurToken();
    std::string token_str = tp->TokenStr();
    std::size_t pos = (token_str[0] == '\'' ? 1 : 2);
    unsigned int val = 0;
    unsigned int arith_kind = ArithType::kASInt;
    if (token_str[pos] == '\\') {
        val = ParseEscapeSequence(token_str, ++pos, tp->Loc());
        std::size_t str_size = 0;
        if (token_str[0] == '\'') {
            std::string str = Ucs4ToUtf8(val);
            str_size = str.size();
            val = static_cast<unsigned int>(str[0]);
        } else if (token_str[0] == 'u') {
            std::u16string u16str = Ucs4ToUtf16(val);
            str_size = u16str.size();
            arith_kind = ArithType::kASShort;
            val = static_cast<unsigned int>(u16str[0]);
        } else {
            std::u32string u32str = Ucs4ToUtf32(val);
            str_size = u32str.size();
            val = static_cast<unsigned int>(u32str[0]);
        }
        if (str_size > 1)
            Error("character too large for enclosing character literal type",
                  tp->Loc());
    } else {
        val = token_str[pos];
    }
    if (++pos < token_str.size() && token_str[pos] != '\'')
        Warning("multi-character character constant", tp->Loc());
    return MakeNodePtr<Constant>(tp->LocPtr(), MakeQType<ArithType>(arith_kind),
                                 static_cast<unsigned long long>(val));
}

StrLiteralPtr Parser::ParseStrLiterals() {
    StrLiteralPtr strp = ParseStrLiteral();
    while (ts_.Try(TokenType::STRING)) {
        strp->Concat(*ParseStrLiteral());
    }
    return strp;
}

StrLiteralPtr Parser::ParseStrLiteral() {
    Token* tp = ts_.CurToken();
    std::string token_str = tp->TokenStr();
    std::size_t pos = (token_str[0] == '\"' ? 1 : 2);
    std::string str{};
    for (; pos < token_str.size(); ++pos) {
        unsigned int val = 0;
        if (token_str[pos] == '\\') {
            val = ParseEscapeSequence(token_str, ++pos, tp->Loc());
            str += Ucs4ToUtf8(val);
        } else {
            str.push_back(token_str[pos]);
        }
    }
    EncKind enc = EncKind::kUtf8;
    if (token_str[0] == 'u' && token_str[1] == '\"') {
        enc = EncKind::kUtf16;
    } else if (token_str[0] == 'U' || token_str[0] == 'L') {
        enc = EncKind::kUtf32;
    }
    return MakeNodePtr<StrLiteral>(tp->LocPtr(), str, enc);
}

bool Parser::IsTypeNameToken(const Token& t) {
    // Also count storage class specifiers and function specifiers in because
    // whether count them or not, we have to check the if the result returned
    // by ParseDeclSpec() contains thoes specifiers.
    if (static_cast<int>(t.Tag()) >= static_cast<int>(TokenType::VOID) &&
        static_cast<int>(t.Tag()) <= static_cast<int>(TokenType::NO_RETURN))
        return true;
    if (IsIdentToken(t)) {
        IdentPtr identp = scopesp_->GetOrdIdentpInAllScope(t.TokenStr());
        if (identp.get() != nullptr && IsTypedefName(*identp))
            return true;
    }
    return false;
}

void Parser::ExpectCur(const TokenType& tag) {
    if (!ts_.CurIs(tag))
        throw ParseError{"'" + Token::TypeToStr(tag) + "' expected",
                         ts_.CurToken()->LocPtr()};
}

void Parser::ExpectNext(const TokenType& tag) {
    if (!ts_.Try(tag))
        throw ParseError{"'" + Token::TypeToStr(tag) + "' expected",
                         ts_.LookAhead()->LocPtr()};
}

void Parser::SkipToSyncToken() {
    int unmatch_left = 0;
    Token* tp = ts_.CurToken();
    TokenType pair_left = scopesp_->CurKind() == ScopeKind::kProto ?
                              TokenType::LPAR : TokenType::LBRACE;
    TokenType pair_right = scopesp_->CurKind() == ScopeKind::kProto ?
                               TokenType::RPAR : TokenType::RBRACE;
    while (!IsEndToken(*tp)) {
        if (tp->Tag() == TokenType::SCLN) {
            break;
        } else if (tp->Tag() == pair_left) {
            ++unmatch_left;
        } else if (tp->Tag() == pair_right) {
            if (unmatch_left == 0)
                break;
            --unmatch_left;
        }
        tp = ts_.Next();
    }
}

}
