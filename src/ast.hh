#ifndef _AXCC_AST_HH_
#define _AXCC_AST_HH_

#include <memory>
#include <vector>
#include <string>
#include <cassert>
#include <cstdlib>

#include "token.hh"
#include "type.hh"
#include "encoding.hh"
#include "error.hh"

namespace axcc {

// Forward Declartion
class AstNode;
class FuncDef;
class Stmt;
class NullStmt;
class ExprStmt;
class ObjDeclStmt;
class LabelStmt;
class IfStmt;
class JumpStmt;
class CmpdStmt;
class Expr;
class UnaryExpr;
class BinaryExpr;
class TernaryExpr;
class FuncCall;
class Constant;
class StrLiteral;
class AddrConstant;
class Ident;
class Tag;
class Label;
class FuncName;
class TypedefName;
class Enumerator;
class Object;
class BitField;

// Type alias
using AstNodePtr = std::shared_ptr<AstNode>;
using FuncDefPtr = std::shared_ptr<FuncDef>;
using StmtPtr = std::shared_ptr<Stmt>;
using NullStmtPtr = std::shared_ptr<NullStmt>;
using ExprStmtPtr = std::shared_ptr<ExprStmt>;
using ObjDeclStmtPtr = std::shared_ptr<ObjDeclStmt>;
using LabelStmtPtr = std::shared_ptr<LabelStmt>;
using IfStmtPtr = std::shared_ptr<IfStmt>;
using JumpStmtPtr = std::shared_ptr<JumpStmt>;
using CmpdStmtPtr = std::shared_ptr<CmpdStmt>;
using ExprPtr = std::shared_ptr<Expr>;
using UnaryExprPtr = std::shared_ptr<UnaryExpr>;
using BinaryExprPtr = std::shared_ptr<BinaryExpr>;
using TernaryExprPtr = std::shared_ptr<TernaryExpr>;
using FuncCallPtr = std::shared_ptr<FuncCall>;
using ConstantPtr = std::shared_ptr<Constant>;
using StrLiteralPtr = std::shared_ptr<StrLiteral>;
using AddrConstantPtr = std::shared_ptr<AddrConstant>;
using IdentPtr = std::shared_ptr<Ident>;
using TagPtr = std::shared_ptr<Tag>;
using LabelPtr = std::shared_ptr<Label>;
using FuncNamePtr = std::shared_ptr<FuncName>;
using TypedefNamePtr = std::shared_ptr<TypedefName>;
using EnumeratorPtr = std::shared_ptr<Enumerator>;
using ObjectPtr = std::shared_ptr<Object>;
using BitFieldPtr = std::shared_ptr<BitField>;

enum class AstNodeKind {
    kFuncDef,
    // Statements
    kNullStmt,
    kExprStmt,
    kObjDeclStmt,
    kLabelStmt,
    kIfStmt,
    kJumpStmt,
    kCmpdStmt,
    // Expressions
    kUnaryExpr,
    kBinaryExpr,
    kTernaryExpr,
    kFuncCall,
    kConstant,
    kStrLiteral,
    kAddrConstant,
    // Identifiers
    kTag,
    kLabel,
    kFuncName,
    kTypedefName,
    kEnumerator,
    kObject,
    kBitField,
};

class AstNode {
public:
    AstNode(const AstNode&) = delete;
    AstNode(AstNode&&) = delete;
    AstNode& operator=(const AstNode&) = delete;
    AstNode& operator=(AstNode&&) = delete;
    virtual ~AstNode() = default;
    AstNodeKind Kind() const { return kind_; }
protected:
    AstNode(const AstNodeKind& kind) : kind_{kind} {}
private:
    AstNodeKind kind_;
};

template<typename T, typename... Args>
std::shared_ptr<T> MakeNodePtr(Args&&... args) {
    return std::make_shared<T>(std::forward<Args>(args)...);
}

class FuncDef : public AstNode {
public:
    FuncDef(const CmpdStmtPtr& bodyp, const IdentPtr& identp)
        : AstNode{AstNodeKind::kFuncDef}, bodyp_{bodyp}, identp_{identp} {}
    CmpdStmtPtr FuncBodyp() const { return bodyp_; }
    IdentPtr FuncIdentp() const { return identp_; }
    // For allocating space on the stack.
    std::vector<ObjectPtr> LocalVars() const;
private:
    CmpdStmtPtr bodyp_;
    IdentPtr identp_;
};

// Statements
class Stmt : public AstNode {
protected:
    Stmt(const AstNodeKind& kind) : AstNode{kind} {}
};

class NullStmt : public Stmt {
public:
    NullStmt() : Stmt{AstNodeKind::kNullStmt} {}
};

class ExprStmt : public Stmt {
public:
    ExprStmt(const ExprPtr& exprp)
        : Stmt{AstNodeKind::kExprStmt}, exprp_{exprp} {}
    ExprPtr GetExprp() const { return exprp_; }
private:
    ExprPtr exprp_;
};

struct Initializer {
    int off;
    TypePtr typep;
    ExprPtr initp;
};

class ObjDeclStmt : public Stmt {
public:
    ObjDeclStmt(const ObjectPtr& objp, const std::vector<Initializer>& inits)
        : Stmt{AstNodeKind::kObjDeclStmt}, objp_{objp}, inits_{inits} {}
    ObjectPtr Objp() const { return objp_; }
    const std::vector<Initializer>& Inits() const { return inits_; }
private:
    ObjectPtr objp_;
    std::vector<Initializer> inits_;
};

class LabelStmt : public Stmt {
public:
    LabelStmt(const LabelPtr& labelp)
        : Stmt{AstNodeKind::kLabelStmt}, labelp_{labelp} {}
    LabelPtr GetLabelp() const { return labelp_; }
private:
    LabelPtr labelp_;
};

class IfStmt : public Stmt {
public:
    IfStmt(const ExprPtr& condp, const StmtPtr& thenp, const StmtPtr& elsep)
        : Stmt{AstNodeKind::kIfStmt}, condp_{condp}, thenp_{thenp}, elsep_{elsep} {}
    ExprPtr CondExprp() const { return condp_; }
    StmtPtr ThenStmtp() const { return thenp_; }
    StmtPtr ElseStmtp() const { return elsep_; }
private:
    ExprPtr condp_;
    StmtPtr thenp_;
    StmtPtr elsep_;
};

class JumpStmt : public Stmt {
public:
    JumpStmt(const LabelPtr& dstp) : Stmt{AstNodeKind::kJumpStmt}, dstp_{dstp} {}
    LabelPtr DstLabelp() const { return dstp_; }
private:
    LabelPtr dstp_;
};

class CmpdStmt : public Stmt {
public:
    CmpdStmt(const std::vector<StmtPtr>& stmts)
        : Stmt{AstNodeKind::kCmpdStmt}, stmts_{stmts} {}
    const std::vector<StmtPtr>& Stmts() const { return stmts_; }
private:
    std::vector<StmtPtr> stmts_;
};

// Expressions
class Expr : public Stmt {
public:
    const SourceLoc& Loc() const {
        assert(locp_.get() != nullptr); return *locp_; }
    SourceLocPtr Locp() const { return locp_; }
    QualType QType() const { return qtype_; }
    bool IsLVal() const { return is_lval_; }
    bool HasErr() const { return !err_flags_; }
    void SetErrFlags() { err_flags_ = 1; }
    void ErrInExpr(const std::string& msg, const SourceLoc& loc) {
        Error(msg, loc); SetErrFlags(); }
    void ErrInExpr(const std::string& msg) { ErrInExpr(msg, Loc()); }
protected:
    Expr(const AstNodeKind& kind, const SourceLocPtr& locp,
         const QualType& qtype = {})
        : Stmt{kind}, locp_{locp}, qtype_{qtype} {}
    // For identifiers, the location information may need to be modified.
    void SetLocp(const SourceLocPtr& locp) { locp_ = locp; }
    // In some cases, we need to set the type later.
    void SetQType(const QualType& qtype) { qtype_ = qtype; }
    void SetLVal() { is_lval_ = true; }
private:
    SourceLocPtr locp_;
    QualType qtype_{};
    bool is_lval_{false};
    // Now simply use it as a boolean.
    unsigned char err_flags_{0};
};

enum class UnaryOpKind {
    // Increment & Decrement
    kPreInc, kPreDec,
    kPostInc, kPostDec,
    // Arithmetic
    kPlus, kMinus, kBitNot,
    // Logical
    kLogicNot,
    // Member Access
    kDeref, kAddrOf,
    // Other
    kCast
};

class UnaryExpr : public Expr {
public:
    UnaryExpr(const SourceLocPtr& locp, const UnaryOpKind& op_kind,
              const ExprPtr& operandp);
    // For cast operation only
    UnaryExpr(const SourceLocPtr& locp, const QualType& qtype,
              const ExprPtr& operandp);
    UnaryOpKind OpKind() const { return op_kind_; }
    ExprPtr Operandp() const { return operandp_; }
private:
    void SetTypeIncDec();
    void SetTypePlusMinus();
    void SetTypeBitNot();
    void SetTypeLogicNot();
    void SetTypeDeref();
    void SetTypeAddrOf();
    void SetTypeCast();
    UnaryOpKind op_kind_;
    ExprPtr operandp_;
};

enum class BinaryOpKind {
    // Assignment
    kAsgn,
    // Arithmetic
    kAdd, kSub, kPro, kDiv, kMod,
    kBitAnd, kBitOr, kBitXor,
    kBitShl, kBitShr,
    // Logical
    kLogicAnd, kLogicOr,
    // Comparison
    kEqual, kNEqual,
    kLess, kGreater,
    kLessEq, kGreaterEq,
    // Member Access
    kMemAccs,
    // Other
    kComma
};

class BinaryExpr : public Expr {
public:
    BinaryExpr(const SourceLocPtr& locp, const BinaryOpKind& op_kind,
               const ExprPtr& lhsp, const ExprPtr& rhsp);
    BinaryOpKind OpKind() const { return op_kind_; }
    ExprPtr Lhsp() const { return lhsp_; }
    ExprPtr Rhsp() const { return rhsp_; }
private:
    void SetTypeAsgn();
    void SetTypeAddOps();
    void SetTypeMulOps();
    void SetTypeBitLogicOps();
    void SetTypeShiftOps();
    void SetTypeLogicOps();
    void SetTypeEqualOps();
    void SetTypeRelationOps();
    void SetTypeMemAccs();
    void SetTypeComma();
    BinaryOpKind op_kind_;
    ExprPtr lhsp_;
    ExprPtr rhsp_;
};

// Now only the conditional operator is ternary.
class TernaryExpr : public Expr {
public:
    TernaryExpr(const SourceLocPtr& locp, const ExprPtr& condp,
                const ExprPtr& truep, const ExprPtr& falsep);
    ExprPtr Condp() const { return condp_; }
    ExprPtr Truep() const { return truep_; }
    ExprPtr Falsep() const { return falsep_; }
private:
    ExprPtr condp_;
    ExprPtr truep_;
    ExprPtr falsep_;
};

class FuncCall : public Expr {
public:
    FuncCall(const SourceLocPtr& locp, const ExprPtr& funcp,
             const std::vector<ExprPtr>& args);
    ExprPtr Funcp() const { return funcp_; }
    const std::vector<ExprPtr>& Args() const { return args_; }
private:
    ExprPtr funcp_;
    std::vector<ExprPtr> args_;
};

class Constant : public Expr {
public:
    Constant(const SourceLocPtr& locp, const QualType& qtype,
             unsigned long long ull_val)
        : Expr{AstNodeKind::kConstant, locp, qtype}, ull_val_{ull_val} {}
    Constant(const SourceLocPtr& locp, const QualType& qtype,
             long double ld_val)
        : Expr{AstNodeKind::kConstant, locp, qtype}, ld_val_{ld_val} {}
    unsigned long long UIntVal() const { return ull_val_; }
    // unsigned long long to signed long long conversion is an
    // implementation-defined behavior.
    long long IntVal() const {
        return reinterpret_cast<const long long&>(ull_val_); }
    long double FloatVal() const { return ld_val_; }
private:
    union {
        unsigned long long ull_val_;
        long double ld_val_;
    };
};

class StrLiteral : public Expr {
public:
    StrLiteral(const SourceLocPtr& locp, const std::string& str,
               const EncKind& enc);
    std::string Str() const { return str_; }
    std::u16string U16Str() const { return Utf8ToUtf16(str_); }
    std::u32string U32Str() const { return Utf8ToUtf32(str_); }
    EncKind Encoding() const { return enc_; }
    LabelPtr Labelp() const { return labelp_; }
    void Concat(const StrLiteral& other);
private:
    std::string str_;
    EncKind enc_;
    LabelPtr labelp_{};
};

// Used for representing the node of address constant expression generated by
// evaluation of static initializers.
class AddrConstant : public Expr {
public:
    AddrConstant(const std::string& label_name, long long off = 0)
        : Expr{AstNodeKind::kAddrConstant, {}, {}},
          label_name_{label_name}, off_{off} {}
    AddrConstant(const StrLiteralPtr& literalp);
    void AddOff(long long off) { off_ += off; }
    std::string LabelName() const { return label_name_; }
    long long Off() const { return off_; }
    StrLiteralPtr Literalp() const { return literalp_; }
    std::string Addr() const { return label_name_ + std::to_string(off_); }
private:
    std::string label_name_;
    long long off_;
    StrLiteralPtr literalp_{};
};

// Linkage property only make sense in the cases of functions and objects.
enum class LinkKind {
    kInvalid,
    kNoLink,
    kIntern,
    kExtern
};

class Ident : public Expr {
public:
    std::string Name() const { return name_; }
    LinkKind Linkage() const { return link_; }
    void UpdateLocp(const SourceLocPtr& locp) { SetLocp(locp); }
protected:
    Ident(const AstNodeKind& kind, const SourceLocPtr& locp,
          const QualType& qtype, const std::string& name,
          const LinkKind& link = LinkKind::kInvalid)
        : Expr{kind, locp, qtype}, name_{name}, link_{link} {}
private:
    std::string name_;
    LinkKind link_;
};

class Tag : public Ident {
public:
    Tag(const SourceLocPtr& locp, const QualType& qtype, const std::string& name)
        : Ident{AstNodeKind::kTag, locp, qtype, name} {}
};

class Label : public Ident {
public:
    // If these null pointers are used, the assert should be triggered.
    Label(const SourceLocPtr& locp, const std::string& name)
        : Ident{AstNodeKind::kLabel, locp, {}, name} {}
    Label() : Ident{AstNodeKind::kLabel, {}, {}, GenLabelName()} {}
private:
    std::string GenLabelName() {
        static int c = 0;
        return ".L" + std::to_string(c++);
    }
};

class FuncName : public Ident {
public:
    FuncName(const SourceLocPtr& locp, const QualType& qtype,
             const std::string& name, const LinkKind& link)
        : Ident{AstNodeKind::kFuncName, locp, qtype, name, link} {}
};

class TypedefName : public Ident {
public:
    TypedefName(const SourceLocPtr& locp, const QualType& qtype,
                const std::string& name)
        : Ident{AstNodeKind::kTypedefName, locp, qtype, name} {}
};

class Enumerator : public Ident {
public:
    Enumerator(const SourceLocPtr& locp, const std::string& name, int val)
        : Ident{AstNodeKind::kEnumerator, locp,
                MakeQType<ArithType>(ArithType::kASInt), name},
          constantp_{MakeNodePtr<Constant>(locp,
                         MakeQType<ArithType>(ArithType::kASInt),
                         static_cast<unsigned long long>(val))} {}
    ConstantPtr Constantp() const { return constantp_; }
private:
    ConstantPtr constantp_;
};

// Storage property only make sense for normal object.
enum class StorKind {
    kInvalid,
    kAuto,
    kStatic,
    kRegister
};

// This class is also used to represent members in the struct/union.
class Object : public Ident {
public:
    // For normal object.
    Object(const SourceLocPtr& locp, const QualType& qtype,
           const std::string& name, const LinkKind& link,
           const StorKind& stor, bool has_def = true)
        : Ident{AstNodeKind::kObject, locp, qtype, name, link}, stor_{stor} {}
    // For struct member.
    Object(const SourceLocPtr& locp, const QualType& qtype,
           const std::string& name)
        : Object{locp, qtype, name, LinkKind::kInvalid, StorKind::kInvalid} {}
    StorKind Storage() const { return stor_; }
    void EncounterDef() { has_def_ = true; }
    bool HasDef() const { return has_def_; }
    // Empty name means anonymous object.
    bool IsAnonymous() const { return Name().empty(); }
    void SetOff(long long off) { off_ = off; }
    long long Off() const { return off_; }
    // In some cases, we need to reset the true type later.
    void UpdateQType(const QualType& qtype) { SetQType(qtype); }
protected:
    // For BitField, declp_ and stor_ make no sense any more.
    Object(const AstNodeKind& kind, const SourceLocPtr& locp,
           const QualType& qtype, const std::string& name)
        : Ident{kind, locp, qtype, name}, stor_{StorKind::kInvalid} {}
private:
    StorKind stor_;
    // Only when in file scope, we may need to initialize it to false.
    bool has_def_;
    // This field is used to record the offset of this object in the stack
    // during code generation, or the offset in the record type if this object
    // represent a member of a struct or union.
    long long off_{0};
};

class BitField : public Object {
public:
    BitField(const SourceLocPtr& locp, const QualType& qtype,
             const std::string& name, std::size_t width)
        : Object{AstNodeKind::kBitField, locp, qtype, name}, bit_width_{width} {}
    long long BitOff() const { return bit_off_; }
    void SetBitOff(long long bit_off) { bit_off_ = bit_off; }
    std::size_t BitWidth() const { return bit_width_; }
private:
    long long bit_off_{0};
    std::size_t bit_width_;
};

template<typename T>
T& NodeConv(AstNode& node) {
    return static_cast<T&>(node);
}

template<typename T>
const T& NodeConv(const AstNode& node) {
    return static_cast<const T&>(node);
}

template<typename T>
std::shared_ptr<T> NodepConv(const AstNodePtr& nodep) {
    return std::static_pointer_cast<T>(nodep);
}

}
#endif
