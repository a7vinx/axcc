#ifndef _AXCC_AST_HH_
#define _AXCC_AST_HH_

#include <memory>
#include <vector>
#include <string>
#include <cassert>

#include "token.hh"
#include "type.hh"
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

}
#endif
