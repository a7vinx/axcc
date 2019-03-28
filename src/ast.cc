#include "ast.hh"

namespace axcc {

void FuncDef::Print(std::ostream& os, std::size_t indent) const {
    os << std::string(indent, ' ') << "FuncDef " << identp_->Repr() << "\n";
    bodyp_->Print(os, indent + 2);
}

void NullStmt::Print(std::ostream& os, std::size_t indent) const {
    os << std::string(indent, ' ') << "NullStmt\n";
}

void ExprStmt::Print(std::ostream& os, std::size_t indent) const {
    os << std::string(indent, ' ') << "ExprStmt\n";
    exprp_->Print(os, indent + 2);
}

void Initializer::Print(std::ostream& os, std::size_t indent) const {
    os << std::string(indent, ' ') << "Initializer ";
    os << "offset: " << off_ << '\n';
    exprp_->Print(os, indent + 2);
}

void ObjDefStmt::Print(std::ostream& os, std::size_t indent) const {
    os << std::string(indent, ' ') << "ObjDefStmt " << objp_->Repr() << "\n";
    for (const auto& init : inits_)
        init.Print(os, indent + 2);
}

void LabelStmt::Print(std::ostream& os, std::size_t indent) const {
    os << std::string(indent, ' ') << "LabelStmt " << labelp_->Repr() << "\n";
}

IfStmt::IfStmt(const ExprPtr& condp, const StmtPtr& thenp, const StmtPtr& elsep)
    : Stmt{AstNodeKind::kIfStmt}, condp_{condp}, thenp_{thenp}, elsep_{elsep} {
    if (!IsScalarTy(condp->QType()))
        Error("statement requires expression of scalar type", condp->Loc());
}

void IfStmt::Print(std::ostream& os, std::size_t indent) const {
    os << std::string(indent, ' ') << "IfStmt\n";
    condp_->Print(os, indent + 2);
    thenp_->Print(os, indent + 2);
    if (elsep_.get() != nullptr)
        elsep_->Print(os, indent + 2);
}

void JumpStmt::Print(std::ostream& os, std::size_t indent) const {
    os << std::string(indent, ' ') << "JumpStmt " << dstp_->Repr() << "\n";
}

void ReturnStmt::Print(std::ostream& os, std::size_t indent) const {
    os << std::string(indent, ' ') << "ReturnStmt\n";
    if (ret_exprp_.get() != nullptr)
        ret_exprp_->Print(os, indent + 2);
}

void CmpdStmt::Print(std::ostream& os, std::size_t indent) const {
    os << std::string(indent, ' ') << "CmpdStmt\n";
    for (const auto& stmtp : stmts_)
        stmtp->Print(os, indent + 2);
}

UnaryExpr::UnaryExpr(const SourceLocPtr& locp, const UnaryOpKind& op_kind,
                     const ExprPtr& operandp)
    : Expr{AstNodeKind::kUnaryExpr, locp},
      op_kind_{op_kind}, operandp_{operandp} {
    assert(op_kind != UnaryOpKind::kCast);
    if (!operandp_ || operandp_->HasErr()) {
        // Suppress subsequent errors.
        SetErrFlags();
        return;
    }
    // Do type checking and set the expression's type and lvalue property.
    switch (op_kind_) {
        case UnaryOpKind::kPreInc:
        case UnaryOpKind::kPreDec:
        case UnaryOpKind::kPostInc:
        case UnaryOpKind::kPostDec:
            SetTypeIncDec();
            break;
        case UnaryOpKind::kPlus:
        case UnaryOpKind::kMinus:
            SetTypePlusMinus();
            break;
        case UnaryOpKind::kBitNot:
            SetTypeBitNot();
            break;
        case UnaryOpKind::kLogicNot:
            SetTypeLogicNot();
            break;
        case UnaryOpKind::kDeref:
            SetTypeDeref();
            break;
        case UnaryOpKind::kAddrOf:
            SetTypeAddrOf();
            break;
        default:
            assert(false);
    }
}

UnaryExpr::UnaryExpr(const SourceLocPtr& locp, const QualType& dst_qtype,
                     const ExprPtr& operandp)
    : Expr{AstNodeKind::kUnaryExpr, locp, dst_qtype},
      op_kind_{UnaryOpKind::kCast}, operandp_{operandp} {
    if (!operandp_ || operandp_->HasErr()) {
        SetErrFlags();
        return;
    }
    SetTypeCast();
}

// C11 6.5.2.4p1 & 6.5.3.1p1: The operand of the postfix/prefix increment or
// decrement operator shall have atomic, qualified, or unqualified real or
// pointer type, and shall be a modifiable lvalue.
void UnaryExpr::SetTypeIncDec() {
    // We don't need to perform the value transformation on the array type or
    // the function type first because whether it is transformed or not, they
    // can not pass the check of IsModifiableLVal().
    if (!IsModifiableLVal(*operandp_)) {
        ErrInExpr("operand should be a modifiable lvalue");
    } else if (!IsScalarTy(operandp_->QType())) {
        ErrInExpr("operand of real or pointer type expected");
    } else {
        SetQType(LoseAllQuals(operandp_->QType()));
    }
}

// C11 6.5.3.3p1: The operand of the unary + or - operator shall have
// arithmetic type; of the ~ operator, integer type; of the ! operator,
// scalar type.
void UnaryExpr::SetTypePlusMinus() {
    if (!IsArithTy(operandp_->QType())) {
        ErrInExpr("operand of arithmetic type expected");
    } else {
        // First apply integral promotions.
        SetQType(TryIntPromote(operandp_));
    }
}

void UnaryExpr::SetTypeBitNot() {
    if (!IsIntegerTy(operandp_->QType())) {
        ErrInExpr("operand of integer type expected");
    } else {
        SetQType(TryIntPromote(operandp_));
    }
}

void UnaryExpr::SetTypeLogicNot() {
    QualType operand_qty = ValueTrans(operandp_->QType());
    if (!IsScalarTy(operand_qty))
        // Do not set the error flags here so that we can check for more errors.
        Error("operand of scalar type expected", Loc());
    SetQType(MakeQType<ArithType>(ArithType::kASInt));
}

// C11 6.5.3.2p2: The operand of the unary * operator shall have pointer type.
void UnaryExpr::SetTypeDeref() {
    QualType operand_qty = ValueTrans(operandp_->QType());
    if (!IsPointerTy(operand_qty)) {
        ErrInExpr("operand of pointer type expected");
    } else {
        SetQType(TypeConv<PointerType>(operand_qty).PointeeQTy());
        if (!IsFuncTy(QType()))
            SetLVal();
    }
}

// C11 6.5.3.2p1: The operand of the unary & operator shall be either a function
// designator, the result of a [] or unary * operator, or an lvalue that
// designates an object that is not a bit-field and is not declared with the
// register storage-class specifier.
void UnaryExpr::SetTypeAddrOf() {
    // The cases where the operand is the result of a [] or unary * operator are
    // left to the parser to handle.
    if (!IsFuncTy(operandp_->QType()) && !operandp_->IsLVal()) {
        ErrInExpr("operand should be an lvalue or have a function type");
    } else if (IsObject(*operandp_) &&
               NodeConv<Object>(*operandp_).Storage() == StorKind::kRegister) {
        ErrInExpr("address of register variable requested");
    } else {
        if (IsBinaryExpr(*operandp_)) {
            auto& bin_expr = NodeConv<BinaryExpr>(*operandp_);
            if (bin_expr.OpKind() == BinaryOpKind::kMemAccs &&
                IsBitField(*bin_expr.Rhsp())) {
                ErrInExpr("address of bit-field requested");
                return;
            }
        }
        SetQType(MakeQType<PointerType>(operandp_->QType()));
    }
}

// C11 6.5.4 Cast operators
void UnaryExpr::SetTypeCast() {
    QualType operand_qty = ValueTrans(operandp_->QType());
    if (IsVoidTy(QType())) {
        // If the target type is void, then expression is evaluated for its
        // side-effects and its returned value is discarded.
    } else if (!IsScalarTy(QType())) {
        ErrInExpr("scalar type expected");
    } else if (!IsScalarTy(operand_qty)) {
        ErrInExpr("operand of scalar type expected", operandp_->Loc());
    } else if (IsPointerTy(QType()) && IsFloatingTy(operand_qty)) {
        ErrInExpr("float type can not be convert to pointer type");
    } else if (IsPointerTy(operand_qty) && IsFloatingTy(QType())) {
        ErrInExpr("pointer type can not be convert to float type");
    } else {
        if ((IsObjPtrTy(QType()) && IsFuncPtrTy(operand_qty)) ||
            (IsObjPtrTy(operand_qty) && IsFuncPtrTy(QType()))) {
            // In all C standards the conversion between pointer-to-function and
            // pointer-to-object is not defined.
            Warning("incompatible pointer types conversion", Loc());
        }
        // Reset the type. The value category of the cast expression is always
        // non-lvalue. Use LoseAllQuals() here, not ValueTrans().
        SetQType(LoseAllQuals(QType()));
    }
}

void UnaryExpr::Print(std::ostream& os, std::size_t indent) const {
    os << std::string(indent, ' ') << "UnaryExpr '" << QType().Repr() << "' ";
    switch (op_kind_) {
        case UnaryOpKind::kPreInc: os << "prefix '++'"; break;
        case UnaryOpKind::kPreDec: os << "prefix '--'"; break;
        case UnaryOpKind::kPostInc: os << "postfix '++'"; break;
        case UnaryOpKind::kPostDec: os << "postfix '--'"; break;
        case UnaryOpKind::kPlus: os << "'+'"; break;
        case UnaryOpKind::kMinus: os << "'-'"; break;
        case UnaryOpKind::kBitNot: os << "'~'"; break;
        case UnaryOpKind::kLogicNot: os << "'!'"; break;
        case UnaryOpKind::kDeref: os << "'*'"; break;
        case UnaryOpKind::kAddrOf: os << "'&'"; break;
        case UnaryOpKind::kCast: os << "cast"; break;
        default: assert(false);
    }
    os << "\n";
    operandp_->Print(os, indent + 2);
}

BinaryExpr::BinaryExpr(const SourceLocPtr& locp, const BinaryOpKind& op_kind,
                       const ExprPtr& lhsp, const ExprPtr& rhsp)
    : Expr{AstNodeKind::kBinaryExpr, locp},
      op_kind_{op_kind}, lhsp_{lhsp}, rhsp_{rhsp} {
    if (!lhsp_ || !rhsp_ || lhsp_->HasErr() || rhsp_->HasErr()) {
        SetErrFlags();
        return;
    }
    switch (op_kind_) {
        case BinaryOpKind::kAsgn:
            SetTypeAsgn();
            break;
        case BinaryOpKind::kAdd:
        case BinaryOpKind::kSub:
            SetTypeAddOps();
            break;
        case BinaryOpKind::kPro:
        case BinaryOpKind::kDiv:
        case BinaryOpKind::kMod:
            SetTypeMulOps();
            break;
        case BinaryOpKind::kBitAnd:
        case BinaryOpKind::kBitOr:
        case BinaryOpKind::kBitXor:
            SetTypeBitLogicOps();
            break;
        case BinaryOpKind::kBitShl:
        case BinaryOpKind::kBitShr:
            SetTypeShiftOps();
            break;
        case BinaryOpKind::kLogicAnd:
        case BinaryOpKind::kLogicOr:
            SetTypeLogicOps();
            break;
        case BinaryOpKind::kEqual:
        case BinaryOpKind::kNEqual:
            SetTypeEqualOps();
            break;
        case BinaryOpKind::kLess:
        case BinaryOpKind::kGreater:
        case BinaryOpKind::kLessEq:
        case BinaryOpKind::kGreaterEq:
            SetTypeRelationOps();
            break;
        case BinaryOpKind::kMemAccs:
            SetTypeMemAccs();
            break;
        case BinaryOpKind::kComma:
            SetTypeComma();
            break;
        default:
            assert(false);
    }
}

// C11 6.5.16.1 Simple assignment
void BinaryExpr::SetTypeAsgn() {
    if (!IsModifiableLVal(*lhsp_)) {
        ErrInExpr("left operand should be a modifiable lvalue");
    } else {
        SetQType(ConvAsIfByAsgn(rhsp_, lhsp_->QType()));
        // We don't have to set the error flags here if errors occur during
        // the type conversion.
    }
}

// C11 6.5.6 Additive operators
void BinaryExpr::SetTypeAddOps() {
    QualType lhs_qtype = ValueTrans(lhsp_->QType());
    QualType rhs_qtype = ValueTrans(rhsp_->QType());
    if (IsArithTy(lhs_qtype) && IsArithTy(rhs_qtype)) {
        SetQType(UsualArithConv(lhsp_, rhsp_));
    } else if (IsPointerTy(lhs_qtype) || IsPointerTy(rhs_qtype)) {
        auto is_complete_obj_ptr_ty = [this] (QualType ptr_qtype) {
            if (!IsVoidPtrTy(ptr_qtype) && !IsFuncPtrTy(ptr_qtype) &&
                TypeConv<PointerType>(ptr_qtype).PointeeQTy()->IsComplete()) {
                ErrInExpr("arithmetic on incomplete object type");
                return false;
            } else {
                if (IsVoidPtrTy(ptr_qtype)) {
                    Warning("arithmetic on pointer to void type");
                } else if (IsFuncPtrTy(ptr_qtype)) {
                    Warning("arithmetic on pointer to function type");
                }
                return true;
            }
        };
        if (IsPointerTy(lhs_qtype) && IsPointerTy(rhs_qtype) &&
            op_kind_ == BinaryOpKind::kSub) {
            QualType lhs_pte_qty = TypeConv<PointerType>(lhs_qtype).PointeeQTy();
            QualType rhs_pte_qty = TypeConv<PointerType>(rhs_qtype).PointeeQTy();
            if (!lhs_pte_qty->IsCompatible(rhs_pte_qty)) {
                ErrInExpr("arithmetic on pointers to incompatible types");
            } else if (is_complete_obj_ptr_ty(lhs_qtype)) {
                // C11 6.5.6p9: The size of the result is implementation-defined,
                // and its type (a signed integer type) is ptrdiff_t defined in
                // the <stddef.h> header.
                SetQType(MakeQType<ArithType>(ArithType::kASLong));
            }
        } else if (!IsIntegerTy(lhs_qtype) && !IsIntegerTy(rhs_qtype)) {
            ErrInExpr("invalid operands to additive operators");
        } else {
            QualType ptr_qtype = IsIntegerTy(lhs_qtype) ? rhs_qtype : lhs_qtype;
            if (is_complete_obj_ptr_ty(ptr_qtype))
                SetQType(ptr_qtype);
        }
    } else {
        ErrInExpr("invalid operands to additive operators");
    }
}

// C11 6.5.5p2: Each of the operands shall have arithmetic type. The operands of
// the % operator shall have integer type.
void BinaryExpr::SetTypeMulOps() {
    if (op_kind_ == BinaryOpKind::kMod &&
        (!IsIntegerTy(lhsp_->QType()) || !IsIntegerTy(rhsp_->QType()))) {
        ErrInExpr("operands of integer type expected");
    } else if (!IsArithTy(lhsp_->QType()) || !IsArithTy(rhsp_->QType())) {
        ErrInExpr("operands of arithmetic type expected");
    } else {
        SetQType(UsualArithConv(lhsp_, rhsp_));
    }
}

// C11 6.5.10p2 & 6.5.11p2 & 6.5.12p2 : Each of the operands shall have integer
// type.
void BinaryExpr::SetTypeBitLogicOps() {
    if (!IsIntegerTy(lhsp_->QType()) || !IsIntegerTy(rhsp_->QType())) {
        ErrInExpr("operands of integer type expected");
    } else {
        SetQType(UsualArithConv(lhsp_, rhsp_));
    }
}

// C11 6.5.7p2: Each of the operands shall have integer type.
void BinaryExpr::SetTypeShiftOps() {
    if (!IsIntegerTy(lhsp_->QType()) || !IsIntegerTy(rhsp_->QType())) {
        ErrInExpr("operands of integer type expected");
    } else {
        IntPromote(rhsp_);
        SetQType(IntPromote(lhsp_));
    }
}

// C11 6.5.13p2 & 6.5.14p2: Each of the operands shall have scalar type.
void BinaryExpr::SetTypeLogicOps() {
    QualType lhs_qtype = ValueTrans(lhsp_->QType());
    QualType rhs_qtype = ValueTrans(rhsp_->QType());
    if (!IsScalarTy(lhs_qtype) || !IsScalarTy(rhs_qtype))
        Error("operands of scalar type expected", Loc());
    SetQType(MakeQType<ArithType>(ArithType::kASInt));
}

// C11 6.5.9 Equality operators
void BinaryExpr::SetTypeEqualOps() {
    QualType lhs_qtype = ValueTrans(lhsp_->QType());
    QualType rhs_qtype = ValueTrans(rhsp_->QType());
    if (IsArithTy(lhs_qtype) && IsArithTy(rhs_qtype)) {
        UsualArithConv(lhsp_, rhsp_);
    } else if (IsPointerTy(lhs_qtype) && IsPointerTy(rhs_qtype)) {
        QualType lhs_pte_qty = TypeConv<PointerType>(lhs_qtype).PointeeQTy();
        QualType rhs_pte_qty = TypeConv<PointerType>(rhs_qtype).PointeeQTy();
        if (!lhs_pte_qty->IsCompatible(rhs_pte_qty)) {
            if (!IsVoidTy(lhs_pte_qty) && !IsVoidTy(rhs_pte_qty)) {
                Warning("equality comparison between pointers to incompatible "
                        "types", Loc());
            } else if (IsFuncTy(lhs_pte_qty) || IsFuncTy(rhs_pte_qty)) {
                Warning("equality comparison between function pointer and "
                        "void pointer", Loc());
            }
        }
    } else {
        Error("invalid operands to equality operators", Loc());
    }
    SetQType(MakeQType<ArithType>(ArithType::kASInt));
}

// C11 6.5.8 Relational operators
void BinaryExpr::SetTypeRelationOps() {
    QualType lhs_qtype = ValueTrans(lhsp_->QType());
    QualType rhs_qtype = ValueTrans(rhsp_->QType());
    if (IsArithTy(lhs_qtype) && IsArithTy(rhs_qtype)) {
        UsualArithConv(lhsp_, rhsp_);
    } else if (IsPointerTy(lhs_qtype) && IsPointerTy(rhs_qtype)) {
        if (!TypeConv<PointerType>(lhs_qtype).PointeeQTy()->IsCompatible(
                 TypeConv<PointerType>(rhs_qtype).PointeeQTy())) {
            Warning("comparsion between pointers to incompatible types", Loc());
        } else if (IsFuncPtrTy(lhs_qtype) || IsFuncPtrTy(rhs_qtype)) {
            Warning("comparsion with function pointer", Loc());
        }
    } else {
        Error("invalid operands to relational operators", Loc());
    }
    SetQType(MakeQType<ArithType>(ArithType::kASInt));
}

// C11 6.5.2.3p1: The first operand of the . operator shall have an atomic,
// qualified, or unqualified structure or union type, and the second operand
// shall name a member of that type.
void BinaryExpr::SetTypeMemAccs() {
    assert(IsObject(*rhsp_));
    if (!IsRecordTy(lhsp_->QType())) {
        ErrInExpr("left operand should be a structure or union");
    } else {
        std::string name = NodeConv<Object>(*rhsp_).Name();
        ObjectPtr memberp = TypeConv<RecordType>(lhsp_->QType()).GetMember(name);
        if (memberp.get() == nullptr) {
            ErrInExpr("no member named '" + name + "' in the struct/union type"
                      "of left operand");
        } else {
            // Replace the temporarily constructed object with the one stored
            // in the struct/union.
            rhsp_ = memberp;
            if (rhsp_->HasErr()) {
                SetErrFlags();
                return;
            }
            QualType member_qtype = memberp->QType();
            member_qtype.MergeQuals(lhsp_->QType());
            SetQType(member_qtype);
            if (lhsp_->IsLVal())
                SetLVal();
        }
    }
}

// C11 6.5.17 Comma operator
void BinaryExpr::SetTypeComma() {
    SetQType(ValueTrans(rhsp_->QType()));
}

void BinaryExpr::Print(std::ostream& os, std::size_t indent) const {
    os << std::string(indent, ' ') << "BinaryExpr '" << QType().Repr() << "' ";
    switch (op_kind_) {
        case BinaryOpKind::kAsgn: os << "'='"; break;
        case BinaryOpKind::kAdd: os << "'+'"; break;
        case BinaryOpKind::kSub: os << "'-'"; break;
        case BinaryOpKind::kPro: os << "'*'"; break;
        case BinaryOpKind::kDiv: os << "'/'"; break;
        case BinaryOpKind::kMod: os << "'%'"; break;
        case BinaryOpKind::kBitAnd: os << "'&'"; break;
        case BinaryOpKind::kBitOr: os << "'|'"; break;
        case BinaryOpKind::kBitXor: os << "'^'"; break;
        case BinaryOpKind::kBitShl: os << "'<<'"; break;
        case BinaryOpKind::kBitShr: os << "'>>'"; break;
        case BinaryOpKind::kLogicAnd: os << "'&&'"; break;
        case BinaryOpKind::kLogicOr: os << "'||'"; break;
        case BinaryOpKind::kEqual: os << "'=='"; break;
        case BinaryOpKind::kNEqual: os << "'!='"; break;
        case BinaryOpKind::kLess: os << "'<'"; break;
        case BinaryOpKind::kGreater: os << "'>'"; break;
        case BinaryOpKind::kLessEq: os << "'<='"; break;
        case BinaryOpKind::kGreaterEq: os << "'>='"; break;
        case BinaryOpKind::kMemAccs: os << "'.'"; break;
        case BinaryOpKind::kComma: os << "','"; break;
        default: assert(false);
    }
    os << "\n";
    lhsp_->Print(os, indent + 2);
    rhsp_->Print(os, indent + 2);
}

// C11 6.5.15 Conditional operator
TernaryExpr::TernaryExpr(const SourceLocPtr& locp, const ExprPtr& condp,
                         const ExprPtr& truep, const ExprPtr& falsep)
    : Expr{AstNodeKind::kTernaryExpr, locp},
      condp_{condp}, truep_{truep}, falsep_{falsep} {
    if (!condp_ || !truep_ || !falsep_ ||
        condp_->HasErr() || truep_->HasErr() || falsep_->HasErr()) {
        SetErrFlags();
        return;
    }
    QualType lhs_qtype = ValueTrans(truep_->QType());
    QualType rhs_qtype = ValueTrans(falsep_->QType());
    if (!IsScalarTy(ValueTrans(condp_->QType()))) {
        ErrInExpr("the condition expression should have scalar type");
    } else if (IsArithTy(lhs_qtype) && IsArithTy(rhs_qtype)) {
        SetQType(UsualArithConv(truep_, falsep_));
    } else if (IsPointerTy(lhs_qtype) && IsPointerTy(rhs_qtype)) {
        QualType lhs_pte_qty = TypeConv<PointerType>(lhs_qtype).PointeeQTy();
        QualType rhs_pte_qty = TypeConv<PointerType>(rhs_qtype).PointeeQTy();
        QualType final_pte_qty = lhs_pte_qty;
        if (!lhs_pte_qty->IsCompatible(rhs_pte_qty)) {
            if (IsVoidTy(lhs_pte_qty) || IsVoidTy(rhs_pte_qty)) {
                if (IsFuncTy(lhs_pte_qty) || IsFuncTy(rhs_pte_qty))
                    Warning("conditional operator with function pointer and "
                            "void pointer", Loc());
            } else {
                Warning("pointer type mismatch", Loc());
            }
            final_pte_qty = MakeQType<VoidType>();
            final_pte_qty.MergeQuals(lhs_pte_qty);
        }
        final_pte_qty.MergeQuals(rhs_pte_qty);
        SetQType(MakeQType<PointerType>(final_pte_qty));
    } else if (lhs_qtype->IsCompatible(rhs_qtype)) {
        // Now the type must be void type or struct/union type.
        assert(IsVoidTy(lhs_qtype) || IsRecordTy(lhs_qtype));
        SetQType(lhs_qtype);
    } else {
        ErrInExpr("incompatible operand types");
    }
}

void TernaryExpr::Print(std::ostream& os, std::size_t indent) const {
    os << std::string(indent, ' ') << "ConditionalExpr '";
    os << QType().Repr() << "'\n";
    condp_->Print(os, indent + 2);
    truep_->Print(os, indent + 2);
    falsep_->Print(os, indent + 2);
}

// C11 6.5.2.2 Function calls
FuncCall::FuncCall(const SourceLocPtr& locp, const ExprPtr& funcp,
                   const std::vector<ExprPtr>& args)
    : Expr{AstNodeKind::kFuncCall, locp},
      funcp_{funcp}, args_{args} {
    // Only checks if there are some errors in the function pointer expression
    // and put off the check of arguments.
    if (!funcp_ || funcp_->HasErr()) {
        SetErrFlags();
        return;
    }
    if (!IsFuncPtrTy(funcp_->QType())) {
        ErrInExpr("called object is not a function or function pointer");
    } else {
        QualType funcp_qty = TypeConv<PointerType>(funcp_->QType()).PointeeQTy();
        const auto& func_type = TypeConv<FuncType>(funcp_qty);
        QualType ret_qtype = func_type.RetQType();
        const auto& params = func_type.Params();
        // The function type we got here will never return an array type or a
        // function type. This kind of error will be diagnosed by the parser.
        assert(!IsArrayTy(ret_qtype) && !IsFuncTy(ret_qtype));
        if (!IsVoidTy(ret_qtype) && !ret_qtype->IsComplete()) {
            ErrInExpr("calling function with incomplete return type");
        } else {
            if (params.size() > args_.size()) {
                Error("too few arguments to function call", Loc());
            } else if (params.size() < args_.size()) {
                Error("too many arguments to function call", Loc());
            } else {
                for (int i = 0; i < args_.size(); ++i) {
                    if (args_[i]->HasErr()) {
                        // Do nothing here.
                    } else if (!args_[i]->QType()->IsComplete()) {
                        Error("argument type is incomplete", Loc());
                    } else {
                        ConvAsIfByAsgn(args_[i], params[i]->QType());
                    }
                }
            }
            SetQType(LoseAllQuals(ret_qtype));
        }
    }
}

void FuncCall::Print(std::ostream& os, std::size_t indent) const {
    os << std::string(indent, ' ') << "FuncCall '" << QType().Repr() << "'\n";
    funcp_->Print(os, indent + 2);
    for (const auto& argp : args_)
        argp->Print(os, indent + 2);
}

void Constant::Print(std::ostream& os, std::size_t indent) const {
    os << std::string(indent, ' ') << "Constant '" << QType().Repr() << "' ";
    if (IsSignedTy(QType()))
        os << ll_val_ << "\n";
    else
        os << ull_val_ << "\n";
}

StrLiteral::StrLiteral(const SourceLocPtr& locp, const std::string& str,
                       const EncKind& enc)
    : Expr{AstNodeKind::kStrLiteral, locp}, str_{str}, enc_{enc} {
    unsigned int arith_kind = 0;
    std::size_t str_size = 0;
    switch (enc_) {
        case EncKind::kUtf8:
            arith_kind |= ArithType::kASChar;
            str_size = Str().size();
            break;
        case EncKind::kUtf16:
            arith_kind |= ArithType::kASShort;
            str_size = U16Str().size();
            break;
        case EncKind::kUtf32:
            arith_kind |= ArithType::kASInt;
            str_size = U32Str().size();
            break;
        default: assert(false);
    }
    SetQType(MakeQType<ArrayType>(MakeQType<ArithType>(arith_kind), str_size));
}

void StrLiteral::Print(std::ostream& os, std::size_t indent) const {
    os << std::string(indent, ' ') << "StrLiteral '" << QType().Repr() << "' ";
    os << std::hex;
    for (const auto c : str_)
        os << "\\x" << static_cast<unsigned int>(c);
    os << std::dec << "\n";
}

// C11 6.4.5p5: If any of the tokens has an encoding prefix, the resulting
// multibyte character sequence is treated as having the same prefix;
void StrLiteral::Concat(const StrLiteral& other) {
    str_ += other.str_;
    if (enc_ == EncKind::kUtf8 &&
        (other.enc_ == EncKind::kUtf16 || other.enc_ == EncKind::kUtf32))
        enc_ = other.enc_;
}

AddrConstant::AddrConstant(const StrLiteralPtr& literalp)
    : AddrConstant(literalp->Labelp()->Name()) {
    literalp_ = literalp;
}

void AddrConstant::Print(std::ostream& os, std::size_t indent) const {
    os << std::string(indent, ' ') << "AddrConstant ";
    os << "offset: " << off_;
    if (literalp_.get() != nullptr) {
        os << "\n";
        literalp_->Print(os, indent + 2);
    } else {
        os << " label name: " << label_name_ << '\n';
    }
}

void Ident::Print(std::ostream& os, std::size_t indent) const {
    os << std::string(indent, ' ') << "Identifier " << Repr() << "\n";
}

std::string Ident::Repr() const {
    std::string repr{};
    if (QType().HasRawType()) {
        repr += "'";
        repr += QType().Repr();
        repr += "' ";
    }
    repr += name_;
    return repr;
}

void TempObj::Print(std::ostream& os, std::size_t indent) const {
    os << std::string(indent, ' ') << "TempObj " << Repr() << '\n';
    for (const auto& init : inits_)
        init.Print(os, indent + 2);
}

void AstRoot::Print(std::ostream& os) const {
    for (const auto& ext_declp : ext_decls_)
        ext_declp->Print(os, 0);
}

// C11 6.3.2.1p1: A modifiable lvalue is an lvalue that does not have array
// type, does not have an incomplete type, does not have a const-qualified
// type, and if it is a structure or union, does not have any member
// (including, recursively, any member or element of all contained aggregates
// or unions) with a const-qualified type.
bool IsModifiableLVal(const Expr& expr) {
    if (!expr.IsLVal())
        return false;
    QualType qtype = expr.QType();
    if (!qtype->IsComplete() || qtype.IsConst() || IsArrayTy(qtype))
        return false;
    if (IsRecordTy(qtype))
        return !TypeConv<RecordType>(qtype).HasConstMember();
    return true;
}

}
