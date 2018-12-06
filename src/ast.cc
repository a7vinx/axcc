#include "ast.hh"

namespace axcc {

std::vector<ObjectPtr> FuncDef::LocalVars() const {
    std::vector<ObjectPtr> local_vars{};
    for (const auto& stmtp : bodyp_->Stmts()) {
        if (IsObjDeclStmt(*stmtp))
            local_vars.emplace_back(NodeConv<ObjDeclStmt>(*stmtp).Objp());
    }
    return local_vars;
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
        case UnaryOpKind::kPlus:
        case UnaryOpKind::kMinus:
            SetTypePlusMinus();
        case UnaryOpKind::kBitNot:
            SetTypeBitNot();
        case UnaryOpKind::kLogicNot:
            SetTypeLogicNot();
        case UnaryOpKind::kDeref:
            SetTypeDeref();
        case UnaryOpKind::kAddrOf:
            SetTypeAddrOf();
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

}
