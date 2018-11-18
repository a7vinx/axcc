#include <algorithm>

#include "type.hh"
#include "ast.hh"
#include "error.hh"

namespace axcc {

bool VoidType::IsCompatible(const Type& other) const {
    return IsVoidTy(other);
}

const std::size_t ArithType::kCharWidth = 1;
const std::size_t ArithType::kShortWidth = 2;
const std::size_t ArithType::kIntWidth = 4;
const std::size_t ArithType::kLongWidth = 8;

ArithType::ArithType(const ArithTyKind& kind, const ArithTySize& size,
                     const ArithTySign& sign)
    : Type{TypeKind::kArith, true},
      aty_kind_{kind}, aty_size_{size}, aty_sign_{sign} {
    if (aty_sign_ == ArithTySign::kDefault)
        aty_sign_ = ArithTySign::kSigned;
    if (aty_kind_ == ArithTyKind::kDefault)
        aty_kind_ = ArithTyKind::kInt;
    switch (aty_size_) {
        case ArithTySize::kShort:
            SetSize(kShortWidth); break;
        case ArithTySize::kLong:
        case ArithTySize::kLLong:
            SetSize(kLongWidth); break;
        case ArithTySize::kDefault:
            switch (aty_kind_) {
                case ArithTyKind::kBool:
                case ArithTyKind::kChar:
                    SetSize(kCharWidth); break;
                case ArithTyKind::kInt:
                case ArithTyKind::kFloat:
                    SetSize(kIntWidth); break;
                case ArithTyKind::kDouble:
                    SetSize(kLongWidth); break;
                default:
                    assert(false);
            }
            break;
        default:
            assert(false);
    }
    SetAlign(Size());
}

bool ArithType::IsCompatible(const Type& other) const {
    if (!IsArithTy(other))
        return false;
    const auto& other_arithty = TypeConv<ArithType>(other);
    return (aty_kind_ == other_arithty.aty_kind_ &&
            aty_size_ == other_arithty.aty_size_ &&
            aty_sign_ == other_arithty.aty_sign_);
}

int ArithType::ConvRank() const {
    int rank = 0;
    switch (aty_kind_) {
        case ArithTyKind::kBool: rank = 1; break;
        case ArithTyKind::kChar: rank = 2; break;
        case ArithTyKind::kInt:
            switch (aty_size_) {
                case ArithTySize::kShort: rank = 3; break;
                case ArithTySize::kDefault: rank = 4; break;
                case ArithTySize::kLong: rank = 5; break;
                case ArithTySize::kLLong: rank = 6; break;
                default: assert(false);
            }
            break;
        case ArithTyKind::kFloat: rank = 7; break;
        case ArithTyKind::kDouble:
            switch (aty_size_) {
                case ArithTySize::kDefault: rank = 8; break;
                case ArithTySize::kLong: rank = 9; break;
                default: assert(false);
            }
            break;
        default:
            assert(false);
    }
    return rank;
}

bool PointerType::IsCompatible(const Type& other) const {
    if (!IsPointerTy(other))
        return false;
    const auto& other_ptrty = TypeConv<PointerType>(other);
    return pointee_qty_.IsCompatible(other_ptrty.pointee_qty_);
}

void ArrayType::SetArrSize(std::size_t arr_size) {
    arr_size_ = arr_size;
    SetSize(arr_size * elem_qty_->Size());
    SetComplete();
}

bool ArrayType::IsCompatible(const Type& other) const {
    if (!IsArrayTy(other))
        return false;
    const auto& other_arrty = TypeConv<ArrayType>(other);
    return (elem_qty_.IsCompatible(other_arrty.elem_qty_) &&
            (IsComplete() != other_arrty.IsComplete() ||
             arr_size_ == other_arrty.arr_size_));
}

bool FuncType::IsCompatible(const Type& other) const {
    if (!IsFuncTy(other))
        return false;
    const auto& other_functy = TypeConv<FuncType>(other);
    if (!ret_qty_->IsCompatible(other_functy.ret_qty_) ||
        params_.size() != other_functy.params_.size())
        return false;
    auto self_piter = params_.cbegin();
    auto other_piter = other_functy.params_.cbegin();
    while (self_piter != params_.cend()) {
        if (!(*self_piter)->QType().IsCompatible((*other_piter)->QType()))
            return false;
        ++self_piter;
        ++other_piter;
    }
    return true;
}

QualType LoseAllQuals(const QualType& qtype) {
    QualType unqual_ty{qtype};
    unqual_ty.LoseAllQuals();
    return unqual_ty;
}

// C11 6.3.2.1 Lvalues, arrays, and functions designators
QualType ValueTrans(const QualType& qtype) {
    if (IsArrayTy(qtype)) {
        const auto& arr_type = TypeConv<ArrayType>(qtype);
        return MakeQType<PointerType>(arr_type.ElemQType());
    } else if (IsFuncTy(qtype)) {
        return MakeQType<PointerType>(qtype);
    }
    return LoseAllQuals(qtype);
}

// C11 6.3.1.1p2: If an int can represent all values of the original type (as
// restricted by the width, for a bit-field), the value is converted to an int;
// otherwise, it is converted to an unsigned int. These are called the integer
// promotions. All other types are unchanged by the integer promotions.
QualType IntPromote(const QualType& qtype) {
    assert(IsIntegerTy(qtype));
    const auto& arith_type = TypeConv<ArithType>(qtype);
    auto int_typep = std::make_shared<ArithType>(ArithTyKind::kInt);
    if (arith_type.ConvRank() < int_typep->ConvRank())
        return QualType{int_typep};
    return LoseAllQuals(qtype);
}

QualType IntPromote(ExprPtr& exprp) {
    assert(IsIntegerTy(exprp->QType()));
    QualType promoted_qty = IntPromote(exprp->QType());
    if (!exprp->QType()->IsCompatible(promoted_qty))
        exprp = MakeNodePtr<UnaryExpr>(exprp->Locp(), promoted_qty, exprp);
    return promoted_qty;
}

// C11 6.3.1.8 Usual arithmetic conversions
QualType UsualArithConv(ExprPtr& lhsp, ExprPtr& rhsp) {
    assert(IsArithTy(lhsp->QType()) && IsArithTy(rhsp->QType()));
    const auto& lhs_arithty = TypeConv<ArithType>(lhsp->QType());
    const auto& rhs_arithty = TypeConv<ArithType>(rhsp->QType());
    QualType highest_qty = lhs_arithty.ConvRank() < rhs_arithty.ConvRank() ?
                               rhsp->QType() : lhsp->QType();
    highest_qty = TryIntPromote(highest_qty);
    auto cast_if_need = [&highest_qty](ExprPtr& exprp) {
        if (!exprp->QType()->IsCompatible(highest_qty))
            exprp = MakeNodePtr<UnaryExpr>(exprp->Locp(), highest_qty, exprp);
    };
    cast_if_need(lhsp);
    cast_if_need(rhsp);
    return highest_qty;
}

// C11 6.5.16.1 Simple assignment
// NULL pointer constant is defined as ((void*)0) so it will be handled by
// cast operator, where no error message will be generated.
QualType ConvAsIfByAsgn(ExprPtr& exprp, const QualType& dst_qtype) {
    QualType expr_qtype = ValueTrans(exprp->QType());
    // dst_qtype shall not be array type or function type. LoseAllQuals() is
    // enough here.
    assert(!IsArrayTy(dst_qtype) && !IsFuncTy(dst_qtype));
    QualType ret_qtype = LoseAllQuals(dst_qtype);
    if (expr_qtype->IsCompatible(dst_qtype))
        return ret_qtype;
    // Only check those cases which the cast operation permit while implicit
    // conversion do not. Warn those cases but still perform the conversion
    // and leave those error cases to the UnaryExpr class to deal with.
    if (IsIntegerTy(expr_qtype) && IsPointerTy(dst_qtype) &&
        !IsBoolTy(expr_qtype)) {
        Warning("incompatible integer to pointer conversion", exprp->Loc());
    } else if (IsIntegerTy(dst_qtype) && IsPointerTy(expr_qtype) &&
               !IsBoolTy(dst_qtype)) {
        Warning("incompatible pointer to integer conversion", exprp->Loc());
    } else if (IsPointerTy(expr_qtype) && IsPointerTy(dst_qtype)) {
        QualType expr_pte_qty = TypeConv<PointerType>(expr_qtype).PointeeQTy();
        QualType dst_pte_qty = TypeConv<PointerType>(dst_qtype).PointeeQTy();
        if ((IsVoidTy(expr_pte_qty) && IsObjectTy(dst_pte_qty)) ||
            (IsVoidTy(dst_pte_qty) && IsObjectTy(expr_pte_qty)) ||
            expr_pte_qty->IsCompatible(dst_pte_qty)) {
            // The conversion-as-if-by-assignment permit the conversion where
            // two pointers point to compatible(ignoring qualifiers) types,
            // or one of the pointers is a pointer to void, and the type
            // pointed to by the original pointer has all the qualifiers of
            // the type pointed to by the target pointer.
            if ((!dst_pte_qty.IsConst() && expr_pte_qty.IsConst()) ||
                (!dst_pte_qty.IsVolatile() && expr_pte_qty.IsVolatile()) ||
                (!dst_pte_qty.IsRestrict() && expr_pte_qty.IsRestrict())) {
                Warning("incompatible pointer types conversion discards "
                        "qualifiers", exprp->Loc());
            }
        } else if ((IsObjectTy(expr_pte_qty) && IsObjectTy(dst_pte_qty)) ||
                   (IsFuncTy(expr_pte_qty) && IsFuncTy(dst_pte_qty))) {
            Warning("incompatible pointer types conversion", exprp->Loc());
        }
    }
    exprp = MakeNodePtr<UnaryExpr>(exprp->Locp(), ret_qtype, exprp);
    return ret_qtype;
}

QualType TryIntPromote(const QualType& qtype) {
    if (!IsIntegerTy(qtype))
        return ValueTrans(qtype);
    return IntPromote(qtype);
}

QualType TryIntPromote(ExprPtr& exprp) {
    if (!IsIntegerTy(exprp->QType()))
        return ValueTrans(exprp->QType());
    return IntPromote(exprp);
}

}
