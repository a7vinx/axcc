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

ArithType::ArithType(unsigned int arith_kind)
    : Type{TypeKind::kArith, true},
      arith_kind_{arith_kind} {
    // Make the flags as few as possible in order to make subsequent operations
    // eaiser.
    if ((arith_kind_ & kASShort) || (arith_kind_ & kASLong) ||
        (arith_kind_ & kASLLong))
        arith_kind_ &= ~kASInt;
    if (arith_kind_ == kASSigned || arith_kind_ == kASUnsigned)
        arith_kind_ = kASInt;
    arith_kind_ &= ~kASSigned;
    switch (arith_kind_) {
        case kASBool:
        case kASChar:
        case kASChar | kASUnsigned:
            SetSize(kCharWidth); break;
        case kASShort:
        case kASShort | kASUnsigned:
            SetSize(kShortWidth); break;
        case kASInt:
        case kASInt | kASUnsigned:
        case kASFloat:
            SetSize(kIntWidth); break;
        case kASLong:
        case kASLong | kASUnsigned:
        case kASDouble:
        case kASDouble | kASLong:
        case kASLLong:
        case kASLLong | kASUnsigned:
            SetSize(kLongWidth); break;
        default:
            assert(false);
    }
    SetAlign(Size());
}

bool ArithType::IsCompatible(const Type& other) const {
    return IsArithTy(other) ?
               arith_kind_ == TypeConv<ArithType>(other).arith_kind_ : false;
}

int ArithType::ConvRank() const {
    int rank = 0;
    switch (arith_kind_) {
        case kASBool: rank = 1; break;
        case kASChar: rank = 2; break;
        case kASChar | kASUnsigned: rank = 3; break;
        case kASShort: rank = 4; break;
        case kASShort | kASUnsigned: rank = 5; break;
        case kASInt: rank = 6; break;
        case kASInt | kASUnsigned: rank = 7; break;
        case kASLong: rank = 8; break;
        case kASLong | kASUnsigned: rank = 9; break;
        case kASLLong: rank = 10; break;
        case kASLLong | kASUnsigned: rank = 11; break;
        case kASFloat: rank = 12; break;
        case kASDouble: rank = 13; break;
        case kASDouble | kASLong: rank = 14; break;
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
    if (!ret_qty_->IsCompatible(other_functy.ret_qty_))
        return false;
    // C11 6.7.6.3p15
    if (!has_proto_ && !other_functy.has_proto_) {
        return true;
    } else if (has_proto_ && other_functy.has_proto_) {
        auto is_cmpt = [](const ObjectPtr& lhs, const ObjectPtr& rhs) {
            return lhs->QType().IsCompatible(rhs->QType()); };
        return std::equal(params_.cbegin(), params_.cend(),
                   other_functy.params_.cbegin(), other_functy.params_.cend(),
                   is_cmpt);
    } else {
        const auto& no_proto_functy = has_proto_ ? other_functy : *this;
        const auto& has_proto_functy = has_proto_ ? *this : other_functy;
        if (no_proto_functy.IsComplete()) {
            auto is_cmpt = [](const ObjectPtr& lhs, const ObjectPtr& rhs) {
                return lhs->QType().IsCompatible(DefaultArgPromote(rhs->QType()));
            };
            return std::equal(has_proto_functy.params_.cbegin(),
                              has_proto_functy.params_.cend(),
                              no_proto_functy.params_.cbegin(),
                              no_proto_functy.params_.cend(), is_cmpt);
        } else {
            auto is_cmpt = [](const ObjectPtr& objp) {
                return objp->QType().IsCompatible(DefaultArgPromote(
                                                      objp->QType())); };
            return std::all_of(has_proto_functy.params_.cbegin(),
                               has_proto_functy.params_.cend(),
                               is_cmpt);
        }
    }
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
    auto int_typep = std::make_shared<ArithType>(ArithType::kASInt);
    if (arith_type.ConvRank() < int_typep->ConvRank())
        return QualType{int_typep};
    return LoseAllQuals(qtype);
}

// C11 6.5.2.2p6: If the expression that denotes the called function has a type
// that does not include a prototype, the integer promotions are performed on
// each argument, and arguments that have type float are promoted to double.
QualType DefaultArgPromote(const QualType& qtype) {
    if (IsArithTy(qtype) &&
        TypeConv<ArithType>(qtype).ArithKind() & ArithType::kASFloat)
        return MakeQType<ArithType>(ArithType::kASDouble);
    return TryIntPromote(qtype);
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

QualType DefaultArgPromote(ExprPtr& exprp) {
    QualType promoted_qty = DefaultArgPromote(exprp->QType());
    if (!exprp->QType()->IsCompatible(promoted_qty))
        exprp = MakeNodePtr<UnaryExpr>(exprp->Locp(), promoted_qty, exprp);
    return promoted_qty;
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
