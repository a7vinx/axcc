#include <algorithm>

#include "type.hh"
#include "ast.hh"

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

}
