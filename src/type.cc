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
const unsigned long long ArithType::kSignedShortMax = (1ull << 15) - 1;
const unsigned long long ArithType::kSignedIntMax = (1ull << 31) - 1;
const unsigned long long ArithType::kSignedLongMax = (1ull << 63) - 1;
const unsigned long long ArithType::kUnsignedShortMax = (1ull << 16) - 1;
const unsigned long long ArithType::kUnsignedIntMax = (1ull << 32) - 1;
const unsigned long long ArithType::kUnsignedLongMax = ~0ull;
const long double ArithType::kFloatMax = 3.402823e+38;
const long double ArithType::kDoubleMax = 1.797693e+308;

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

namespace {

// Also align the offset.
void FinishBitFields(long long& off, long long& bit_off, std::size_t align) {
    off += (bit_off + 7) / 8;
    bit_off = 0;
    off = AlignOff(off, align);
}

// Return false if this bit field is anonymous, which means it does not need
// to be added to the members list.
bool HandleBitFieldOff(BitField& bitfield, long long& off, long long& bit_off) {
    // C11 6.7.2.1p12: As a special case, a bit-field structure member
    // with a width of 0 indicates that no further bit-field is to be
    // packed into the unit in which the previous bit-field, if any,
    // was placed.
    if (bitfield.BitWidth() == 0) {
        FinishBitFields(off, bit_off, bitfield.QType()->Align());
        return false;
    }
    long long bf_tybits = bitfield.QType()->Size() * 8;
    long long left_space = bf_tybits - (off * 8 + bit_off) % bf_tybits;
    if (left_space < bitfield.BitWidth())
        FinishBitFields(off, bit_off, bitfield.QType()->Align());
    bitfield.SetOff(off);
    bitfield.SetBitOff(bit_off);
    bit_off += bitfield.BitWidth();
    if (bitfield.IsAnonymous())
        return false;
    return true;
}

} // unnamed namespace

RecordType::RecordType(const std::vector<ObjectPtr>& members, bool is_struct)
    : Type{is_struct ? TypeKind::kStruct : TypeKind::kUnion, true} {
    if (is_struct)
        StructTypeCtor(members);
    else
        UnionTypeCtor(members);
}

bool RecordType::IsCompatible(const Type& other) const {
    // Direct pointer comparsion.
    return &other == this;
}

void RecordType::EncounterDef(const std::vector<ObjectPtr>& members) {
    if (Kind() == TypeKind::kStruct)
        StructTypeCtor(members);
    else
        UnionTypeCtor(members);
    SetComplete();
}

ObjectPtr RecordType::GetMember(const std::string& name) const {
    auto iter = members_map_.find(name);
    if (iter != members_map_.cend())
        return iter->second;
    return {};
}

void RecordType::StructTypeCtor(const std::vector<ObjectPtr>& members) {
    long long off = 0;
    long long bit_off = 0;
    std::size_t align = 0;
    for (auto iter = members.cbegin(); iter != members.cend(); ++iter) {
        auto& obj = **iter;
        if (!obj.IsAnonymous() && HasMember(obj.Name())) {
            Error("duplicate member '" + obj.Name() + "'", obj.Loc());
            continue;
        }
        // We also accept error nodes for suppressing subsequent errors and
        // if it is, add it directly into the members list after the duplicate
        // members check.
        if (obj.HasErr()) {
            PushMember(*iter);
            continue;
        }
        if (!obj.QType()->IsComplete()) {
            if (IsArrayTy(obj.QType()) && (iter + 1) == members.cend()) {
                // C11 6.7.2.1p18: As a special case, the last element of a
                // structure with more than one named member may have an
                // incomplete array type; this is called a flexible array
                // member.
                if (members.size() == 1)
                    // Error but no "continue" here, this member will still be
                    // added in order to suppress subsequent errors such as "no
                    // such member".
                    Error("flexible array member '" + obj.Name() +
                          "' not allowed in otherwise empty struct", obj.Loc());
                // No "continue" here. This flexible array member will be
                // handled as a normal object member below.
            } else {
                // Error but no "continue" here, same as above.
                Error("field has incomplete type", obj.Loc());
            }
        }
        if (obj.IsAnonymous() && !IsBitField(obj)) {
            // Now deal with anonymous struct or union.
            assert(IsRecordTy(obj.QType()));
            const auto& rec_type = TypeConv<RecordType>(obj.QType());
            FinishBitFields(off, bit_off, rec_type.Align());
            MergeAnonyRecord(rec_type, off);
            off += rec_type.Size();
            align = std::max(align, rec_type.Align());
            continue;
        }
        // Now deal with normal members including bit fields.
        if (IsBitField(obj)) {
            if (!HandleBitFieldOff(NodeConv<BitField>(obj), off, bit_off))
                continue;
        } else {
            FinishBitFields(off, bit_off, obj.QType()->Align());
            obj.SetOff(off);
            off += obj.QType()->Size();
        }
        align = std::max(align, obj.QType()->Align());
        PushMember(*iter);
    }
    FinishBitFields(off, bit_off, align);
    SetAlign(align);
    SetSize(off);
}

void RecordType::UnionTypeCtor(const std::vector<ObjectPtr>& members) {
    std::size_t align = 0;
    std::size_t size = 0;
    for (auto iter = members.cbegin(); iter != members.cend(); ++iter) {
        auto& obj = **iter;
        if (!obj.IsAnonymous() && HasMember(obj.Name())) {
            Error("duplicate member '" + obj.Name() + "'", obj.Loc());
            continue;
        }
        // Same as struct.
        if (obj.HasErr()) {
            PushMember(*iter);
            continue;
        }
        if (!obj.QType()->IsComplete())
            // Error but no "continue" here for suppressing subsequent errors.
            Error("field has incomplete type", obj.Loc());
        if (obj.IsAnonymous() && !IsBitField(obj)) {
            // Now deal with anonymous struct or union.
            assert(IsRecordTy(obj.QType()));
            const auto& rec_type = TypeConv<RecordType>(obj.QType());
            MergeAnonyRecord(rec_type);
            align = std::max(align, rec_type.Align());
            size = std::max(size, rec_type.Size());
        } else {
            // Now deal with normal members. The offset of objects and the bit
            // offset of bit fields remain 0.
            align = std::max(align, obj.QType()->Align());
            size = std::max(size, obj.QType()->Size());
            PushMember(*iter);
        }
    }
    SetAlign(align);
    SetSize(size);
}

void RecordType::MergeAnonyRecord(const RecordType& rec_type, long long base_off) {
    for (const auto& objp : rec_type.members_) {
        if (!objp->IsAnonymous() && HasMember(objp->Name())) {
            Error("member of anonymous struct/union redeclares '" +
                  objp->Name() + "'", objp->Loc());
            // This continue may result in the size of the enclosing
            // struct/union being unequal to the sum of the sizes of
            // all members.
            continue;
        }
        // The bit offset of bit fields will not be affected.
        objp->SetOff(base_off + objp->Off());
        PushMember(objp);
    }
}

void RecordType::PushMember(const ObjectPtr& objp) {
    members_.emplace_back(objp);
    members_map_.emplace(objp->Name(), objp);
    if (objp->QType().IsConst())
        has_const_member_ = true;
}

bool IsCharArrayTy(const Type& type) {
    if (!IsArrayTy(type))
        return false;
    QualType elem_qtype = TypeConv<ArrayType>(type).ElemQType();
    if (!IsArithTy(elem_qtype))
        return false;
    unsigned int elem_arith_kind = TypeConv<ArithType>(elem_qtype).ArithKind();
    return (elem_arith_kind & ArithType::kASChar) ||
           (elem_arith_kind == (ArithType::kASShort | ArithType::kASUnsigned)) ||
           (elem_arith_kind == (ArithType::kASInt | ArithType::kASUnsigned));
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
