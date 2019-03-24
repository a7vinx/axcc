#ifndef _AXCC_TYPE_HH_
#define _AXCC_TYPE_HH_

#include <memory>
#include <cstdlib>
#include <vector>
#include <string>
#include <map>
#include <cassert>

namespace axcc {

// Forward Declarations
class Object;
using ObjectPtr = std::shared_ptr<Object>;
class Expr;
using ExprPtr = std::shared_ptr<Expr>;

enum class TypeKind : unsigned char {
    kVoid,
    kArith,
    kPointer,
    kArray,
    kFunc,
    kStruct,
    kUnion
};

class Type {
public:
    Type(const Type&) = delete;
    Type(Type&&) = delete;
    Type& operator=(const Type&) = delete;
    Type& operator=(Type&&) = delete;
    virtual ~Type() = default;

    virtual bool IsCompatible(const Type& other) const = 0;
    bool IsComplete() const { return is_complete_; }
    std::size_t Size() const { return size_; }
    std::size_t Align() const { return align_; }
    TypeKind Kind() const { return kind_; }

protected:
    Type(const TypeKind& kind, bool is_complete,
         std::size_t size = 0, std::size_t align = 0)
        : kind_{kind}, is_complete_{is_complete}, size_{size}, align_{align} {}
    void SetComplete() { is_complete_ = true; }
    void SetSize(std::size_t size) { size_ = size; }
    void SetAlign(std::size_t align) { align_ = align; }
private:
    const TypeKind kind_;
    bool is_complete_;
    std::size_t size_;
    std::size_t align_;
};

using TypePtr = std::shared_ptr<Type>;

class QualType {
public:
    enum Qualifier : unsigned char {
        kQualConst = 1 << 0,
        kQualVolatile = 1 << 1,
        kQualRestrict = 1 << 2,
    };
    QualType(unsigned char qualifiers = 0) : qualifiers_{qualifiers} {}
    QualType(const TypePtr& typep, unsigned char qualifiers = 0)
        : typep_{typep}, qualifiers_{qualifiers} {}
    bool IsConst() const { return qualifiers_ & kQualConst; }
    bool IsVolatile() const { return qualifiers_ & kQualVolatile; }
    bool IsRestrict() const { return qualifiers_ & kQualRestrict; }
    void AddConst() { qualifiers_ |= kQualConst; }
    void AddVolatile() { qualifiers_ |= kQualVolatile; }
    void AddRestrict() { qualifiers_ |= kQualRestrict; }
    void MergeQuals(const QualType& qtype) { qualifiers_ |= qtype.qualifiers_; }
    void RmConst() { qualifiers_ &= ~kQualConst; }
    void RmVolatile() { qualifiers_ &= ~kQualVolatile; }
    void RmRestrict() { qualifiers_ &= ~kQualRestrict; }
    void LoseAllQuals() { qualifiers_ = 0; }
    // Helper functions for the raw type.
    bool HasRawType() const { return typep_.get() != nullptr; }
    void SetRawType(const TypePtr& typep) { typep_ = typep; }
    void ResetRawType() { typep_.reset(); }
    Type& RawType() const { assert(typep_.get() != nullptr); return *typep_; }
    TypePtr RawTypep() const { return typep_; }
    // Make QualType behave like a pointer.
    Type* operator->() const {
        assert(typep_.get() != nullptr); return typep_.get(); }
    Type& operator*() const { return RawType(); }
    // Allow the implicit conversion to const Type&
    operator const Type&() const { return RawType(); }
    bool IsCompatible(const QualType& other) const {
        assert(typep_.get() != nullptr);
        return qualifiers_ == other.qualifiers_ && typep_->IsCompatible(other); }
private:
    unsigned char qualifiers_;
    // Use shared_ptr in order to reuse the same Type class, e.g., struct/union
    // types and typedefs.
    TypePtr typep_{};
};

class VoidType : public Type {
public:
    // C11 6.2.5p19: The void type comprises an empty set of values;
    // it is an incomplete object type that cannot be completed.
    VoidType() : Type{TypeKind::kVoid, false} {}
    virtual bool IsCompatible(const Type& other) const override;
};

class ArithType : public Type {
public:
    enum ArithSpec : unsigned int {
        kASBool = 1 << 0,
        kASChar = 1 << 1,
        kASInt = 1 << 2,
        kASFloat = 1 << 3,
        kASDouble = 1 << 4,
        kASShort = 1 << 5,
        kASLong = 1 << 6,
        kASLLong = 1 << 7,
        kASSigned = 1 << 8,
        kASUnsigned = 1 << 9
    };
    static const std::size_t kCharWidth;
    static const std::size_t kShortWidth;
    static const std::size_t kIntWidth;
    static const std::size_t kLongWidth;
    static const unsigned long long kSignedShortMax;
    static const unsigned long long kSignedIntMax;
    static const unsigned long long kSignedLongMax;
    static const unsigned long long kUnsignedShortMax;
    static const unsigned long long kUnsignedIntMax;
    static const unsigned long long kUnsignedLongMax;
    static const long double kFloatMax;
    static const long double kDoubleMax;

    ArithType(unsigned int arith_kind);
    virtual bool IsCompatible(const Type& other) const override;
    unsigned int ArithKind() const { return arith_kind_; }
    // We add conversion ranks for floating types in order to simplify the
    // handling of some type conversions.
    int ConvRank() const;
private:
    unsigned int arith_kind_;
};

class PointerType : public Type {
public:
    PointerType(const QualType& pointee_qty)
        : Type{TypeKind::kPointer, true,
               ArithType::kLongWidth, ArithType::kLongWidth},
          pointee_qty_{pointee_qty} {}
    virtual bool IsCompatible(const Type& other) const override;
    QualType PointeeQTy() const { return pointee_qty_; }
    // Be careful with the use of these reset functions because a Type class
    // instance may be shared by multiple instances of QualType classes.
    void ResetPointeeQTy(const QualType& pointee_qty) {
        pointee_qty_ = pointee_qty; }
private:
    QualType pointee_qty_;
};

class ArrayType : public Type {
public:
    ArrayType(const QualType& elem_qty)
        : Type{TypeKind::kArray, false, 0, elem_qty->Align()},
          elem_qty_{elem_qty} {}
    ArrayType(const QualType& elem_qty, std::size_t arr_size)
        : Type{TypeKind::kArray, true,
               elem_qty->Size() * arr_size, elem_qty->Align()},
          elem_qty_{elem_qty},
          arr_size_{arr_size} {}
    virtual bool IsCompatible(const Type& other) const override;
    QualType ElemQType() const { return elem_qty_; }
    // Check type completeness before using the size of the array.
    std::size_t ArrSize() const { return arr_size_; }
    void SetArrSize(std::size_t arr_size);
    void ResetElemQType(const QualType& elem_qty) { elem_qty_ = elem_qty; }
private:
    QualType elem_qty_;
    std::size_t arr_size_{0};
};

class FuncType : public Type {
public:
    enum FuncSpec : unsigned char {
        kFSInline = 1 << 0,
        kFSNoreturn = 1 << 1,
    };
    FuncType(const QualType& ret_qty, const std::vector<ObjectPtr>& params,
             bool is_old_style = true)
        : Type{TypeKind::kFunc, false},
          ret_qty_{ret_qty}, params_{params}, has_proto_{!is_old_style} {}
    virtual bool IsCompatible(const Type& other) const override;
    bool IsInline() const { return func_specs_ & kFSInline; }
    bool IsNoreturn() const { return func_specs_ & kFSNoreturn; }
    void AddInline() { func_specs_ |= kFSInline; }
    void AddNoreturn() { func_specs_ |= kFSNoreturn; }
    QualType RetQType() const { return ret_qty_; }
    const std::vector<ObjectPtr>& Params() const { return params_; }
    void UpdateParams(const std::vector<ObjectPtr>& params) { params_ = params; }
    // Use the type completeness atrribute to check function redefinition.
    void EncounterDef() { SetComplete(); }
    bool HasDef() const { return IsComplete(); }
    void EncounterProto() { has_proto_ = true; }
    bool HasProto() const { return has_proto_; }
    void ResetRetQType(const QualType& ret_qty) { ret_qty_ = ret_qty; }
private:
    QualType ret_qty_;
    std::vector<ObjectPtr> params_;
    bool has_proto_;
    unsigned char func_specs_{0};
};

class RecordType : public Type {
public:
    RecordType(bool is_struct, const std::string& tag_name)
        : Type{is_struct ? TypeKind::kStruct : TypeKind::kUnion, false},
          tag_name_{tag_name} {}
    RecordType(bool is_struct, const std::vector<ObjectPtr>& members,
               const std::string& tag_name = {});
    virtual bool IsCompatible(const Type& other) const override;
    std::string TagName() const { return tag_name_; }
    const std::vector<ObjectPtr>& Members() const { return members_; }
    void EncounterDef(const std::vector<ObjectPtr>& members);
    bool HasMember(const std::string& name) const {
        return members_map_.find(name) != members_map_.cend(); }
    // Return a null pointer if no such member.
    ObjectPtr GetMember(const std::string& name) const;
    bool HasConstMember() const { return has_const_member_; }
private:
    void StructTypeCtor(const std::vector<ObjectPtr>& members);
    void UnionTypeCtor(const std::vector<ObjectPtr>& members);
    void MergeAnonyRecord(const RecordType& rec_type, long long base_off = 0);
    void PushMember(const ObjectPtr& objp);
    std::string tag_name_;
    // The order of members should be remembered.
    std::vector<ObjectPtr> members_{};
    std::map<std::string, ObjectPtr> members_map_{};
    bool has_const_member_{false};
};

template<typename T>
T& TypeConv(Type& type) {
    return static_cast<T&>(type);
}

template<typename T>
const T& TypeConv(const Type& type) {
    return static_cast<const T&>(type);
}

// Helper functions for determining type categories.
template<TypeKind kind>
bool IsTypeKind(const Type& type) {
    return type.Kind() == kind;
}
static constexpr auto& IsVoidTy = IsTypeKind<TypeKind::kVoid>;
static constexpr auto& IsArithTy = IsTypeKind<TypeKind::kArith>;
static constexpr auto& IsPointerTy = IsTypeKind<TypeKind::kPointer>;
static constexpr auto& IsArrayTy = IsTypeKind<TypeKind::kArray>;
static constexpr auto& IsFuncTy = IsTypeKind<TypeKind::kFunc>;
static constexpr auto& IsStructTy = IsTypeKind<TypeKind::kStruct>;
static constexpr auto& IsUnionTy = IsTypeKind<TypeKind::kUnion>;

inline bool IsRecordTy(const Type& type) {
    return IsStructTy(type) || IsUnionTy(type);
}
inline bool IsScalarTy(const Type& type) {
    return IsArithTy(type) || IsPointerTy(type);
}
inline bool IsBoolTy(const ArithType& arith_type) {
    return arith_type.ArithKind() & ArithType::kASBool;
}
inline bool IsBoolTy(const Type& type) {
    return IsArithTy(type) ? IsBoolTy(TypeConv<ArithType>(type)) : false;
}
inline bool IsFloatingTy(const ArithType& arith_type) {
    return (arith_type.ArithKind() & ArithType::kASFloat) ||
           (arith_type.ArithKind() & ArithType::kASDouble);
}
inline bool IsFloatingTy(const Type& type) {
    return IsArithTy(type) ? IsFloatingTy(TypeConv<ArithType>(type)) : false;
}
inline bool IsIntegerTy(const ArithType& arith_type) {
    return !IsFloatingTy(arith_type);
}
inline bool IsIntegerTy(const Type& type) {
    return IsArithTy(type) && !IsFloatingTy(type);
}
inline bool IsSignedTy(const ArithType& arith_type) {
    return !(arith_type.ArithKind() & ArithType::kASUnsigned);
}
inline bool IsSignedTy(const Type& type) {
    return IsArithTy(type) ? IsSignedTy(TypeConv<ArithType>(type)) : false;
}
inline bool IsUnsignedTy(const ArithType& arith_type) {
    return !IsSignedTy(arith_type);
}
inline bool IsUnsignedTy(const Type& type) {
    return !IsSignedTy(type);
}
inline bool IsObjectTy(const Type& type) {
    return !IsFuncTy(type);
}
inline bool IsFuncPtrTy(const Type& type) {
    return IsPointerTy(type) &&
           IsFuncTy(TypeConv<PointerType>(type).PointeeQTy());
}
inline bool IsObjPtrTy(const Type& type) {
    return !IsFuncPtrTy(type);
}
inline bool IsVoidPtrTy(const Type& type) {
    return IsPointerTy(type) &&
           IsVoidTy(TypeConv<PointerType>(type).PointeeQTy());
}
bool IsCharArrayTy(const Type& type);

template<typename T, typename... Args>
QualType MakeQType(Args&&... args) {
    return QualType{std::make_shared<T>(std::forward<Args>(args)...)};
}

// Produce an unqualified copy.
QualType LoseAllQuals(const QualType& qtype);

// Include lvalue conversion, array to pointer conversion, function to pointer
// conversion.
QualType ValueTrans(const QualType& qtype);

// Return the promoted type. This function can only be used with integer type.
QualType IntPromote(const QualType& qtype);
// Return the promoted type.
QualType DefaultArgPromote(const QualType& qtype);
// These four functions, IntPromote(), UsualArithConv(), DefaultArgPromote(),
// ConvAsIfByAsgn(), will generate the conversion node according to the
// conversion rules, if needed, and set the expression pointer to the newly
// generated node. They return the type determined according to the conversion
// rules and the returned type must have been processed by value transformation.
// Note that IntPromote(), UsualArithConv() and DefaultArgPromote() will never
// generate a conversion node with its error flag set while ConvAsIfByAsgn() may.
QualType IntPromote(ExprPtr& exprp);
QualType UsualArithConv(ExprPtr& lhsp, ExprPtr& rhsp);
QualType DefaultArgPromote(ExprPtr& exprp);
QualType ConvAsIfByAsgn(ExprPtr& exprp, const QualType& dst_qtype);

// For integer type, return the promoted type. For other types, return the
// type after value transformation.
QualType TryIntPromote(const QualType& qtype);
// For integer type, try to do the promotion and update the expression pointer.
// For other types, simply return the type after value transformation.
QualType TryIntPromote(ExprPtr& exprp);

// Return the aligned offset.
inline long long AlignOff(long long off, std::size_t align) {
    return ((off + align - 1) / align * align);
}

}
#endif
