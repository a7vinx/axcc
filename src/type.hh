#ifndef _AXCC_TYPE_HH_
#define _AXCC_TYPE_HH_

#include <memory>
#include <cstdlib>
#include <vector>
#include <string>
#include <map>

namespace axcc {

// Forward Declarations
class Object;
using ObjectPtr = std::shared_ptr<Object>;

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

class QualType {
public:
    QualType() = default;
    QualType(const std::shared_ptr<Type>& typep) : typep_{typep} {}
    bool IsConst() const { return qualifiers_ & kQualConst; }
    bool IsVolatile() const { return qualifiers_ & kQualVolatile; }
    bool IsRestrict() const { return qualifiers_ & kQualRestrict; }
    void AddConst() { qualifiers_ |= kQualConst; }
    void AddVolatile() { qualifiers_ |= kQualVolatile; }
    void AddRestrict() { qualifiers_ |= kQualRestrict; }
    void RmConst() { qualifiers_ &= ~kQualConst; }
    void RmVolatile() { qualifiers_ &= ~kQualVolatile; }
    void RmRestrict() { qualifiers_ &= ~kQualRestrict; }
    // Helper functions for the raw type.
    bool HasRawType() const { return typep_.get() != nullptr; }
    void SetRawType(const std::shared_ptr<Type>& typep) { typep_ = typep; }
    void ResetRawType() { typep_.reset(); }
    Type& RawType() const { return *typep_; }
    // Make QualType behave like a pointer.
    Type* operator->() const { return typep_.get(); }
    Type& operator*() const { return RawType(); }
    // Allow the implicit conversion to const Type&
    operator const Type&() const { return RawType(); }
    bool IsCompatible(const QualType& other) const {
        return qualifiers_ == other.qualifiers_ && typep_->IsCompatible(other); }
private:
    enum QualFlags : unsigned char {
        kQualConst = 1 << 0,
        kQualVolatile = 1 << 1,
        kQualRestrict = 1 << 2,
    };
    unsigned char qualifiers_{0};
    // Use shared_ptr in order to reuse the same Type class, e.g., struct/union
    // types and typedefs.
    std::shared_ptr<Type> typep_{};
};

class VoidType : public Type {
public:
    // C11 6.2.5p19: The void type comprises an empty set of values;
    // it is an incomplete object type that cannot be completed.
    VoidType() : Type{TypeKind::kVoid, false} {}
    virtual bool IsCompatible(const Type& other) const override;
};

enum class ArithTyKind : unsigned char {
    kDefault,
    kBool,
    kChar,
    kInt,
    kFloat,
    kDouble
};

enum class ArithTySize : unsigned char {
    kDefault,
    kShort,
    kLong,
    kLLong
};

enum class ArithTySign : unsigned char {
    kDefault,
    kSigned,
    kUnsigned
};

class ArithType : public Type {
public:
    static const std::size_t kCharWidth;
    static const std::size_t kShortWidth;
    static const std::size_t kIntWidth;
    static const std::size_t kLongWidth;

    ArithType(const ArithTyKind& kind,
              const ArithTySize& size = ArithTySize::kDefault,
              const ArithTySign& sign = ArithTySign::kDefault);
    virtual bool IsCompatible(const Type& other) const override;
    ArithTyKind ATyKind() const { return aty_kind_; }
    ArithTySize ATySize() const { return aty_size_; }
    ArithTySign ATySign() const { return aty_sign_; }
    // We add conversion ranks for floating types in order to simplify the
    // handling of some type conversions.
    int ConvRank() const;
private:
    ArithTyKind aty_kind_;
    ArithTySize aty_size_;
    ArithTySign aty_sign_;
};

class PointerType : public Type {
public:
    PointerType(const QualType& pointee_qty)
        : Type{TypeKind::kPointer, true,
               ArithType::kLongWidth, ArithType::kLongWidth},
          pointee_qty_{pointee_qty} {}
    virtual bool IsCompatible(const Type& other) const override;
    QualType PointeeQTy() const { return pointee_qty_; }
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
private:
    QualType elem_qty_;
    std::size_t arr_size_{0};
};

class FuncType : public Type {
public:
    FuncType(const QualType& ret_qty, const std::vector<ObjectPtr>& params)
        : Type{TypeKind::kFunc, false},
          ret_qty_{ret_qty}, params_{params} {}
    virtual bool IsCompatible(const Type& other) const override;
    bool IsInline() const { return func_specs_ & kFSInline; }
    bool IsNoreturn() const { return func_specs_ & kFSNoreturn; }
    void AddInline() { func_specs_ |= kFSInline; }
    void AddNoreturn() { func_specs_ |= kFSNoreturn; }
    QualType RetQType() const { return ret_qty_; }
    std::vector<ObjectPtr> Params() const { return params_; }
    void UpdateParams(const std::vector<ObjectPtr>& params) { params_ = params; }
    // Use the type completeness atrribute to check function redefinition.
    void EncounterDef() { SetComplete(); }
    bool HasDef() const { return IsComplete(); }
private:
    enum FuncSpecFlags : unsigned char {
        kFSInline = 1 << 0,
        kFSNoreturn = 1 << 1,
    };
    QualType ret_qty_;
    std::vector<ObjectPtr> params_;
    unsigned char func_specs_{0};
};

class RecordType : public Type {
public:
    RecordType(const std::vector<ObjectPtr>& members, bool is_struct);
    virtual bool IsCompatible(const Type& other) const override;
    // Return a null pointer if no such member.
    ObjectPtr GetMember(const std::string& name) const;
private:
    void StructTypeCtor(const std::vector<ObjectPtr>& members);
    void UnionTypeCtor(const std::vector<ObjectPtr>& members);
    // The order of members should be remembered.
    std::vector<ObjectPtr> members_{};
    std::map<std::string, ObjectPtr> members_map_{};
};

}
#endif
