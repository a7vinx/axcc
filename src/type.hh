#ifndef _AXCC_TYPE_HH_
#define _AXCC_TYPE_HH_

#include <memory>
#include <cstdlib>

namespace axcc {

enum class TypeKind : unsigned char {
    kVoid,
    kArith,
    kPointer,
    kArray,
    kFunc,
    kStruct,
    kUnion,
    kBitField
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

}
#endif
