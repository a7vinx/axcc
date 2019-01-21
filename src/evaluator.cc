#include <exception>
#include <string>
#include <cassert>
#include <cstdlib>

#include "evaluator.hh"
#include "type.hh"

namespace axcc {

namespace {

class EvalError : public std::exception {
public:
    EvalError(const std::string& msg, const SourceLocPtr& locp)
        : msg_{msg}, locp_{locp} {}
    virtual const char* what() const noexcept { return msg_.c_str(); }
    const SourceLoc& Loc() const noexcept {
        assert(locp_.get() != nullptr); return *locp_; }
private:
    std::string msg_;
    SourceLocPtr locp_;
};

} // unnamed namespace

template<typename T>
T Evaluator::EvalExpr(const ExprPtr& exprp) {
    switch (exprp->Kind()) {
        case AstNodeKind::kFuncCall:
            throw EvalError{"compile-time constant expected", exprp->Locp()};
        case AstNodeKind::kUnaryExpr:
            return EvalUnaryExpr<T>(NodepConv<UnaryExpr>(exprp));
        case AstNodeKind::kBinaryExpr:
            return EvalBinaryExpr<T>(NodepConv<BinaryExpr>(exprp));
        case AstNodeKind::kTernaryExpr:
            return EvalTernaryExpr<T>(NodepConv<TernaryExpr>(exprp));
        case AstNodeKind::kConstant:
            return EvalConstant<T>(NodepConv<Constant>(exprp));
        case AstNodeKind::kStrLiteral:
            return EvalStrLiteral<T>(NodepConv<StrLiteral>(exprp));
        case AstNodeKind::kEnumerator:
            return EvalConstant<T>(NodepConv<Enumerator>(exprp)->Constantp());
        case AstNodeKind::kFuncName:
            return EvalIdent<T>(NodepConv<FuncName>(exprp));
        case AstNodeKind::kObject:
            return EvalIdent<T>(NodepConv<Object>(exprp));
        default:
            assert(false);
    }
}

// Evaluate expressions of integer type. Usually the sign is meaningless in
// the place where this function is used. Use long long as the return type so
// that when we really need its unsigned value we can directly cast it to
// unsigned long long.
long long Evaluator::EvalIntExpr(const ExprPtr& exprp) {
    if (IsSignedTy(exprp->QType())) {
        return EvalExpr<long long>(exprp);
    } else {
        // Unsigned long long to signed long long conversion is an
        // implementation-defined behavior.
        long long val = EvalExpr<unsigned long long>(exprp);
        return reinterpret_cast<long long&>(val);
    }
}

// Evaluate expressions and convert the result to the long double type. This
// function is used where the type of the operands no longer affects the type
// of result.
long double Evaluator::EvalExprToFloat(const ExprPtr& exprp) {
    if (IsFloatingTy(exprp->QType())) {
        return EvalExpr<long double>(exprp);
    } else if (IsSignedTy(exprp->QType())) {
        return EvalExpr<long long>(exprp);
    } else if (IsUnsignedTy(exprp->QType())) {
        return EvalExpr<unsigned long long>(exprp);
    } else {
        throw EvalError{"compile-time constant expected", exprp->Locp()};
    }
}

template<typename T>
T Evaluator::EvalUnaryExpr(const UnaryExprPtr& unary_exprp) {
    const auto& operandp = unary_exprp->Operandp();
    switch (unary_exprp->OpKind()) {
        case UnaryOpKind::kPreInc:
        case UnaryOpKind::kPreDec:
        case UnaryOpKind::kPostInc:
        case UnaryOpKind::kPostDec:
        case UnaryOpKind::kDeref:
        case UnaryOpKind::kAddrOf:
            throw EvalError{"compile-time constant expected",
                            unary_exprp->Locp()};
        case UnaryOpKind::kPlus:
            return +EvalExpr<T>(operandp);
        case UnaryOpKind::kMinus:
            return -EvalExpr<T>(operandp);
        // The result of '~' have integer types.
        case UnaryOpKind::kBitNot:
            return ~EvalIntExpr(operandp);
        // The type of the result of '!' is int.
        case UnaryOpKind::kLogicNot:
            return !EvalExprToFloat(operandp);
        case UnaryOpKind::kCast:
            if (IsPointerTy(operandp->QType()) || IsIntegerTy(operandp->QType())) {
                // In fact if the operand has pointer type, the unary expression
                // can only be integer type.
                return EvalIntExpr(operandp);
            } else {
                return EvalExpr<long double>(operandp);
            }
        default:
            assert(false);
    }
}

template<>
AddrConstantPtr Evaluator::EvalUnaryExpr(const UnaryExprPtr& unary_exprp) {
    const auto& operandp = unary_exprp->Operandp();
    switch (unary_exprp->OpKind()) {
        case UnaryOpKind::kPreInc:
        case UnaryOpKind::kPreDec:
        case UnaryOpKind::kPostInc:
        case UnaryOpKind::kPostDec:
        case UnaryOpKind::kDeref:
            throw EvalError{"compile-time constant expected",
                            unary_exprp->Locp()};
        case UnaryOpKind::kAddrOf:
            if (IsBinaryExpr(*operandp) &&
                NodeConv<BinaryExpr>(*operandp).OpKind() == BinaryOpKind::kMemAccs)
                expect_mem_accs_ = true;
            return EvalExpr<AddrConstantPtr>(operandp);
        case UnaryOpKind::kCast:
            // Other types have no chance to get here.
            assert(IsPointerTy(unary_exprp->QType()));
            if (IsIntegerTy(operandp->QType()))
                return MakeNodePtr<AddrConstant>("", EvalIntExpr(operandp));
            return EvalExpr<AddrConstantPtr>(operandp);
        default:
            // It should not reach here.
            assert(false);
    }
}

template<typename T>
T Evaluator::EvalBinaryExpr(const BinaryExprPtr& bin_exprp) {
    const auto& lhsp = bin_exprp->Lhsp();
    const auto& rhsp = bin_exprp->Rhsp();
    switch (bin_exprp->OpKind()) {
        case BinaryOpKind::kAsgn:
        case BinaryOpKind::kComma:
        case BinaryOpKind::kMemAccs:
            throw EvalError{"compile-time constant expected", bin_exprp->Locp()};
        case BinaryOpKind::kAdd:
            return EvalExpr<T>(lhsp) + EvalExpr<T>(rhsp);
        case BinaryOpKind::kSub:
            return EvalExpr<T>(lhsp) - EvalExpr<T>(rhsp);
        case BinaryOpKind::kPro:
            return EvalExpr<T>(lhsp) * EvalExpr<T>(rhsp);
        case BinaryOpKind::kDiv:
            return EvalExpr<T>(lhsp) / EvalExpr<T>(rhsp);
        // The operands of these operators must have integer types and their
        // results also have integer types.
        case BinaryOpKind::kMod:
            return EvalIntExpr(lhsp) % EvalIntExpr(rhsp);
        case BinaryOpKind::kBitAnd:
            return EvalIntExpr(lhsp) & EvalIntExpr(rhsp);
        case BinaryOpKind::kBitOr:
            return EvalIntExpr(lhsp) | EvalIntExpr(rhsp);
        case BinaryOpKind::kBitXor:
            return EvalIntExpr(lhsp) ^ EvalIntExpr(rhsp);
        case BinaryOpKind::kBitShl:
            return EvalIntExpr(lhsp) << EvalIntExpr(rhsp);
        case BinaryOpKind::kBitShr:
            return EvalIntExpr(lhsp) >> EvalIntExpr(rhsp);
        // The type of the result of these operators is int.
        case BinaryOpKind::kLogicAnd:
            return EvalExprToFloat(lhsp) && EvalExprToFloat(rhsp);
        case BinaryOpKind::kLogicOr:
            return EvalExprToFloat(lhsp) || EvalExprToFloat(rhsp);
        case BinaryOpKind::kEqual:
            return EvalExprToFloat(lhsp) == EvalExprToFloat(rhsp);
        case BinaryOpKind::kNEqual:
            return EvalExprToFloat(lhsp) != EvalExprToFloat(rhsp);
        case BinaryOpKind::kLess:
            return EvalExprToFloat(lhsp) < EvalExprToFloat(rhsp);
        case BinaryOpKind::kGreater:
            return EvalExprToFloat(lhsp) > EvalExprToFloat(rhsp);
        case BinaryOpKind::kLessEq:
            return EvalExprToFloat(lhsp) <= EvalExprToFloat(rhsp);
        case BinaryOpKind::kGreaterEq:
            return EvalExprToFloat(lhsp) >= EvalExprToFloat(rhsp);
        default:
            assert(false);
    }
}

template<>
AddrConstantPtr Evaluator::EvalBinaryExpr(const BinaryExprPtr& bin_exprp) {
    const auto& lhsp = bin_exprp->Lhsp();
    const auto& rhsp = bin_exprp->Rhsp();
    AddrConstantPtr val{};
    switch (bin_exprp->OpKind()) {
        case BinaryOpKind::kAsgn:
        case BinaryOpKind::kComma:
            throw EvalError{"compile-time constant expected", bin_exprp->Locp()};
        case BinaryOpKind::kAdd:
        case BinaryOpKind::kSub: {
            // Note that the operand may be an array or function.
            QualType lhs_qtype = ValueTrans(lhsp->QType());
            QualType rhs_qtype = ValueTrans(rhsp->QType());
            if (IsPointerTy(lhs_qtype) && IsPointerTy(rhs_qtype))
                throw EvalError("compile-time constant expected",
                                bin_exprp->Locp());
            const auto& ptrty_exprp = IsPointerTy(lhs_qtype) ? lhsp : rhsp;
            const auto& intty_exprp = IsPointerTy(lhs_qtype) ? rhsp : lhsp;
            val = EvalExpr<AddrConstantPtr>(ptrty_exprp);
            std::size_t step_size =
                TypeConv<PointerType>(
                    ValueTrans(ptrty_exprp->QType())).PointeeQTy()->Size();
            if (bin_exprp->OpKind() == BinaryOpKind::kAdd)
                val->AddOff(EvalIntExpr(intty_exprp) * step_size);
            else
                val->AddOff(0 - (EvalIntExpr(intty_exprp) * step_size));
            return val;
        }
        case BinaryOpKind::kMemAccs: {
            if (!expect_mem_accs_)
                throw EvalError{"compile-time constant expected",
                                bin_exprp->Locp()};
            val = EvalExpr<AddrConstantPtr>(lhsp);
            val->AddOff(NodeConv<Object>(*rhsp).Off());
            expect_mem_accs_ = false;
            return val;
        }
        default:
            assert(false);
    }
}

template<typename T>
T Evaluator::EvalTernaryExpr(const TernaryExprPtr& ternary_exprp) {
    const auto& cond_exprp = ternary_exprp->Condp();
    const auto& true_exprp = ternary_exprp->Truep();
    const auto& false_exprp = ternary_exprp->Falsep();
    bool cond = false;
    if (IsPointerTy(cond_exprp->QType())) {
        AddrConstantPtr val = EvalExpr<AddrConstantPtr>(cond_exprp);
        cond = !val->LabelName().empty() || val->Off();
    } else {
        cond = EvalExprToFloat(cond_exprp);
    }
    if (cond) {
        return EvalExpr<T>(true_exprp);
    } else {
        return EvalExpr<T>(false_exprp);
    }
}

template<>
long double Evaluator::EvalConstant(const ConstantPtr& constantp) {
    return constantp->FloatVal();
}

template<>
unsigned long long Evaluator::EvalConstant(const ConstantPtr& constantp) {
    return constantp->UIntVal();
}

template<>
long long Evaluator::EvalConstant(const ConstantPtr& constantp) {
    return constantp->IntVal();
}

template<typename T>
T Evaluator::EvalStrLiteral(const StrLiteralPtr& literalp) {
    throw EvalError{"compile-time constant expected", literalp->Locp()};
}

template<>
AddrConstantPtr Evaluator::EvalStrLiteral(const StrLiteralPtr& literalp) {
    return MakeNodePtr<AddrConstant>(literalp);
}

template<typename T>
T Evaluator::EvalIdent(const IdentPtr& identp) {
    throw EvalError{"compile-time constant expected", identp->Locp()};
}

template<>
AddrConstantPtr Evaluator::EvalIdent(const IdentPtr& identp) {
    return MakeNodePtr<AddrConstant>(identp->Name());
}

}
