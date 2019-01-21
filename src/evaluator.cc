#include <exception>
#include <string>
#include <cassert>

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

}
