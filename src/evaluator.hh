#ifndef _AXCC_EVALUATOR_HH_
#define _AXCC_EVALUATOR_HH_

#include "token.hh"
#include "ast.hh"

namespace axcc {

class Evaluator {
public:
    ExprPtr EvalStaticInitializer(const ExprPtr& exprp);
    ConstantPtr EvalIntConstantExpr(const ExprPtr& exprp);
    bool EvalPPConstantExpr(const ExprPtr& exprp);
private:
    template<typename T>
    T EvalExpr(const ExprPtr& exprp);
    long long EvalIntExpr(const ExprPtr& exprp);
    long double EvalExprToFloat(const ExprPtr& exprp);
    template<typename T>
    T EvalUnaryExpr(const UnaryExprPtr& unary_exprp);
    template<typename T>
    T EvalBinaryExpr(const BinaryExprPtr& bin_exprp);
    template<typename T>
    T EvalTernaryExpr(const TernaryExprPtr& ternary_exprp);
    template<typename T>
    T EvalConstant(const ConstantPtr& constantp);
    template<typename T>
    T EvalStrLiteral(const StrLiteralPtr& literalp);
    template<typename T>
    T EvalIdent(const IdentPtr& identp);

    // Member access expression can only follow the address of operator.
    bool expect_mem_accs_{false};
};

}
#endif
