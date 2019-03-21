#ifndef _AXCC_PARSER_HH_
#define _AXCC_PARSER_HH_

#include <memory>
#include <string>
#include <stack>
#include <vector>
#include <map>
#include <utility>

#include "token.hh"
#include "type.hh"
#include "ast.hh"

namespace axcc {

class Parser {
public:
    Parser(TokenSequence& ts);
    virtual ~Parser();
    AstRoot Parse();
    ExprPtr ParseAsExpr();
private:
    class Scope;
    class Scopes;
    enum class DeclPos;
    enum class StorSpec;
    enum class TypeSpec;
    struct DeclSpecInfo;
    struct DeclaratorInfo;

    void ParseTranslationUnit();
    void ParseExtDecl();
    void ParseStaticAssert();
    // Parse Declaration specifiers.
    DeclSpecInfo ParseDeclSpec(const DeclPos& decl_pos);
    void TrySetStorSpec(const DeclPos& decl_pos, StorSpec& stor,
                        const StorSpec& spec, const SourceLoc& loc);
    void TrySetQual(unsigned char& qualifiers, QualType::Qualifier qual,
                    const SourceLoc& loc);
    void TrySetFuncSpec(unsigned char& func_specs, FuncType::FuncSpec spec,
                        const SourceLoc& loc);
    void TrySetAlign(const DeclPos& decl_pos, long long& align,
                     long long val, const SourceLoc& loc);
    void TrySetTypeSpec(TypeSpec& type_spec, const TypeSpec& spec,
                        const SourceLoc& loc);
    void TrySetArithKind(TypeSpec& type_spec, unsigned int& arith_kind,
                         ArithType::ArithSpec spec, unsigned int cmpt,
                         const SourceLoc& loc);
    long long ParseAlignAs();
    QualType ParseTypeName();
    void ParseEnumSpec();
    void ParseEnumDef();
    int ParseEnumDefItem(int val);
    TypePtr ParseRecordSpec();
    std::vector<ObjectPtr> ParseRecordDef();
    void ParseRecordDefItem(std::vector<ObjectPtr>& members);
    ObjectPtr ParseBitField(const ObjectPtr& orig_objp);
    // Parse Declarators.
    IdentPtr ParseDeclarator(const DeclPos& decl_pos,
                             const DeclSpecInfo& spec_info,
                             bool check_err = true,
                             TypePtr* prev_hookp = nullptr);
    unsigned char ParseQuals();
    QualType ParseDeclaratorTail(const QualType& base_qty);
    QualType ParseFuncDeclTail(const QualType& base_qty);
    std::vector<ObjectPtr> ParseFuncParamsList();
    std::vector<ObjectPtr> ParseKRFuncParamsList();
    QualType ParseArrayDeclTail(const QualType& base_qty);
    IdentPtr MakeDeclarator(const DeclPos& decl_pos,
                            const DeclSpecInfo& spec_info,
                            const DeclaratorInfo& d_info,
                            bool check_err);
    LinkKind GetLinkKind(const DeclPos& decl_pos, const DeclSpecInfo& spec_info,
                         bool is_obj);
    StorKind GetStorKind(const DeclPos& decl_pos, const DeclSpecInfo& spec_info);
    // Parse function definitions.
    bool CurIsFuncDef(const Ident& ident, const QualType& base_qty);
    FuncDefPtr ParseFuncDef(const IdentPtr& identp);
    std::vector<ObjDefStmtPtr> TryParseKRFuncDeclList();
    CmpdStmtPtr ParseFuncBody(const FuncType& func_type);
    // Parse static initializers.
    ObjDefStmtPtr ParseStaticInitializer(const ObjectPtr& objp);
    // Handle tentative definitions.
    void AddTentativeDefIfNeed(const ObjectPtr& objp);
    void RmTentativeDefIfHas(const std::string& name);
    void TryGenTentativeDefs();
    // Scope handlers.
    void TryAddToScope(const IdentPtr& identp);
    void TryAddTagToScope(const TagPtr& tagp);
    void TryAddLabelToScope(const LabelPtr& labelp);
    void TryAddOrdIdentToScope(const IdentPtr& identp);
    // Parse compound statements.
    CmpdStmtPtr ParseCmpdStmt(const std::vector<ObjectPtr>& merged_objs = {});
    StmtPtr ParseStmt();
    std::vector<ObjDefStmtPtr> ParseDeclStmt(const DeclPos& declpos);
    void HandleLocalExternDecl(const IdentPtr& identp);
    // Parse initializers.
    ObjDefStmtPtr ParseInitializer(const ObjectPtr& objp);
    std::vector<Initializer> ParseInitializer(const QualType& qtype,
                                              long long off,
                                              bool designated,
                                              bool follow_asgn);
    std::vector<Initializer> ParseRecordInitializer(const RecordType& rec_type,
                                                    long long off,
                                                    bool designated,
                                                    bool follow_asgn);
    bool RecordInitTrySingleExpr(std::vector<Initializer>& inits,
                                 const RecordType& rec_type,
                                 long long off,
                                 bool designated,
                                 bool has_brace);
    std::vector<Initializer> ParseArrayInitializer(ArrayType& arr_type,
                                                   long long off,
                                                   bool designated,
                                                   bool follow_asgn);
    bool ArrayInitTryStrLiteral(std::vector<Initializer>& inits,
                                ArrayType& arr_type,
                                long long off,
                                bool designated,
                                bool has_brace);
    bool CurObjInitIsEnd(bool all_init, bool designated, bool has_brace);
    Initializer ParseScalarInitializer(const QualType& qtype, long long off);
    // Parse statements.
    IfStmtPtr ParseIfStmt();
    CmpdStmtPtr ParseWhileStmt();
    CmpdStmtPtr ParseDoStmt();
    CmpdStmtPtr ParseForStmt();
    CmpdStmtPtr ParseSwitchStmt();
    void GenSwitchJumpStmts(std::vector<StmtPtr>& stmts,
                            const QualType& ctrl_qtype,
                            const ObjectPtr& tmp_varp,
                            const LabelPtr& end_labelp);
    JumpStmtPtr ParseGotoStmt();
    StmtPtr ParseContinueStmt();
    StmtPtr ParseBreakStmt();
    ReturnStmtPtr ParseReturnStmt();
    CmpdStmtPtr ParseCaseStmt();
    CmpdStmtPtr ParseDefaultStmt();
    CmpdStmtPtr ParseLabelStmt();
    StmtPtr ParseExprStmt();
    // Parse expressions.
    ExprPtr ParseExpr();
    ExprPtr ParseAssignExpr();
    ConstantPtr ParseIntConstantExpr();
    ExprPtr ParseConditionalExpr();
    ExprPtr ParseLogicalOrExpr();
    ExprPtr ParseLogicalAndExpr();
    ExprPtr ParseInclusiveOrExpr();
    ExprPtr ParseExclusiveOrExpr();
    ExprPtr ParseAndExpr();
    template<ExprPtr (Parser::* ParseTerm)(), TokenType op_token,
             BinaryOpKind op_kind>
    ExprPtr ParseSimpleBinExpr();
    ExprPtr ParseEqualityExpr();
    ExprPtr ParseRelationalExpr();
    ExprPtr ParseShiftExpr();
    ExprPtr ParseAdditiveExpr();
    ExprPtr ParseMultExpr();
    ExprPtr ParseCastExpr();
    // Parse unary expressions.
    ExprPtr ParseUnaryExpr();
    UnaryExprPtr ParsePreIncDecExpr(const UnaryOpKind& op_kind);
    UnaryExprPtr ParseUnaryOpExpr(const UnaryOpKind& op_kind);
    ConstantPtr ParseSizeof();
    ConstantPtr ParseAlignof();
    // Parse postfix expressions.
    ExprPtr ParsePostfixExpr();
    ExprPtr ParsePostfixExprTail(ExprPtr exprp);
    ObjectPtr ParseCompoundLiteral();
    ObjectPtr ParseCompoundLiteralTail(const SourceLocPtr& obj_locp,
                                       const QualType& qtype);
    UnaryExprPtr ParseSubscript(const ExprPtr& exprp);
    FuncCallPtr ParseFuncCall(const ExprPtr& exprp);
    BinaryExprPtr ParseMemAccs(const ExprPtr& exprp);
    BinaryExprPtr ParseMemAccsPtr(const ExprPtr& exprp);
    UnaryExprPtr ParsePostIncDecExpr(const ExprPtr& exprp,
                                     const UnaryOpKind& op_kind);
    // Parse primary expressions.
    ExprPtr ParsePrimaryExpr();
    IdentPtr ParseIdentifier();
    ExprPtr ParseParenExpr();
    ExprPtr ParseGenericSelect();
    ExprPtr ParseGenericAssocList(const QualType& ctrl_qtype);
    ConstantPtr ParseIConstant();
    ConstantPtr ParseFConstant();
    ConstantPtr ParseCConstant();
    StrLiteralPtr ParseStrLiterals();
    StrLiteralPtr ParseStrLiteral();

    // Helper functions.
    bool IsTypeNameToken(const Token& t);
    void ExpectCur(const TokenType& tag);
    void ExpectNext(const TokenType& tag);
    void SkipToSyncToken();

    TokenSequence& ts_;
    AstRoot ast_{};
    // Parsing context.
    std::unique_ptr<Scopes> scopesp_;
    std::map<std::string, ObjectPtr> tentative_defs_{};
    QualType cur_ret_qty_{};
    std::vector<ObjectPtr> cur_local_vars_{};
    std::vector<LabelPtr> undecl_labels_{};
    std::stack<LabelPtr> break_dsts_{};
    std::stack<LabelPtr> continue_dsts_{};
    std::stack<std::vector<std::pair<ConstantPtr, LabelPtr>>> cases_{};
    std::stack<LabelPtr> default_labels_{};
};

}
#endif
