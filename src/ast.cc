#include "ast.hh"

namespace axcc {

std::vector<ObjectPtr> FuncDef::LocalVars() const {
    std::vector<ObjectPtr> local_vars{};
    for (const auto& stmtp : bodyp_->Stmts()) {
        if (IsObjDeclStmt(*stmtp))
            local_vars.emplace_back(NodeConv<ObjDeclStmt>(*stmtp).Objp());
    }
    return local_vars;
}

}
