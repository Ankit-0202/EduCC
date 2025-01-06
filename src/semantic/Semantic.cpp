/**
 * @file Semantic.cpp
 * @brief Implementation of the SemanticAnalyzer class.
 */

#include "semantic/Semantic.h"
#include "ast/AST.h"   // for DeclStmtNode, etc.

SemanticAnalyzer::SemanticAnalyzer(TranslationUnitNode &root)
    : m_root(root)
{
}

void SemanticAnalyzer::analyze()
{
    auto globalSymTable = std::make_shared<SymbolTable>();
    analyzeTranslationUnit(m_root, globalSymTable);
}

void SemanticAnalyzer::semanticError(ASTNode &node, const std::string &msg)
{
    std::string fullMsg = "[Semantic Error] " + msg +
                          " at line " + std::to_string(node.line()) +
                          ", column " + std::to_string(node.column()) + ".";
    throw std::runtime_error(fullMsg);
}

void SemanticAnalyzer::analyzeTranslationUnit(TranslationUnitNode &unit,
                                              std::shared_ptr<SymbolTable> symTable)
{
    for (auto &declPtr : unit.declarations()) {
        analyzeNode(*declPtr, symTable);
    }
}

void SemanticAnalyzer::analyzeNode(ASTNode &node, std::shared_ptr<SymbolTable> symTable)
{
    switch (node.kind()) {
        case ASTNodeKind::VarDecl:
            analyzeVarDecl(static_cast<VarDeclNode&>(node), symTable);
            break;
        case ASTNodeKind::FunctionDecl:
            analyzeFunctionDecl(static_cast<FunctionDeclNode&>(node), symTable);
            break;
        case ASTNodeKind::FunctionDef:
            analyzeFunctionDef(static_cast<FunctionDefNode&>(node), symTable);
            break;
        case ASTNodeKind::CompoundStmt:
            analyzeCompoundStmt(static_cast<CompoundStmtNode&>(node), symTable);
            break;
        case ASTNodeKind::IfStmt:
            analyzeIfStmt(static_cast<IfStmtNode&>(node), symTable);
            break;
        case ASTNodeKind::WhileStmt:
            analyzeWhileStmt(static_cast<WhileStmtNode&>(node), symTable);
            break;
        case ASTNodeKind::ForStmt:
            analyzeForStmt(static_cast<ForStmtNode&>(node), symTable);
            break;
        case ASTNodeKind::ReturnStmt:
            analyzeReturnStmt(static_cast<ReturnStmtNode&>(node), symTable);
            break;
        case ASTNodeKind::ExprStmt:
            analyzeExprStmt(static_cast<ExprStmtNode&>(node), symTable);
            break;

        // NEW: local declaration statement
        case ASTNodeKind::DeclStmt:
            analyzeDeclStmt(static_cast<DeclStmtNode&>(node), symTable);
            break;

        // Expression nodes
        case ASTNodeKind::BinaryExpr:
        case ASTNodeKind::UnaryExpr:
        case ASTNodeKind::CallExpr:
        case ASTNodeKind::LiteralExpr:
        case ASTNodeKind::IdentifierExpr:
            analyzeExpr(static_cast<ExprNode&>(node), symTable);
            break;

        case ASTNodeKind::TranslationUnit:
            // already handled
            break;

        default:
            semanticError(node, "Unrecognized AST node kind");
    }
}

// -------------------------------------------------------------------
// Declarations
// -------------------------------------------------------------------

void SemanticAnalyzer::analyzeVarDecl(VarDeclNode &node,
                                      std::shared_ptr<SymbolTable> symTable)
{
    Symbol sym(SymbolKind::Variable, node.varName(), node.typeName());
    try {
        symTable->addSymbol(sym);
    } catch (const std::runtime_error &e) {
        semanticError(node, e.what());
    }
}

void SemanticAnalyzer::analyzeFunctionDecl(FunctionDeclNode &node,
                                           std::shared_ptr<SymbolTable> symTable)
{
    Symbol sym(SymbolKind::Function, node.funcName(), node.returnType());
    try {
        symTable->addSymbol(sym);
    } catch (const std::runtime_error &e) {
        semanticError(node, e.what());
    }
}

void SemanticAnalyzer::analyzeFunctionDef(FunctionDefNode &node,
                                          std::shared_ptr<SymbolTable> symTable)
{
    auto decl = node.decl();
    if (!decl) {
        semanticError(node, "FunctionDefNode has null decl()");
    }
    Symbol funcSym(SymbolKind::Function, decl->funcName(), decl->returnType());
    try {
        symTable->addSymbol(funcSym);
    } catch (const std::runtime_error &ex) {
        semanticError(node, ex.what());
    }

    auto funcScope = std::make_shared<SymbolTable>(symTable);
    // add parameters
    for (size_t i = 0; i < decl->paramCount(); i++) {
        Symbol paramSym(SymbolKind::Variable, decl->paramName(i), decl->paramType(i));
        try {
            funcScope->addSymbol(paramSym);
        } catch (const std::runtime_error &ex) {
            semanticError(node, ex.what());
        }
    }

    if (!node.body()) {
        semanticError(node, "Function definition has no body");
    }
    analyzeNode(*(const_cast<ASTNode*>(node.body())), funcScope);
}

// -------------------------------------------------------------------
// DeclStmt (NEW) - local variable declaration
// -------------------------------------------------------------------
void SemanticAnalyzer::analyzeDeclStmt(DeclStmtNode &node,
                                       std::shared_ptr<SymbolTable> symTable)
{
    if (!node.varDecl()) {
        semanticError(node, "DeclStmtNode has null VarDeclNode");
    }

    // Insert variable into symbol table
    Symbol sym(SymbolKind::Variable,
               node.varDecl()->varName(),
               node.varDecl()->typeName());
    try {
        symTable->addSymbol(sym);
    } catch (const std::runtime_error &ex) {
        semanticError(node, ex.what());
    }

    // If there's an initializer expression, analyze it
    if (node.initExpr()) {
        // We treat it as an expression
        auto exprNode = dynamic_cast<ExprNode*>(const_cast<ASTNode*>(node.initExpr()));
        if (exprNode) {
            analyzeExpr(*exprNode, symTable);
        } else {
            semanticError(node, "Initializer is not an expression");
        }
    }
}

// -------------------------------------------------------------------
// Statements
// -------------------------------------------------------------------
void SemanticAnalyzer::analyzeCompoundStmt(CompoundStmtNode &node,
                                           std::shared_ptr<SymbolTable> symTable)
{
    auto blockScope = std::make_shared<SymbolTable>(symTable);
    for (auto &item : node.items()) {
        analyzeNode(*item, blockScope);
    }
}

void SemanticAnalyzer::analyzeIfStmt(IfStmtNode &node,
                                     std::shared_ptr<SymbolTable> symTable)
{
    if (node.condition()) {
        auto condExpr = dynamic_cast<ExprNode*>(const_cast<ASTNode*>(node.condition()));
        if (condExpr) {
            analyzeExpr(*condExpr, symTable);
        }
    }
    if (node.thenBranch()) {
        analyzeNode(*(ASTNode*)node.thenBranch(), symTable);
    }
    if (node.elseBranch()) {
        analyzeNode(*(ASTNode*)node.elseBranch(), symTable);
    }
}

void SemanticAnalyzer::analyzeWhileStmt(WhileStmtNode &node,
                                        std::shared_ptr<SymbolTable> symTable)
{
    if (node.condition()) {
        auto condExpr = dynamic_cast<ExprNode*>(const_cast<ASTNode*>(node.condition()));
        if (condExpr) analyzeExpr(*condExpr, symTable);
    }
    if (node.body()) {
        analyzeNode(*(ASTNode*)node.body(), symTable);
    }
}

void SemanticAnalyzer::analyzeForStmt(ForStmtNode &node,
                                      std::shared_ptr<SymbolTable> symTable)
{
    auto forScope = std::make_shared<SymbolTable>(symTable);

    if (node.init()) {
        analyzeNode(*(ASTNode*)node.init(), forScope);
    }
    if (node.condition()) {
        auto condExpr = dynamic_cast<ExprNode*>(const_cast<ASTNode*>(node.condition()));
        if (condExpr) analyzeExpr(*condExpr, forScope);
    }
    if (node.increment()) {
        auto incrExpr = dynamic_cast<ExprNode*>(const_cast<ASTNode*>(node.increment()));
        if (incrExpr) analyzeExpr(*incrExpr, forScope);
    }
    if (node.body()) {
        analyzeNode(*(ASTNode*)node.body(), forScope);
    }
}

void SemanticAnalyzer::analyzeReturnStmt(ReturnStmtNode &node,
                                         std::shared_ptr<SymbolTable> symTable)
{
    if (node.expr()) {
        auto exprPtr = dynamic_cast<ExprNode*>(const_cast<ASTNode*>(node.expr()));
        if (exprPtr) analyzeExpr(*exprPtr, symTable);
    }
}

void SemanticAnalyzer::analyzeExprStmt(ExprStmtNode &node,
                                       std::shared_ptr<SymbolTable> symTable)
{
    if (node.expr()) {
        auto exprPtr = dynamic_cast<ExprNode*>(const_cast<ASTNode*>(node.expr()));
        if (exprPtr) analyzeExpr(*exprPtr, symTable);
    }
}

// -------------------------------------------------------------------
// Expression Analysis
// -------------------------------------------------------------------
void SemanticAnalyzer::analyzeExpr(ExprNode &expr,
                                   std::shared_ptr<SymbolTable> symTable)
{
    switch (expr.kind()) {
        case ASTNodeKind::BinaryExpr:
            analyzeBinaryExpr(static_cast<BinaryExprNode&>(expr), symTable);
            break;
        case ASTNodeKind::UnaryExpr:
            analyzeUnaryExpr(static_cast<UnaryExprNode&>(expr), symTable);
            break;
        case ASTNodeKind::CallExpr:
            analyzeCallExpr(static_cast<CallExprNode&>(expr), symTable);
            break;
        case ASTNodeKind::LiteralExpr:
            analyzeLiteralExpr(static_cast<LiteralExprNode&>(expr), symTable);
            break;
        case ASTNodeKind::IdentifierExpr:
            analyzeIdentifierExpr(static_cast<IdentifierExprNode&>(expr), symTable);
            break;
        default:
            semanticError(expr, "Unknown expression type");
    }
}

void SemanticAnalyzer::analyzeBinaryExpr(BinaryExprNode &expr,
                                         std::shared_ptr<SymbolTable> symTable)
{
    if (expr.left()) {
        auto leftExpr = dynamic_cast<ExprNode*>(const_cast<ASTNode*>(expr.left()));
        if (leftExpr) analyzeExpr(*leftExpr, symTable);
    }
    if (expr.right()) {
        auto rightExpr = dynamic_cast<ExprNode*>(const_cast<ASTNode*>(expr.right()));
        if (rightExpr) analyzeExpr(*rightExpr, symTable);
    }
}

void SemanticAnalyzer::analyzeUnaryExpr(UnaryExprNode &expr,
                                        std::shared_ptr<SymbolTable> symTable)
{
    if (expr.operand()) {
        auto opExpr = dynamic_cast<ExprNode*>(const_cast<ASTNode*>(expr.operand()));
        if (opExpr) analyzeExpr(*opExpr, symTable);
    }
}

void SemanticAnalyzer::analyzeCallExpr(CallExprNode &expr,
                                       std::shared_ptr<SymbolTable> symTable)
{
    auto sym = symTable->lookup(expr.callee());
    if (!sym) {
        semanticError(expr, "Undefined function '" + expr.callee() + "'");
    } else if (sym->kind != SymbolKind::Function) {
        semanticError(expr, "'" + expr.callee() + "' is not a function");
    }

    for (auto &arg : expr.args()) {
        auto argExpr = dynamic_cast<ExprNode*>(arg.get());
        if (argExpr) analyzeExpr(*argExpr, symTable);
    }
}

void SemanticAnalyzer::analyzeLiteralExpr(LiteralExprNode &expr,
                                          std::shared_ptr<SymbolTable> symTable)
{
    (void) symTable;
}

void SemanticAnalyzer::analyzeIdentifierExpr(IdentifierExprNode &expr,
                                             std::shared_ptr<SymbolTable> symTable)
{
    auto sym = symTable->lookup(expr.name());
    if (!sym) {
        semanticError(expr, "Undefined variable '" + expr.name() + "'");
    }
    if (sym->kind != SymbolKind::Variable) {
        semanticError(expr, "'" + sym->name + "' is not a variable");
    }
}
