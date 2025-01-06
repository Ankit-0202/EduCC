/**
 * @file Semantic.h
 * @brief Declaration of the SemanticAnalyzer class for EduCC.
 */

#ifndef EDUCC_SEMANTIC_H
#define EDUCC_SEMANTIC_H

#include "EduCC.h"
#include "ast/AST.h"
#include "semantic/SymbolTable.h"
#include <memory>

// Forward declarations if needed
class SymbolTable;

/**
 * @class SemanticAnalyzer
 * @brief Performs semantic analysis on the AST:
 *  - Builds symbol tables,
 *  - Checks for correctness (e.g. no redeclaration),
 *  - Basic type checking (where possible).
 */
class SemanticAnalyzer
{
public:
    /**
     * @brief Constructs a SemanticAnalyzer with a root TranslationUnitNode.
     * @param root The root translation unit node.
     */
    SemanticAnalyzer(TranslationUnitNode &root);

    /**
     * @brief Main entry point for semantic checks.
     */
    void analyze();

private:
    TranslationUnitNode &m_root;

    void semanticError(ASTNode &node, const std::string &msg);
    void analyzeTranslationUnit(TranslationUnitNode &unit,
                                std::shared_ptr<SymbolTable> symTable);

    void analyzeNode(ASTNode &node, std::shared_ptr<SymbolTable> symTable);

    // Declarations
    void analyzeVarDecl(VarDeclNode &node, std::shared_ptr<SymbolTable> symTable);
    void analyzeFunctionDecl(FunctionDeclNode &node, std::shared_ptr<SymbolTable> symTable);
    void analyzeFunctionDef(FunctionDefNode &node, std::shared_ptr<SymbolTable> symTable);

    void analyzeDeclStmt(DeclStmtNode &node, std::shared_ptr<SymbolTable> symTable); 

    // Statements
    void analyzeCompoundStmt(CompoundStmtNode &node,
                             std::shared_ptr<SymbolTable> symTable);
    void analyzeIfStmt(IfStmtNode &node,
                       std::shared_ptr<SymbolTable> symTable);
    void analyzeWhileStmt(WhileStmtNode &node,
                          std::shared_ptr<SymbolTable> symTable);
    void analyzeForStmt(ForStmtNode &node,
                        std::shared_ptr<SymbolTable> symTable);
    void analyzeReturnStmt(ReturnStmtNode &node,
                           std::shared_ptr<SymbolTable> symTable);
    void analyzeExprStmt(ExprStmtNode &node,
                         std::shared_ptr<SymbolTable> symTable);

    // Expressions
    void analyzeExpr(ExprNode &expr, std::shared_ptr<SymbolTable> symTable);
    void analyzeBinaryExpr(BinaryExprNode &expr, std::shared_ptr<SymbolTable> symTable);
    void analyzeUnaryExpr(UnaryExprNode &expr, std::shared_ptr<SymbolTable> symTable);
    void analyzeCallExpr(CallExprNode &expr, std::shared_ptr<SymbolTable> symTable);
    void analyzeLiteralExpr(LiteralExprNode &expr, std::shared_ptr<SymbolTable> symTable);
    void analyzeIdentifierExpr(IdentifierExprNode &expr, std::shared_ptr<SymbolTable> symTable);
};

#endif // EDUCC_SEMANTIC_H
