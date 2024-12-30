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
     * @brief Constructs the analyzer with a reference to the AST's root node.
     * @param root The top-level AST node (TranslationUnit).
     */
    SemanticAnalyzer(TranslationUnitNode &root);

    /**
     * @brief Runs the semantic analysis, throwing std::runtime_error on errors.
     */
    void analyze();

private:
    TranslationUnitNode &m_root;

    // Helper methods:
    void analyzeTranslationUnit(TranslationUnitNode &unit, std::shared_ptr<SymbolTable> symTable);
    void analyzeNode(ASTNode &node, std::shared_ptr<SymbolTable> symTable);

    // Overloads for different node types:
    void analyzeVarDecl(VarDeclNode &node, std::shared_ptr<SymbolTable> symTable);
    void analyzeFunctionDecl(FunctionDeclNode &node, std::shared_ptr<SymbolTable> symTable);
    void analyzeFunctionDef(FunctionDefNode &node, std::shared_ptr<SymbolTable> symTable);
    void analyzeCompoundStmt(CompoundStmtNode &node, std::shared_ptr<SymbolTable> symTable);
    void analyzeIfStmt(IfStmtNode &node, std::shared_ptr<SymbolTable> symTable);
    void analyzeWhileStmt(WhileStmtNode &node, std::shared_ptr<SymbolTable> symTable);
    void analyzeForStmt(ForStmtNode &node, std::shared_ptr<SymbolTable> symTable);
    void analyzeReturnStmt(ReturnStmtNode &node, std::shared_ptr<SymbolTable> symTable);
    void analyzeExprStmt(ExprStmtNode &node, std::shared_ptr<SymbolTable> symTable);

    // Expression analysis
    void analyzeExpr(ExprNode &expr, std::shared_ptr<SymbolTable> symTable);
    void analyzeBinaryExpr(BinaryExprNode &expr, std::shared_ptr<SymbolTable> symTable);
    void analyzeUnaryExpr(UnaryExprNode &expr, std::shared_ptr<SymbolTable> symTable);
    void analyzeCallExpr(CallExprNode &expr, std::shared_ptr<SymbolTable> symTable);
    void analyzeLiteralExpr(LiteralExprNode &expr, std::shared_ptr<SymbolTable> symTable);
    void analyzeIdentifierExpr(IdentifierExprNode &expr, std::shared_ptr<SymbolTable> symTable);

    // Helpers:
    [[noreturn]] void semanticError(ASTNode &node, const std::string &msg);
};

#endif // EDUCC_SEMANTIC_H
