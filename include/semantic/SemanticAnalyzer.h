#ifndef SEMANTIC_ANALYZER_H
#define SEMANTIC_ANALYZER_H

#include "../parser/ASTNodes.h"
#include "semantic/SymbolTable.h"
#include <memory>

namespace Semantic {

// Semantic Analyzer class
class SemanticAnalyzer {
public:
    SemanticAnalyzer();
    ~SemanticAnalyzer();

    // Analyze the AST starting from the root
    void analyze(std::unique_ptr<Parser::ASTNode>& ast);

private:
    SymbolTable symbolTable;

    // Visitor functions for different AST node types
    void visitProgram(Parser::Program* program);
    void visitFunctionDefinition(Parser::FunctionDefinition* funcDef);
    void visitVariableDeclaration(Parser::VariableDeclaration* varDecl);
    void visitBlock(Parser::Block* block);
    void visitIfStatement(Parser::IfStatement* ifStmt);
    void visitForStatement(Parser::ForStatement* forStmt);
    void visitWhileStatement(Parser::WhileStatement* whileStmt);
    void visitReturnStatement(Parser::ReturnStatement* retStmt);
    void visitExpressionStatement(Parser::ExpressionStatement* exprStmt);
    void visitAssignmentExpression(Parser::AssignmentExpression* assignExpr);
    void visitBinaryExpression(Parser::BinaryExpression* binExpr);
    void visitUnaryExpression(Parser::UnaryExpression* unaryExpr);
    void visitLiteral(Parser::Literal* literal);
    void visitIdentifier(Parser::Identifier* identifier);

    // Generic visit function
    void visit(Parser::ASTNode* node);

    // Error reporting
    void reportError(const std::string& message, int line, int column);
};

} // namespace Semantic

#endif // SEMANTIC_ANALYZER_H
