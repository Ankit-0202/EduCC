#ifndef IRGENERATOR_H
#define IRGENERATOR_H

#include "ir/IR.h"
#include "../parser/ASTNodes.h"
#include "../semantic/SymbolTable.h"
#include <memory>
#include <string>

namespace IR {

// IR Generator class
class IRGenerator {
public:
    IRGenerator();
    ~IRGenerator();

    // Generate IR from the AST
    std::unique_ptr<IntermediateRepresentation> generate(std::unique_ptr<Parser::ASTNode>& ast);

private:
    Semantic::SymbolTable symbolTable; // For accessing symbol information

    // Visitor functions for statements
    void visitProgram(Parser::Program* program, std::unique_ptr<IntermediateRepresentation>& ir);
    void visitFunctionDefinition(Parser::FunctionDefinition* funcDef, std::unique_ptr<IntermediateRepresentation>& ir);
    void visitVariableDeclaration(Parser::VariableDeclaration* varDecl, std::unique_ptr<IntermediateRepresentation>& ir);
    void visitBlock(Parser::Block* block, std::unique_ptr<IntermediateRepresentation>& ir);
    void visitIfStatement(Parser::IfStatement* ifStmt, std::unique_ptr<IntermediateRepresentation>& ir);
    void visitForStatement(Parser::ForStatement* forStmt, std::unique_ptr<IntermediateRepresentation>& ir);
    void visitWhileStatement(Parser::WhileStatement* whileStmt, std::unique_ptr<IntermediateRepresentation>& ir);
    void visitReturnStatement(Parser::ReturnStatement* retStmt, std::unique_ptr<IntermediateRepresentation>& ir);
    void visitExpressionStatement(Parser::ExpressionStatement* exprStmt, std::unique_ptr<IntermediateRepresentation>& ir);

    // Visitor functions for expressions
    std::string visitAssignmentExpression(Parser::AssignmentExpression* assignExpr, std::unique_ptr<IntermediateRepresentation>& ir);
    std::string visitBinaryExpression(Parser::BinaryExpression* binExpr, std::unique_ptr<IntermediateRepresentation>& ir);
    std::string visitUnaryExpression(Parser::UnaryExpression* unaryExpr, std::unique_ptr<IntermediateRepresentation>& ir);
    std::string visitLiteral(Parser::Literal* literal, std::unique_ptr<IntermediateRepresentation>& ir);
    std::string visitIdentifier(Parser::Identifier* identifier, std::unique_ptr<IntermediateRepresentation>& ir);

    // Generic visit function for expressions
    std::string visit(Parser::ASTNode* node, std::unique_ptr<IntermediateRepresentation>& ir);
};

} // namespace IR

#endif // IRGENERATOR_H
