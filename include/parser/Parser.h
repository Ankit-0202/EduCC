#ifndef PARSER_H
#define PARSER_H

#include "../lexer/Token.h"
#include "ASTNodes.h"
#include <vector>
#include <memory>
#include <string>

namespace Parser {

class Parser {
public:
    Parser(const std::vector<Lexer::Token>& tokens);
    ~Parser();

    // Parse the tokens and return the AST
    std::unique_ptr<ASTNode> parse();

private:
    std::vector<Lexer::Token> tokens;
    size_t current;

    // Utility methods
    bool match(Lexer::TokenType type);
    bool check(Lexer::TokenType type) const;
    Lexer::Token advance();
    Lexer::Token peek() const;
    bool isAtEnd() const;
    Lexer::Token previous() const;

    // Error handling
    void error(const std::string& message, int line, int column);

    // Parsing methods
    std::unique_ptr<Program> parseProgram();
    std::unique_ptr<ASTNode> parseDeclaration();
    std::unique_ptr<FunctionDefinition> parseFunctionDefinition();
    std::unique_ptr<VariableDeclaration> parseVariableDeclaration();
    std::unique_ptr<Statement> parseStatement();
    std::unique_ptr<Block> parseBlock();
    std::unique_ptr<IfStatement> parseIfStatement();
    std::unique_ptr<ForStatement> parseForStatement();
    std::unique_ptr<WhileStatement> parseWhileStatement();
    std::unique_ptr<ReturnStatement> parseReturnStatement();
    std::unique_ptr<ExpressionStatement> parseExpressionStatement();
    std::unique_ptr<Expression> parseExpression();
    std::unique_ptr<AssignmentExpression> parseAssignmentExpression();
    std::unique_ptr<BinaryExpression> parseBinaryExpression();
    std::unique_ptr<UnaryExpression> parseUnaryExpression();
    std::unique_ptr<Literal> parseLiteral();
    std::unique_ptr<Identifier> parseIdentifier();

    // Helper for operator precedence
    std::unique_ptr<Expression> parseBinaryOpRHS(int expr_prec, std::unique_ptr<Expression> lhs);
    int getPrecedence(const std::string& op) const;
};

} // namespace Parser

#endif // PARSER_H
