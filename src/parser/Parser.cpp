#include "parser/Parser.h"
#include "parser/ASTNodes.h" // Include AST node definitions
#include <stdexcept>
#include <iostream>
#include <cctype>

namespace Parser {

// Constructor
Parser::Parser(const std::vector<Lexer::Token>& tokens)
    : tokens(tokens), current(0) {}

// Destructor
Parser::~Parser() {}

// Entry point for parsing
std::unique_ptr<ASTNode> Parser::parse() {
    return parseProgram();
}

// Parse the entire program
std::unique_ptr<Program> Parser::parseProgram() {
    auto program = std::make_unique<Program>();

    while (!isAtEnd()) {
        auto decl = parseDeclaration();
        if (decl) {
            program->declarations.emplace_back(std::move(decl));
        } else {
            // Skip invalid declarations
            advance();
        }
    }

    return program;
}

// Parse a declaration (function or variable)
std::unique_ptr<ASTNode> Parser::parseDeclaration() {
    // For simplicity, assume that declarations start with a type specifier
    if (check(Lexer::TokenType::INT) || check(Lexer::TokenType::FLOAT) ||
        check(Lexer::TokenType::CHAR) || check(Lexer::TokenType::DOUBLE) ||
        check(Lexer::TokenType::VOID)) {

        // Capture the type
        std::string type = advance().lexeme;

        // Expect an identifier
        if (check(Lexer::TokenType::IDENTIFIER)) {
            std::string name = advance().lexeme;

            // Check if it's a function (followed by '(')
            if (match(Lexer::TokenType::LPAREN)) {
                // Parse function parameters
                std::vector<std::pair<std::string, std::string>> parameters;
                if (!check(Lexer::TokenType::RPAREN)) {
                    do {
                        // Expect parameter type
                        if (check(Lexer::TokenType::INT) || check(Lexer::TokenType::FLOAT) ||
                            check(Lexer::TokenType::CHAR) || check(Lexer::TokenType::DOUBLE) ||
                            check(Lexer::TokenType::VOID)) {
                            std::string paramType = advance().lexeme;

                            // Expect parameter name
                            if (check(Lexer::TokenType::IDENTIFIER)) {
                                std::string paramName = advance().lexeme;
                                parameters.emplace_back(std::make_pair(paramType, paramName));
                            } else {
                                error("Expected parameter name.", peek().line, peek().column);
                            }
                        } else {
                            error("Expected parameter type.", peek().line, peek().column);
                        }
                    } while (match(Lexer::TokenType::COMMA));
                }

                // Expect ')' 
                if (!match(Lexer::TokenType::RPAREN)) {
                    error("Expected ')' after function parameters.", peek().line, peek().column);
                }

                // Expect '{' to start function body
                if (match(Lexer::TokenType::LBRACE)) {
                    auto body = parseBlock();
                    return std::make_unique<FunctionDefinition>(type, name, parameters, std::move(body));
                } else {
                    // Function declaration without body (prototype) is not handled in this simplified parser
                    error("Function declaration without body is not supported.", peek().line, peek().column);
                }
            } else {
                // It's a variable declaration
                // Check if there's an initializer
                std::unique_ptr<Expression> initializer = nullptr;
                if (match(Lexer::TokenType::ASSIGN)) {
                    initializer = parseExpression();
                }

                // Expect ';' at the end
                if (!match(Lexer::TokenType::SEMICOLON)) {
                    error("Expected ';' after variable declaration.", peek().line, peek().column);
                }

                return std::make_unique<VariableDeclaration>(type, name, std::move(initializer));
            }
        } else {
            error("Expected identifier after type specifier.", peek().line, peek().column);
        }
    }

    // If not a declaration, it could be a statement
    return parseStatement();
}

// Parse a statement
std::unique_ptr<Statement> Parser::parseStatement() {
    if (match(Lexer::TokenType::LBRACE)) {
        return parseBlock();
    }
    if (match(Lexer::TokenType::IF)) {
        return parseIfStatement();
    }
    if (match(Lexer::TokenType::FOR)) {
        return parseForStatement();
    }
    if (match(Lexer::TokenType::WHILE)) {
        return parseWhileStatement();
    }
    if (match(Lexer::TokenType::RETURN)) {
        return parseReturnStatement();
    }

    // Otherwise, it's an expression statement
    return parseExpressionStatement();
}

// Parse a block: { ... }
std::unique_ptr<Block> Parser::parseBlock() {
    std::vector<std::unique_ptr<ASTNode>> statements;

    while (!check(Lexer::TokenType::RBRACE) && !isAtEnd()) {
        auto stmt = parseDeclaration();
        if (stmt) {
            statements.emplace_back(std::move(stmt));
        } else {
            // Skip invalid statements
            advance();
        }
    }

    if (!match(Lexer::TokenType::RBRACE)) {
        error("Expected '}' at the end of block.", peek().line, peek().column);
    }

    return std::make_unique<Block>(std::move(statements));
}

// Parse an if statement
std::unique_ptr<IfStatement> Parser::parseIfStatement() {
    // Expect '('
    if (!match(Lexer::TokenType::LPAREN)) {
        error("Expected '(' after 'if'.", peek().line, peek().column);
    }

    // Parse condition
    auto condition = parseExpression();

    // Expect ')'
    if (!match(Lexer::TokenType::RPAREN)) {
        error("Expected ')' after if condition.", peek().line, peek().column);
    }

    // Parse then branch
    auto thenBranch = parseStatement();

    // Parse else branch if present
    std::unique_ptr<Statement> elseBranch = nullptr;
    if (match(Lexer::TokenType::ELSE)) {
        elseBranch = parseStatement();
    }

    return std::make_unique<IfStatement>(std::move(condition), std::move(thenBranch), std::move(elseBranch));
}

// Parse a for statement
std::unique_ptr<ForStatement> Parser::parseForStatement() {
    // Expect '('
    if (!match(Lexer::TokenType::LPAREN)) {
        error("Expected '(' after 'for'.", peek().line, peek().column);
    }

    // Parse initialization
    std::unique_ptr<ASTNode> init = nullptr;
    if (!check(Lexer::TokenType::SEMICOLON)) {
        if (check(Lexer::TokenType::INT) || check(Lexer::TokenType::FLOAT) ||
            check(Lexer::TokenType::CHAR) || check(Lexer::TokenType::DOUBLE) ||
            check(Lexer::TokenType::VOID)) {
            init = parseDeclaration();
        } else {
            init = parseExpressionStatement();
        }
    }
    match(Lexer::TokenType::SEMICOLON); // Consume ';'

    // Parse condition
    std::unique_ptr<Expression> condition = nullptr;
    if (!check(Lexer::TokenType::SEMICOLON)) {
        condition = parseExpression();
    }
    match(Lexer::TokenType::SEMICOLON); // Consume ';'

    // Parse increment
    std::unique_ptr<Expression> increment = nullptr;
    if (!check(Lexer::TokenType::RPAREN)) {
        increment = parseExpression();
    }

    // Expect ')'
    if (!match(Lexer::TokenType::RPAREN)) {
        error("Expected ')' after for clauses.", peek().line, peek().column);
    }

    // Parse loop body
    auto body = parseStatement();

    return std::make_unique<ForStatement>(std::move(init), std::move(condition), std::move(increment), std::move(body));
}

// Parse a while statement
std::unique_ptr<WhileStatement> Parser::parseWhileStatement() {
    // Expect '('
    if (!match(Lexer::TokenType::LPAREN)) {
        error("Expected '(' after 'while'.", peek().line, peek().column);
    }

    // Parse condition
    auto condition = parseExpression();

    // Expect ')'
    if (!match(Lexer::TokenType::RPAREN)) {
        error("Expected ')' after while condition.", peek().line, peek().column);
    }

    // Parse loop body
    auto body = parseStatement();

    return std::make_unique<WhileStatement>(std::move(condition), std::move(body));
}

// Parse a return statement
std::unique_ptr<ReturnStatement> Parser::parseReturnStatement() {
    std::unique_ptr<Expression> expr = nullptr;
    if (!check(Lexer::TokenType::SEMICOLON)) {
        expr = parseExpression();
    }

    // Expect ';'
    if (!match(Lexer::TokenType::SEMICOLON)) {
        error("Expected ';' after return statement.", peek().line, peek().column);
    }

    return std::make_unique<ReturnStatement>(std::move(expr));
}

// Parse an expression statement
std::unique_ptr<ExpressionStatement> Parser::parseExpressionStatement() {
    auto expr = parseExpression();

    // Expect ';'
    if (!match(Lexer::TokenType::SEMICOLON)) {
        error("Expected ';' after expression.", peek().line, peek().column);
    }

    return std::make_unique<ExpressionStatement>(std::move(expr));
}

// Parse an expression (handles assignment and binary expressions)
std::unique_ptr<Expression> Parser::parseExpression() {
    return parseAssignmentExpression();
}

// Parse an assignment expression
std::unique_ptr<Expression> Parser::parseAssignmentExpression() {
    auto lhs = parseBinaryExpression();

    if (match(Lexer::TokenType::ASSIGN)) {
        auto rhs = parseAssignmentExpression();
        return std::make_unique<AssignmentExpression>(std::move(lhs), std::move(rhs));
    }

    return lhs; // Return lhs if no assignment is present
}

// Parse a binary expression with precedence
std::unique_ptr<Expression> Parser::parseBinaryExpression() {
    auto lhs = parseUnaryExpression();

    while (true) {
        int precedence = getPrecedence(peek().lexeme);
        if (precedence < 1) break;

        std::string op = advance().lexeme;
        auto rhs = parseUnaryExpression();

        lhs = std::make_unique<BinaryExpression>(op, std::move(lhs), std::move(rhs));
    }

    return lhs;
}

// Parse a unary expression
std::unique_ptr<Expression> Parser::parseUnaryExpression() {
    if (match(Lexer::TokenType::MINUS) || match(Lexer::TokenType::NOT_EQUAL)) {
        std::string op = previous().lexeme;
        auto operand = parseUnaryExpression();
        return std::make_unique<UnaryExpression>(op, std::move(operand));
    }

    return parsePrimary();
}

// Parse primary expressions (identifiers, literals, and parenthesized expressions)
std::unique_ptr<Expression> Parser::parsePrimary() {
    if (match(Lexer::TokenType::INTEGER_LITERAL)) {
        return std::make_unique<Literal>(previous().lexeme);
    }
    if (match(Lexer::TokenType::FLOAT_LITERAL)) {
        return std::make_unique<Literal>(previous().lexeme);
    }
    if (match(Lexer::TokenType::CHAR_LITERAL)) {
        return std::make_unique<Literal>(previous().lexeme);
    }
    if (match(Lexer::TokenType::STRING_LITERAL)) {
        return std::make_unique<Literal>(previous().lexeme);
    }
    if (match(Lexer::TokenType::IDENTIFIER)) {
        return std::make_unique<Identifier>(previous().lexeme);
    }
    if (match(Lexer::TokenType::LPAREN)) {
        auto expr = parseExpression();
        if (!match(Lexer::TokenType::RPAREN)) {
            error("Expected ')' after expression.", peek().line, peek().column);
        }
        return expr;
    }

    error("Expected expression.", peek().line, peek().column);
    return nullptr;
}

// Helper: Get precedence of the operator
int Parser::getPrecedence(const std::string& op) const {
    if (op == "*" || op == "/") return 3;
    if (op == "+" || op == "-") return 2;
    if (op == "<" || op == ">" || op == "<=" || op == ">=") return 1;
    return 0;
}

// Utility methods

bool Parser::match(Lexer::TokenType type) {
    if (check(type)) {
        advance();
        return true;
    }
    return false;
}

bool Parser::check(Lexer::TokenType type) const {
    if (isAtEnd()) return false;
    return peek().type == type;
}

Lexer::Token Parser::advance() {
    if (!isAtEnd()) current++;
    return previous();
}

Lexer::Token Parser::peek() const {
    if (isAtEnd()) {
        return tokens.back(); // Return EOF token
    }
    return tokens[current];
}

bool Parser::isAtEnd() const {
    return peek().type == Lexer::TokenType::EOF_TOKEN;
}

Lexer::Token Parser::previous() const {
    return tokens[current - 1];
}

// Error handling
void Parser::error(const std::string& message, int line, int column) {
    std::cerr << "[Parser Error] " << message << " at line " << line << ", column " << column << "\n";
    throw std::runtime_error("Parser error");
}

} // namespace Parser
