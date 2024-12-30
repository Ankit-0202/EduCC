#ifndef ASTNODES_H
#define ASTNODES_H

#include <string>
#include <vector>
#include <memory>

// Namespace for the Parser
namespace Parser {

// Enumeration for AST Node Types
enum class ASTNodeType {
    PROGRAM,
    FUNCTION_DEFINITION,
    VARIABLE_DECLARATION,
    BLOCK,
    IF_STATEMENT,
    FOR_STATEMENT,
    WHILE_STATEMENT,
    RETURN_STATEMENT,
    EXPRESSION_STATEMENT,
    ASSIGNMENT_EXPRESSION,
    BINARY_EXPRESSION,
    UNARY_EXPRESSION,
    LITERAL,
    IDENTIFIER
};

// Base class for all AST nodes
struct ASTNode {
    ASTNodeType type;
    int line;
    int column;

    ASTNode(ASTNodeType nodeType = ASTNodeType::PROGRAM, int ln = 0, int col = 0)
        : type(nodeType), line(ln), column(col) {}

    virtual ~ASTNode() = default;
};

// Expression base class
struct Expression : ASTNode {
    Expression(ASTNodeType nodeType, int ln = 0, int col = 0)
        : ASTNode(nodeType, ln, col) {}
};

// Statement base class
struct Statement : ASTNode {
    Statement(ASTNodeType nodeType, int ln = 0, int col = 0)
        : ASTNode(nodeType, ln, col) {}
};

// Program node: represents the entire program
struct Program : ASTNode {
    std::vector<std::unique_ptr<ASTNode>> declarations;

    Program() : ASTNode(ASTNodeType::PROGRAM) {}
};

// Function Definition node
struct FunctionDefinition : ASTNode {
    std::string returnType;
    std::string name;
    std::vector<std::pair<std::string, std::string>> parameters; // pair of type and name
    std::unique_ptr<Statement> body;

    FunctionDefinition(const std::string& retType, const std::string& funcName,
                       const std::vector<std::pair<std::string, std::string>>& params,
                       std::unique_ptr<Statement> funcBody, int ln = 0, int col = 0)
        : ASTNode(ASTNodeType::FUNCTION_DEFINITION, ln, col),
          returnType(retType),
          name(funcName),
          parameters(params),
          body(std::move(funcBody)) {}
};

// Variable Declaration node
struct VariableDeclaration : Statement {
    std::string type;
    std::string name;
    std::unique_ptr<Expression> initializer;

    VariableDeclaration(const std::string& varType, const std::string& varName,
                        std::unique_ptr<Expression> init = nullptr, int ln = 0, int col = 0)
        : Statement(ASTNodeType::VARIABLE_DECLARATION, ln, col),
          type(varType),
          name(varName),
          initializer(std::move(init)) {}
};

// Block node: represents a compound statement { ... }
struct Block : Statement {
    std::vector<std::unique_ptr<ASTNode>> statements;

    Block(std::vector<std::unique_ptr<ASTNode>> stmts, int ln = 0, int col = 0)
        : Statement(ASTNodeType::BLOCK, ln, col),
          statements(std::move(stmts)) {}
};

// If Statement node
struct IfStatement : Statement {
    std::unique_ptr<Expression> condition;
    std::unique_ptr<Statement> thenBranch;
    std::unique_ptr<Statement> elseBranch;

    IfStatement(std::unique_ptr<Expression> cond,
                std::unique_ptr<Statement> thenStmt,
                std::unique_ptr<Statement> elseStmt = nullptr,
                int ln = 0, int col = 0)
        : Statement(ASTNodeType::IF_STATEMENT, ln, col),
          condition(std::move(cond)),
          thenBranch(std::move(thenStmt)),
          elseBranch(std::move(elseStmt)) {}
};

// For Statement node
struct ForStatement : Statement {
    std::unique_ptr<ASTNode> init; // Could be a VariableDeclaration or ExpressionStatement
    std::unique_ptr<Expression> condition;
    std::unique_ptr<Expression> increment;
    std::unique_ptr<Statement> body;

    ForStatement(std::unique_ptr<ASTNode> initializer,
                std::unique_ptr<Expression> cond,
                std::unique_ptr<Expression> incr,
                std::unique_ptr<Statement> loopBody,
                int ln = 0, int col = 0)
        : Statement(ASTNodeType::FOR_STATEMENT, ln, col),
          init(std::move(initializer)),
          condition(std::move(cond)),
          increment(std::move(incr)),
          body(std::move(loopBody)) {}
};

// While Statement node
struct WhileStatement : Statement {
    std::unique_ptr<Expression> condition;
    std::unique_ptr<Statement> body;

    WhileStatement(std::unique_ptr<Expression> cond,
                   std::unique_ptr<Statement> loopBody,
                   int ln = 0, int col = 0)
        : Statement(ASTNodeType::WHILE_STATEMENT, ln, col),
          condition(std::move(cond)),
          body(std::move(loopBody)) {}
};

// Return Statement node
struct ReturnStatement : Statement {
    std::unique_ptr<Expression> expression;

    ReturnStatement(std::unique_ptr<Expression> expr = nullptr, int ln = 0, int col = 0)
        : Statement(ASTNodeType::RETURN_STATEMENT, ln, col),
          expression(std::move(expr)) {}
};

// Expression Statement node
struct ExpressionStatement : Statement {
    std::unique_ptr<Expression> expression;

    ExpressionStatement(std::unique_ptr<Expression> expr, int ln = 0, int col = 0)
        : Statement(ASTNodeType::EXPRESSION_STATEMENT, ln, col),
          expression(std::move(expr)) {}
};

// Assignment Expression node
struct AssignmentExpression : Expression {
    std::unique_ptr<Expression> left;  // Should be Identifier
    std::unique_ptr<Expression> right;

    AssignmentExpression(std::unique_ptr<Expression> lhs,
                        std::unique_ptr<Expression> rhs,
                        int ln = 0, int col = 0)
        : Expression(ASTNodeType::ASSIGNMENT_EXPRESSION, ln, col),
          left(std::move(lhs)),
          right(std::move(rhs)) {}
};

// Binary Expression node
struct BinaryExpression : Expression {
    std::string op;
    std::unique_ptr<Expression> left;
    std::unique_ptr<Expression> right;

    BinaryExpression(const std::string& oper,
                     std::unique_ptr<Expression> lhs,
                     std::unique_ptr<Expression> rhs,
                     int ln = 0, int col = 0)
        : Expression(ASTNodeType::BINARY_EXPRESSION, ln, col),
          op(oper),
          left(std::move(lhs)),
          right(std::move(rhs)) {}
};

// Unary Expression node
struct UnaryExpression : Expression {
    std::string op;
    std::unique_ptr<Expression> operand;

    UnaryExpression(const std::string& oper,
                    std::unique_ptr<Expression> expr,
                    int ln = 0, int col = 0)
        : Expression(ASTNodeType::UNARY_EXPRESSION, ln, col),
          op(oper),
          operand(std::move(expr)) {}
};

// Literal node
struct Literal : Expression {
    std::string value; // For simplicity, store all literals as strings

    Literal(const std::string& val, int ln = 0, int col = 0)
        : Expression(ASTNodeType::LITERAL, ln, col),
          value(val) {}
};

// Identifier node
struct Identifier : Expression {
    std::string name;

    Identifier(const std::string& nm, int ln = 0, int col = 0)
        : Expression(ASTNodeType::IDENTIFIER, ln, col),
          name(nm) {}
};

} // namespace Parser

#endif // ASTNODES_H
