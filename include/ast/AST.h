/**
 * @file AST.h
 * @brief Declarations for Abstract Syntax Tree (AST) classes in EduCC.
 *
 * This file defines the class hierarchy for representing C programs
 * as abstract syntax trees. The parser will construct these nodes based
 * on the tokens from the lexer, and subsequent phases (semantic analysis,
 * code generation) will traverse this tree.
 */

#ifndef EDUCC_AST_H
#define EDUCC_AST_H

#include "EduCC.h"
#include <vector>
#include <string>
#include <memory>

/**
 * @enum ASTNodeKind
 * @brief Identifiers for different AST node subclasses.
 */
enum class ASTNodeKind
{
    TranslationUnit,  // The root for the entire file
    FunctionDecl,
    FunctionDef,
    VarDecl,
    CompoundStmt,
    IfStmt,
    WhileStmt,
    ForStmt,
    ReturnStmt,
    ExprStmt,
    // Expressions
    BinaryExpr,
    UnaryExpr,
    CallExpr,
    LiteralExpr,
    IdentifierExpr,
    // ... you can add more as needed
};

/**
 * @class ASTNode
 * @brief Base class for all AST nodes.
 */
class ASTNode
{
protected:
    ASTNodeKind m_kind;
    size_t m_line;
    size_t m_column;

public:
    ASTNode(ASTNodeKind kind, size_t line, size_t column)
        : m_kind(kind), m_line(line), m_column(column) {}

    virtual ~ASTNode() = default;

    ASTNodeKind kind() const { return m_kind; }
    size_t line() const { return m_line; }
    size_t column() const { return m_column; }

    /**
     * @brief Utility for debugging: returns a string describing this node.
     */
    virtual std::string toString() const = 0;
};

// ============================================================================
//  Declarations
// ============================================================================

/**
 * @class DeclNode
 * @brief Base class for declarations (e.g., variables, functions).
 */
class DeclNode : public ASTNode
{
public:
    DeclNode(ASTNodeKind kind, size_t line, size_t column)
        : ASTNode(kind, line, column)
    {}

    ~DeclNode() override = default;
};

/**
 * @class VarDeclNode
 * @brief Represents a variable declaration at the global or local scope.
 */
class VarDeclNode : public DeclNode
{
private:
    std::string m_type;   ///< e.g., "int", "float", "char"
    std::string m_name;   ///< variable name

public:
    VarDeclNode(const std::string &type, const std::string &name,
                size_t line, size_t column)
        : DeclNode(ASTNodeKind::VarDecl, line, column),
          m_type(type), m_name(name)
    {}

    const std::string &typeName() const { return m_type; }
    const std::string &varName() const  { return m_name; }

    std::string toString() const override
    {
        std::ostringstream oss;
        oss << "(VarDecl " << m_type << " " << m_name
            << " @[" << line() << "," << column() << "])";
        return oss.str();
    }
};

/**
 * @class FunctionDeclNode
 * @brief Represents a function declaration/prototype (without a body).
 */
class FunctionDeclNode : public DeclNode
{
private:
    std::string m_returnType;
    std::string m_name;
    std::vector<std::string> m_paramTypes;
    std::vector<std::string> m_paramNames;

public:
    FunctionDeclNode(const std::string &retType,
                     const std::string &name,
                     const std::vector<std::string> &paramTypes,
                     const std::vector<std::string> &paramNames,
                     size_t line, size_t column)
        : DeclNode(ASTNodeKind::FunctionDecl, line, column),
          m_returnType(retType), m_name(name),
          m_paramTypes(paramTypes), m_paramNames(paramNames)
    {}

    const std::string &returnType() const { return m_returnType; }
    const std::string &funcName() const   { return m_name; }

    size_t paramCount() const { return m_paramNames.size(); }
    const std::string &paramType(size_t i) const { return m_paramTypes[i]; }
    const std::string &paramName(size_t i) const { return m_paramNames[i]; }

    std::string toString() const override
    {
        std::ostringstream oss;
        oss << "(FuncDecl " << m_returnType << " " << m_name << "(";
        for (size_t i = 0; i < m_paramNames.size(); i++) {
            if (i > 0) oss << ", ";
            oss << m_paramTypes[i] << " " << m_paramNames[i];
        }
        oss << ") @[" << line() << "," << column() << "])";
        return oss.str();
    }
};

/**
 * @class FunctionDefNode
 * @brief Represents a full function definition (declaration + body).
 */
class FunctionDefNode : public DeclNode
{
private:
    std::unique_ptr<FunctionDeclNode> m_decl;  ///< The function declaration part
    // Body is a statement block (compound statement)
    // We'll define a CompoundStmtNode later.
    std::unique_ptr<ASTNode> m_body;

public:
    FunctionDefNode(std::unique_ptr<FunctionDeclNode> decl,
                    std::unique_ptr<ASTNode> body,
                    size_t line, size_t column)
        : DeclNode(ASTNodeKind::FunctionDef, line, column),
          m_decl(std::move(decl)), m_body(std::move(body))
    {}

    const FunctionDeclNode* decl() const { return m_decl.get(); }
    const ASTNode* body() const { return m_body.get(); }

    std::string toString() const override
    {
        std::ostringstream oss;
        oss << "(FuncDef " << m_decl->funcName()
            << " @[" << line() << "," << column() << "])";
        return oss.str();
    }
};

// ============================================================================
//  Statements
// ============================================================================

/**
 * @class StmtNode
 * @brief Base class for statements (e.g., if, while, return).
 */
class StmtNode : public ASTNode
{
public:
    StmtNode(ASTNodeKind kind, size_t line, size_t column)
        : ASTNode(kind, line, column)
    {}
    ~StmtNode() override = default;
};

/**
 * @class CompoundStmtNode
 * @brief A sequence of statements enclosed in braces { ... }.
 */
class CompoundStmtNode : public StmtNode
{
private:
    std::vector<std::unique_ptr<ASTNode>> m_items;

public:
    CompoundStmtNode(size_t line, size_t column)
        : StmtNode(ASTNodeKind::CompoundStmt, line, column)
    {}

    void addItem(std::unique_ptr<ASTNode> item)
    {
        m_items.push_back(std::move(item));
    }

    const std::vector<std::unique_ptr<ASTNode>> &items() const
    {
        return m_items;
    }

    std::string toString() const override
    {
        std::ostringstream oss;
        oss << "(CompoundStmt [";
        for (size_t i = 0; i < m_items.size(); i++) {
            if (i > 0) oss << ", ";
            oss << m_items[i]->toString();
        }
        oss << "] @[" << line() << "," << column() << "])";
        return oss.str();
    }
};

/**
 * @class IfStmtNode
 * @brief Represents an if(...) {...} [else {...}] statement.
 */
class IfStmtNode : public StmtNode
{
private:
    std::unique_ptr<ASTNode> m_condition;
    std::unique_ptr<ASTNode> m_thenBranch;
    std::unique_ptr<ASTNode> m_elseBranch; // may be null

public:
    IfStmtNode(std::unique_ptr<ASTNode> condition,
               std::unique_ptr<ASTNode> thenBranch,
               std::unique_ptr<ASTNode> elseBranch,
               size_t line, size_t column)
        : StmtNode(ASTNodeKind::IfStmt, line, column),
          m_condition(std::move(condition)),
          m_thenBranch(std::move(thenBranch)),
          m_elseBranch(std::move(elseBranch))
    {}

    const ASTNode* condition() const { return m_condition.get(); }
    const ASTNode* thenBranch() const { return m_thenBranch.get(); }
    const ASTNode* elseBranch() const { return m_elseBranch.get(); }

    std::string toString() const override
    {
        std::ostringstream oss;
        oss << "(IfStmt cond=" << (m_condition ? m_condition->toString() : "null")
            << " then=" << (m_thenBranch ? m_thenBranch->toString() : "null")
            << " else=" << (m_elseBranch ? m_elseBranch->toString() : "null")
            << " @[" << line() << "," << column() << "])";
        return oss.str();
    }
};

/**
 * @class WhileStmtNode
 * @brief Represents a while(...) {...} statement.
 */
class WhileStmtNode : public StmtNode
{
private:
    std::unique_ptr<ASTNode> m_condition;
    std::unique_ptr<ASTNode> m_body;

public:
    WhileStmtNode(std::unique_ptr<ASTNode> condition,
                  std::unique_ptr<ASTNode> body,
                  size_t line, size_t column)
        : StmtNode(ASTNodeKind::WhileStmt, line, column),
          m_condition(std::move(condition)),
          m_body(std::move(body))
    {}

    const ASTNode* condition() const { return m_condition.get(); }
    const ASTNode* body() const { return m_body.get(); }

    std::string toString() const override
    {
        std::ostringstream oss;
        oss << "(WhileStmt cond=" << (m_condition ? m_condition->toString() : "null")
            << " body=" << (m_body ? m_body->toString() : "null")
            << " @[" << line() << "," << column() << "])";
        return oss.str();
    }
};

/**
 * @class ForStmtNode
 * @brief Represents a for(...) {...} statement with optional init/cond/incr.
 */
class ForStmtNode : public StmtNode
{
private:
    std::unique_ptr<ASTNode> m_init;
    std::unique_ptr<ASTNode> m_condition;
    std::unique_ptr<ASTNode> m_increment;
    std::unique_ptr<ASTNode> m_body;

public:
    ForStmtNode(std::unique_ptr<ASTNode> init,
                std::unique_ptr<ASTNode> condition,
                std::unique_ptr<ASTNode> increment,
                std::unique_ptr<ASTNode> body,
                size_t line, size_t column)
        : StmtNode(ASTNodeKind::ForStmt, line, column),
          m_init(std::move(init)),
          m_condition(std::move(condition)),
          m_increment(std::move(increment)),
          m_body(std::move(body))
    {}

    const ASTNode* init() const      { return m_init.get(); }
    const ASTNode* condition() const { return m_condition.get(); }
    const ASTNode* increment() const { return m_increment.get(); }
    const ASTNode* body() const      { return m_body.get(); }

    std::string toString() const override
    {
        std::ostringstream oss;
        oss << "(ForStmt init=" << (m_init ? m_init->toString() : "null")
            << " cond=" << (m_condition ? m_condition->toString() : "null")
            << " incr=" << (m_increment ? m_increment->toString() : "null")
            << " body=" << (m_body ? m_body->toString() : "null")
            << " @[" << line() << "," << column() << "])";
        return oss.str();
    }
};

/**
 * @class ReturnStmtNode
 * @brief Represents a return statement (may have an expression).
 */
class ReturnStmtNode : public StmtNode
{
private:
    std::unique_ptr<ASTNode> m_expr;

public:
    ReturnStmtNode(std::unique_ptr<ASTNode> expr,
                   size_t line, size_t column)
        : StmtNode(ASTNodeKind::ReturnStmt, line, column),
          m_expr(std::move(expr))
    {}

    const ASTNode* expr() const { return m_expr.get(); }

    std::string toString() const override
    {
        std::ostringstream oss;
        oss << "(ReturnStmt expr=" 
            << (m_expr ? m_expr->toString() : "null")
            << " @[" << line() << "," << column() << "])";
        return oss.str();
    }
};

/**
 * @class ExprStmtNode
 * @brief A statement consisting of a single expression (e.g., "x++;").
 */
class ExprStmtNode : public StmtNode
{
private:
    std::unique_ptr<ASTNode> m_expr;

public:
    ExprStmtNode(std::unique_ptr<ASTNode> expr,
                 size_t line, size_t column)
        : StmtNode(ASTNodeKind::ExprStmt, line, column),
          m_expr(std::move(expr))
    {}

    const ASTNode* expr() const { return m_expr.get(); }

    std::string toString() const override
    {
        std::ostringstream oss;
        oss << "(ExprStmt expr=" 
            << (m_expr ? m_expr->toString() : "null")
            << " @[" << line() << "," << column() << "])";
        return oss.str();
    }
};

// ============================================================================
//  Expressions
// ============================================================================

/**
 * @class ExprNode
 * @brief Base class for expressions.
 */
class ExprNode : public ASTNode
{
public:
    ExprNode(ASTNodeKind kind, size_t line, size_t column)
        : ASTNode(kind, line, column) {}
    ~ExprNode() override = default;
};

/**
 * @class BinaryExprNode
 * @brief A binary expression, e.g., "left + right".
 */
class BinaryExprNode : public ExprNode
{
private:
    std::string m_op;  ///< string for operator, e.g. "+", "-", "=="
    std::unique_ptr<ASTNode> m_left;
    std::unique_ptr<ASTNode> m_right;

public:
    BinaryExprNode(const std::string &op,
                   std::unique_ptr<ASTNode> left,
                   std::unique_ptr<ASTNode> right,
                   size_t line, size_t column)
        : ExprNode(ASTNodeKind::BinaryExpr, line, column),
          m_op(op),
          m_left(std::move(left)),
          m_right(std::move(right))
    {}

    const std::string &op() const { return m_op; }
    const ASTNode* left() const   { return m_left.get(); }
    const ASTNode* right() const  { return m_right.get(); }

    std::string toString() const override
    {
        std::ostringstream oss;
        oss << "(BinaryExpr op=" << m_op
            << " left=" << (m_left ? m_left->toString() : "null")
            << " right=" << (m_right ? m_right->toString() : "null")
            << " @[" << line() << "," << column() << "])";
        return oss.str();
    }
};

/**
 * @class UnaryExprNode
 * @brief A unary expression, e.g., "++x" or "-y".
 */
class UnaryExprNode : public ExprNode
{
private:
    std::string m_op;
    std::unique_ptr<ASTNode> m_operand;

public:
    UnaryExprNode(const std::string &op,
                  std::unique_ptr<ASTNode> operand,
                  size_t line, size_t column)
        : ExprNode(ASTNodeKind::UnaryExpr, line, column),
          m_op(op),
          m_operand(std::move(operand))
    {}

    const std::string &op() const { return m_op; }
    const ASTNode* operand() const { return m_operand.get(); }

    std::string toString() const override
    {
        std::ostringstream oss;
        oss << "(UnaryExpr op=" << m_op
            << " operand=" << (m_operand ? m_operand->toString() : "null")
            << " @[" << line() << "," << column() << "])";
        return oss.str();
    }
};

/**
 * @class CallExprNode
 * @brief A function call, e.g., "foo(a, b, c)".
 */
class CallExprNode : public ExprNode
{
private:
    std::string m_callee; ///< function name
    std::vector<std::unique_ptr<ASTNode>> m_args;

public:
    CallExprNode(const std::string &callee,
                 std::vector<std::unique_ptr<ASTNode>> args,
                 size_t line, size_t column)
        : ExprNode(ASTNodeKind::CallExpr, line, column),
          m_callee(callee),
          m_args(std::move(args))
    {}

    const std::string &callee() const { return m_callee; }
    const std::vector<std::unique_ptr<ASTNode>> &args() const { return m_args; }

    std::string toString() const override
    {
        std::ostringstream oss;
        oss << "(CallExpr callee=" << m_callee << " args=[";
        for (size_t i = 0; i < m_args.size(); i++) {
            if (i > 0) oss << ", ";
            oss << (m_args[i] ? m_args[i]->toString() : "null");
        }
        oss << "] @[" << line() << "," << column() << "])";
        return oss.str();
    }
};

/**
 * @class LiteralExprNode
 * @brief Represents an integer, float, or string literal.
 */
class LiteralExprNode : public ExprNode
{
private:
    std::string m_value; ///< e.g. "42", "3.14", or "\"Hello\""

public:
    LiteralExprNode(const std::string &value,
                    size_t line, size_t column)
        : ExprNode(ASTNodeKind::LiteralExpr, line, column),
          m_value(value)
    {}

    const std::string &value() const { return m_value; }

    std::string toString() const override
    {
        std::ostringstream oss;
        oss << "(LiteralExpr value=" << m_value
            << " @[" << line() << "," << column() << "])";
        return oss.str();
    }
};

/**
 * @class IdentifierExprNode
 * @brief Represents a reference to a variable or function name in an expression.
 */
class IdentifierExprNode : public ExprNode
{
private:
    std::string m_name;

public:
    IdentifierExprNode(const std::string &name, size_t line, size_t column)
        : ExprNode(ASTNodeKind::IdentifierExpr, line, column),
          m_name(name)
    {}

    const std::string &name() const { return m_name; }

    std::string toString() const override
    {
        std::ostringstream oss;
        oss << "(IdentifierExpr name=" << m_name
            << " @[" << line() << "," << column() << "])";
        return oss.str();
    }
};

// ============================================================================
//  The Root of a C Translation Unit
// ============================================================================

/**
 * @class TranslationUnitNode
 * @brief Represents the entire C file (list of global declarations).
 */
class TranslationUnitNode : public ASTNode
{
private:
    std::vector<std::unique_ptr<ASTNode>> m_decls;

public:
    TranslationUnitNode(size_t line, size_t column)
        : ASTNode(ASTNodeKind::TranslationUnit, line, column)
    {}

    void addDeclaration(std::unique_ptr<ASTNode> decl)
    {
        m_decls.push_back(std::move(decl));
    }

    const std::vector<std::unique_ptr<ASTNode>> &declarations() const
    {
        return m_decls;
    }

    std::string toString() const override
    {
        std::ostringstream oss;
        oss << "(TranslationUnit [";
        for (size_t i = 0; i < m_decls.size(); i++) {
            if (i > 0) oss << ", ";
            oss << m_decls[i]->toString();
        }
        oss << "] @[" << line() << "," << column() << "])";
        return oss.str();
    }
};

#endif // EDUCC_AST_H
