#include "semantic/SemanticAnalyzer.h"
#include <iostream>
#include <stdexcept>

namespace Semantic {

// Constructor
SemanticAnalyzer::SemanticAnalyzer() {}

// Destructor
SemanticAnalyzer::~SemanticAnalyzer() {}

// Entry point for semantic analysis
void SemanticAnalyzer::analyze(std::unique_ptr<Parser::ASTNode>& ast) {
    visit(ast.get());
}

// Generic visit function that dispatches based on node type
void SemanticAnalyzer::visit(Parser::ASTNode* node) {
    if (!node) return;

    switch (node->type) {
        case Parser::ASTNodeType::PROGRAM:
            visitProgram(static_cast<Parser::Program*>(node));
            break;
        case Parser::ASTNodeType::FUNCTION_DEFINITION:
            visitFunctionDefinition(static_cast<Parser::FunctionDefinition*>(node));
            break;
        case Parser::ASTNodeType::VARIABLE_DECLARATION:
            visitVariableDeclaration(static_cast<Parser::VariableDeclaration*>(node));
            break;
        case Parser::ASTNodeType::BLOCK:
            visitBlock(static_cast<Parser::Block*>(node));
            break;
        case Parser::ASTNodeType::IF_STATEMENT:
            visitIfStatement(static_cast<Parser::IfStatement*>(node));
            break;
        case Parser::ASTNodeType::FOR_STATEMENT:
            visitForStatement(static_cast<Parser::ForStatement*>(node));
            break;
        case Parser::ASTNodeType::WHILE_STATEMENT:
            visitWhileStatement(static_cast<Parser::WhileStatement*>(node));
            break;
        case Parser::ASTNodeType::RETURN_STATEMENT:
            visitReturnStatement(static_cast<Parser::ReturnStatement*>(node));
            break;
        case Parser::ASTNodeType::EXPRESSION_STATEMENT:
            visitExpressionStatement(static_cast<Parser::ExpressionStatement*>(node));
            break;
        case Parser::ASTNodeType::ASSIGNMENT_EXPRESSION:
            visitAssignmentExpression(static_cast<Parser::AssignmentExpression*>(node));
            break;
        case Parser::ASTNodeType::BINARY_EXPRESSION:
            visitBinaryExpression(static_cast<Parser::BinaryExpression*>(node));
            break;
        case Parser::ASTNodeType::UNARY_EXPRESSION:
            visitUnaryExpression(static_cast<Parser::UnaryExpression*>(node));
            break;
        case Parser::ASTNodeType::LITERAL:
            visitLiteral(static_cast<Parser::Literal*>(node));
            break;
        case Parser::ASTNodeType::IDENTIFIER:
            visitIdentifier(static_cast<Parser::Identifier*>(node));
            break;
        default:
            // Unsupported node type
            break;
    }
}

// Visit Program node
void SemanticAnalyzer::visitProgram(Parser::Program* program) {
    for (auto& decl : program->declarations) {
        visit(decl.get());
    }
}

// Visit Function Definition node
void SemanticAnalyzer::visitFunctionDefinition(Parser::FunctionDefinition* funcDef) {
    // Check if function is already declared
    auto existing = symbolTable.lookup(funcDef->name);
    if (existing) {
        reportError("Function '" + funcDef->name + "' already declared.", funcDef->line, funcDef->column);
        return;
    }

    // Declare the function in the symbol table
    bool success = symbolTable.declare(funcDef->name, SymbolKind::FUNCTION, funcDef->returnType);
    if (!success) {
        reportError("Failed to declare function '" + funcDef->name + "'.", funcDef->line, funcDef->column);
    }

    // Enter function scope
    symbolTable.enterScope();

    // Declare function parameters
    for (const auto& param : funcDef->parameters) {
        bool paramSuccess = symbolTable.declare(param.second, SymbolKind::VARIABLE, param.first);
        if (!paramSuccess) {
            reportError("Parameter '" + param.second + "' already declared in function '" + funcDef->name + "'.", funcDef->line, funcDef->column);
        }
    }

    // Visit function body
    visit(funcDef->body.get());

    // Exit function scope
    symbolTable.exitScope();
}

// Visit Variable Declaration node
void SemanticAnalyzer::visitVariableDeclaration(Parser::VariableDeclaration* varDecl) {
    // Check if variable is already declared in the current scope
    bool success = symbolTable.declare(varDecl->name, SymbolKind::VARIABLE, varDecl->type);
    if (!success) {
        reportError("Variable '" + varDecl->name + "' already declared in this scope.", varDecl->line, varDecl->column);
    }

    // If there's an initializer, visit the expression
    if (varDecl->initializer) {
        visit(varDecl->initializer.get());
        // Additional type checking between variable type and initializer expression type can be performed here
    }
}

// Visit Block node
void SemanticAnalyzer::visitBlock(Parser::Block* block) {
    // Enter new scope
    symbolTable.enterScope();

    // Visit all statements in the block
    for (auto& stmt : block->statements) {
        visit(stmt.get());
    }

    // Exit scope
    symbolTable.exitScope();
}

// Visit If Statement node
void SemanticAnalyzer::visitIfStatement(Parser::IfStatement* ifStmt) {
    // Visit condition expression
    visit(ifStmt->condition.get());

    // Visit then branch
    visit(ifStmt->thenBranch.get());

    // Visit else branch if it exists
    if (ifStmt->elseBranch) {
        visit(ifStmt->elseBranch.get());
    }
}

// Visit For Statement node
void SemanticAnalyzer::visitForStatement(Parser::ForStatement* forStmt) {
    // Enter new scope for the loop
    symbolTable.enterScope();

    // Visit initialization
    if (forStmt->init) {
        visit(forStmt->init.get());
    }

    // Visit condition
    if (forStmt->condition) {
        visit(forStmt->condition.get());
    }

    // Visit increment
    if (forStmt->increment) {
        visit(forStmt->increment.get());
    }

    // Visit loop body
    visit(forStmt->body.get());

    // Exit loop scope
    symbolTable.exitScope();
}

// Visit While Statement node
void SemanticAnalyzer::visitWhileStatement(Parser::WhileStatement* whileStmt) {
    // Visit condition
    visit(whileStmt->condition.get());

    // Visit loop body
    visit(whileStmt->body.get());
}

// Visit Return Statement node
void SemanticAnalyzer::visitReturnStatement(Parser::ReturnStatement* retStmt) {
    if (retStmt->expression) {
        visit(retStmt->expression.get());
        // Additional type checking between function return type and expression type can be performed here
    }
}

// Visit Expression Statement node
void SemanticAnalyzer::visitExpressionStatement(Parser::ExpressionStatement* exprStmt) {
    if (exprStmt->expression) {
        visit(exprStmt->expression.get());
    }
}

// Visit Assignment Expression node
void SemanticAnalyzer::visitAssignmentExpression(Parser::AssignmentExpression* assignExpr) {
    // Visit left and right expressions
    visit(assignExpr->left.get());
    visit(assignExpr->right.get());

    // Ensure that left is a valid l-value (Identifier)
    auto identifier = dynamic_cast<Parser::Identifier*>(assignExpr->left.get());
    if (!identifier) {
        reportError("Left-hand side of assignment must be an identifier.", assignExpr->line, assignExpr->column);
    }

    // Check if the identifier has been declared
    auto symbol = symbolTable.lookup(identifier->name);
    if (!symbol) {
        reportError("Undeclared variable '" + identifier->name + "'.", assignExpr->line, assignExpr->column);
    }
    else if (symbol->kind != SymbolKind::VARIABLE) {
        reportError("'" + identifier->name + "' is not a variable.", assignExpr->line, assignExpr->column);
    }

    // Additional type checking between variable type and expression type can be performed here
}

// Visit Binary Expression node
void SemanticAnalyzer::visitBinaryExpression(Parser::BinaryExpression* binExpr) {
    // Visit left and right expressions
    visit(binExpr->left.get());
    visit(binExpr->right.get());

    // Type checking based on operator can be implemented here
    // For simplicity, we'll assume all operations are valid
}

// Visit Unary Expression node
void SemanticAnalyzer::visitUnaryExpression(Parser::UnaryExpression* unaryExpr) {
    // Visit operand
    visit(unaryExpr->operand.get());

    // Type checking based on operator can be implemented here
    // For simplicity, we'll assume all operations are valid
}

// Visit Literal node
void SemanticAnalyzer::visitLiteral(Parser::Literal* literal) {
    // Literals are inherently valid
    // Optionally, set the type based on the literal value
}

// Visit Identifier node
void SemanticAnalyzer::visitIdentifier(Parser::Identifier* identifier) {
    // Check if the identifier has been declared
    auto symbol = symbolTable.lookup(identifier->name);
    if (!symbol) {
        reportError("Undeclared identifier '" + identifier->name + "'.", identifier->line, identifier->column);
    }
    else if (symbol->kind != SymbolKind::VARIABLE && symbol->kind != SymbolKind::FUNCTION) {
        reportError("Invalid usage of identifier '" + identifier->name + "'.", identifier->line, identifier->column);
    }

    // Additional type information can be retrieved from the symbol if needed
}

// Error reporting
void SemanticAnalyzer::reportError(const std::string& message, int line, int column) {
    std::cerr << "[Semantic Error] " << message << " at line " << line << ", column " << column << "\n";
    throw std::runtime_error("Semantic analysis failed: " + message);
}

} // namespace Semantic
