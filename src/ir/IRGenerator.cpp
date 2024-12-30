#include "ir/IRGenerator.h"
#include "parser/ASTNodes.h"
#include "semantic/SymbolTable.h"
#include "ir/IR.h"
#include <iostream>
#include <sstream>

namespace IR {

// Constructor
IRGenerator::IRGenerator() {}

// Destructor
IRGenerator::~IRGenerator() {}

// Generate IR from the AST
std::unique_ptr<IntermediateRepresentation> IRGenerator::generate(std::unique_ptr<Parser::ASTNode>& ast) {
    // Initialize IR as a local unique_ptr
    auto ir_local = std::make_unique<IntermediateRepresentation>();

    // Visit the AST root node to populate IR
    visit(ast.get(), ir_local);

    // Return the IR by moving ownership
    return ir_local;
}

// Generic visit function that dispatches based on node type
std::string IRGenerator::visit(Parser::ASTNode* node, std::unique_ptr<IntermediateRepresentation>& ir) {
    if (!node) return "";

    switch (node->type) {
        case Parser::ASTNodeType::ASSIGNMENT_EXPRESSION:
            return visitAssignmentExpression(static_cast<Parser::AssignmentExpression*>(node), ir);
        case Parser::ASTNodeType::BINARY_EXPRESSION:
            return visitBinaryExpression(static_cast<Parser::BinaryExpression*>(node), ir);
        case Parser::ASTNodeType::UNARY_EXPRESSION:
            return visitUnaryExpression(static_cast<Parser::UnaryExpression*>(node), ir);
        case Parser::ASTNodeType::LITERAL:
            return visitLiteral(static_cast<Parser::Literal*>(node), ir);
        case Parser::ASTNodeType::IDENTIFIER:
            return visitIdentifier(static_cast<Parser::Identifier*>(node), ir);
        default:
            std::cerr << "IRGenerator: Unsupported expression node type.\n";
            return "";
    }
}

// Visit Program node
void IRGenerator::visitProgram(Parser::Program* program, std::unique_ptr<IntermediateRepresentation>& ir) {
    for (auto& decl : program->declarations) {
        visit(decl.get(), ir);
    }
}

// Visit Function Definition node
void IRGenerator::visitFunctionDefinition(Parser::FunctionDefinition* funcDef, std::unique_ptr<IntermediateRepresentation>& ir) {
    // Add function label
    std::string funcLabel = funcDef->name;
    ir->addInstruction(IRInstruction(IRInstructionType::LABEL, funcLabel));

    // Visit function body
    visit(funcDef->body.get(), ir);

    // Add return instruction (assuming functions return a value)
    ir->addInstruction(IRInstruction(IRInstructionType::RET, "", "", ""));
}

// Visit Variable Declaration node
void IRGenerator::visitVariableDeclaration(Parser::VariableDeclaration* varDecl, std::unique_ptr<IntermediateRepresentation>& ir) {
    if (varDecl->initializer) {
        // Visit initializer expression to compute its value
        std::string initVal = visit(varDecl->initializer.get(), ir);

        // Store the computed value into the variable
        ir->addInstruction(IRInstruction(IRInstructionType::STORE_VAR, initVal, "", varDecl->name));
    } else {
        // Initialize variable to default value (e.g., 0)
        std::string temp = ir->newTemp();
        ir->addInstruction(IRInstruction(IRInstructionType::LOAD_CONST, "0", "", temp));
        ir->addInstruction(IRInstruction(IRInstructionType::STORE_VAR, temp, "", varDecl->name));
    }
}

// Visit Block node
void IRGenerator::visitBlock(Parser::Block* block, std::unique_ptr<IntermediateRepresentation>& ir) {
    for (auto& stmt : block->statements) {
        visit(stmt.get(), ir);
    }
}

// Visit If Statement node
void IRGenerator::visitIfStatement(Parser::IfStatement* ifStmt, std::unique_ptr<IntermediateRepresentation>& ir) {
    // Visit condition expression
    std::string cond = visit(ifStmt->condition.get(), ir);

    // Generate labels
    std::string labelTrue = ir->newLabel();
    std::string labelEnd = ir->newLabel();

    // Conditional jump based on condition
    ir->addInstruction(IRInstruction(IRInstructionType::CJMP, cond, "", labelTrue));

    // Else branch
    if (ifStmt->elseBranch) {
        visit(ifStmt->elseBranch.get(), ir);
    }
    ir->addInstruction(IRInstruction(IRInstructionType::JMP, "", "", labelEnd));

    // Then branch label
    ir->addInstruction(IRInstruction(IRInstructionType::LABEL, labelTrue));
    visit(ifStmt->thenBranch.get(), ir);

    // End label
    ir->addInstruction(IRInstruction(IRInstructionType::LABEL, labelEnd));
}

// Visit For Statement node
void IRGenerator::visitForStatement(Parser::ForStatement* forStmt, std::unique_ptr<IntermediateRepresentation>& ir) {
    // Generate labels
    std::string labelCondition = ir->newLabel();
    std::string labelEnd = ir->newLabel();

    // Initialization
    if (forStmt->init) {
        visit(forStmt->init.get(), ir);
    }

    // Jump to condition check
    ir->addInstruction(IRInstruction(IRInstructionType::JMP, "", "", labelCondition));

    // Loop body
    visit(forStmt->body.get(), ir);

    // Increment expression
    if (forStmt->increment) {
        visit(forStmt->increment.get(), ir);
    }

    // Condition check label
    ir->addInstruction(IRInstruction(IRInstructionType::LABEL, labelCondition));

    // Visit condition expression
    if (forStmt->condition) {
        std::string cond = visit(forStmt->condition.get(), ir);
        ir->addInstruction(IRInstruction(IRInstructionType::CJMP, cond, "", "loop_body"));
    } else {
        // If no condition, create an infinite loop
        ir->addInstruction(IRInstruction(IRInstructionType::JMP, "", "", "loop_body"));
    }

    // End label
    ir->addInstruction(IRInstruction(IRInstructionType::LABEL, labelEnd));
}

// Visit While Statement node
void IRGenerator::visitWhileStatement(Parser::WhileStatement* whileStmt, std::unique_ptr<IntermediateRepresentation>& ir) {
    // Generate labels
    std::string labelCondition = ir->newLabel();
    std::string labelBody = ir->newLabel();
    std::string labelEnd = ir->newLabel();

    // Jump to condition check
    ir->addInstruction(IRInstruction(IRInstructionType::JMP, "", "", labelCondition));

    // Loop body label
    ir->addInstruction(IRInstruction(IRInstructionType::LABEL, labelBody));

    // Loop body
    visit(whileStmt->body.get(), ir);

    // Condition check label
    ir->addInstruction(IRInstruction(IRInstructionType::LABEL, labelCondition));

    // Visit condition expression
    std::string cond = visit(whileStmt->condition.get(), ir);
    ir->addInstruction(IRInstruction(IRInstructionType::CJMP, cond, "", labelBody));

    // End label
    ir->addInstruction(IRInstruction(IRInstructionType::LABEL, labelEnd));
}

// Visit Return Statement node
void IRGenerator::visitReturnStatement(Parser::ReturnStatement* retStmt, std::unique_ptr<IntermediateRepresentation>& ir) {
    if (retStmt->expression) {
        // Visit return expression to compute its value
        std::string retVal = visit(retStmt->expression.get(), ir);
        // Move the return value into a predefined return register (e.g., rax)
        ir->addInstruction(IRInstruction(IRInstructionType::LOAD_CONST, retVal, "", "rax"));
    } else {
        // No return expression; default to 0
        ir->addInstruction(IRInstruction(IRInstructionType::LOAD_CONST, "0", "", "rax"));
    }
    // Add return instruction
    ir->addInstruction(IRInstruction(IRInstructionType::RET, "", "", ""));
}

// Visit Expression Statement node
void IRGenerator::visitExpressionStatement(Parser::ExpressionStatement* exprStmt, std::unique_ptr<IntermediateRepresentation>& ir) {
    if (exprStmt->expression) {
        // Visit the expression to compute its value
        visit(exprStmt->expression.get(), ir);
        // The result is not used; no need to store
    }
}

// Visit Assignment Expression node
std::string IRGenerator::visitAssignmentExpression(Parser::AssignmentExpression* assignExpr, std::unique_ptr<IntermediateRepresentation>& ir) {
    // Ensure left side is an identifier
    auto identifier = dynamic_cast<Parser::Identifier*>(assignExpr->left.get());
    if (!identifier) {
        std::cerr << "IRGenerator: Left-hand side of assignment is not an identifier.\n";
        return "";
    }
    std::string varName = identifier->name;

    // Visit right-hand side expression to compute its value
    std::string rhs = visit(assignExpr->right.get(), ir);

    // Store the computed value into the variable
    ir->addInstruction(IRInstruction(IRInstructionType::STORE_VAR, rhs, "", varName));

    return varName;
}

// Visit Binary Expression node
std::string IRGenerator::visitBinaryExpression(Parser::BinaryExpression* binExpr, std::unique_ptr<IntermediateRepresentation>& ir) {
    // Visit left and right expressions to compute their values
    std::string left = visit(binExpr->left.get(), ir);
    std::string right = visit(binExpr->right.get(), ir);

    // Generate a temporary to hold the result
    std::string temp = ir->newTemp();

    // Map the operator to the corresponding IR instruction type
    IRInstructionType instrType;
    if (binExpr->op == "+") {
        instrType = IRInstructionType::ADD;
    } else if (binExpr->op == "-") {
        instrType = IRInstructionType::SUB;
    } else if (binExpr->op == "*") {
        instrType = IRInstructionType::MUL;
    } else if (binExpr->op == "/") {
        instrType = IRInstructionType::DIV;
    } else if (binExpr->op == "<") {
        // Implement less than as a comparison (assuming SUB and then CMP)
        instrType = IRInstructionType::SUB;
        ir->addInstruction(IRInstruction(instrType, left, right, temp));
        // Additional comparison handling can be implemented here
        return temp; // Placeholder
    } else {
        std::cerr << "IRGenerator: Unsupported binary operator '" << binExpr->op << "'.\n";
        return "";
    }

    // Add the binary operation instruction
    ir->addInstruction(IRInstruction(instrType, left, right, temp));

    return temp;
}

// Visit Unary Expression node
std::string IRGenerator::visitUnaryExpression(Parser::UnaryExpression* unaryExpr, std::unique_ptr<IntermediateRepresentation>& ir) {
    // Visit the operand to compute its value
    std::string operand = visit(unaryExpr->operand.get(), ir);

    // Generate a temporary to hold the result
    std::string temp = ir->newTemp();

    // Map the operator to the corresponding IR instruction type
    if (unaryExpr->op == "-") {
        // Implement unary minus as 0 - operand
        ir->addInstruction(IRInstruction(IRInstructionType::SUB, "0", operand, temp));
    } else if (unaryExpr->op == "!") {
        // Implement logical NOT (assuming !x is equivalent to x == 0)
        std::string zeroTemp = ir->newTemp();
        ir->addInstruction(IRInstruction(IRInstructionType::LOAD_CONST, "0", "", zeroTemp));
        ir->addInstruction(IRInstruction(IRInstructionType::SUB, operand, zeroTemp, temp));
        // Further processing may be needed to handle logical values
    } else {
        std::cerr << "IRGenerator: Unsupported unary operator '" << unaryExpr->op << "'.\n";
        return "";
    }

    return temp;
}

// Visit Literal node
std::string IRGenerator::visitLiteral(Parser::Literal* literal, std::unique_ptr<IntermediateRepresentation>& ir) {
    // Return the literal value directly
    return literal->value;
}

// Visit Identifier node
std::string IRGenerator::visitIdentifier(Parser::Identifier* identifier, std::unique_ptr<IntermediateRepresentation>& ir) {
    // Generate a temporary to hold the loaded value
    std::string temp = ir->newTemp();
    ir->addInstruction(IRInstruction(IRInstructionType::LOAD_VAR, identifier->name, "", temp));
    return temp;
}

} // namespace IR
