#include "optimizer/Optimizer.h"
#include <regex>
#include <iostream>
#include <cstdlib>

namespace Optimizer {

// Constructor
Optimizer::Optimizer() {
    // Initialize the constants map if necessary
    // Example:
    // constants["PI"] = 3.14159;
}

Optimizer::~Optimizer() {}

// Entry point for optimization
void Optimizer::optimize(IR::IntermediateRepresentation& ir) {
    constantFolding(ir);
    peepholeOptimization(ir);
}

// Peephole Optimization: Simplify instruction patterns
void Optimizer::peepholeOptimization(IR::IntermediateRepresentation& ir) {
    std::vector<IR::IRInstruction> optimizedInstructions;
    const auto& instructions = ir.getInstructions();

    for (size_t i = 0; i < instructions.size(); ++i) {
        // Example: Remove redundant load/store
        if (i < instructions.size() - 1) {
            const auto& current = instructions[i];
            const auto& next = instructions[i + 1];

            // If current is LOAD_VAR into temp and next is STORE_VAR from the same temp to the same var, skip both
            if (current.type == IR::IRInstructionType::LOAD_VAR &&
                next.type == IR::IRInstructionType::STORE_VAR &&
                current.result == next.arg1 &&
                current.arg1 == next.result) {
                i += 1; // Skip the next instruction
                continue;
            }
        }

        // Otherwise, keep the instruction
        optimizedInstructions.emplace_back(instructions[i]);
    }

    // Replace IR instructions with optimized ones
    // Create a new IntermediateRepresentation with optimized instructions
    IR::IntermediateRepresentation newIR;
    for (const auto& instr : optimizedInstructions) {
        newIR.addInstruction(instr);
    }

    // Replace the original IR with the optimized IR
    ir = std::move(newIR);
}

// Constant Folding: Evaluate constant expressions at compile time
void Optimizer::constantFolding(IR::IntermediateRepresentation& ir) {
    for (auto& instr : ir.getMutableInstructions()) { // Use mutable access
        switch (instr.type) {
            case IR::IRInstructionType::LOAD_CONST: {
                double value = std::stod(instr.arg1);
                constants[instr.result] = value;
                break;
            }
            case IR::IRInstructionType::ADD:
            case IR::IRInstructionType::SUB:
            case IR::IRInstructionType::MUL:
            case IR::IRInstructionType::DIV: {
                double left = 0.0, right = 0.0;
                bool leftConst = isConstant(instr.arg1, left);
                bool rightConst = isConstant(instr.arg2, right);

                if (leftConst && rightConst) {
                    double result = 0.0;
                    if (instr.type == IR::IRInstructionType::ADD) {
                        result = left + right;
                    }
                    else if (instr.type == IR::IRInstructionType::SUB) {
                        result = left - right;
                    }
                    else if (instr.type == IR::IRInstructionType::MUL) {
                        result = left * right;
                    }
                    else if (instr.type == IR::IRInstructionType::DIV) {
                        if (right != 0.0) {
                            result = left / right;
                        }
                        else {
                            // Handle division by zero if necessary
                            std::cerr << "Optimizer: Division by zero detected.\n";
                            break;
                        }
                    }

                    // Replace the instruction with LOAD_CONST
                    instr.type = IR::IRInstructionType::LOAD_CONST;
                    instr.arg1 = std::to_string(result);
                    instr.arg2 = "";
                    // 'result' remains the same

                    // Update constants map
                    constants[instr.result] = result;
                }
                break;
            }
            default:
                break;
        }
    }
}

// Helper: Check if operand is a constant
bool Optimizer::isConstant(const std::string& operand, double& value) {
    if (constants.find(operand) != constants.end()) {
        value = constants[operand];
        return true;
    }

    // Attempt to parse as a numeric constant
    try {
        value = std::stod(operand);
        return true;
    }
    catch (...) {
        return false;
    }
}

// Helper: Check if operand is a temporary variable
bool Optimizer::isTemporary(const std::string& operand) {
    return std::regex_match(operand, std::regex("t\\d+"));
}

} // namespace Optimizer
