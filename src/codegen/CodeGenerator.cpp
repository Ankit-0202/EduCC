#include "codegen/CodeGenerator.h"
#include <sstream>
#include <iostream>

namespace Codegen {

// Constructor
CodeGenerator::CodeGenerator() {}

// Destructor
CodeGenerator::~CodeGenerator() {}

// Helper: Emit a line of assembly code
void CodeGenerator::emit(const std::string& code) {
    assemblyCode += code + "\n";
}

// Helper: Get or assign a register for a temporary
std::string CodeGenerator::getRegister(const std::string& temp) {
    // Simplified register allocation: assign a unique register to each temporary
    // In a real compiler, a more sophisticated allocation strategy would be used
    if (registerMap.find(temp) != registerMap.end()) {
        return registerMap[temp];
    }
    else {
        // Assign a new register (using r10-r15 for temporaries)
        static int regCounter = 10;
        std::string reg = "r" + std::to_string(regCounter++);
        registerMap[temp] = reg;
        return reg;
    }
}

// Generate assembly from IR
std::string CodeGenerator::generateAssembly(const IR::IntermediateRepresentation& ir) {
    // Emit global directives
    emit(".globl main");
    emit("main:");

    for (const auto& instr : ir.getInstructions()) {
        generateInstruction(instr);
    }

    return assemblyCode;
}

// Translate individual IR instruction to assembly
void CodeGenerator::generateInstruction(const IR::IRInstruction& instr) {
    std::stringstream ss;

    switch (instr.type) {
        case IR::IRInstructionType::LOAD_CONST: {
            std::string reg = getRegister(instr.result);
            ss << "mov $" << instr.arg1 << ", %" << reg;
            emit(ss.str());
            break;
        }
        case IR::IRInstructionType::LOAD_VAR: {
            std::string reg = getRegister(instr.result);
            ss << "mov " << instr.arg1 << "(%rip), %" << reg;
            emit(ss.str());
            break;
        }
        case IR::IRInstructionType::STORE_VAR: {
            std::string reg = getRegister(instr.arg1);
            ss << "mov %" << reg << ", " << instr.result << "(%rip)";
            emit(ss.str());
            break;
        }
        case IR::IRInstructionType::ADD: {
            std::string src1 = getRegister(instr.arg1);
            std::string src2 = getRegister(instr.arg2);
            std::string dest = getRegister(instr.result);
            ss << "add %" << src2 << ", %" << src1;
            emit(ss.str());
            ss.str(""); // Clear the stream
            ss << "mov %" << src1 << ", %" << dest;
            emit(ss.str());
            break;
        }
        case IR::IRInstructionType::SUB: {
            std::string src1 = getRegister(instr.arg1);
            std::string src2 = getRegister(instr.arg2);
            std::string dest = getRegister(instr.result);
            ss << "sub %" << src2 << ", %" << src1;
            emit(ss.str());
            ss.str("");
            ss << "mov %" << src1 << ", %" << dest;
            emit(ss.str());
            break;
        }
        case IR::IRInstructionType::MUL: {
            // x86-64 has limited support for multiplication, using imul
            std::string src1 = getRegister(instr.arg1);
            std::string src2 = getRegister(instr.arg2);
            std::string dest = getRegister(instr.result);
            ss << "imul %" << src2 << ", %" << src1;
            emit(ss.str());
            ss.str("");
            ss << "mov %" << src1 << ", %" << dest;
            emit(ss.str());
            break;
        }
        case IR::IRInstructionType::DIV: {
            // x86-64 division requires specific handling
            std::string src = getRegister(instr.arg1);
            std::string dest = getRegister(instr.result);
            ss << "mov %" << src << ", %rax";
            emit(ss.str());
            ss.str("");
            ss << "cqo"; // Sign extend %rax into %rdx:%rax
            emit(ss.str());
            ss.str("");
            ss << "idiv " << instr.arg2 << "(%rip)";
            emit(ss.str());
            ss.str("");
            ss << "mov %rax, %" << dest;
            emit(ss.str());
            break;
        }
        case IR::IRInstructionType::JMP: {
            ss << "jmp " << instr.result;
            emit(ss.str());
            break;
        }
        case IR::IRInstructionType::CJMP: {
            std::string condReg = getRegister(instr.arg1);
            ss << "cmp $0, %" << condReg;
            emit(ss.str());
            ss.str("");
            ss << "je " << instr.result;
            emit(ss.str());
            break;
        }
        case IR::IRInstructionType::CALL: {
            ss << "call " << instr.arg1;
            emit(ss.str());
            break;
        }
        case IR::IRInstructionType::RET: {
            if (!instr.arg1.empty()) {
                std::string retReg = getRegister(instr.arg1);
                ss << "mov %" << retReg << ", %rax";
                emit(ss.str());
            }
            ss.str("");
            ss << "ret";
            emit(ss.str());
            break;
        }
        case IR::IRInstructionType::LABEL: {
            ss << instr.arg1 << ":";
            emit(ss.str());
            break;
        }
        default:
            // Unsupported instruction
            break;
    }
}

} // namespace Codegen
