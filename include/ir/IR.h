#ifndef IR_H
#define IR_H

#include <string>
#include <vector>
#include <unordered_map>

namespace IR {

// Enumeration for IR instruction types
enum class IRInstructionType {
    LOAD_CONST,    // Load a constant value
    LOAD_VAR,      // Load a variable
    STORE_VAR,     // Store a value into a variable
    ADD,           // Addition
    SUB,           // Subtraction
    MUL,           // Multiplication
    DIV,           // Division
    JMP,           // Unconditional jump
    CJMP,          // Conditional jump
    CALL,          // Function call
    RET,           // Return from function
    LABEL,         // Label for jump targets
    // Add more instruction types as needed
};

// Structure representing an IR instruction
struct IRInstruction {
    IRInstructionType type;
    std::string arg1;
    std::string arg2;
    std::string result;

    IRInstruction(IRInstructionType instrType,
                  const std::string& argument1 = "",
                  const std::string& argument2 = "",
                  const std::string& res = "")
        : type(instrType), arg1(argument1), arg2(argument2), result(res) {}
};

// Intermediate Representation class
class IntermediateRepresentation {
public:
    IntermediateRepresentation();
    ~IntermediateRepresentation();

    // Add an instruction to the IR
    void addInstruction(const IRInstruction& instr);

    // Retrieve all instructions (const access)
    const std::vector<IRInstruction>& getInstructions() const;

    // Provide mutable access to instructions
    std::vector<IRInstruction>& getMutableInstructions();

    // Generate unique temporary variable names
    std::string newTemp();

    // Generate unique labels
    std::string newLabel();

    // Move assignment operator
    IntermediateRepresentation& operator=(IntermediateRepresentation&& other) noexcept;

private:
    std::vector<IRInstruction> instructions;
    int tempCount;
    int labelCount;
};

} // namespace IR

#endif // IR_H
