#include "ir/IR.h"
#include <utility> // for std::move

namespace IR {

// Constructor
IntermediateRepresentation::IntermediateRepresentation()
    : tempCount(0), labelCount(0) {}

// Destructor
IntermediateRepresentation::~IntermediateRepresentation() {}

// Add an instruction to the IR
void IntermediateRepresentation::addInstruction(const IRInstruction &instr) {
  instructions.emplace_back(instr);
}

// Retrieve all instructions (const access)
const std::vector<IRInstruction> &
IntermediateRepresentation::getInstructions() const {
  return instructions;
}

// Provide mutable access to instructions
std::vector<IRInstruction> &
IntermediateRepresentation::getMutableInstructions() {
  return instructions;
}

// Generate unique temporary variable names
std::string IntermediateRepresentation::newTemp() {
  return "t" + std::to_string(++tempCount);
}

// Generate unique labels
std::string IntermediateRepresentation::newLabel() {
  return "L" + std::to_string(++labelCount);
}

// Move assignment operator
IntermediateRepresentation &IntermediateRepresentation::operator=(
    IntermediateRepresentation &&other) noexcept {
  if (this != &other) {
    instructions = std::move(other.instructions);
    tempCount = other.tempCount;
    labelCount = other.labelCount;
  }
  return *this;
}

} // namespace IR
