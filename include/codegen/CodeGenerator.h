#ifndef CODEGENERATOR_H
#define CODEGENERATOR_H

#include "../ir/IR.h"
#include <string>
#include <unordered_map>

namespace Codegen {

// Code Generator class
class CodeGenerator {
public:
    CodeGenerator();
    ~CodeGenerator();

    // Generate assembly code from IR
    std::string generateAssembly(const IR::IntermediateRepresentation& ir);

private:
    std::string assemblyCode;

    // Register allocation (simplified: using fixed registers for temporaries)
    std::unordered_map<std::string, std::string> registerMap;

    // Helper functions
    void emit(const std::string& code);
    void generateInstruction(const IR::IRInstruction& instr);
    std::string getRegister(const std::string& temp);
};

} // namespace Codegen

#endif // CODEGENERATOR_H
