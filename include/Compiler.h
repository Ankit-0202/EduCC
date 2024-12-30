#ifndef COMPILER_H
#define COMPILER_H

#include <string>

class Compiler {
public:
    Compiler();
    ~Compiler();

    // Compile a C99 source file to assembly
    bool compile(const std::string& sourcePath, const std::string& outputPath);

private:
    // Internal components
    // Lexer, Parser, SemanticAnalyzer, IRGenerator, Optimizer, CodeGenerator
};

#endif // COMPILER_H
