#include "Compiler.h"
#include "lexer/Lexer.h"
#include "parser/Parser.h"
#include "semantic/SemanticAnalyzer.h"
#include "ir/IRGenerator.h"
#include "optimizer/Optimizer.h"
#include "codegen/CodeGenerator.h"
#include <fstream>
#include <sstream>
#include <iostream>

Compiler::Compiler() {}

Compiler::~Compiler() {}

// Helper function to read file content
std::string readFile(const std::string& path) {
    std::ifstream file(path);
    if (!file.is_open()) {
        throw std::runtime_error("Cannot open file: " + path);
    }
    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

bool Compiler::compile(const std::string& sourcePath, const std::string& outputPath) {
    try {
        // Read source code
        std::string sourceCode = readFile(sourcePath);

        // Lexical Analysis
        Lexer::Lexer lexer(sourceCode);
        std::vector<Lexer::Token> tokens = lexer.tokenize();

        // Parsing
        Parser::Parser parser(tokens);
        std::unique_ptr<Parser::ASTNode> ast = parser.parse();

        // Semantic Analysis
        Semantic::SemanticAnalyzer semanticAnalyzer;
        semanticAnalyzer.analyze(ast);

        // IR Generation
        IR::IRGenerator irGenerator;
        std::unique_ptr<IR::IntermediateRepresentation> ir = irGenerator.generate(ast);

        // Optimization
        Optimizer::Optimizer optimizer;
        optimizer.optimize(*ir);

        // Code Generation
        Codegen::CodeGenerator codeGen;
        std::string assembly = codeGen.generateAssembly(*ir);

        // Write assembly to output file
        std::ofstream outFile(outputPath);
        if (!outFile.is_open()) {
            throw std::runtime_error("Cannot write to output file: " + outputPath);
        }
        outFile << assembly;
        outFile.close();

        std::cout << "Compilation successful. Output written to " << outputPath << "\n";
        return true;
    }
    catch (const std::exception& ex) {
        std::cerr << "Compilation failed: " << ex.what() << "\n";
        return false;
    }
}
