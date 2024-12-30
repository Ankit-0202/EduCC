#include "lexer/Lexer.h"
#include "parser/Parser.h"
#include "semantic/Semantic.h"
#include "codegen/CodeGen.h"
#include <iostream>
#include <fstream>

int main(int argc, char** argv)
{
    if (argc < 3) {
        std::cerr << "Usage: " << argv[0] << " <input.c> <output.ll>\n";
        return 1;
    }

    std::string inputFile = argv[1];
    std::string outputIR  = argv[2];

    // 1) Read the input file
    std::ifstream inFile(inputFile);
    if (!inFile) {
        std::cerr << "Cannot open input file: " << inputFile << std::endl;
        return 1;
    }
    std::string source((std::istreambuf_iterator<char>(inFile)),
                       std::istreambuf_iterator<char>());

    try {
        // 2) Lex
        Lexer lexer(source);
        std::vector<Token> tokens;
        while (!lexer.isAtEnd()) {
            tokens.push_back(lexer.getNextToken());
        }

        // 3) Parse
        Parser parser(tokens);
        auto astRoot = parser.parseTranslationUnit();

        // 4) Semantic Analysis
        SemanticAnalyzer sem(*astRoot);
        sem.analyze();

        // 5) Code Generation
        CodeGenerator codeGen("EduCC_Module");
        codeGen.generateCode(*astRoot, outputIR);

        std::cout << "Compilation successful! IR written to " << outputIR << std::endl;
    }
    catch (const std::exception &ex) {
        std::cerr << "Error: " << ex.what() << std::endl;
        return 1;
    }

    return 0;
}
