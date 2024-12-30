#include "Compiler.h"
#include <iostream>

int main(int argc, char* argv[]) {
    if (argc != 3) {
        std::cerr << "Usage: c99compiler <source_file.c> <output_file.asm>\n";
        return 1;
    }

    std::string sourcePath = argv[1];
    std::string outputPath = argv[2];

    Compiler compiler;
    bool success = compiler.compile(sourcePath, outputPath);

    return success ? 0 : 1;
}
