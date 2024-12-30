#ifndef LEXER_H
#define LEXER_H

#include "lexer/Token.h"
#include <string>
#include <vector>

namespace Lexer {

class Lexer {
public:
    Lexer(const std::string& source);
    ~Lexer();

    std::vector<Token> tokenize();

private:
    std::string sourceCode;
    size_t currentIndex;
    int currentLine;
    int currentColumn;

    char peek() const;
    char getNextChar();
    void skipWhitespace();
    Token identifier();
    Token number();
    Token stringLiteral();
    Token characterLiteral();
    Token operatorOrDelimiter();
    bool isAtEnd() const;
};

} // namespace Lexer

#endif // LEXER_H
