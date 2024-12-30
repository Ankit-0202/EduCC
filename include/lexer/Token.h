#ifndef TOKEN_H
#define TOKEN_H

#include <string>

namespace Lexer {

enum class TokenType {
    // Keywords
    IF, ELSE, FOR, WHILE, RETURN, INT, CHAR, FLOAT, DOUBLE, VOID,
    // Operators
    PLUS, MINUS, MULTIPLY, DIVIDE, ASSIGN,
    EQUAL, NOT_EQUAL, LESS, GREATER, LESS_EQUAL, GREATER_EQUAL,
    // Delimiters
    SEMICOLON, COMMA, LPAREN, RPAREN, LBRACE, RBRACE,
    // Literals
    IDENTIFIER, INTEGER_LITERAL, FLOAT_LITERAL, CHAR_LITERAL, STRING_LITERAL,
    // End of File
    EOF_TOKEN,
    // Unknown
    UNKNOWN
};

struct Token {
    TokenType type;
    std::string lexeme;
    int line;
    int column;

    Token(TokenType type = TokenType::UNKNOWN, const std::string& lexeme = "", int line = 0, int column = 0)
        : type(type), lexeme(lexeme), line(line), column(column) {}
};

} // namespace Lexer

#endif // TOKEN_H
