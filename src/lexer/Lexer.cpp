#include "lexer/Lexer.h"
#include <cctype>
#include <unordered_map>

namespace Lexer {

// Map of keywords
const std::unordered_map<std::string, TokenType> keywords = {
    {"if", TokenType::IF},
    {"else", TokenType::ELSE},
    {"for", TokenType::FOR},
    {"while", TokenType::WHILE},
    {"return", TokenType::RETURN},
    {"int", TokenType::INT},
    {"char", TokenType::CHAR},
    {"float", TokenType::FLOAT},
    {"double", TokenType::DOUBLE},
    {"void", TokenType::VOID},
    // Add more keywords as needed
};

// Constructor
Lexer::Lexer(const std::string& source)
    : sourceCode(source), currentIndex(0), currentLine(1), currentColumn(1) {}

// Destructor
Lexer::~Lexer() {}

// Tokenize the entire source code
std::vector<Token> Lexer::tokenize() {
    std::vector<Token> tokens;
    while (!isAtEnd()) {
        skipWhitespace();
        if (isAtEnd()) break;

        char c = peek();
        if (std::isalpha(c) || c == '_') {
            tokens.push_back(identifier());
        } else if (std::isdigit(c)) {
            tokens.push_back(number());
        } else if (c == '"') {
            tokens.push_back(stringLiteral());
        } else if (c == '\'') {
            tokens.push_back(characterLiteral());
        } else {
            tokens.push_back(operatorOrDelimiter());
        }
    }
    tokens.emplace_back(TokenType::EOF_TOKEN, "", currentLine, currentColumn);
    return tokens;
}

// Peek the current character
char Lexer::peek() const {
    if (isAtEnd()) return '\0';
    return sourceCode[currentIndex];
}

// Get the next character and advance
char Lexer::getNextChar() {
    if (isAtEnd()) return '\0';
    char c = sourceCode[currentIndex++];
    if (c == '\n') {
        currentLine++;
        currentColumn = 1;
    } else {
        currentColumn++;
    }
    return c;
}

// Check if end of source code
bool Lexer::isAtEnd() const {
    return currentIndex >= sourceCode.length();
}

// Skip whitespace and comments
void Lexer::skipWhitespace() {
    while (!isAtEnd()) {
        char c = peek();
        if (std::isspace(c)) {
            getNextChar();
        } else if (c == '/') {
            // Possible comment
            if (currentIndex + 1 < sourceCode.length()) {
                char next = sourceCode[currentIndex + 1];
                if (next == '/') {
                    // Single-line comment
                    getNextChar(); // Consume '/'
                    getNextChar(); // Consume second '/'
                    while (!isAtEnd() && peek() != '\n') {
                        getNextChar();
                    }
                } else if (next == '*') {
                    // Multi-line comment
                    getNextChar(); // Consume '/'
                    getNextChar(); // Consume '*'
                    while (!isAtEnd()) {
                        if (peek() == '*' && currentIndex + 1 < sourceCode.length() && sourceCode[currentIndex + 1] == '/') {
                            getNextChar(); // Consume '*'
                            getNextChar(); // Consume '/'
                            break;
                        } else {
                            getNextChar();
                        }
                    }
                } else {
                    break;
                }
            } else {
                break;
            }
        } else {
            break;
        }
    }
}

// Handle identifiers and keywords
Token Lexer::identifier() {
    int startLine = currentLine;
    int startColumn = currentColumn;
    std::string lexeme;
    while (!isAtEnd() && (std::isalnum(peek()) || peek() == '_')) {
        lexeme += getNextChar();
    }
    auto it = keywords.find(lexeme);
    if (it != keywords.end()) {
        return Token(it->second, lexeme, startLine, startColumn);
    }
    return Token(TokenType::IDENTIFIER, lexeme, startLine, startColumn);
}

// Handle numbers (integers and floats)
Token Lexer::number() {
    int startLine = currentLine;
    int startColumn = currentColumn;
    std::string lexeme;
    bool isFloat = false;

    while (!isAtEnd() && std::isdigit(peek())) {
        lexeme += getNextChar();
    }

    if (!isAtEnd() && peek() == '.') {
        isFloat = true;
        lexeme += getNextChar();
        while (!isAtEnd() && std::isdigit(peek())) {
            lexeme += getNextChar();
        }
    }

    if (isFloat) {
        return Token(TokenType::FLOAT_LITERAL, lexeme, startLine, startColumn);
    }
    return Token(TokenType::INTEGER_LITERAL, lexeme, startLine, startColumn);
}

// Handle string literals
Token Lexer::stringLiteral() {
    int startLine = currentLine;
    int startColumn = currentColumn;
    std::string lexeme;
    getNextChar(); // Consume opening "

    while (!isAtEnd() && peek() != '"') {
        if (peek() == '\\') { // Handle escape sequences
            lexeme += getNextChar();
            if (!isAtEnd()) {
                lexeme += getNextChar();
            }
        } else {
            lexeme += getNextChar();
        }
    }

    getNextChar(); // Consume closing "
    return Token(TokenType::STRING_LITERAL, lexeme, startLine, startColumn);
}

// Handle character literals
Token Lexer::characterLiteral() {
    int startLine = currentLine;
    int startColumn = currentColumn;
    std::string lexeme;
    getNextChar(); // Consume opening '

    if (!isAtEnd()) {
        if (peek() == '\\') { // Escape sequence
            lexeme += getNextChar();
            if (!isAtEnd()) {
                lexeme += getNextChar();
            }
        } else {
            lexeme += getNextChar();
        }
    }

    getNextChar(); // Consume closing '
    return Token(TokenType::CHAR_LITERAL, lexeme, startLine, startColumn);
}

// Handle operators and delimiters
Token Lexer::operatorOrDelimiter() {
    int startLine = currentLine;
    int startColumn = currentColumn;
    char c = getNextChar();
    std::string lexeme(1, c);

    switch (c) {
        // Single-character tokens
        case '+': return Token(TokenType::PLUS, lexeme, startLine, startColumn);
        case '-': return Token(TokenType::MINUS, lexeme, startLine, startColumn);
        case '*': return Token(TokenType::MULTIPLY, lexeme, startLine, startColumn);
        case '/': return Token(TokenType::DIVIDE, lexeme, startLine, startColumn);
        case ';': return Token(TokenType::SEMICOLON, lexeme, startLine, startColumn);
        case ',': return Token(TokenType::COMMA, lexeme, startLine, startColumn);
        case '(': return Token(TokenType::LPAREN, lexeme, startLine, startColumn);
        case ')': return Token(TokenType::RPAREN, lexeme, startLine, startColumn);
        case '{': return Token(TokenType::LBRACE, lexeme, startLine, startColumn);
        case '}': return Token(TokenType::RBRACE, lexeme, startLine, startColumn);
        case '=':
            if (!isAtEnd() && peek() == '=') {
                getNextChar();
                return Token(TokenType::EQUAL, "==", startLine, startColumn);
            }
            return Token(TokenType::ASSIGN, lexeme, startLine, startColumn);
        case '!':
            if (!isAtEnd() && peek() == '=') {
                getNextChar();
                return Token(TokenType::NOT_EQUAL, "!=", startLine, startColumn);
            }
            break;
        case '<':
            if (!isAtEnd() && peek() == '=') {
                getNextChar();
                return Token(TokenType::LESS_EQUAL, "<=", startLine, startColumn);
            }
            return Token(TokenType::LESS, lexeme, startLine, startColumn);
        case '>':
            if (!isAtEnd() && peek() == '=') {
                getNextChar();
                return Token(TokenType::GREATER_EQUAL, ">=", startLine, startColumn);
            }
            return Token(TokenType::GREATER, lexeme, startLine, startColumn);
        default:
            break;
    }

    return Token(TokenType::UNKNOWN, lexeme, startLine, startColumn);
}

} // namespace Lexer
