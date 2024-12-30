/**
 * @file Lexer.cpp
 * @brief Implementation of the Lexer class for EduCC.
 *
 * This file defines all methods for scanning C source code into tokens
 * (identifiers, keywords, operators, literals, etc.). The resulting tokens
 * are stored in a vector, which is then consumed by the parser.
 */

#include "lexer/Lexer.h"
#include "lexer/Token.h"
#include <cctype>
#include <map>

///////////////////////////////////////////////////////////////////////////
//  Constructor & Public Methods
///////////////////////////////////////////////////////////////////////////

Lexer::Lexer(const std::string &source)
    : m_source(source),
      m_currentPos(0),
      m_line(1),
      m_column(1),
      m_tokenIndex(0)
{
    tokenize();
}

const Token &Lexer::peek()
{
    if (isAtEnd()) {
        throw std::runtime_error("Lexer::peek() called at EOF");
    }
    return m_tokens[m_tokenIndex];
}

Token Lexer::getNextToken()
{
    if (isAtEnd()) {
        throw std::runtime_error("Lexer::getNextToken() called at EOF");
    }
    return m_tokens[m_tokenIndex++];
}

bool Lexer::isAtEnd() const
{
    return (m_tokenIndex >= m_tokens.size());
}

///////////////////////////////////////////////////////////////////////////
//  Main Tokenization Flow
///////////////////////////////////////////////////////////////////////////

void Lexer::tokenize()
{
    // Loop until we've consumed the entire input.
    while (m_currentPos < m_source.size()) {
        // Record the start of this token (for lexeme extraction).
        scanToken();
    }

    // Append a special end-of-file token.
    addToken(TokenType::EOF_TOK);
}

void Lexer::scanToken()
{
    // Skip any whitespace (spaces, tabs, newlines).
    skipWhitespaceAndComments();

    // If we've reached the end after skipping whitespace, return.
    if (m_currentPos >= m_source.size()) {
        return;
    }

    // Record where the token starts (for capturing the lexeme).
    size_t tokenStart = m_currentPos;

    char c = advance();

    // 1) Check if it might be an identifier or keyword (starting with letter or underscore).
    if (isAlpha(c)) {
        // We'll keep consuming alphanumeric/underscore characters for the identifier.
        while (!isEnd() && isAlphaNum(peekChar())) {
            advance();
        }
        size_t length = m_currentPos - tokenStart;
        addToken(identifierType(m_source.substr(tokenStart, length)), tokenStart, length);
        return;
    }

    // 2) Check if it's a digit => numeric literal (integer or float).
    if (std::isdigit(static_cast<unsigned char>(c))) {
        scanNumber(tokenStart, c);
        return;
    }

    // 3) String literal?
    if (c == '\"') {
        scanString(tokenStart);
        return;
    }

    // 4) Character literal?
    if (c == '\'') {
        scanCharLiteral(tokenStart);
        return;
    }

    // 5) Multi-character operators or punctuation (e.g., ==, <=, ++, etc.)
    //    We'll check pairs of characters if needed.
    switch (c) {
        // Assignment or equality
        case '=':
            if (!isEnd() && peekChar() == '=') {
                advance();
                addToken(TokenType::EQUALS);
            } else {
                addToken(TokenType::ASSIGN);
            }
            break;
        case '!':
            if (!isEnd() && peekChar() == '=') {
                advance();
                addToken(TokenType::NOT_EQUALS);
            } else {
                addToken(TokenType::NOT);
            }
            break;
        case '<':
            if (!isEnd() && peekChar() == '=') {
                advance();
                addToken(TokenType::LESS_EQ);
            } else if (!isEnd() && peekChar() == '<') {
                advance();
                addToken(TokenType::LSHIFT);
            } else {
                addToken(TokenType::LESS);
            }
            break;
        case '>':
            if (!isEnd() && peekChar() == '=') {
                advance();
                addToken(TokenType::GREATER_EQ);
            } else if (!isEnd() && peekChar() == '>') {
                advance();
                addToken(TokenType::RSHIFT);
            } else {
                addToken(TokenType::GREATER);
            }
            break;
        case '+':
            if (!isEnd() && peekChar() == '+') {
                advance();
                addToken(TokenType::INCREMENT);
            } else if (!isEnd() && peekChar() == '=') {
                advance();
                addToken(TokenType::PLUS_ASSIGN);
            } else {
                addToken(TokenType::PLUS);
            }
            break;
        case '-':
            if (!isEnd() && peekChar() == '-') {
                advance();
                addToken(TokenType::DECREMENT);
            } else if (!isEnd() && peekChar() == '=') {
                advance();
                addToken(TokenType::MINUS_ASSIGN);
            } else {
                addToken(TokenType::MINUS);
            }
            break;
        case '*':
            if (!isEnd() && peekChar() == '=') {
                advance();
                addToken(TokenType::STAR_ASSIGN);
            } else {
                addToken(TokenType::STAR);
            }
            break;
        case '/':
            if (!isEnd() && peekChar() == '=') {
                advance();
                addToken(TokenType::SLASH_ASSIGN);
            } else {
                // NOTE: We handled comments in skipWhitespaceAndComments(),
                // so if we get here, it's not a comment. It's a slash token.
                addToken(TokenType::SLASH);
            }
            break;
        case '%':
            if (!isEnd() && peekChar() == '=') {
                advance();
                addToken(TokenType::MOD_ASSIGN);
            } else {
                addToken(TokenType::MODULO);
            }
            break;
        case '&':
            if (!isEnd() && peekChar() == '&') {
                advance();
                addToken(TokenType::AND);
            } else {
                addToken(TokenType::AMP);
            }
            break;
        case '|':
            if (!isEnd() && peekChar() == '|') {
                advance();
                addToken(TokenType::OR);
            } else {
                addToken(TokenType::BITWISE_OR);
            }
            break;
        case '^':
            addToken(TokenType::BITWISE_XOR);
            break;
        case '~':
            addToken(TokenType::BITWISE_NOT);
            break;
        case '(':
            addToken(TokenType::LPAREN);
            break;
        case ')':
            addToken(TokenType::RPAREN);
            break;
        case '{':
            addToken(TokenType::LBRACE);
            break;
        case '}':
            addToken(TokenType::RBRACE);
            break;
        case '[':
            addToken(TokenType::LBRACKET);
            break;
        case ']':
            addToken(TokenType::RBRACKET);
            break;
        case ';':
            addToken(TokenType::SEMICOLON);
            break;
        case ',':
            addToken(TokenType::COMMA);
            break;
        case '.':
            addToken(TokenType::PERIOD);
            break;
        case '?':
            addToken(TokenType::QUESTION);
            break;
        case ':':
            addToken(TokenType::COLON);
            break;
        default:
            // If we reach here, it's something unknown/unexpected.
            addToken(TokenType::UNKNOWN, tokenStart, 1);
            break;
    }
}

///////////////////////////////////////////////////////////////////////////
//  Helper Methods for Scanning Specific Lexemes
///////////////////////////////////////////////////////////////////////////

void Lexer::skipWhitespaceAndComments()
{
    while (!isEnd()) {
        char c = peekChar();

        if (std::isspace(static_cast<unsigned char>(c))) {
            // If it's a newline, increment the line count.
            if (c == '\n') {
                m_line++;
                m_column = 1;
            } else {
                m_column++;
            }
            m_currentPos++;
        }
        // Single-line comment: //
        else if (c == '/' && (m_currentPos + 1 < m_source.size()) && 
                 m_source[m_currentPos + 1] == '/') {
            // Skip until end of line
            m_currentPos += 2; // skip "//"
            m_column += 2;
            while (!isEnd() && peekChar() != '\n') {
                m_currentPos++;
                m_column++;
            }
        }
        // Multi-line comment: /* ... */
        else if (c == '/' && (m_currentPos + 1 < m_source.size()) &&
                 m_source[m_currentPos + 1] == '*') {
            m_currentPos += 2; // skip "/*"
            m_column += 2;
            // Skip until we find "*/" or hit EOF
            while (!isEnd()) {
                if (peekChar() == '\n') {
                    m_line++;
                    m_column = 1;
                    m_currentPos++;
                    continue;
                }
                if (peekChar() == '*' && (m_currentPos + 1 < m_source.size()) &&
                    m_source[m_currentPos + 1] == '/') {
                    m_currentPos += 2; 
                    m_column += 2;
                    break;
                }
                m_currentPos++;
                m_column++;
            }
        }
        else {
            // It's not whitespace or a comment, break out.
            break;
        }
    }
}

void Lexer::scanNumber(size_t tokenStart, char firstChar)
{
    // We already consumed firstChar.
    // We can handle decimal integers, floats, etc.
    bool isFloat = false;
    while (!isEnd() && std::isdigit(static_cast<unsigned char>(peekChar()))) {
        advance();
    }

    // Check if next char is a dot => floating point
    if (!isEnd() && peekChar() == '.') {
        // Could be a float
        isFloat = true;
        advance(); // consume the dot
        // consume digits after the dot
        while (!isEnd() && std::isdigit(static_cast<unsigned char>(peekChar()))) {
            advance();
        }
    }

    // Could also handle exponent notation (e.g., 1.23e+10) if we want full coverage:
    if (!isEnd() && (peekChar() == 'e' || peekChar() == 'E')) {
        // Then we definitely have a float in scientific notation
        isFloat = true;
        advance(); // consume 'e' or 'E'

        // optional '+' or '-'
        if (!isEnd() && (peekChar() == '+' || peekChar() == '-')) {
            advance();
        }
        // consume digits
        while (!isEnd() && std::isdigit(static_cast<unsigned char>(peekChar()))) {
            advance();
        }
    }

    // Now we have the full number. Let's add the token.
    size_t length = m_currentPos - tokenStart;
    if (isFloat) {
        addToken(TokenType::FLOAT_LITERAL, tokenStart, length);
    } else {
        addToken(TokenType::INT_LITERAL, tokenStart, length);
    }
}

void Lexer::scanString(size_t tokenStart)
{
    // We've already consumed the initial '"'.
    // Keep reading until we see another '"' or end-of-file/line.
    bool closed = false;
    while (!isEnd()) {
        char c = advance();
        if (c == '\"') {
            closed = true;
            break;
        }
        if (c == '\n') {
            // In C, a newline in a string literal is typically an error or 
            // the compiler merges it with the next line. We'll treat it as an error for simplicity.
            // Real compilers might handle multiline string literals with backslash-escaped newlines.
            // For simplicity, let's just break.
            break;
        }
        // Handle escaped characters if you wish (e.g., \n, \t, etc.).
    }
    size_t length = m_currentPos - tokenStart;
    addToken(TokenType::STRING_LITERAL, tokenStart, length);
}

void Lexer::scanCharLiteral(size_t tokenStart)
{
    // We consumed the initial '\''.
    // We keep reading until we see another '\'' or end-of-line/file.
    bool closed = false;
    while (!isEnd()) {
        char c = advance();
        if (c == '\'') {
            closed = true;
            break;
        }
        if (c == '\n') {
            break;  // error or unclosed char literal in real compilers
        }
        // Could handle escaped characters here as well.
    }
    size_t length = m_currentPos - tokenStart;
    addToken(TokenType::CHAR_LITERAL, tokenStart, length);
}

///////////////////////////////////////////////////////////////////////////
//  Primitive Character Operations
///////////////////////////////////////////////////////////////////////////

char Lexer::advance()
{
    if (isEnd()) {
        return '\0';
    }
    char c = m_source[m_currentPos];
    m_currentPos++;
    m_column++;
    return c;
}

char Lexer::peekChar() const
{
    if (isEnd()) {
        return '\0';
    }
    return m_source[m_currentPos];
}

bool Lexer::isAlpha(char c) const
{
    // Letters or underscore
    return (std::isalpha(static_cast<unsigned char>(c)) || c == '_');
}

bool Lexer::isDigit(char c) const
{
    return std::isdigit(static_cast<unsigned char>(c)) != 0;
}

bool Lexer::isAlphaNum(char c) const
{
    return (isAlpha(c) || isDigit(c));
}

inline bool Lexer::isEnd() const
{
    return (m_currentPos >= m_source.size());
}

///////////////////////////////////////////////////////////////////////////
//  Token Creation Helpers
///////////////////////////////////////////////////////////////////////////

void Lexer::addToken(TokenType type)
{
    // The lexeme is a single character at (m_currentPos - 1).
    // Because we already advanced, let's define the start = currentPos-1.
    size_t startPos = m_currentPos - 1;
    addToken(type, startPos, 1);
}

void Lexer::addToken(TokenType type, size_t startPos, size_t length)
{
    std::string lex = m_source.substr(startPos, length);
    Token token(type, lex, m_line, m_column - static_cast<unsigned>(length));
    m_tokens.push_back(token);
}

TokenType Lexer::identifierType(const std::string &identifier) const
{
    // Basic set of C keywords. Expand as needed for C99 coverage.
    static const std::map<std::string, TokenType> keywords = {
        {"int",    TokenType::KW_INT},
        {"float",  TokenType::KW_FLOAT},
        {"char",   TokenType::KW_CHAR},
        {"double", TokenType::KW_DOUBLE},
        {"void",   TokenType::KW_VOID},
        {"if",     TokenType::KW_IF},
        {"else",   TokenType::KW_ELSE},
        {"while",  TokenType::KW_WHILE},
        {"for",    TokenType::KW_FOR},
        {"return", TokenType::KW_RETURN},
        // Add more as needed: "const", "static", "struct", "union", ...
    };

    auto it = keywords.find(identifier);
    if (it != keywords.end()) {
        return it->second;
    }
    return TokenType::IDENTIFIER;
}
