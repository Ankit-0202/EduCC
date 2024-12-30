/**
 * @file Token.h
 * @brief Declarations for token-related data structures in the EduCC compiler.
 *
 * This header defines the different token types, along with a Token class
 * to encapsulate all relevant information about a token (type, lexeme, location).
 */

#ifndef TOKEN_H
#define TOKEN_H

#include "EduCC.h"
#include <string>

/**
 * @enum TokenType
 * @brief Enumerates all possible token types for the C99 language.
 *
 * This enumeration is used by the lexer to classify each lexeme
 * it scans from the input source.
 */
enum class TokenType
{
    // Special / End of file
    EOF_TOK,         ///< End of file token
    
    // Identifiers and literals
    IDENTIFIER,      ///< Variable/function name, etc.
    INT_LITERAL,     ///< Integer constant
    FLOAT_LITERAL,   ///< Floating-point constant
    CHAR_LITERAL,    ///< Character constant
    STRING_LITERAL,  ///< String constant
    
    // Keywords
    KW_INT,
    KW_FLOAT,
    KW_CHAR,
    KW_DOUBLE,
    KW_VOID,
    KW_IF,
    KW_ELSE,
    KW_WHILE,
    KW_FOR,
    KW_RETURN,
    // ... you can add more C keywords here
    
    // Operators and punctuators
    PLUS,       ///< '+'
    MINUS,      ///< '-'
    STAR,       ///< '*'
    SLASH,      ///< '/'
    MODULO,     ///< '%'
    INCREMENT,  ///< '++'
    DECREMENT,  ///< '--'
    
    ASSIGN,     ///< '='
    PLUS_ASSIGN, ///< '+='
    MINUS_ASSIGN, ///< '-='
    STAR_ASSIGN,  ///< '*='
    SLASH_ASSIGN, ///< '/='
    MOD_ASSIGN,   ///< '%='
    
    EQUALS,     ///< '=='
    NOT_EQUALS, ///< '!='
    LESS,       ///< '<'
    GREATER,    ///< '>'
    LESS_EQ,    ///< '<='
    GREATER_EQ, ///< '>='
    
    AND,        ///< '&&'
    OR,         ///< '||'
    NOT,        ///< '!'
    
    AMP,        ///< '&'
    BITWISE_OR, ///< '|'
    BITWISE_XOR,///< '^'
    BITWISE_NOT,///< '~'
    LSHIFT,     ///< '<<'
    RSHIFT,     ///< '>>'
    
    LPAREN,     ///< '('
    RPAREN,     ///< ')'
    LBRACE,     ///< '{'
    RBRACE,     ///< '}'
    LBRACKET,   ///< '['
    RBRACKET,   ///< ']'
    
    SEMICOLON,  ///< ';'
    COMMA,      ///< ','
    PERIOD,     ///< '.'
    QUESTION,   ///< '?'
    COLON,      ///< ':'
    
    // Additional tokens or placeholders
    UNKNOWN     ///< Something unrecognized by the lexer
};

/**
 * @class Token
 * @brief Encapsulates information about a single token in the source code.
 *
 * Each token has:
 * - A TokenType (e.g., identifier, keyword, operator, etc.).
 * - A lexeme string (the exact text from the source).
 * - The line and column where it appeared (for error reporting).
 */
class Token
{
private:
    TokenType m_type;
    std::string m_lexeme;
    size_t m_line;
    size_t m_column;

public:
    /**
     * @brief Constructs a new Token object.
     *
     * @param type The token type (e.g., IDENTIFIER, KW_INT).
     * @param lexeme The exact string that was scanned from source.
     * @param line The line number where the token begins (0-based or 1-based).
     * @param column The column number where the token begins (0-based or 1-based).
     */
    Token(TokenType type, const std::string &lexeme, size_t line, size_t column)
        : m_type(type), m_lexeme(lexeme), m_line(line), m_column(column)
    {
    }
    
    // Accessors
    TokenType type() const { return m_type; }
    const std::string &lexeme() const { return m_lexeme; }
    size_t line() const { return m_line; }
    size_t column() const { return m_column; }

    /**
     * @brief Debug helper: Convert token to a string representation.
     * 
     * @return std::string A user-friendly string (e.g., "KW_INT(int) at line=1, col=2").
     */
    std::string toString() const
    {
        std::ostringstream oss;
        oss << "Token("
            << static_cast<int>(m_type) << ", \"" 
            << m_lexeme << "\", "
            << m_line << ", "
            << m_column << ")";
        return oss.str();
    }
};

#endif // TOKEN_H
