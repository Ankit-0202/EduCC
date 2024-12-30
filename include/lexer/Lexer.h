/**
 * @file Lexer.h
 * @brief Declaration of the Lexer class responsible for scanning C source code into tokens.
 *
 * The Lexer (also called a tokenizer or scanner) takes an input stream of characters
 * and converts them into tokens, which the parser can then use for syntactic analysis.
 */

#ifndef LEXER_H
#define LEXER_H

#include "EduCC.h"
#include "Token.h"
#include <string>
#include <vector>

class Lexer
{
public:
    /**
     * @brief Constructs a new Lexer object from a source string.
     * 
     * @param source The entire C source code to tokenize.
     */
    Lexer(const std::string &source);

    /**
     * @brief Retrieves the next token from the input stream without consuming it.
     * 
     * @return const Token& The next token.
     * 
     * @throw std::runtime_error if there are no more tokens available (EOF).
     */
    const Token &peek();

    /**
     * @brief Retrieves the next token from the input stream and consumes it.
     * 
     * @return Token The next token.
     * 
     * @throw std::runtime_error if there are no more tokens available (EOF).
     */
    Token getNextToken();

    /**
     * @brief Checks if the lexer has reached the end of input (EOF).
     * 
     * @return true If no more tokens are left.
     * @return false Otherwise.
     */
    bool isAtEnd() const;

private:
    std::string m_source;         ///< The entire source code.
    size_t m_currentPos;          ///< Current index into m_source.
    size_t m_line;                ///< Current line number (for token location).
    size_t m_column;              ///< Current column number (for token location).

    std::vector<Token> m_tokens;  ///< Buffer of lexed tokens.
    size_t m_tokenIndex;          ///< Index into m_tokens.

    /**
     * @brief Lex all tokens from the source code into m_tokens.
     *
     * This function should be called by the constructor.
     */
    void tokenize();

    /**
     * @brief Extracts the next token from the source and appends it to m_tokens.
     * 
     * This function will handle the logic to identify if the next token is
     * an identifier, a keyword, a literal, an operator, etc.
     */
    void scanToken();

    /**
     * @brief Advances the current position in the source by one character.
     * 
     * @return char The character at the old position.
     */
    char advance();

    /**
     * @brief Peeks at the current character without consuming it.
     * 
     * @return char The current character, or '\0' if we are at the end.
     */
    char peekChar() const;

    /**
     * @brief Checks if a character is alphabetical (A-Z or a-z or underscore).
     */
    bool isAlpha(char c) const;

    /**
     * @brief Checks if a character is a digit (0-9).
     */
    bool isDigit(char c) const;

    /**
     * @brief Checks if a character is alphanumeric (alphabet or digit or underscore).
     */
    bool isAlphaNum(char c) const;

    /**
     * @brief Adds a simple single-character token, e.g., '+' or '('.
     * 
     * @param type The token type to add.
     */
    void addToken(TokenType type);

    /**
     * @brief Adds a token of the specified type with a given lexeme substring from the source.
     * 
     * @param type The token type to add.
     * @param startPos The starting index in m_source for the token text.
     * @param length The length of the token text in characters.
     */
    void addToken(TokenType type, size_t startPos, size_t length);

    /**
     * @brief Converts an identifier lexeme into the appropriate TokenType if it is a keyword.
     * 
     * @param identifier The lexeme to check.
     * @return TokenType KW_xxx if it is a recognized keyword, otherwise IDENTIFIER.
     */
    TokenType identifierType(const std::string &identifier) const;

    bool isEnd() const;
};

#endif // LEXER_H
