/**
 * @file Parser.h
 * @brief Declaration of the Parser class for EduCC.
 *
 * The Parser takes a sequence of tokens from the Lexer and converts them
 * into an Abstract Syntax Tree (AST) according to the C99 grammar.
 */

#ifndef EDUCC_PARSER_H
#define EDUCC_PARSER_H

#include "EduCC.h"
#include "lexer/Token.h"
#include "ast/AST.h"
#include <vector>
#include <memory>

/**
 * @class Parser
 * @brief A recursive-descent parser for a simplified subset of C99.
 *
 * Usage:
 *   1) Construct a Parser with a token list.
 *   2) Call parseTranslationUnit() to parse an entire .c file.
 *   3) Receive a TranslationUnitNode that represents the AST root.
 *
 * Error handling:
 *   - Throws std::runtime_error on syntax errors.
 */
class Parser
{
public:
    /**
     * @brief Constructs a Parser object with a given token list.
     * @param tokens The tokens produced by the Lexer.
     */
    Parser(const std::vector<Token> &tokens);

    /**
     * @brief Parses an entire translation unit (i.e., a complete .c file).
     * @return A unique_ptr to a TranslationUnitNode representing the AST root.
     */
    std::unique_ptr<TranslationUnitNode> parseTranslationUnit();

private:
    const std::vector<Token> &m_tokens;
    size_t m_currentIndex;

    // -----------------------------------------------------------------------
    //  Helper: Current Token Info
    // -----------------------------------------------------------------------
    /**
     * @brief Gets the current token.
     */
    const Token &currentToken() const;

    /**
     * @brief Checks if we've reached the end (EOF).
     */
    bool isAtEnd() const;

    /**
     * @brief Consumes and returns the current token, then advances.
     */
    Token advance();

    /**
     * @brief Peeks ahead, matching a particular TokenType if possible.
     *        If matched, consumes the token and returns true; otherwise false.
     */
    bool match(TokenType ttype);

    /**
     * @brief Expects the current token to be of the given type, or throws an error.
     *        Returns the consumed token if successful.
     */
    Token expect(TokenType ttype, const std::string &errorMsg);

    /**
     * @brief Throws a std::runtime_error indicating a parsing error.
     * @param message The error message.
     */
    void error(const std::string &message) const;

    // -----------------------------------------------------------------------
    //  Grammar / Parsing Methods
    // -----------------------------------------------------------------------
    /**
     * @brief Parses one external declaration (function definition or global var).
     */
    std::unique_ptr<ASTNode> parseExternalDeclaration();

    /**
     * @brief Parses a function definition (return type, name, params, body).
     */
    std::unique_ptr<FunctionDefNode> parseFunctionDefinition(const std::string &type, const std::string &name);

    /**
     * @brief Parses zero or more parameters in a function parameter list.
     */
    void parseParameterList(std::vector<std::string> &paramTypes,
                            std::vector<std::string> &paramNames);

    /**
     * @brief Parses a variable or function parameter declaration (like "int x").
     */
    std::pair<std::string, std::string> parseParamDeclaration();

    /**
     * @brief Parses a variable declaration at global or local scope (no initializer).
     */
    std::unique_ptr<VarDeclNode> parseVarDeclaration(const std::string &type, const std::string &name);

    /**
     * @brief Parses a statement (if, while, for, return, compound, expression, etc.).
     */
    std::unique_ptr<StmtNode> parseStatement();

    /**
     * @brief Parse a local variable declaration statement, e.g. "int sum = x + y;".
     */
    std::unique_ptr<StmtNode> parseLocalDeclarationStatement();

    /**
     * @brief Parses a compound statement (i.e., '{' stmt* '}').
     */
    std::unique_ptr<CompoundStmtNode> parseCompoundStatement();

    /**
     * @brief Parses an if statement.
     */
    std::unique_ptr<IfStmtNode> parseIfStatement();

    /**
     * @brief Parses a while statement.
     */
    std::unique_ptr<WhileStmtNode> parseWhileStatement();

    /**
     * @brief Parses a for statement.
     */
    std::unique_ptr<ForStmtNode> parseForStatement();

    /**
     * @brief Parses a return statement.
     */
    std::unique_ptr<ReturnStmtNode> parseReturnStatement();

    // -----------------------------------------------------------------------
    //  Expressions
    // -----------------------------------------------------------------------
    /**
     * @brief Parses an expression statement (expression?) followed by ';'.
     */
    std::unique_ptr<StmtNode> parseExpressionStatement();

    /**
     * @brief parseExpression -> parseAssignment
     */
    std::unique_ptr<ExprNode> parseExpression();

    /**
     * @brief parseAssignment -> parseEquality (('=' parseAssignment)?)
     */
    std::unique_ptr<ExprNode> parseAssignment();

    /**
     * @brief parseEquality -> parseRelational (('=='|'!=') parseRelational)*
     */
    std::unique_ptr<ExprNode> parseEquality();

    /**
     * @brief parseRelational -> parseAdditive (('<'|'>'|'<='|'>=') parseAdditive)*
     */
    std::unique_ptr<ExprNode> parseRelational();

    /**
     * @brief parseAdditive -> parseMultiplicative (('+'|'-') parseMultiplicative)*
     */
    std::unique_ptr<ExprNode> parseAdditive();

    /**
     * @brief parseMultiplicative -> parseUnary (('*'|'/'|'%') parseUnary)*
     */
    std::unique_ptr<ExprNode> parseMultiplicative();

    /**
     * @brief parseUnary -> ('+'|'-'|'++'|'--') parseUnary | parsePrimary
     */
    std::unique_ptr<ExprNode> parseUnary();

    /**
     * @brief parsePrimary -> literal | '(' expression ')' | identifier ( funcCall? )
     */
    std::unique_ptr<ExprNode> parsePrimary();

    /**
     * @brief Checks if the current token is an integer, float, char, or string literal.
     */
    bool isLiteral(const Token &tok) const;

    /**
     * @brief Utility to check if a token type is a known "type keyword" (int/float/etc).
     */
    bool isTypeKeyword(TokenType tt) const;
};

#endif // EDUCC_PARSER_H
