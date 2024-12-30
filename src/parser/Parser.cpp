/**
 * @file Parser.cpp
 * @brief Implementation of the Parser class for EduCC.
 */

#include "parser/Parser.h"

///////////////////////////////////////////////////////////////////////////
// Constructor
///////////////////////////////////////////////////////////////////////////

Parser::Parser(const std::vector<Token> &tokens)
    : m_tokens(tokens), m_currentIndex(0)
{
}

///////////////////////////////////////////////////////////////////////////
// Utility Functions
///////////////////////////////////////////////////////////////////////////

const Token &Parser::currentToken() const
{
    if (isAtEnd()) {
        // Return the last token if at end,
        // or throw if you prefer strict handling.
        return m_tokens.back(); 
    }
    return m_tokens[m_currentIndex];
}

bool Parser::isAtEnd() const
{
    return (m_currentIndex >= m_tokens.size() 
            || m_tokens[m_currentIndex].type() == TokenType::EOF_TOK);
}

Token Parser::advance()
{
    if (!isAtEnd()) {
        m_currentIndex++;
    }
    return m_tokens[m_currentIndex - 1];
}

bool Parser::match(TokenType ttype)
{
    if (!isAtEnd() && currentToken().type() == ttype) {
        advance();
        return true;
    }
    return false;
}

Token Parser::expect(TokenType ttype, const std::string &errorMsg)
{
    if (isAtEnd() || currentToken().type() != ttype) {
        error(errorMsg);
    }
    return advance();
}

void Parser::error(const std::string &message) const
{
    const Token &tok = (isAtEnd() ? m_tokens.back() : currentToken());
    std::string msg = "[Parser Error] " + message +
                      " at line " + std::to_string(tok.line()) +
                      ", column " + std::to_string(tok.column()) +
                      ", near token '" + tok.lexeme() + "'.";
    throw std::runtime_error(msg);
}

///////////////////////////////////////////////////////////////////////////
// Top-Level Entry: parseTranslationUnit
///////////////////////////////////////////////////////////////////////////

std::unique_ptr<TranslationUnitNode> Parser::parseTranslationUnit()
{
    auto root = std::make_unique<TranslationUnitNode>(1, 1);

    // While not at EOF, parse external declarations
    while (!isAtEnd()) {
        if (currentToken().type() == TokenType::EOF_TOK) {
            break;
        }
        auto decl = parseExternalDeclaration();
        if (decl) {
            root->addDeclaration(std::move(decl));
        }
    }

    return root;
}

///////////////////////////////////////////////////////////////////////////
// parseExternalDeclaration: function-def or global var
///////////////////////////////////////////////////////////////////////////

std::unique_ptr<ASTNode> Parser::parseExternalDeclaration()
{
    if (isTypeKeyword(currentToken().type()))
    {
        Token typeTok = advance(); 
        std::string typeStr = typeTok.lexeme();

        // Next must be an identifier (the function or variable name)
        if (currentToken().type() != TokenType::IDENTIFIER) {
            error("Expected identifier after type");
        }
        Token nameTok = advance();
        std::string nameStr = nameTok.lexeme();

        // Check if next is '(' => function definition, else => global var
        if (match(TokenType::LPAREN)) {
            // It's a function
            return parseFunctionDefinition(typeStr, nameStr);
        } else {
            // It's a global variable
            auto varDecl = parseVarDeclaration(typeStr, nameStr);
            // Expect semicolon
            expect(TokenType::SEMICOLON, "Expected ';' after global variable declaration");
            return varDecl;
        }
    }
    else {
        // Not a known type -> syntax error
        error("Expected type specifier (int, float, char, double, void)");
    }

    return nullptr; // unreachable
}

std::unique_ptr<FunctionDefNode> Parser::parseFunctionDefinition(const std::string &type, const std::string &name)
{
    // We consumed '(' already
    std::vector<std::string> paramTypes;
    std::vector<std::string> paramNames;

    if (!match(TokenType::RPAREN)) {
        // Parse parameter list if not an immediate ')'
        parseParameterList(paramTypes, paramNames);
        expect(TokenType::RPAREN, "Expected ')' after function parameter list");
    }

    // Build function decl
    auto funcDecl = std::make_unique<FunctionDeclNode>(
        type, name, paramTypes, paramNames,
        currentToken().line(), currentToken().column()
    );

    // Parse body => compound statement
    auto body = parseCompoundStatement();

    // Return a FunctionDefNode
    return std::make_unique<FunctionDefNode>(
        std::move(funcDecl),
        std::move(body),
        currentToken().line(), 
        currentToken().column()
    );
}

void Parser::parseParameterList(std::vector<std::string> &paramTypes,
                                std::vector<std::string> &paramNames)
{
    // We parse one param declaration -> optional more if comma
    while (true) {
        auto [ptype, pname] = parseParamDeclaration();
        paramTypes.push_back(ptype);
        paramNames.push_back(pname);

        if (match(TokenType::COMMA)) {
            // continue parsing next parameter
            continue;
        } else {
            break;
        }
    }
}

std::pair<std::string, std::string> Parser::parseParamDeclaration()
{
    if (!isTypeKeyword(currentToken().type())) {
        error("Expected type specifier in function parameter");
    }
    Token typeTok = advance();
    std::string typeStr = typeTok.lexeme();

    // Expect an identifier
    if (currentToken().type() != TokenType::IDENTIFIER) {
        error("Expected identifier in function parameter");
    }
    Token nameTok = advance();
    std::string nameStr = nameTok.lexeme();

    return {typeStr, nameStr};
}

std::unique_ptr<VarDeclNode> Parser::parseVarDeclaration(const std::string &type, const std::string &name)
{
    // Optional initializer
    // If the next token is '=', parse an initializer expression
    // We'll skip advanced type qualifiers/pointers for brevity
    // This method returns a VarDeclNode; we don't store the initializer in AST for now.

    return std::make_unique<VarDeclNode>(
        type,
        name,
        currentToken().line(),
        currentToken().column()
    );
}

///////////////////////////////////////////////////////////////////////////
// parseStatement
///////////////////////////////////////////////////////////////////////////

std::unique_ptr<StmtNode> Parser::parseStatement()
{
    TokenType tk = currentToken().type();

    // If this is a local variable declaration like "int x = 5;"
    // or "float y;"
    if (isTypeKeyword(tk)) {
        // parse local var declaration statement
        return parseLocalDeclarationStatement();
    }

    if (match(TokenType::LBRACE)) {
        // Compound statement
        // We already consumed '{', so parse the inside:
        auto block = std::make_unique<CompoundStmtNode>(
            currentToken().line(), currentToken().column()
        );

        // parse statements until we find '}'
        while (!match(TokenType::RBRACE)) {
            if (isAtEnd()) {
                error("Unterminated block; missing '}'");
            }
            // "un-get" the token if we didn't match RBRACE
            auto stmt = parseStatement();
            block->addItem(std::move(stmt));
        }
        return block;
    }
    else if (match(TokenType::KW_IF)) {
        return parseIfStatement();
    }
    else if (match(TokenType::KW_WHILE)) {
        return parseWhileStatement();
    }
    else if (match(TokenType::KW_FOR)) {
        return parseForStatement();
    }
    else if (match(TokenType::KW_RETURN)) {
        return parseReturnStatement();
    }
    else {
        // Expression statement or empty statement
        return parseExpressionStatement();
    }
}

/**
 * @brief Parse a local variable declaration statement, e.g. "int sum = x + y;".
 *
 * This method expects the current token to be a type keyword (checked by caller).
 */
std::unique_ptr<StmtNode> Parser::parseLocalDeclarationStatement()
{
    // We already know currentToken() is a type
    Token typeTok = advance(); // consume the type
    std::string typeStr = typeTok.lexeme();

    // Next must be an identifier
    if (currentToken().type() != TokenType::IDENTIFIER) {
        error("Expected identifier in local variable declaration");
    }
    Token nameTok = advance();
    std::string nameStr = nameTok.lexeme();

    // Optional initializer
    std::unique_ptr<ExprNode> initExpr = nullptr;
    if (match(TokenType::ASSIGN)) {
        // parse an expression for initializer
        initExpr = parseExpression();
    }

    // Expect semicolon
    expect(TokenType::SEMICOLON, "Expected ';' after local variable declaration");

    // Build a VarDeclNode
    auto varDeclNode = std::make_unique<VarDeclNode>(
        typeStr,
        nameStr,
        currentToken().line(),
        currentToken().column()
    );

    // If you want, you can store initExpr in a custom node type (e.g. `LocalVarDeclStmtNode`)
    // for codegen to do an alloca + store. Here, let's just store it in a "DeclStmt" for simplicity.

    return std::make_unique<DeclStmtNode>(
        std::move(varDeclNode),
        std::move(initExpr),
        currentToken().line(),
        currentToken().column()
    );
}

std::unique_ptr<CompoundStmtNode> Parser::parseCompoundStatement()
{
    // We expect a '{'
    if (!match(TokenType::LBRACE)) {
        error("Expected '{' at start of compound statement");
    }
    auto block = std::make_unique<CompoundStmtNode>(
        currentToken().line(), currentToken().column()
    );

    // parse statements until we find '}'
    while (!match(TokenType::RBRACE)) {
        if (isAtEnd()) {
            error("Unterminated block; missing '}'");
        }
        auto stmt = parseStatement();
        block->addItem(std::move(stmt));
    }

    return block;
}

std::unique_ptr<IfStmtNode> Parser::parseIfStatement()
{
    // We matched the 'if' token in parseStatement()
    // Expect '(' expression ')'
    expect(TokenType::LPAREN, "Expected '(' after 'if'");
    auto condExpr = parseExpression();
    expect(TokenType::RPAREN, "Expected ')' after if condition");

    // Then parse the 'then' statement
    auto thenStmt = parseStatement();

    // Optionally parse 'else'
    std::unique_ptr<ASTNode> elseStmt = nullptr;
    if (match(TokenType::KW_ELSE)) {
        elseStmt = parseStatement();
    }

    return std::make_unique<IfStmtNode>(
        std::move(condExpr),
        std::move(thenStmt),
        std::move(elseStmt),
        currentToken().line(), 
        currentToken().column()
    );
}

std::unique_ptr<WhileStmtNode> Parser::parseWhileStatement()
{
    // We matched 'while'
    // Expect '(' expression ')'
    expect(TokenType::LPAREN, "Expected '(' after 'while'");
    auto condExpr = parseExpression();
    expect(TokenType::RPAREN, "Expected ')' after while condition");

    // Then parse loop body
    auto body = parseStatement();

    return std::make_unique<WhileStmtNode>(
        std::move(condExpr),
        std::move(body),
        currentToken().line(),
        currentToken().column()
    );
}

std::unique_ptr<ForStmtNode> Parser::parseForStatement()
{
    // We matched 'for'
    // Expect '('
    expect(TokenType::LPAREN, "Expected '(' after 'for'");

    // parse init (could be an expression statement or var decl or empty)
    std::unique_ptr<ASTNode> init = nullptr;
    if (!match(TokenType::SEMICOLON)) {
        // it might be a local decl or an expression statement
        // if the next token is type, parse local decl
        if (isTypeKeyword(currentToken().type())) {
            init = parseLocalDeclarationStatement();
        } else {
            // parse expression statement
            auto exprStmt = parseExpressionStatement();
            init = std::move(exprStmt);
        }
    }

    // parse condition
    std::unique_ptr<ASTNode> cond = nullptr;
    if (!match(TokenType::SEMICOLON)) {
        // parse expression
        cond = parseExpression();
        expect(TokenType::SEMICOLON, "Expected ';' in for-statement");
    }

    // parse increment
    std::unique_ptr<ASTNode> incr = nullptr;
    if (!match(TokenType::RPAREN)) {
        incr = parseExpression();
        expect(TokenType::RPAREN, "Expected ')' after for-statement increment");
    }

    // parse the body
    auto body = parseStatement();

    return std::make_unique<ForStmtNode>(
        std::move(init),
        std::move(cond),
        std::move(incr),
        std::move(body),
        currentToken().line(),
        currentToken().column()
    );
}

std::unique_ptr<ReturnStmtNode> Parser::parseReturnStatement()
{
    // We matched 'return'
    // parse optional expression
    std::unique_ptr<ASTNode> expr = nullptr;
    if (!match(TokenType::SEMICOLON)) {
        // we didn't match ';', so parse expression
        expr = parseExpression();
        expect(TokenType::SEMICOLON, "Expected ';' after return expression");
    }

    return std::make_unique<ReturnStmtNode>(
        std::move(expr),
        currentToken().line(),
        currentToken().column()
    );
}

///////////////////////////////////////////////////////////////////////////
// parseExpressionStatement
///////////////////////////////////////////////////////////////////////////

std::unique_ptr<StmtNode> Parser::parseExpressionStatement()
{
    // possibly empty statement => if we match ';' immediately, it's an empty statement
    if (match(TokenType::SEMICOLON)) {
        // empty
        return std::make_unique<CompoundStmtNode>(
            currentToken().line(), currentToken().column()
        ); 
    }
    // otherwise parse an expression, then expect ';'
    auto expr = parseExpression();
    expect(TokenType::SEMICOLON, "Expected ';' after expression statement");
    return std::make_unique<ExprStmtNode>(
        std::move(expr),
        currentToken().line(),
        currentToken().column()
    );
}

///////////////////////////////////////////////////////////////////////////
// Expressions (recursive descent approach)
// expression -> assignment
///////////////////////////////////////////////////////////////////////////

std::unique_ptr<ExprNode> Parser::parseExpression()
{
    return parseAssignment();
}

/*
    assignment -> equality ( '=' assignment )?
*/
std::unique_ptr<ExprNode> Parser::parseAssignment()
{
    auto left = parseEquality();

    if (match(TokenType::ASSIGN)) {
        // we have left = right
        auto right = parseAssignment();
        // Build a BinaryExprNode with operator "=" (or store separately)
        return std::make_unique<BinaryExprNode>(
            "=",
            std::move(left),
            std::move(right),
            currentToken().line(),
            currentToken().column()
        );
    }

    return left; // no assignment operator
}

/*
    equality -> relational ( ( '==' | '!=' ) relational )*
*/
std::unique_ptr<ExprNode> Parser::parseEquality()
{
    auto expr = parseRelational();

    while (true) {
        if (match(TokenType::EQUALS)) {
            auto right = parseRelational();
            expr = std::make_unique<BinaryExprNode>(
                "==",
                std::move(expr),
                std::move(right),
                currentToken().line(),
                currentToken().column()
            );
        }
        else if (match(TokenType::NOT_EQUALS)) {
            auto right = parseRelational();
            expr = std::make_unique<BinaryExprNode>(
                "!=",
                std::move(expr),
                std::move(right),
                currentToken().line(),
                currentToken().column()
            );
        }
        else {
            break;
        }
    }
    return expr;
}

/*
    relational -> additive ( ( '<' | '>' | '<=' | '>=' ) additive )*
*/
std::unique_ptr<ExprNode> Parser::parseRelational()
{
    auto expr = parseAdditive();

    while (true) {
        if (match(TokenType::LESS)) {
            auto right = parseAdditive();
            expr = std::make_unique<BinaryExprNode>(
                "<",
                std::move(expr),
                std::move(right),
                currentToken().line(),
                currentToken().column()
            );
        }
        else if (match(TokenType::GREATER)) {
            auto right = parseAdditive();
            expr = std::make_unique<BinaryExprNode>(
                ">",
                std::move(expr),
                std::move(right),
                currentToken().line(),
                currentToken().column()
            );
        }
        else if (match(TokenType::LESS_EQ)) {
            auto right = parseAdditive();
            expr = std::make_unique<BinaryExprNode>(
                "<=",
                std::move(expr),
                std::move(right),
                currentToken().line(),
                currentToken().column()
            );
        }
        else if (match(TokenType::GREATER_EQ)) {
            auto right = parseAdditive();
            expr = std::make_unique<BinaryExprNode>(
                ">=",
                std::move(expr),
                std::move(right),
                currentToken().line(),
                currentToken().column()
            );
        }
        else {
            break;
        }
    }
    return expr;
}

/*
    additive -> multiplicative ( ( '+' | '-' ) multiplicative )*
*/
std::unique_ptr<ExprNode> Parser::parseAdditive()
{
    auto expr = parseMultiplicative();

    while (true) {
        if (match(TokenType::PLUS)) {
            auto right = parseMultiplicative();
            expr = std::make_unique<BinaryExprNode>(
                "+",
                std::move(expr),
                std::move(right),
                currentToken().line(),
                currentToken().column()
            );
        }
        else if (match(TokenType::MINUS)) {
            auto right = parseMultiplicative();
            expr = std::make_unique<BinaryExprNode>(
                "-",
                std::move(expr),
                std::move(right),
                currentToken().line(),
                currentToken().column()
            );
        }
        else {
            break;
        }
    }
    return expr;
}

/*
    multiplicative -> unary ( ( '*' | '/' | '%' ) unary )*
*/
std::unique_ptr<ExprNode> Parser::parseMultiplicative()
{
    auto expr = parseUnary();

    while (true) {
        if (match(TokenType::STAR)) {
            auto right = parseUnary();
            expr = std::make_unique<BinaryExprNode>(
                "*",
                std::move(expr),
                std::move(right),
                currentToken().line(),
                currentToken().column()
            );
        }
        else if (match(TokenType::SLASH)) {
            auto right = parseUnary();
            expr = std::make_unique<BinaryExprNode>(
                "/",
                std::move(expr),
                std::move(right),
                currentToken().line(),
                currentToken().column()
            );
        }
        else if (match(TokenType::MODULO)) {
            auto right = parseUnary();
            expr = std::make_unique<BinaryExprNode>(
                "%",
                std::move(expr),
                std::move(right),
                currentToken().line(),
                currentToken().column()
            );
        }
        else {
            break;
        }
    }
    return expr;
}

/*
    unary -> ( '+' | '-' | '++' | '--' ) unary | primary
*/
std::unique_ptr<ExprNode> Parser::parseUnary()
{
    // Check for unary operators
    if (match(TokenType::PLUS)) {
        auto operand = parseUnary();
        return std::make_unique<UnaryExprNode>("+", std::move(operand),
                                               currentToken().line(),
                                               currentToken().column());
    }
    else if (match(TokenType::MINUS)) {
        auto operand = parseUnary();
        return std::make_unique<UnaryExprNode>("-", std::move(operand),
                                               currentToken().line(),
                                               currentToken().column());
    }
    else if (match(TokenType::INCREMENT)) {
        auto operand = parseUnary();
        return std::make_unique<UnaryExprNode>("++", std::move(operand),
                                               currentToken().line(),
                                               currentToken().column());
    }
    else if (match(TokenType::DECREMENT)) {
        auto operand = parseUnary();
        return std::make_unique<UnaryExprNode>("--", std::move(operand),
                                               currentToken().line(),
                                               currentToken().column());
    }
    else {
        // no unary operator => parse primary
        return parsePrimary();
    }
}

/*
    primary -> 
        INT_LITERAL 
      | FLOAT_LITERAL 
      | STRING_LITERAL 
      | CHAR_LITERAL 
      | '(' expression ')' 
      | identifier ( funcCall? )
*/
std::unique_ptr<ExprNode> Parser::parsePrimary()
{
    const Token &tok = currentToken();

    if (isLiteral(tok)) {
        // integer, float, char, or string
        Token litTok = advance();
        return std::make_unique<LiteralExprNode>(
            litTok.lexeme(),
            litTok.line(),
            litTok.column()
        );
    }
    else if (match(TokenType::LPAREN)) {
        // parse ( expr )
        auto expr = parseExpression();
        expect(TokenType::RPAREN, "Expected ')' after grouped expression");
        return expr; // already an ExprNode
    }
    else if (tok.type() == TokenType::IDENTIFIER) {
        // could be a variable reference or a function call
        Token idTok = advance();
        std::string name = idTok.lexeme();

        // check if next token is '(' => function call
        if (match(TokenType::LPAREN)) {
            // parse call arguments
            std::vector<std::unique_ptr<ASTNode>> args;
            if (!match(TokenType::RPAREN)) {
                // parse arguments separated by ','
                while (true) {
                    auto argExpr = parseExpression();
                    args.push_back(std::move(argExpr));
                    if (match(TokenType::COMMA)) {
                        continue;
                    }
                    expect(TokenType::RPAREN, "Expected ')' after function call arguments");
                    break;
                }
            }
            return std::make_unique<CallExprNode>(
                name,
                std::move(args),
                currentToken().line(),
                currentToken().column()
            );
        } else {
            // just an identifier expression
            return std::make_unique<IdentifierExprNode>(
                name,
                idTok.line(),
                idTok.column()
            );
        }
    }

    error("Expected expression");
    return nullptr; // unreachable
}

bool Parser::isLiteral(const Token &tok) const
{
    switch (tok.type()) {
        case TokenType::INT_LITERAL:
        case TokenType::FLOAT_LITERAL:
        case TokenType::CHAR_LITERAL:
        case TokenType::STRING_LITERAL:
            return true;
        default:
            return false;
    }
}

/**
 * @brief Utility to check if a token type is a known "type keyword"
 */
bool Parser::isTypeKeyword(TokenType tt) const
{
    return (tt == TokenType::KW_INT ||
            tt == TokenType::KW_FLOAT ||
            tt == TokenType::KW_CHAR ||
            tt == TokenType::KW_DOUBLE ||
            tt == TokenType::KW_VOID);
}
