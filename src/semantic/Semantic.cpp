/**
 * @file Semantic.cpp
 * @brief Implementation of the SemanticAnalyzer class.
 */

#include "semantic/Semantic.h"

///////////////////////////////////////////////////////////////////////////
// Constructor
///////////////////////////////////////////////////////////////////////////

SemanticAnalyzer::SemanticAnalyzer(TranslationUnitNode &root)
    : m_root(root)
{
}

///////////////////////////////////////////////////////////////////////////
// Public analyze() entry point
///////////////////////////////////////////////////////////////////////////

void SemanticAnalyzer::analyze()
{
    // Start with a global symbol table
    auto globalSymTable = std::make_shared<SymbolTable>();

    // Analyze the translation unit
    analyzeTranslationUnit(m_root, globalSymTable);
}

///////////////////////////////////////////////////////////////////////////
// Helper: Error
///////////////////////////////////////////////////////////////////////////

void SemanticAnalyzer::semanticError(ASTNode &node, const std::string &msg)
{
    std::string fullMsg = "[Semantic Error] " + msg + 
                          " at line " + std::to_string(node.line()) +
                          ", column " + std::to_string(node.column()) + ".";
    throw std::runtime_error(fullMsg);
}

///////////////////////////////////////////////////////////////////////////
// analyzeTranslationUnit
///////////////////////////////////////////////////////////////////////////

void SemanticAnalyzer::analyzeTranslationUnit(TranslationUnitNode &unit,
                                              std::shared_ptr<SymbolTable> symTable)
{
    // For each top-level declaration, analyze
    for (auto &declPtr : unit.declarations()) {
        analyzeNode(*declPtr, symTable);
    }
}

///////////////////////////////////////////////////////////////////////////
// analyzeNode: dispatch by kind
///////////////////////////////////////////////////////////////////////////

void SemanticAnalyzer::analyzeNode(ASTNode &node, std::shared_ptr<SymbolTable> symTable)
{
    switch (node.kind()) {
        case ASTNodeKind::VarDecl:
            analyzeVarDecl(static_cast<VarDeclNode&>(node), symTable);
            break;
        case ASTNodeKind::FunctionDecl:
            analyzeFunctionDecl(static_cast<FunctionDeclNode&>(node), symTable);
            break;
        case ASTNodeKind::FunctionDef:
            analyzeFunctionDef(static_cast<FunctionDefNode&>(node), symTable);
            break;
        case ASTNodeKind::CompoundStmt:
            analyzeCompoundStmt(static_cast<CompoundStmtNode&>(node), symTable);
            break;
        case ASTNodeKind::IfStmt:
            analyzeIfStmt(static_cast<IfStmtNode&>(node), symTable);
            break;
        case ASTNodeKind::WhileStmt:
            analyzeWhileStmt(static_cast<WhileStmtNode&>(node), symTable);
            break;
        case ASTNodeKind::ForStmt:
            analyzeForStmt(static_cast<ForStmtNode&>(node), symTable);
            break;
        case ASTNodeKind::ReturnStmt:
            analyzeReturnStmt(static_cast<ReturnStmtNode&>(node), symTable);
            break;
        case ASTNodeKind::ExprStmt:
            analyzeExprStmt(static_cast<ExprStmtNode&>(node), symTable);
            break;
        // Expressions handled either inside statements or by direct calls:
        case ASTNodeKind::BinaryExpr:
        case ASTNodeKind::UnaryExpr:
        case ASTNodeKind::CallExpr:
        case ASTNodeKind::LiteralExpr:
        case ASTNodeKind::IdentifierExpr:
            // It's unlikely we get an expression node at top-level, but let's handle anyway
            analyzeExpr(static_cast<ExprNode&>(node), symTable);
            break;
        case ASTNodeKind::TranslationUnit:
            // Already handled in parseTranslationUnit
            break;
        default:
            // Unrecognized node
            semanticError(node, "Unrecognized AST node kind");
    }
}

///////////////////////////////////////////////////////////////////////////
// Declarations
///////////////////////////////////////////////////////////////////////////

void SemanticAnalyzer::analyzeVarDecl(VarDeclNode &node, std::shared_ptr<SymbolTable> symTable)
{
    // Insert symbol into current scope
    Symbol sym(SymbolKind::Variable, node.varName(), node.typeName());
    try {
        symTable->addSymbol(sym);
    } catch (const std::runtime_error &e) {
        semanticError(node, e.what());
    }
}

void SemanticAnalyzer::analyzeFunctionDecl(FunctionDeclNode &node, std::shared_ptr<SymbolTable> symTable)
{
    // Insert function symbol
    Symbol sym(SymbolKind::Function, node.funcName(), node.returnType());
    try {
        symTable->addSymbol(sym);
    } catch (const std::runtime_error &e) {
        semanticError(node, e.what());
    }
    // We won't do full param checks here; that can happen in function def.
}

void SemanticAnalyzer::analyzeFunctionDef(FunctionDefNode &node, std::shared_ptr<SymbolTable> symTable)
{
    // The child function decl
    auto decl = node.decl();
    if (!decl) {
        semanticError(node, "FunctionDefNode has null decl()");
    }
    // Insert the function symbol in the global table
    Symbol funcSym(SymbolKind::Function, decl->funcName(), decl->returnType());
    try {
        symTable->addSymbol(funcSym);
    } catch (const std::runtime_error &e) {
        semanticError(node, e.what());
    }

    // Create a new scope for function parameters & local variables
    auto funcScope = std::make_shared<SymbolTable>(symTable);

    // Add parameters
    for (size_t i = 0; i < decl->paramCount(); i++) {
        Symbol paramSym(SymbolKind::Variable, decl->paramName(i), decl->paramType(i));
        try {
            funcScope->addSymbol(paramSym);
        } catch (const std::runtime_error &e) {
            semanticError(node, e.what());
        }
    }

    // analyze the body (which is a statement, typically a CompoundStmt)
    if (!node.body()) {
        semanticError(node, "Function definition has no body");
    }
    analyzeNode(*(const_cast<ASTNode*>(node.body())), funcScope);
}

///////////////////////////////////////////////////////////////////////////
// Statements
///////////////////////////////////////////////////////////////////////////

void SemanticAnalyzer::analyzeCompoundStmt(CompoundStmtNode &node,
                                           std::shared_ptr<SymbolTable> symTable)
{
    // new scope for local declarations
    auto blockScope = std::make_shared<SymbolTable>(symTable);

    // analyze each item
    for (auto &item : node.items()) {
        analyzeNode(*item, blockScope);
    }
}

void SemanticAnalyzer::analyzeIfStmt(IfStmtNode &node, std::shared_ptr<SymbolTable> symTable)
{
    // analyze condition as an expression
    if (node.condition()) {
        analyzeExpr(*const_cast<ExprNode*>((const ExprNode*)node.condition()), symTable);
    }
    // then branch
    if (node.thenBranch()) {
        analyzeNode(*(ASTNode*)node.thenBranch(), symTable);
    }
    // else branch
    if (node.elseBranch()) {
        analyzeNode(*(ASTNode*)node.elseBranch(), symTable);
    }
}

void SemanticAnalyzer::analyzeWhileStmt(WhileStmtNode &node,
                                        std::shared_ptr<SymbolTable> symTable)
{
    // analyze condition
    if (node.condition()) {
        analyzeExpr(*const_cast<ExprNode*>((const ExprNode*)node.condition()), symTable);
    }
    // analyze body
    if (node.body()) {
        analyzeNode(*(ASTNode*)node.body(), symTable);
    }
}

void SemanticAnalyzer::analyzeForStmt(ForStmtNode &node,
                                      std::shared_ptr<SymbolTable> symTable)
{
    // for statements create a new scope for their init or loop variables
    auto forScope = std::make_shared<SymbolTable>(symTable);

    // init
    if (node.init()) {
        analyzeNode(*(ASTNode*)node.init(), forScope);
    }
    // cond
    if (node.condition()) {
        analyzeExpr(*const_cast<ExprNode*>((const ExprNode*)node.condition()), forScope);
    }
    // incr
    if (node.increment()) {
        analyzeExpr(*const_cast<ExprNode*>((const ExprNode*)node.increment()), forScope);
    }
    // body
    if (node.body()) {
        analyzeNode(*(ASTNode*)node.body(), forScope);
    }
}

void SemanticAnalyzer::analyzeReturnStmt(ReturnStmtNode &node,
                                         std::shared_ptr<SymbolTable> symTable)
{
    // If there's an expression, analyze it
    if (node.expr()) {
        analyzeExpr(*const_cast<ExprNode*>((const ExprNode*)node.expr()), symTable);
    }
    // Real type checking would confirm that the returned expression
    // matches the function's return type, but we haven't tracked the
    // function’s declared type in the symbol table scope in detail here.
}

void SemanticAnalyzer::analyzeExprStmt(ExprStmtNode &node,
                                       std::shared_ptr<SymbolTable> symTable)
{
    // If there's an expression, analyze it
    if (node.expr()) {
        analyzeExpr(*const_cast<ExprNode*>((const ExprNode*)node.expr()), symTable);
    }
}

///////////////////////////////////////////////////////////////////////////
// Expression Analysis
///////////////////////////////////////////////////////////////////////////

void SemanticAnalyzer::analyzeExpr(ExprNode &expr, std::shared_ptr<SymbolTable> symTable)
{
    switch (expr.kind()) {
        case ASTNodeKind::BinaryExpr:
            analyzeBinaryExpr(static_cast<BinaryExprNode&>(expr), symTable);
            break;
        case ASTNodeKind::UnaryExpr:
            analyzeUnaryExpr(static_cast<UnaryExprNode&>(expr), symTable);
            break;
        case ASTNodeKind::CallExpr:
            analyzeCallExpr(static_cast<CallExprNode&>(expr), symTable);
            break;
        case ASTNodeKind::LiteralExpr:
            analyzeLiteralExpr(static_cast<LiteralExprNode&>(expr), symTable);
            break;
        case ASTNodeKind::IdentifierExpr:
            analyzeIdentifierExpr(static_cast<IdentifierExprNode&>(expr), symTable);
            break;
        default:
            semanticError(expr, "Unknown expression type");
    }
}

void SemanticAnalyzer::analyzeBinaryExpr(BinaryExprNode &expr, std::shared_ptr<SymbolTable> symTable)
{
    // left
    if (expr.left()) {
        analyzeExpr(*const_cast<ExprNode*>((const ExprNode*)expr.left()), symTable);
    }
    // right
    if (expr.right()) {
        analyzeExpr(*const_cast<ExprNode*>((const ExprNode*)expr.right()), symTable);
    }
    // Type checks could go here
}

void SemanticAnalyzer::analyzeUnaryExpr(UnaryExprNode &expr, std::shared_ptr<SymbolTable> symTable)
{
    if (expr.operand()) {
        analyzeExpr(*const_cast<ExprNode*>((const ExprNode*)expr.operand()), symTable);
    }
    // Check operator validity, e.g. can't increment a string literal, etc.
}

void SemanticAnalyzer::analyzeCallExpr(CallExprNode &expr, std::shared_ptr<SymbolTable> symTable)
{
    // Check if function name is declared
    auto sym = symTable->lookup(expr.callee());
    if (!sym) {
        semanticError(expr, "Undefined function '" + expr.callee() + "'");
    } else if (sym->kind != SymbolKind::Function) {
        semanticError(expr, "'" + expr.callee() + "' is not a function");
    }

    // analyze each argument
    for (auto &arg : expr.args()) {
        analyzeExpr(*const_cast<ExprNode*>((const ExprNode*)arg.get()), symTable);
    }
    // Real type checking would compare argument types vs. function param types
}

void SemanticAnalyzer::analyzeLiteralExpr(LiteralExprNode &expr, std::shared_ptr<SymbolTable> symTable)
{
    // Nothing special for basic literal
    (void) symTable;
}

void SemanticAnalyzer::analyzeIdentifierExpr(IdentifierExprNode &expr, std::shared_ptr<SymbolTable> symTable)
{
    // Check if this identifier is declared as a variable
    auto sym = symTable->lookup(expr.name());
    if (!sym) {
        semanticError(expr, "Undefined variable '" + expr.name() + "'");
    }
    // else, we have a valid variable or function. If it's a function, but used as a variable, that's an error
    if (sym->kind != SymbolKind::Variable) {
        semanticError(expr, "'" + sym->name + "' is not a variable");
    }
}
