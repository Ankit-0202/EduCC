/**
 * @file CodeGen.h
 * @brief Declaration of the CodeGenerator class for generating LLVM IR.
 */

#ifndef EDUCC_CODEGEN_H
#define EDUCC_CODEGEN_H

#include "EduCC.h"
#include "ast/AST.h"
#include "semantic/SymbolTable.h"

// LLVM headers:
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"

#include <memory>
#include <string>
#include <unordered_map>
#include <stack>

/**
 * @class CodeGenBlock
 * @brief Represents a scope/block during codegen. Tracks local variables, etc.
 */
class CodeGenBlock
{
public:
    // An LLVM BasicBlock*
    llvm::BasicBlock *block;
    // A map from variable name -> alloca instruction
    std::unordered_map<std::string, llvm::AllocaInst*> locals;

    CodeGenBlock(llvm::BasicBlock *bb)
        : block(bb) {}
};

/**
 * @class CodeGenerator
 * @brief Responsible for converting the AST into LLVM IR.
 *
 * Usage:
 *   1) Construct CodeGenerator with a module name.
 *   2) call generateCode(ASTNode&, outputFilename).
 *   3) The IR is emitted to 'outputFilename' in textual form (.ll).
 */
class CodeGenerator
{
public:
    CodeGenerator(const std::string &moduleName);
    ~CodeGenerator();

    /**
     * @brief Main entry point to generate code from an AST root (TranslationUnit).
     *
     * @param root A reference to the root node of the AST (TranslationUnitNode).
     * @param outputFilename The name of the file to which we write the .ll output.
     */
    void generateCode(TranslationUnitNode &root, const std::string &outputFilename);

private:
    // The core LLVM machinery
    llvm::LLVMContext m_context;
    std::unique_ptr<llvm::Module> m_module;
    llvm::IRBuilder<> m_builder;

    // A stack of CodeGenBlock scopes
    std::stack<CodeGenBlock*> m_blocks;

    // Helpers to manage the current block
    CodeGenBlock *currentBlock();
    void pushBlock(llvm::BasicBlock *block);
    void popBlock();

    // Utility: get an LLVM type from a string (e.g., "int" -> i32).
    llvm::Type* getLLVMType(const std::string &typeName);

    // AST visitors / codegen methods:
    void generateTranslationUnit(TranslationUnitNode &unit);
    void generateNode(ASTNode &node);
    void generateFunctionDef(FunctionDefNode &node);
    void generateVarDecl(VarDeclNode &node);
    void generateCompoundStmt(CompoundStmtNode &node);
    void generateIfStmt(IfStmtNode &node);
    void generateWhileStmt(WhileStmtNode &node);
    void generateForStmt(ForStmtNode &node);
    void generateReturnStmt(ReturnStmtNode &node);
    void generateExprStmt(ExprStmtNode &node);

    // Expression codegen -> returns llvm::Value*
    llvm::Value* generateExpr(ExprNode &expr);
    llvm::Value* generateBinaryExpr(BinaryExprNode &expr);
    llvm::Value* generateUnaryExpr(UnaryExprNode &expr);
    llvm::Value* generateCallExpr(CallExprNode &expr);
    llvm::Value* generateLiteralExpr(LiteralExprNode &expr);
    llvm::Value* generateIdentifierExpr(IdentifierExprNode &expr);

    // Helpers for variable load/store
    llvm::Value* getVariableValue(const std::string &name);
    void setVariableValue(const std::string &name, llvm::Value* value);
};

#endif // EDUCC_CODEGEN_H
