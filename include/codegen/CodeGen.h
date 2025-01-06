#ifndef CODEGEN_H
#define CODEGEN_H

#include "EduCC.h"
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>

#include "ast/AST.h"      // so we can see DeclStmtNode, etc.
#include <string>
#include <unordered_map>
#include <stack>

/**
 * @brief A small helper struct for codegen block scopes.
 */
struct CodeGenBlock
{
    llvm::BasicBlock *block;
    // A map from variable name -> alloca instruction
    std::unordered_map<std::string, llvm::AllocaInst*> locals;

    CodeGenBlock(llvm::BasicBlock *bb)
        : block(bb) {}
};

class CodeGenerator
{
public:
    // Constructor / Destructor
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
    // BasicBlock stack
    std::stack<CodeGenBlock*> m_blocks;
    llvm::LLVMContext m_context;
    std::unique_ptr<llvm::Module> m_module;
    llvm::IRBuilder<> m_builder;

    // AST Generation
    void generateTranslationUnit(TranslationUnitNode &unit);
    void generateNode(ASTNode &node);

    // Declaration code
    void generateVarDecl(VarDeclNode &node);

    /**
     * @brief Generates code for a local variable declaration statement (DeclStmtNode).
     *        Allocates stack memory (alloca) and optionally initializes it.
     */
    void generateDeclStmt(DeclStmtNode &node);  //<--- ADD THIS LINE

    // Statement code
    void generateFunctionDef(FunctionDefNode &node);
    void generateCompoundStmt(CompoundStmtNode &node);
    void generateIfStmt(IfStmtNode &node);
    void generateWhileStmt(WhileStmtNode &node);
    void generateForStmt(ForStmtNode &node);
    void generateReturnStmt(ReturnStmtNode &node);
    void generateExprStmt(ExprStmtNode &node);

    // Expression code
    llvm::Value* generateExpr(ExprNode &expr);
    llvm::Value* generateBinaryExpr(BinaryExprNode &expr);
    llvm::Value* generateUnaryExpr(UnaryExprNode &expr);
    llvm::Value* generateCallExpr(CallExprNode &expr);
    llvm::Value* generateLiteralExpr(LiteralExprNode &expr);
    llvm::Value* generateIdentifierExpr(IdentifierExprNode &expr);

    // Helpers
    llvm::Type* getLLVMType(const std::string &typeName);

    CodeGenBlock* currentBlock();
    void pushBlock(llvm::BasicBlock *block);
    void popBlock();

    // Load/Store
    llvm::Value* getVariableValue(const std::string &name);
    void setVariableValue(const std::string &name, llvm::Value* value);
};

#endif // CODEGEN_H
