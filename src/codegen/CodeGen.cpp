/**
 * @file CodeGen.cpp
 * @brief Implementation of the CodeGenerator class for generating LLVM IR from the AST.
 *
 * This file defines the logic that converts AST nodes into LLVM IR instructions
 * via LLVM's C++ API. It supports:
 *   - Global variable declarations (VarDeclNode)
 *   - Local variable declarations (DeclStmtNode)
 *   - Function definitions (FunctionDefNode)
 *   - Compound statements, if, while, for, return, expression statements
 *   - Basic expressions (binary, unary, calls, literals, identifiers)
 */

#include "codegen/CodeGen.h"
#include "ast/AST.h"

#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"

#include <iostream>

/**
 * @brief Constructor for CodeGenerator.
 * @param moduleName Name of the LLVM module to create.
 */
CodeGenerator::CodeGenerator(const std::string &moduleName)
    : m_context(),
      m_module(std::make_unique<llvm::Module>(moduleName, m_context)),
      m_builder(m_context)
{
}

/**
 * @brief Destructor for CodeGenerator. Cleans up any remaining blocks.
 */
CodeGenerator::~CodeGenerator()
{
    // Clean up leftover blocks if any remain
    while (!m_blocks.empty()) {
        CodeGenBlock *top = m_blocks.top();
        m_blocks.pop();
        delete top;
    }
}

/**
 * @brief Generates LLVM IR for the entire AST and writes it to @p outputFilename.
 * @param root The root TranslationUnitNode of the AST.
 * @param outputFilename The .ll file to write the IR into.
 */
void CodeGenerator::generateCode(TranslationUnitNode &root, const std::string &outputFilename)
{
    // Clear any existing blocks
    while (!m_blocks.empty()) {
        CodeGenBlock *top = m_blocks.top();
        m_blocks.pop();
        delete top;
    }

    // 1) Generate IR from AST
    generateTranslationUnit(root);

    // 2) Verify the module
    if (llvm::verifyModule(*m_module, &llvm::errs())) {
        std::cerr << "Error: module failed verification. IR might be invalid.\n";
    }

    // 3) Write IR to file
    std::error_code EC;
    llvm::raw_fd_ostream dest(outputFilename, EC, llvm::sys::fs::OF_None);
    if (EC) {
        std::cerr << "Could not open file: " << outputFilename
                  << " Error: " << EC.message() << std::endl;
        return;
    }
    m_module->print(dest, nullptr);
    dest.flush();

    std::cout << "LLVM IR generated and written to " << outputFilename << std::endl;
}

// -----------------------------------------------------------------------------
// Basic Block Stack Helpers
// -----------------------------------------------------------------------------

/**
 * @brief Retrieves the top CodeGenBlock, representing the current local scope.
 * @return Pointer to the current block, or nullptr if none exist.
 */
CodeGenBlock* CodeGenerator::currentBlock()
{
    if (m_blocks.empty()) return nullptr;
    return m_blocks.top();
}

/**
 * @brief Pushes a new block scope onto the stack, updating the IRBuilder's insertion point.
 * @param block The LLVM BasicBlock to become the new insertion point.
 */
void CodeGenerator::pushBlock(llvm::BasicBlock *block)
{
    CodeGenBlock *newBlock = new CodeGenBlock(block);
    m_blocks.push(newBlock);
    m_builder.SetInsertPoint(block);
}

/**
 * @brief Pops the top block from the stack, deleting it and restoring the previous insertion point.
 */
void CodeGenerator::popBlock()
{
    if (m_blocks.empty()) return;

    CodeGenBlock *top = m_blocks.top();
    m_blocks.pop();
    delete top;

    if (!m_blocks.empty()) {
        m_builder.SetInsertPoint(m_blocks.top()->block);
    }
}

// -----------------------------------------------------------------------------
// Type Conversion Helper
// -----------------------------------------------------------------------------

/**
 * @brief Maps a C type name (e.g. "int") to the corresponding LLVM Type*.
 * @param typeName The textual type name from the VarDecl/FunctionDecl.
 * @return The corresponding LLVM Type*, or i32 if the type is unrecognized.
 */
llvm::Type* CodeGenerator::getLLVMType(const std::string &typeName)
{
    if (typeName == "int") {
        return llvm::Type::getInt32Ty(m_context);
    }
    else if (typeName == "float") {
        return llvm::Type::getFloatTy(m_context);
    }
    else if (typeName == "double") {
        return llvm::Type::getDoubleTy(m_context);
    }
    else if (typeName == "char") {
        return llvm::Type::getInt8Ty(m_context);
    }
    else if (typeName == "void") {
        return llvm::Type::getVoidTy(m_context);
    }
    // Default fallback: i32
    return llvm::Type::getInt32Ty(m_context);
}

// -----------------------------------------------------------------------------
// ASTNode Generation Entry Points
// -----------------------------------------------------------------------------

/**
 * @brief Generates code for the entire translation unit (global scope).
 * @param unit The TranslationUnitNode containing global declarations.
 */
void CodeGenerator::generateTranslationUnit(TranslationUnitNode &unit)
{
    for (auto &decl : unit.declarations()) {
        generateNode(*decl);
    }
}

/**
 * @brief Dispatch method: calls the appropriate generateXXX function based on node.kind().
 * @param node The AST node to generate IR for.
 */
void CodeGenerator::generateNode(ASTNode &node)
{
    switch (node.kind()) {
        case ASTNodeKind::FunctionDef:
            generateFunctionDef(static_cast<FunctionDefNode&>(node));
            break;

        case ASTNodeKind::VarDecl:
            // Global variable
            generateVarDecl(static_cast<VarDeclNode&>(node));
            break;

        case ASTNodeKind::CompoundStmt:
            generateCompoundStmt(static_cast<CompoundStmtNode&>(node));
            break;

        case ASTNodeKind::IfStmt:
            generateIfStmt(static_cast<IfStmtNode&>(node));
            break;

        case ASTNodeKind::WhileStmt:
            generateWhileStmt(static_cast<WhileStmtNode&>(node));
            break;

        case ASTNodeKind::ForStmt:
            generateForStmt(static_cast<ForStmtNode&>(node));
            break;

        case ASTNodeKind::ReturnStmt:
            generateReturnStmt(static_cast<ReturnStmtNode&>(node));
            break;

        case ASTNodeKind::ExprStmt:
            generateExprStmt(static_cast<ExprStmtNode&>(node));
            break;

        // NEW: local variable declaration statement
        case ASTNodeKind::DeclStmt:
            generateDeclStmt(static_cast<DeclStmtNode&>(node));
            break;

        default:
            // Possibly an expression node or unhandled type
            if (node.kind() == ASTNodeKind::BinaryExpr ||
                node.kind() == ASTNodeKind::UnaryExpr ||
                node.kind() == ASTNodeKind::CallExpr ||
                node.kind() == ASTNodeKind::LiteralExpr ||
                node.kind() == ASTNodeKind::IdentifierExpr)
            {
                generateExpr(static_cast<ExprNode&>(node));
            }
            else {
                // e.g. FunctionDecl or unknown node
                // We can safely ignore or handle error
            }
    }
}

// -----------------------------------------------------------------------------
// Global Variables
// -----------------------------------------------------------------------------

/**
 * @brief Generates a global variable declaration (VarDeclNode).
 * @note The parser emits VarDeclNode for global vars.
 * @param node The VarDeclNode to generate code for.
 */
void CodeGenerator::generateVarDecl(VarDeclNode &node)
{
    llvm::Type *varType = getLLVMType(node.typeName());
    auto *gvar = new llvm::GlobalVariable(
        *m_module,
        varType,
        /*isConstant=*/ false,
        llvm::GlobalValue::ExternalLinkage,
        /*init=*/ nullptr,
        node.varName()
    );
    // Initialize to 0 for now
    llvm::Constant *initVal = llvm::Constant::getNullValue(varType);
    gvar->setInitializer(initVal);
}

// -----------------------------------------------------------------------------
// NEW: Local Var Declaration Stmt
// -----------------------------------------------------------------------------

/**
 * @brief Generates code for a local var declaration statement (DeclStmtNode).
 *        Allocates stack space (alloca) and optionally initializes it.
 * @param node The DeclStmtNode with a VarDeclNode + optional initializer.
 */
void CodeGenerator::generateDeclStmt(DeclStmtNode &node)
{
    // 1) Create an alloca for the variable in the current function
    llvm::Type *varType = getLLVMType(node.varDecl()->typeName());
    llvm::AllocaInst *allocaInst = m_builder.CreateAlloca(
        varType, nullptr, node.varDecl()->varName()
    );

    // 2) Store the alloca in the current scope's locals
    if (currentBlock()) {
        currentBlock()->locals[node.varDecl()->varName()] = allocaInst;
    }

    // 3) If there's an initializer, generate code for it and store the result
    if (node.initExpr()) {
        auto initExpr = dynamic_cast<ExprNode*>(
            const_cast<ASTNode*>(node.initExpr())
        );
        if (initExpr) {
            llvm::Value *initVal = generateExpr(*initExpr);
            if (initVal) {
                m_builder.CreateStore(initVal, allocaInst);
            }
        }
    }
}

// -----------------------------------------------------------------------------
// Function Definitions
// -----------------------------------------------------------------------------

/**
 * @brief Generates code for a function definition (FunctionDefNode).
 *        Creates a function in the LLVM module, sets up parameters,
 *        and generates code for the function body.
 * @param node The FunctionDefNode describing the function.
 */
void CodeGenerator::generateFunctionDef(FunctionDefNode &node)
{
    // Retrieve the function declaration info
    const auto *decl = node.decl();
    if (!decl) return; // or handle error

    llvm::Type *retType = getLLVMType(decl->returnType());
    std::vector<llvm::Type*> paramTypes;

    for (size_t i = 0; i < decl->paramCount(); i++) {
        paramTypes.push_back(getLLVMType(decl->paramType(i)));
    }

    auto *funcType = llvm::FunctionType::get(retType, paramTypes, /*isVarArg=*/false);
    llvm::Function *function = llvm::Function::Create(
        funcType,
        llvm::GlobalValue::ExternalLinkage,
        decl->funcName(),
        m_module.get()
    );

    // Name parameters
    unsigned idx = 0;
    for (auto &arg : function->args()) {
        arg.setName(decl->paramName(idx));
        idx++;
    }

    // Create an entry block in the function
    auto *entryBlock = llvm::BasicBlock::Create(m_context, "entry", function);
    pushBlock(entryBlock);

    // For each parameter, create an alloca and store the param
    idx = 0;
    for (auto &arg : function->args()) {
        llvm::AllocaInst *alloca = m_builder.CreateAlloca(arg.getType(), nullptr, arg.getName());
        m_builder.CreateStore(&arg, alloca);

        // Add to locals map
        currentBlock()->locals[arg.getName().str()] = alloca;
        idx++;
    }

    // Generate the body
    if (node.body()) {
        generateNode(*(ASTNode*)node.body());
    }

    // If no return was generated, insert a default ret
    if (retType->isVoidTy()) {
        if (!entryBlock->getTerminator()) {
            m_builder.CreateRetVoid();
        }
    } else {
        if (!entryBlock->getTerminator()) {
            auto *defaultVal = llvm::ConstantInt::get(retType, 0);
            m_builder.CreateRet(defaultVal);
        }
    }

    popBlock();
}

// -----------------------------------------------------------------------------
// Compound Statement
// -----------------------------------------------------------------------------

/**
 * @brief Generates code for a compound statement (block). Creates a new
 *        logical scope, but reuses the same BasicBlock unless you need
 *        separate jump targets.
 * @param node The CompoundStmtNode.
 */
void CodeGenerator::generateCompoundStmt(CompoundStmtNode &node)
{
    llvm::BasicBlock *currentBB = m_builder.GetInsertBlock();
    // push a block scope with the same BB
    pushBlock(currentBB);

    for (auto &item : node.items()) {
        generateNode(*item);
        // If a terminator (e.g. return) was inserted, we can break early
        if (currentBB->getTerminator()) {
            break;
        }
    }

    popBlock();
}

// -----------------------------------------------------------------------------
// If Statement
// -----------------------------------------------------------------------------

void CodeGenerator::generateIfStmt(IfStmtNode &node)
{
    // Evaluate condition -> i1
    llvm::Value *condVal = nullptr;
    if (node.condition()) {
        condVal = generateExpr(*(ExprNode*)node.condition());
    }
    if (!condVal) {
        condVal = llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_context), 0);
    }
    if (condVal->getType()->isIntegerTy(32)) {
        condVal = m_builder.CreateICmpNE(
            condVal,
            llvm::ConstantInt::get(condVal->getType(), 0),
            "ifcond"
        );
    }

    llvm::Function *function = m_builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *thenBB  = llvm::BasicBlock::Create(m_context, "then", function);
    llvm::BasicBlock *elseBB  = llvm::BasicBlock::Create(m_context, "else", function);
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(m_context, "ifend", function);

    m_builder.CreateCondBr(condVal, thenBB, elseBB);

    // then block
    m_builder.SetInsertPoint(thenBB);
    pushBlock(thenBB);
    if (node.thenBranch()) {
        generateNode(*(ASTNode*)node.thenBranch());
    }
    bool hasThenTerm = (thenBB->getTerminator() != nullptr);
    popBlock();
    if (!hasThenTerm) {
        m_builder.CreateBr(mergeBB);
    }

    // else block
    m_builder.SetInsertPoint(elseBB);
    pushBlock(elseBB);
    if (node.elseBranch()) {
        generateNode(*(ASTNode*)node.elseBranch());
    }
    bool hasElseTerm = (elseBB->getTerminator() != nullptr);
    popBlock();
    if (!hasElseTerm) {
        m_builder.CreateBr(mergeBB);
    }

    // merge
    m_builder.SetInsertPoint(mergeBB);
}

// -----------------------------------------------------------------------------
// While Statement
// -----------------------------------------------------------------------------

void CodeGenerator::generateWhileStmt(WhileStmtNode &node)
{
    llvm::Function *function = m_builder.GetInsertBlock()->getParent();

    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(m_context, "whilecond", function);
    llvm::BasicBlock *loopBB = llvm::BasicBlock::Create(m_context, "whileloop", function);
    llvm::BasicBlock *exitBB = llvm::BasicBlock::Create(m_context, "whileexit", function);

    m_builder.CreateBr(condBB);

    // cond
    m_builder.SetInsertPoint(condBB);
    pushBlock(condBB);

    llvm::Value *condVal = nullptr;
    if (node.condition()) {
        condVal = generateExpr(*(ExprNode*)node.condition());
    }
    if (!condVal) {
        condVal = llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_context), 1);
    }
    if (condVal->getType()->isIntegerTy(32)) {
        condVal = m_builder.CreateICmpNE(
            condVal,
            llvm::ConstantInt::get(condVal->getType(), 0),
            "whilecond"
        );
    }
    m_builder.CreateCondBr(condVal, loopBB, exitBB);
    popBlock();

    // loop
    m_builder.SetInsertPoint(loopBB);
    pushBlock(loopBB);
    if (node.body()) {
        generateNode(*(ASTNode*)node.body());
    }
    if (!loopBB->getTerminator()) {
        m_builder.CreateBr(condBB);
    }
    popBlock();

    // exit
    m_builder.SetInsertPoint(exitBB);
}

// -----------------------------------------------------------------------------
// For Statement
// -----------------------------------------------------------------------------

void CodeGenerator::generateForStmt(ForStmtNode &node)
{
    llvm::Function *function = m_builder.GetInsertBlock()->getParent();

    llvm::BasicBlock *initBB = llvm::BasicBlock::Create(m_context, "forinit", function);
    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(m_context, "forcond", function);
    llvm::BasicBlock *loopBB = llvm::BasicBlock::Create(m_context, "forloop", function);
    llvm::BasicBlock *incrBB = llvm::BasicBlock::Create(m_context, "forincr", function);
    llvm::BasicBlock *exitBB = llvm::BasicBlock::Create(m_context, "forexit", function);

    m_builder.CreateBr(initBB);

    // init
    m_builder.SetInsertPoint(initBB);
    pushBlock(initBB);
    if (node.init()) {
        generateNode(*(ASTNode*)node.init());
    }
    if (!initBB->getTerminator()) {
        m_builder.CreateBr(condBB);
    }
    popBlock();

    // cond
    m_builder.SetInsertPoint(condBB);
    pushBlock(condBB);

    llvm::Value *condVal = nullptr;
    if (node.condition()) {
        condVal = generateExpr(*(ExprNode*)node.condition());
    }
    if (!condVal) {
        condVal = llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_context), 1);
    }
    if (condVal->getType()->isIntegerTy(32)) {
        condVal = m_builder.CreateICmpNE(
            condVal,
            llvm::ConstantInt::get(condVal->getType(), 0),
            "forcond"
        );
    }
    m_builder.CreateCondBr(condVal, loopBB, exitBB);
    popBlock();

    // loop
    m_builder.SetInsertPoint(loopBB);
    pushBlock(loopBB);
    if (node.body()) {
        generateNode(*(ASTNode*)node.body());
    }
    if (!loopBB->getTerminator()) {
        m_builder.CreateBr(incrBB);
    }
    popBlock();

    // incr
    m_builder.SetInsertPoint(incrBB);
    pushBlock(incrBB);
    if (node.increment()) {
        generateExpr(*(ExprNode*)node.increment());
    }
    if (!incrBB->getTerminator()) {
        m_builder.CreateBr(condBB);
    }
    popBlock();

    // exit
    m_builder.SetInsertPoint(exitBB);
}

// -----------------------------------------------------------------------------
// Return Statement
// -----------------------------------------------------------------------------

void CodeGenerator::generateReturnStmt(ReturnStmtNode &node)
{
    llvm::Value *retVal = nullptr;
    if (node.expr()) {
        retVal = generateExpr(*(ExprNode*)node.expr());
    }
    if (!retVal) {
        // For a void function, do "ret void"
        m_builder.CreateRetVoid();
    } else {
        m_builder.CreateRet(retVal);
    }
}

// -----------------------------------------------------------------------------
// Expression Statement
// -----------------------------------------------------------------------------

void CodeGenerator::generateExprStmt(ExprStmtNode &node)
{
    if (node.expr()) {
        generateExpr(*(ExprNode*)node.expr());
    }
    // Discard result
}

// -----------------------------------------------------------------------------
// Expression Generation
// -----------------------------------------------------------------------------

llvm::Value* CodeGenerator::generateExpr(ExprNode &expr)
{
    switch (expr.kind()) {
        case ASTNodeKind::BinaryExpr:
            return generateBinaryExpr(static_cast<BinaryExprNode&>(expr));
        case ASTNodeKind::UnaryExpr:
            return generateUnaryExpr(static_cast<UnaryExprNode&>(expr));
        case ASTNodeKind::CallExpr:
            return generateCallExpr(static_cast<CallExprNode&>(expr));
        case ASTNodeKind::LiteralExpr:
            return generateLiteralExpr(static_cast<LiteralExprNode&>(expr));
        case ASTNodeKind::IdentifierExpr:
            return generateIdentifierExpr(static_cast<IdentifierExprNode&>(expr));
        default:
            // unexpected expression type
            return nullptr;
    }
}

// -----------------------------------------------------------------------------
// Binary Expression
// -----------------------------------------------------------------------------

llvm::Value* CodeGenerator::generateBinaryExpr(BinaryExprNode &expr)
{
    llvm::Value *L = generateExpr(*(ExprNode*)expr.left());
    llvm::Value *R = generateExpr(*(ExprNode*)expr.right());
    if (!L || !R) return nullptr;

    std::string op = expr.op();

    // assignment "a = b"
    if (op == "=") {
        auto *identifierLeft = dynamic_cast<IdentifierExprNode*>(
            const_cast<ASTNode*>(expr.left())
        );
        if (!identifierLeft) {
            // not a plain identifier => not handled in this example
            return nullptr;
        }
        // store R into the variable named by left
        setVariableValue(identifierLeft->name(), R);
        return R;
    }

    // i32 arithmetic
    if (L->getType()->isIntegerTy() && R->getType()->isIntegerTy()) {
        if (op == "+")  return m_builder.CreateAdd(L, R, "addtmp");
        if (op == "-")  return m_builder.CreateSub(L, R, "subtmp");
        if (op == "*")  return m_builder.CreateMul(L, R, "multmp");
        if (op == "/")  return m_builder.CreateSDiv(L, R, "divtmp");
        if (op == "%")  return m_builder.CreateSRem(L, R, "modtmp");
        if (op == "==") return m_builder.CreateICmpEQ(L, R, "eqtmp");
        if (op == "!=") return m_builder.CreateICmpNE(L, R, "netmp");
        if (op == "<")  return m_builder.CreateICmpSLT(L, R, "lttmp");
        if (op == ">")  return m_builder.CreateICmpSGT(L, R, "gttmp");
        if (op == "<=") return m_builder.CreateICmpSLE(L, R, "letmp");
        if (op == ">=") return m_builder.CreateICmpSGE(L, R, "getmp");
    }
    // If float/double, you'd do CreateFAdd, etc. Not shown.
    return nullptr;
}

// -----------------------------------------------------------------------------
// Unary Expression
// -----------------------------------------------------------------------------

llvm::Value* CodeGenerator::generateUnaryExpr(UnaryExprNode &expr)
{
    llvm::Value *operand = generateExpr(*(ExprNode*)expr.operand());
    if (!operand) return nullptr;

    std::string op = expr.op();
    if (op == "-") {
        if (operand->getType()->isIntegerTy()) {
            return m_builder.CreateNeg(operand, "negtmp");
        }
        // float/double => CreateFNeg
    }
    else if (op == "+") {
        // unary plus => no-op
        return operand;
    }
    else if (op == "++" || op == "--") {
        // pre-increment/decrement
        auto *identifier = dynamic_cast<IdentifierExprNode*>(
            const_cast<ASTNode*>(expr.operand())
        );
        if (!identifier) return nullptr;

        llvm::Value *val = getVariableValue(identifier->name());
        if (!val) return nullptr;

        llvm::Value *one = llvm::ConstantInt::get(val->getType(), 1);
        llvm::Value *res = (op == "++")
            ? m_builder.CreateAdd(val, one, "inc")
            : m_builder.CreateSub(val, one, "dec");
        setVariableValue(identifier->name(), res);
        return res;
    }
    return nullptr;
}

// -----------------------------------------------------------------------------
// Function Call Expression
// -----------------------------------------------------------------------------

llvm::Value* CodeGenerator::generateCallExpr(CallExprNode &expr)
{
    llvm::Function *calleeF = m_module->getFunction(expr.callee());
    if (!calleeF) {
        // function not found => error unless declared external
        return nullptr;
    }

    std::vector<llvm::Value*> argsV;
    for (auto &arg : expr.args()) {
        auto val = generateExpr(*(ExprNode*)arg.get());
        if (!val) return nullptr;
        argsV.push_back(val);
    }

    return m_builder.CreateCall(calleeF, argsV, "calltmp");
}

// -----------------------------------------------------------------------------
// Literal Expression
// -----------------------------------------------------------------------------

llvm::Value* CodeGenerator::generateLiteralExpr(LiteralExprNode &expr)
{
    const std::string &valStr = expr.value();
    // naive parse
    if (valStr.find('.') != std::string::npos) {
        // float parse
        float fVal = std::stof(valStr);
        return llvm::ConstantFP::get(llvm::Type::getFloatTy(m_context), fVal);
    } else {
        // integer parse
        int iVal = std::stoi(valStr);
        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_context), iVal);
    }
}

// -----------------------------------------------------------------------------
// Identifier Expression
// -----------------------------------------------------------------------------

llvm::Value* CodeGenerator::generateIdentifierExpr(IdentifierExprNode &expr)
{
    return getVariableValue(expr.name());
}

// -----------------------------------------------------------------------------
// Variable Load/Store Helpers
// -----------------------------------------------------------------------------

/**
 * @brief Retrieves the current value of the variable named @p name,
 *        searching local scopes outward, or a global variable if no local found.
 * @param name The variable name.
 * @return The loaded llvm::Value*, or nullptr if not found.
 */
llvm::Value* CodeGenerator::getVariableValue(const std::string &name)
{
    std::stack<CodeGenBlock*> tmpStack = m_blocks;
    while (!tmpStack.empty()) {
        CodeGenBlock *blk = tmpStack.top();
        tmpStack.pop();

        auto it = blk->locals.find(name);
        if (it != blk->locals.end()) {
            // load from the alloca
            return m_builder.CreateLoad(it->second->getAllocatedType(),
                                        it->second,
                                        name.c_str());
        }
    }

    // check if it's a global
    if (auto *gvar = m_module->getGlobalVariable(name)) {
        return m_builder.CreateLoad(gvar->getValueType(), gvar, name.c_str());
    }
    return nullptr;
}

/**
 * @brief Assigns @p value to the variable named @p name, searching local scopes or global.
 * @param name The variable name.
 * @param value The llvm::Value* to store.
 */
void CodeGenerator::setVariableValue(const std::string &name, llvm::Value* value)
{
    std::stack<CodeGenBlock*> tmpStack = m_blocks;
    while (!tmpStack.empty()) {
        CodeGenBlock *blk = tmpStack.top();
        tmpStack.pop();

        auto it = blk->locals.find(name);
        if (it != blk->locals.end()) {
            m_builder.CreateStore(value, it->second);
            return;
        }
    }

    // If it's global, store directly
    if (auto *gvar = m_module->getGlobalVariable(name)) {
        m_builder.CreateStore(value, gvar);
        return;
    }
    // not found => no-op or error
}
