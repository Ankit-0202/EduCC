/**
 * @file CodeGen.cpp
 * @brief Implementation of the CodeGenerator class for generating LLVM IR from the AST.
 */

#include "codegen/CodeGen.h"

#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"

#include <iostream>

///////////////////////////////////////////////////////////////////////////
// Constructor / Destructor
///////////////////////////////////////////////////////////////////////////

CodeGenerator::CodeGenerator(const std::string &moduleName)
    : m_context(),
      m_module(std::make_unique<llvm::Module>(moduleName, m_context)),
      m_builder(m_context)
{
}

CodeGenerator::~CodeGenerator()
{
    // Optionally do cleanup, though unique_ptr handles the module
}

///////////////////////////////////////////////////////////////////////////
// Public Interface
///////////////////////////////////////////////////////////////////////////

void CodeGenerator::generateCode(TranslationUnitNode &root, const std::string &outputFilename)
{
    // Clear any leftover blocks
    while (!m_blocks.empty()) {
        m_blocks.pop();
    }

    // Generate code
    generateTranslationUnit(root);

    // Validate the generated code
    if (llvm::verifyModule(*m_module, &llvm::errs())) {
        std::cerr << "Error: module failed verification. IR might be invalid.\n";
    }

    // Write to file
    std::error_code EC;
    llvm::raw_fd_ostream dest(outputFilename, EC, llvm::sys::fs::F_Text);
    if (EC) {
        std::cerr << "Could not open file: " << outputFilename << " Error: " << EC.message() << std::endl;
        return;
    }
    m_module->print(dest, nullptr);
    dest.flush();

    std::cout << "LLVM IR generated and written to " << outputFilename << std::endl;
}

///////////////////////////////////////////////////////////////////////////
// Basic Block Stack Helpers
///////////////////////////////////////////////////////////////////////////

CodeGenBlock *CodeGenerator::currentBlock()
{
    if (m_blocks.empty()) return nullptr;
    return m_blocks.top().get();
}

void CodeGenerator::pushBlock(llvm::BasicBlock *block)
{
    m_blocks.push(std::make_unique<CodeGenBlock>(block));
    m_builder.SetInsertPoint(block);
}

void CodeGenerator::popBlock()
{
    m_blocks.pop();
    if (!m_blocks.empty()) {
        m_builder.SetInsertPoint(m_blocks.top()->block);
    }
}

///////////////////////////////////////////////////////////////////////////
// Type Conversion Helper
///////////////////////////////////////////////////////////////////////////

llvm::Type* CodeGenerator::getLLVMType(const std::string &typeName)
{
    // Basic mapping for demonstration. Real C has far more nuance.
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
    // Default fallback: treat as int
    return llvm::Type::getInt32Ty(m_context);
}

///////////////////////////////////////////////////////////////////////////
// Top-Level AST: TranslationUnit
///////////////////////////////////////////////////////////////////////////

void CodeGenerator::generateTranslationUnit(TranslationUnitNode &unit)
{
    // For each global declaration (function def, global var, etc.), generate
    for (auto &decl : unit.declarations()) {
        generateNode(*decl);
    }
}

///////////////////////////////////////////////////////////////////////////
// generateNode: dispatch on ASTNodeKind
///////////////////////////////////////////////////////////////////////////

void CodeGenerator::generateNode(ASTNode &node)
{
    switch (node.kind()) {
        case ASTNodeKind::FunctionDef:
            generateFunctionDef(static_cast<FunctionDefNode&>(node));
            break;
        case ASTNodeKind::VarDecl:
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
        default:
            // If it’s an expression node, handle with generateExpr
            if (node.kind() == ASTNodeKind::BinaryExpr ||
                node.kind() == ASTNodeKind::UnaryExpr ||
                node.kind() == ASTNodeKind::CallExpr ||
                node.kind() == ASTNodeKind::LiteralExpr ||
                node.kind() == ASTNodeKind::IdentifierExpr)
            {
                generateExpr(static_cast<ExprNode&>(node));
            }
            else {
                // Possibly a FunctionDecl, etc. We might ignore or skip them.
                // Or handle them if you want function prototypes.
            }
    }
}

///////////////////////////////////////////////////////////////////////////
// Global Variables (VarDecl)
///////////////////////////////////////////////////////////////////////////

void CodeGenerator::generateVarDecl(VarDeclNode &node)
{
    // We'll treat a global VarDecl as a global variable with default init = 0.
    // For local variables, we do that in generateCompoundStmt or function body logic.
    llvm::Type *varType = getLLVMType(node.typeName());
    // Create a global variable in the module
    auto *gvar = new llvm::GlobalVariable(
        *m_module,
        varType,
        false,                             // isConstant?
        llvm::GlobalValue::ExternalLinkage,
        nullptr,                           // initializer
        node.varName()
    );
    // Default initializer -> 0 for now
    llvm::Constant *initVal = llvm::Constant::getNullValue(varType);
    gvar->setInitializer(initVal);
}

///////////////////////////////////////////////////////////////////////////
// Function Definitions
///////////////////////////////////////////////////////////////////////////

void CodeGenerator::generateFunctionDef(FunctionDefNode &node)
{
    // 1) Retrieve function declaration info
    const auto *decl = node.decl();
    if (!decl) return; // or handle error

    llvm::Type *retType = getLLVMType(decl->returnType());

    // Build the function's parameter type list
    std::vector<llvm::Type*> paramTypes;
    for (size_t i = 0; i < decl->paramCount(); i++) {
        paramTypes.push_back(getLLVMType(decl->paramType(i)));
    }

    // Create function type
    auto *funcType = llvm::FunctionType::get(retType, paramTypes, false);

    // Create the function in the module
    llvm::Function *function = llvm::Function::Create(
        funcType,
        llvm::GlobalValue::ExternalLinkage,
        decl->funcName(),
        m_module.get()
    );

    // Name the function parameters
    unsigned idx = 0;
    for (auto &arg : function->args()) {
        arg.setName(decl->paramName(idx));
        idx++;
    }

    // 2) Create a basic block to start insertion
    auto *entryBlock = llvm::BasicBlock::Create(m_context, "entry", function);
    pushBlock(entryBlock);

    // 3) For each parameter, create an alloca and store the param
    idx = 0;
    for (auto &arg : function->args()) {
        llvm::AllocaInst *alloca = m_builder.CreateAlloca(arg.getType(), nullptr, arg.getName());
        // store the function argument into the local variable
        m_builder.CreateStore(&arg, alloca);
        // add to locals map
        currentBlock()->locals[arg.getName().str()] = alloca;
        idx++;
    }

    // 4) Generate the body
    if (node.body()) {
        generateNode(*(ASTNode*)node.body());
    }

    // If the function does not end in a return, and return type is void, insert `ret void`
    if (retType->isVoidTy()) {
        if (!entryBlock->getTerminator()) {
            m_builder.CreateRetVoid();
        }
    }
    else {
        // If the function is non-void but missing a return, let's return 0
        if (!entryBlock->getTerminator()) {
            auto *defaultVal = llvm::ConstantInt::get(retType, 0);
            m_builder.CreateRet(defaultVal);
        }
    }

    popBlock();
}

///////////////////////////////////////////////////////////////////////////
// Compound Statement
///////////////////////////////////////////////////////////////////////////

void CodeGenerator::generateCompoundStmt(CompoundStmtNode &node)
{
    // Create a new block scope (but keep in same function).
    // We won't create a new BasicBlock unless we want distinct jump targets.
    // For local variable declarations, we do them here with alloca.
    // But let's maintain a new "logical scope" in our CodeGenBlock stack so that
    // locals from this block don't leak out.
    llvm::BasicBlock *currentBB = m_builder.GetInsertBlock();
    pushBlock(currentBB); // reuse the same BB, but we get a new map for locals

    // Generate each statement inside
    for (auto &item : node.items()) {
        generateNode(*item);
        // If we already inserted a terminator (like a return), we could break if we want
        if (currentBB->getTerminator()) {
            break; // no further instructions
        }
    }

    popBlock();
}

///////////////////////////////////////////////////////////////////////////
// If Statement
///////////////////////////////////////////////////////////////////////////

void CodeGenerator::generateIfStmt(IfStmtNode &node)
{
    // generate condition
    llvm::Value *condVal = nullptr;
    if (node.condition()) {
        condVal = generateExpr(*(ExprNode*)node.condition());
    }
    if (!condVal) {
        // fallback: always false
        condVal = llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_context), 0);
    }

    // We have an i32 condition typically. Convert to i1:
    // We'll do a simple != 0 check if condVal is i32
    if (condVal->getType()->isIntegerTy(32)) {
        condVal = m_builder.CreateICmpNE(
            condVal,
            llvm::ConstantInt::get(condVal->getType(), 0),
            "ifcond"
        );
    }

    llvm::Function *function = m_builder.GetInsertBlock()->getParent();

    // create blocks for then, else, and merge
    llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(m_context, "then", function);
    llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(m_context, "else");
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(m_context, "ifend");

    // if condVal => jump to thenBB, else => elseBB
    m_builder.CreateCondBr(condVal, thenBB, elseBB);

    // then block
    m_builder.SetInsertPoint(thenBB);
    pushBlock(thenBB);

    if (node.thenBranch()) {
        generateNode(*(ASTNode*)node.thenBranch());
    }
    bool hasThenTerminator = (thenBB->getTerminator() != nullptr);
    popBlock();

    if (!hasThenTerminator) {
        m_builder.CreateBr(mergeBB);
    }

    // else block
    function->getBasicBlockList().push_back(elseBB);
    m_builder.SetInsertPoint(elseBB);
    pushBlock(elseBB);

    if (node.elseBranch()) {
        generateNode(*(ASTNode*)node.elseBranch());
    }
    bool hasElseTerminator = (elseBB->getTerminator() != nullptr);
    popBlock();

    if (!hasElseTerminator) {
        m_builder.CreateBr(mergeBB);
    }

    // merge block
    function->getBasicBlockList().push_back(mergeBB);
    m_builder.SetInsertPoint(mergeBB);
}

///////////////////////////////////////////////////////////////////////////
// While Statement
///////////////////////////////////////////////////////////////////////////

void CodeGenerator::generateWhileStmt(WhileStmtNode &node)
{
    llvm::Function *function = m_builder.GetInsertBlock()->getParent();

    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(m_context, "whilecond", function);
    llvm::BasicBlock *loopBB = llvm::BasicBlock::Create(m_context, "whileloop");
    llvm::BasicBlock *exitBB = llvm::BasicBlock::Create(m_context, "whileexit");

    // jump to condition block
    m_builder.CreateBr(condBB);

    // condBB
    m_builder.SetInsertPoint(condBB);
    pushBlock(condBB);

    llvm::Value *condVal = nullptr;
    if (node.condition()) {
        condVal = generateExpr(*(ExprNode*)node.condition());
    }
    if (!condVal) {
        condVal = llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_context), 1); // default true
    }
    // convert i32 -> i1 if needed
    if (condVal->getType()->isIntegerTy(32)) {
        condVal = m_builder.CreateICmpNE(
            condVal,
            llvm::ConstantInt::get(condVal->getType(), 0),
            "whilecond"
        );
    }
    m_builder.CreateCondBr(condVal, loopBB, exitBB);
    popBlock();

    // loopBB
    function->getBasicBlockList().push_back(loopBB);
    m_builder.SetInsertPoint(loopBB);
    pushBlock(loopBB);

    if (node.body()) {
        generateNode(*(ASTNode*)node.body());
    }

    if (!loopBB->getTerminator()) {
        // jump back to cond
        m_builder.CreateBr(condBB);
    }
    popBlock();

    // exitBB
    function->getBasicBlockList().push_back(exitBB);
    m_builder.SetInsertPoint(exitBB);
}

///////////////////////////////////////////////////////////////////////////
// For Statement
///////////////////////////////////////////////////////////////////////////

void CodeGenerator::generateForStmt(ForStmtNode &node)
{
    llvm::Function *function = m_builder.GetInsertBlock()->getParent();

    // We'll create blocks: forinit, forcond, forbody, forincr, forexit
    llvm::BasicBlock *initBB = llvm::BasicBlock::Create(m_context, "forinit", function);
    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(m_context, "forcond");
    llvm::BasicBlock *loopBB = llvm::BasicBlock::Create(m_context, "forloop");
    llvm::BasicBlock *incrBB = llvm::BasicBlock::Create(m_context, "forincr");
    llvm::BasicBlock *exitBB = llvm::BasicBlock::Create(m_context, "forexit");

    // jump to initBB
    m_builder.CreateBr(initBB);

    // initBB
    m_builder.SetInsertPoint(initBB);
    pushBlock(initBB);
    if (node.init()) {
        generateNode(*(ASTNode*)node.init());
    }
    if (!initBB->getTerminator()) {
        m_builder.CreateBr(condBB);
    }
    popBlock();

    // condBB
    function->getBasicBlockList().push_back(condBB);
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

    // loopBB
    function->getBasicBlockList().push_back(loopBB);
    m_builder.SetInsertPoint(loopBB);
    pushBlock(loopBB);
    if (node.body()) {
        generateNode(*(ASTNode*)node.body());
    }
    if (!loopBB->getTerminator()) {
        m_builder.CreateBr(incrBB);
    }
    popBlock();

    // incrBB
    function->getBasicBlockList().push_back(incrBB);
    m_builder.SetInsertPoint(incrBB);
    pushBlock(incrBB);
    if (node.increment()) {
        generateExpr(*(ExprNode*)node.increment());
    }
    if (!incrBB->getTerminator()) {
        m_builder.CreateBr(condBB);
    }
    popBlock();

    // exitBB
    function->getBasicBlockList().push_back(exitBB);
    m_builder.SetInsertPoint(exitBB);
}

///////////////////////////////////////////////////////////////////////////
// Return Statement
///////////////////////////////////////////////////////////////////////////

void CodeGenerator::generateReturnStmt(ReturnStmtNode &node)
{
    llvm::Value *retVal = nullptr;
    if (node.expr()) {
        retVal = generateExpr(*(ExprNode*)node.expr());
    }
    if (!retVal) {
        // If function is void, then CreateRetVoid
        m_builder.CreateRetVoid();
    } else {
        m_builder.CreateRet(retVal);
    }
}

///////////////////////////////////////////////////////////////////////////
// Expression Statement
///////////////////////////////////////////////////////////////////////////

void CodeGenerator::generateExprStmt(ExprStmtNode &node)
{
    if (node.expr()) {
        generateExpr(*(ExprNode*)node.expr());
    }
    // discard the result
}

///////////////////////////////////////////////////////////////////////////
// Expression Codegen
///////////////////////////////////////////////////////////////////////////

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
            // unexpected
            return nullptr;
    }
}

llvm::Value* CodeGenerator::generateBinaryExpr(BinaryExprNode &expr)
{
    llvm::Value *L = generateExpr(*(ExprNode*)expr.left());
    llvm::Value *R = generateExpr(*(ExprNode*)expr.right());
    if (!L || !R) return nullptr;

    std::string op = expr.op();

    // If we see "=", treat left as an lvalue (identifier?), store right into it.
    if (op == "=") {
        // L must be an identifier or something that can be assigned to
        // For simplicity, we assume L is an identifier expression.
        auto *identifierLeft = dynamic_cast<IdentifierExprNode*>(expr.left());
        if (!identifierLeft) {
            // In a real compiler, you'd handle array, pointer, etc.
            return nullptr;
        }
        // R is the value to store
        setVariableValue(identifierLeft->name(), R);
        return R;
    }

    // For arithmetic, we assume L and R are i32
    // Real compilers must handle type checks, promotions, etc.
    if (L->getType()->isIntegerTy() && R->getType()->isIntegerTy()) {
        if (op == "+") {
            return m_builder.CreateAdd(L, R, "addtmp");
        } else if (op == "-") {
            return m_builder.CreateSub(L, R, "subtmp");
        } else if (op == "*") {
            return m_builder.CreateMul(L, R, "multmp");
        } else if (op == "/") {
            return m_builder.CreateSDiv(L, R, "divtmp");
        } else if (op == "%") {
            return m_builder.CreateSRem(L, R, "modtmp");
        } else if (op == "==") {
            return m_builder.CreateICmpEQ(L, R, "eqtmp");
        } else if (op == "!=") {
            return m_builder.CreateICmpNE(L, R, "netmp");
        } else if (op == "<") {
            return m_builder.CreateICmpSLT(L, R, "lttmp");
        } else if (op == ">") {
            return m_builder.CreateICmpSGT(L, R, "gttmp");
        } else if (op == "<=") {
            return m_builder.CreateICmpSLE(L, R, "letmp");
        } else if (op == ">=") {
            return m_builder.CreateICmpSGE(L, R, "getmp");
        }
    }

    // Otherwise, we skip float handling for brevity. Expand as needed.
    return nullptr;
}

llvm::Value* CodeGenerator::generateUnaryExpr(UnaryExprNode &expr)
{
    llvm::Value *operand = generateExpr(*(ExprNode*)expr.operand());
    if (!operand) return nullptr;

    std::string op = expr.op();
    if (op == "-") {
        // numeric negation
        if (operand->getType()->isIntegerTy()) {
            return m_builder.CreateNeg(operand, "negtmp");
        }
        // skip float, etc. for brevity
    }
    else if (op == "+") {
        // unary plus is a no-op
        return operand;
    }
    else if (op == "++" || op == "--") {
        // pre-increment or pre-decrement
        // we assume operand is an identifier
        auto *identifier = dynamic_cast<IdentifierExprNode*>(expr.operand());
        if (!identifier) return nullptr;

        // load current
        llvm::Value *val = getVariableValue(identifier->name());
        if (!val) return nullptr;

        llvm::Value *one = llvm::ConstantInt::get(val->getType(), 1);
        llvm::Value *res = nullptr;
        if (op == "++") {
            res = m_builder.CreateAdd(val, one, "inc");
        } else {
            res = m_builder.CreateSub(val, one, "dec");
        }
        // store back
        setVariableValue(identifier->name(), res);
        // the expression value is the updated value
        return res;
    }

    return nullptr;
}

llvm::Value* CodeGenerator::generateCallExpr(CallExprNode &expr)
{
    // Look up the function in the module
    llvm::Function *calleeF = m_module->getFunction(expr.callee());
    if (!calleeF) {
        // function not found, IR will be broken unless we want to declare it external
        return nullptr;
    }

    // generate arguments
    std::vector<llvm::Value*> argsV;
    for (auto &arg : expr.args()) {
        llvm::Value *argVal = generateExpr(*(ExprNode*)arg.get());
        if (!argVal) return nullptr;
        argsV.push_back(argVal);
    }

    return m_builder.CreateCall(calleeF, argsV, "calltmp");
}

llvm::Value* CodeGenerator::generateLiteralExpr(LiteralExprNode &expr)
{
    // parse the string to decide if it's int, float, etc.
    // For demonstration, let's assume everything that looks integer is i32:
    const std::string &valStr = expr.value();

    // If it has '.' => treat as float (very naive check):
    if (valStr.find('.') != std::string::npos) {
        // float parse
        float fVal = std::stof(valStr);
        return llvm::ConstantFP::get(llvm::Type::getFloatTy(m_context), fVal);
    }
    else {
        // integer parse
        int iVal = std::stoi(valStr);
        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_context), iVal);
    }
}

llvm::Value* CodeGenerator::generateIdentifierExpr(IdentifierExprNode &expr)
{
    // load the variable from local alloca
    return getVariableValue(expr.name());
}

///////////////////////////////////////////////////////////////////////////
// Variable Load/Store Helpers
///////////////////////////////////////////////////////////////////////////

llvm::Value* CodeGenerator::getVariableValue(const std::string &name)
{
    // Walk up the block stack to find a local
    std::stack<std::unique_ptr<CodeGenBlock>> tmpStack = m_blocks;
    while (!tmpStack.empty()) {
        auto &blk = tmpStack.top();
        auto it = blk->locals.find(name);
        if (it != blk->locals.end()) {
            // load from alloca
            return m_builder.CreateLoad(it->second->getAllocatedType(), it->second, name.c_str());
        }
        tmpStack.pop();
    }

    // Otherwise, maybe it's a global
    if (auto *gvar = m_module->getGlobalVariable(name)) {
        return m_builder.CreateLoad(gvar->getValueType(), gvar, name.c_str());
    }

    // not found
    return nullptr;
}

void CodeGenerator::setVariableValue(const std::string &name, llvm::Value* value)
{
    // same logic as getVariableValue, but store
    std::stack<std::unique_ptr<CodeGenBlock>> tmpStack = m_blocks;
    while (!tmpStack.empty()) {
        auto &blk = tmpStack.top();
        auto it = blk->locals.find(name);
        if (it != blk->locals.end()) {
            m_builder.CreateStore(value, it->second);
            return;
        }
        tmpStack.pop();
    }

    // check global
    if (auto *gvar = m_module->getGlobalVariable(name)) {
        m_builder.CreateStore(value, gvar);
        return;
    }
    // not found
}
