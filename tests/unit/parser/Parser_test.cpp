#include "parser/ASTNodes.h"
#include "parser/Parser.h"
#include <gtest/gtest.h>

namespace ParserTests {

TEST(ParserTest, ParsesVariableDeclaration) {
  std::string code = "int x = 10;";
  Lexer lexer(code);
  auto tokens = lexer.tokenize();
  Parser parser(tokens);
  auto ast = parser.parse();

  ASSERT_NE(ast, nullptr);
  EXPECT_EQ(ast->type, ASTNodeType::PROGRAM);

  auto program = dynamic_cast<Program *>(ast.get());
  ASSERT_NE(program, nullptr);
  ASSERT_EQ(program->declarations.size(), 1);

  auto var_decl =
      dynamic_cast<VariableDeclaration *>(program->declarations[0].get());
  ASSERT_NE(var_decl, nullptr);
  EXPECT_EQ(var_decl->type, "int");
  EXPECT_EQ(var_decl->name, "x");
  ASSERT_NE(var_decl->initializer, nullptr);

  auto literal = dynamic_cast<Literal *>(var_decl->initializer.get());
  ASSERT_NE(literal, nullptr);
  EXPECT_EQ(literal->value, "10");
}

TEST(ParserTest, ParsesFunctionDefinition) {
  std::string code = "int add(int a, int b) { return a + b; }";
  Lexer lexer(code);
  auto tokens = lexer.tokenize();
  Parser parser(tokens);
  auto ast = parser.parse();

  ASSERT_NE(ast, nullptr);
  EXPECT_EQ(ast->type, ASTNodeType::PROGRAM);

  auto program = dynamic_cast<Program *>(ast.get());
  ASSERT_NE(program, nullptr);
  ASSERT_EQ(program->declarations.size(), 1);

  auto func_def =
      dynamic_cast<FunctionDefinition *>(program->declarations[0].get());
  ASSERT_NE(func_def, nullptr);
  EXPECT_EQ(func_def->returnType, "int");
  EXPECT_EQ(func_def->name, "add");
  ASSERT_EQ(func_def->parameters.size(), 2);
  EXPECT_EQ(func_def->parameters[0].first, "int");
  EXPECT_EQ(func_def->parameters[0].second, "a");
  EXPECT_EQ(func_def->parameters[1].first, "int");
  EXPECT_EQ(func_def->parameters[1].second, "b");
  ASSERT_NE(func_def->body, nullptr);

  // Further assertions to verify the body
}

} // namespace ParserTests
