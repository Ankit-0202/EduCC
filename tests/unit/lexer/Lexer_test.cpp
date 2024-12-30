#include "lexer/Lexer.h"
#include <gtest/gtest.h>

namespace LexerTests {

TEST(LexerTest, TokenizesKeywords) {
  std::string code = "int float char";
  Lexer lexer(code);
  auto tokens = lexer.tokenize();

  ASSERT_EQ(tokens.size(), 3);

  EXPECT_EQ(tokens[0].type, TokenType::INT);
  EXPECT_EQ(tokens[1].type, TokenType::FLOAT);
  EXPECT_EQ(tokens[2].type, TokenType::CHAR);
}

TEST(LexerTest, TokenizesIdentifiers) {
  std::string code = "main _var var123";
  Lexer lexer(code);
  auto tokens = lexer.tokenize();

  ASSERT_EQ(tokens.size(), 3);

  EXPECT_EQ(tokens[0].type, TokenType::IDENTIFIER);
  EXPECT_EQ(tokens[0].lexeme, "main");
  EXPECT_EQ(tokens[1].type, TokenType::IDENTIFIER);
  EXPECT_EQ(tokens[1].lexeme, "_var");
  EXPECT_EQ(tokens[2].type, TokenType::IDENTIFIER);
  EXPECT_EQ(tokens[2].lexeme, "var123");
}

TEST(LexerTest, TokenizesOperators) {
  std::string code = "+ - * / = == != <= >= && || !";
  Lexer lexer(code);
  auto tokens = lexer.tokenize();

  ASSERT_EQ(tokens.size(), 13); // Including possible single characters

  EXPECT_EQ(tokens[0].type, TokenType::PLUS);
  EXPECT_EQ(tokens[1].type, TokenType::MINUS);
  EXPECT_EQ(tokens[2].type, TokenType::MULTIPLY);
  EXPECT_EQ(tokens[3].type, TokenType::DIVIDE);
  EXPECT_EQ(tokens[4].type, TokenType::ASSIGN);
  EXPECT_EQ(tokens[5].type, TokenType::EQUAL_EQUAL);
  EXPECT_EQ(tokens[6].type, TokenType::NOT_EQUAL);
  EXPECT_EQ(tokens[7].type, TokenType::LESS_EQUAL);
  EXPECT_EQ(tokens[8].type, TokenType::GREATER_EQUAL);
  EXPECT_EQ(tokens[9].type, TokenType::AND_AND);
  EXPECT_EQ(tokens[10].type, TokenType::OR_OR);
  EXPECT_EQ(tokens[11].type, TokenType::NOT);
  // ... and so on
}

} // namespace LexerTests
