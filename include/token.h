#pragma once

#include <string>

#include "utils.h"

class Token {
 public:
  enum class TokenKind {
    TK_PUNCT,  // Punctuators
    TK_NUM,    // Numeric literals
    TK_EOF,    // End-of-file markers
  };

  TokenKind kind;     // Token kind
  Token *next;        // Next token
  int val;            // If kind is TK_NUM, its value
  std::string punct;  // If kind is TK_PUNCT, its value

  Token();
  Token(TokenKind kind);
  Token(TokenKind kind, std::string punct);

  bool equal(char *op);

  Token *skip(char *s);

  int get_number();
};
