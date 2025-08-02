#pragma once

#include <string>

class Token {
 public:
  enum class TokenKind {
    TK_PUNCT,  // Punctuators
    TK_NUM,    // Numeric literals
    TK_EOF,    // End-of-file markers
  };

  TokenKind kind;  // Token kind
  Token *next;     // Next token
  int val;         // If kind is TK_NUM, its value
  char *loc;       // Token location
  int len;         // Token length

  Token();
  Token(TokenKind kind, char *start, char *end);

  bool equal(char *op);

  Token *skip(char *s);

  int get_number();
};
