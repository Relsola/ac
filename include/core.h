#pragma once

#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

class Type;
class Node;

enum class TokenKind : int {
  TK_IDENT,    // Identifiers
  TK_PUNCT,    // Punctuators
  TK_KEYWORD,  // Keywords
  TK_NUM,      // Numeric literals
  TK_EOF,      // End-of-file markers
};

enum class NodeKind : int {
  ND_ADD,        // +
  ND_SUB,        // -
  ND_MUL,        // *
  ND_DIV,        // /
  ND_NEG,        // unary -
  ND_EQ,         // ==
  ND_NE,         // !=
  ND_LT,         // <
  ND_LE,         // <=
  ND_ASSIGN,     // =
  ND_ADDR,       // unary &
  ND_DEREF,      // unary *
  ND_RETURN,     // "return"
  ND_IF,         // "if"
  ND_FOR,        // "for" or "while"
  ND_BLOCK,      // { ... }
  ND_FUNCALL,    // Function call
  ND_EXPR_STMT,  // Expression statement
  ND_VAR,        // Variable
  ND_NUM,        // Integer
};

enum class TypeKind : int {
  TY_INT,
  TY_PTR,
};

class Token {
 public:
  TokenKind kind = TokenKind::TK_EOF;  // Token kind
  Token *next = nullptr;               // Next token
  int val = 0;                         // If kind is TK_NUM, its value
  char *loc = nullptr;                 // Token location
  int len = 0;                         // Token length

  Token() = default;

  Token(TokenKind kind, char *start, char *end);

  bool equal(char *op);

  Token *skip(char *s);

  int get_number();

  bool consume(Token **rest, char *str);
};

class Obj {
 public:
  Obj *next = nullptr;
  char *name = nullptr;  // Variable name
  Type *ty = nullptr;    // Type
  int offset = 0;        // Offset from RBP

  Obj() = default;
};

class Function {
 public:
  Node *body = nullptr;
  Obj *locals = nullptr;
  int stack_size = 0;

  Function() = default;
};

class Node {
 public:
  NodeKind kind = NodeKind::ND_NUM;  // Node kind
  Node *next = nullptr;              // Next node
  Type *ty = nullptr;                // Type, e.g. int or pointer to int
  Token *tok = nullptr;              // Representative token

  Node *lhs = nullptr;  // Left-hand side
  Node *rhs = nullptr;  // Right-hand side

  // "if" or "for" statement
  Node *cond = nullptr;
  Node *then = nullptr;
  Node *els = nullptr;
  Node *init = nullptr;
  Node *inc = nullptr;

  // Block
  Node *body = nullptr;

  // Function call
  char *funcname = nullptr;

  Obj *var = nullptr;  // Used if kind == ND_VAR
  int val = 0;         // Used if kind == ND_NUM

  Node() = default;
};

class Type {
 public:
  static Type *ty_int;

  static Type *pointer_to(Type *base);

  TypeKind kind;

  // Pointer
  Type *base;

  // Declaration
  Token *name;

  Type(TypeKind kind);

  bool is_integer();
};

void add_type(Node *node);

void error(char *fmt, ...);

void error_at(char *loc, char *fmt, ...);

void error_tok(Token *tok, char *fmt, ...);

Token *tokenize(char *p);

Function *parse(Token *tok);

void codegen(Function *prog);
