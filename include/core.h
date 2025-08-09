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

class Obj {
 public:
  Obj *next;
  char *name;  // Variable name
  int offset;  // Offset from RBP
};

class Function {
 public:
  Node *body;
  Obj *locals;
  int stack_size;
};

class Node {
 public:
  NodeKind kind;  // Node kind
  Node *next;     // Next node
  Type *ty;       // Type, e.g. int or pointer to int
  Token *tok;     // Representative token

  Node *lhs;  // Left-hand side
  Node *rhs;  // Right-hand side

  // "if" or "for" statement
  Node *cond;
  Node *then;
  Node *els;
  Node *init;
  Node *inc;

  // Block
  Node *body;

  Obj *var;  // Used if kind == ND_VAR
  int val;   // Used if kind == ND_NUM

  Node();
  Node(NodeKind kind, Token *tok);
  Node(NodeKind kind, Node *lhs, Node *rhs, Token *tok);
  Node(NodeKind kind, Node *expr, Token *tok);
  Node(int val, Token *tok);
  Node(Obj *var, Token *tok);
};

class Type {
 public:
  static Type *ty_int;

  TypeKind kind;
  Type *base;

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
