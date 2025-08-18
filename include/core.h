#pragma once

#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <string>

class Type;
class Node;

//
// strings.c
//
char *format(char *fmt, ...);

enum class TokenKind : int {
  TK_IDENT,    // Identifiers
  TK_PUNCT,    // Punctuators
  TK_KEYWORD,  // Keywords
  TK_STR,      // String literals
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
  ND_COMMA,      // ,
  ND_ADDR,       // unary &
  ND_DEREF,      // unary *
  ND_RETURN,     // "return"
  ND_IF,         // "if"
  ND_FOR,        // "for" or "while"
  ND_BLOCK,      // { ... }
  ND_FUNCALL,    // Function call
  ND_EXPR_STMT,  // Expression statement
  ND_STMT_EXPR,  // Statement expression
  ND_VAR,        // Variable
  ND_NUM,        // Integer
};

enum class TypeKind : int {
  TY_CHAR,
  TY_INT,
  TY_PTR,
  TY_FUNC,
  TY_ARRAY,
};

class Token {
 public:
  TokenKind kind = TokenKind::TK_EOF;  // Token kind
  Token *next = nullptr;               // Next token
  int val = 0;                         // If kind is TK_NUM, its value
  char *loc = nullptr;                 // Token location
  int len = 0;                         // Token length
  Type *ty = nullptr;                  // Used if TK_STR
  char *str = nullptr;                 // String literal contents including terminating '\0'

  int line_no = 0;  // Line number

  Token() = default;

  Token(TokenKind kind, char *start, char *end) : kind(kind), loc(start), len(end - start){};

  bool equal(char *op) { return memcmp(this->loc, op, this->len) == 0 && op[this->len] == '\0'; };

  Token *skip(char *s);

  int get_number();

  bool consume(Token **rest, char *str);
};

// Variable or function
class Obj {
 public:
  Obj *next = nullptr;
  char *name = nullptr;   // Variable name
  Type *ty = nullptr;     // Type
  bool is_local = false;  // local or global/function

  // Local variable
  int offset = 0;

  // Global variable or function
  bool is_function = false;

  // Global variable
  char *init_data = nullptr;

  // Function
  Obj *params = nullptr;
  Node *body = nullptr;
  Obj *locals = nullptr;
  int stack_size = 0;

  Obj() = default;

  Obj(char *name, Type *ty) : name(name), ty(ty){};
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

  // Block or statement expression
  Node *body = nullptr;

  // Function call
  char *funcname = nullptr;
  Node *args = nullptr;

  Obj *var = nullptr;  // Used if kind == ND_VAR
  int val = 0;         // Used if kind == ND_NUM

  Node() = default;
};

class Type {
 public:
  static Type *ty_int;
  static Type *ty_char;

  static Type *pointer_to(Type *base);

  static Type *func_type(Type *return_ty);

  static Type *copy_type(Type *ty);

  static Type *array_of(Type *base, int size);

  TypeKind kind;

  int size = 0;  // sizeof() value

  // Pointer-to or array-of type. We intentionally use the same member
  // to represent pointer/array duality in C.
  //
  // In many contexts in which a pointer is expected, we examine this
  // member instead of "kind" member to determine whether a type is a
  // pointer or not. That means in many contexts "array of T" is
  // naturally handled as if it were "pointer to T", as required by
  // the C spec.
  Type *base = nullptr;

  // Declaration
  Token *name = nullptr;

  // Array
  int array_len = 0;

  // Function type
  Type *return_ty = nullptr;
  Type *params = nullptr;
  Type *next = nullptr;

  Type() = default;

  Type(TypeKind kind) : kind(kind){};

  Type(TypeKind kind, int size) : kind(kind), size(size){};

  bool is_integer() { return this->kind == TypeKind::TY_INT || this->kind == TypeKind::TY_CHAR; };
};

void add_type(Node *node);

void error(char *fmt, ...);

void error_at(char *loc, char *fmt, ...);

void error_tok(Token *tok, char *fmt, ...);

Token *tokenize_file(char *filename);

Obj *parse(Token *tok);

void codegen(Obj *prog, FILE *out);
