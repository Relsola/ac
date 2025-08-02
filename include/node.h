#pragma once

#include "obj.h";

class Node {
 public:
  enum class NodeKind {
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
    ND_RETURN,     // "return"
    ND_IF,         // "if"
    ND_FOR,        // "for" or "while"
    ND_BLOCK,      // { ... }
    ND_EXPR_STMT,  // Expression statement
    ND_VAR,        // Variable
    ND_NUM,        // Integer
  };

  NodeKind kind;  // Node kind
  Node *next;     // Next node

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
  Node(NodeKind kind);
  Node(NodeKind kind, Node *lhs, Node *rhs);
  Node(NodeKind kind, Node *expr);
  Node(int val);
  Node(Obj *var);
};
