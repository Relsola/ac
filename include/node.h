#pragma once

class Node {
 public:
  enum class NodeKind {
    ND_ADD,  // +
    ND_SUB,  // -
    ND_MUL,  // *
    ND_DIV,  // /
    ND_NEG,  // unary -
    ND_EQ,   // ==
    ND_NE,   // !=
    ND_LT,   // <
    ND_LE,   // <=
    ND_NUM,  // Integer
  };

  NodeKind kind;  // Node kind
  Node *lhs;      // Left-hand side
  Node *rhs;      // Right-hand side
  int val;        // Used if kind == ND_NUM

  Node(NodeKind kind);
  Node(NodeKind kind, Node *lhs, Node *rhs);
  Node(NodeKind kind, Node *expr);
  Node(int val);
};
