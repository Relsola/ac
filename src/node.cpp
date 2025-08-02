#include "node.h"

Node::Node(NodeKind kind) : kind(kind) {}

Node::Node(NodeKind kind, Node *lhs, Node *rhs)
    : kind(kind), lhs(lhs), rhs(rhs) {}

Node::Node(NodeKind kind, Node *expr) : kind(kind), lhs(expr) {}

Node::Node(int val) : kind(NodeKind::ND_NUM), val(val) {}
