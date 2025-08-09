#include "core.h"

Node::Node(){};

Node::Node(NodeKind kind, Token *tok) : kind(kind), tok(tok) {}

Node::Node(NodeKind kind, Node *lhs, Node *rhs, Token *tok)
    : kind(kind), lhs(lhs), rhs(rhs), tok(tok) {}

Node::Node(NodeKind kind, Node *expr, Token *tok)
    : kind(kind), lhs(expr), tok(tok) {}

Node::Node(int val, Token *tok) : kind(NodeKind::ND_NUM), val(val), tok(tok) {}

Node::Node(Obj *var, Token *tok) : kind(NodeKind::ND_VAR), var(var), tok(tok) {}
