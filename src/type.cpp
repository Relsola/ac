#include "core.h"

Type *Type::ty_int = new Type(TypeKind::TY_INT);

Type::Type(TypeKind kind) : kind(kind) {}

bool Type::is_integer() { return this->kind == TypeKind::TY_INT; }

Type *Type::pointer_to(Type *base) {
  Type *ty = new Type(TypeKind::TY_PTR);
  ty->base = base;
  return ty;
}

void add_type(Node *node) {
  if (!node || node->ty) return;

  add_type(node->lhs);
  add_type(node->rhs);
  add_type(node->cond);
  add_type(node->then);
  add_type(node->els);
  add_type(node->init);
  add_type(node->inc);

  for (Node *n = node->body; n; n = n->next) add_type(n);

  switch (node->kind) {
    case NodeKind::ND_ADD:
    case NodeKind::ND_SUB:
    case NodeKind::ND_MUL:
    case NodeKind::ND_DIV:
    case NodeKind::ND_NEG:
    case NodeKind::ND_ASSIGN:
      node->ty = node->lhs->ty;
      return;
    case NodeKind::ND_EQ:
    case NodeKind::ND_NE:
    case NodeKind::ND_LT:
    case NodeKind::ND_LE:
    case NodeKind::ND_NUM:
      node->ty = Type::ty_int;
      return;
    case NodeKind::ND_VAR:
      node->ty = node->var->ty;
      return;
    case NodeKind::ND_ADDR:
      node->ty = Type::pointer_to(node->lhs->ty);
      return;
    case NodeKind::ND_DEREF:
      if (node->lhs->ty->kind != TypeKind::TY_PTR)
        error_tok(node->tok, "invalid pointer dereference");

      node->ty = node->lhs->ty->base;
      return;
  }
}
