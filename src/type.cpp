#include "core.h"

Type *Type::ty_char = new Type(TypeKind::TY_CHAR, 1, 1);
Type *Type::ty_int = new Type(TypeKind::TY_INT, 8, 8);

Type *Type::copy_type(Type *ty) {
  Type *ret = new Type();
  *ret = *ty;
  return ret;
}

Type *Type::pointer_to(Type *base) {
  Type *ty = new Type(TypeKind::TY_PTR, 8, 8);
  ty->base = base;
  return ty;
}

Type *Type::func_type(Type *return_ty) {
  Type *ty = new Type(TypeKind::TY_FUNC);
  ty->return_ty = return_ty;
  return ty;
}

Type *Type::array_of(Type *base, int len) {
  Type *ty = new Type(TypeKind::TY_ARRAY, base->size * len, base->align);
  ty->base = base;
  ty->array_len = len;
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
  for (Node *n = node->args; n; n = n->next) add_type(n);

  switch (node->kind) {
    case NodeKind::ND_ADD:
    case NodeKind::ND_SUB:
    case NodeKind::ND_MUL:
    case NodeKind::ND_DIV:
    case NodeKind::ND_NEG:
      node->ty = node->lhs->ty;
      return;
    case NodeKind::ND_ASSIGN:
      if (node->lhs->ty->kind == TypeKind::TY_ARRAY) error_tok(node->lhs->tok, "not an lvalue");
      node->ty = node->lhs->ty;
      return;
    case NodeKind::ND_EQ:
    case NodeKind::ND_NE:
    case NodeKind::ND_LT:
    case NodeKind::ND_LE:
    case NodeKind::ND_NUM:
    case NodeKind::ND_FUNCALL:
      node->ty = Type::ty_int;
      return;
    case NodeKind::ND_VAR:
      node->ty = node->var->ty;
      return;
    case NodeKind::ND_COMMA:
      node->ty = node->rhs->ty;
      return;
    case NodeKind::ND_MEMBER:
      node->ty = node->member->ty;
      return;
    case NodeKind::ND_ADDR:
      if (node->lhs->ty->kind == TypeKind::TY_ARRAY)
        node->ty = Type::pointer_to(node->lhs->ty->base);
      else
        node->ty = Type::pointer_to(node->lhs->ty);
      return;
    case NodeKind::ND_DEREF:
      if (!node->lhs->ty->base) error_tok(node->tok, "invalid pointer dereference");

      node->ty = node->lhs->ty->base;
      return;
    case NodeKind::ND_STMT_EXPR:
      if (node->body) {
        Node *stmt = node->body;
        while (stmt->next) stmt = stmt->next;
        if (stmt->kind == NodeKind::ND_EXPR_STMT) {
          node->ty = stmt->lhs->ty;
          return;
        }
      }
      error_tok(node->tok, "statement expression returning void is not supported");
      return;
  }
}
