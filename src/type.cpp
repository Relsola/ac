#include "core.h"

Type *Type::ty_void = new Type(TypeKind::TY_VOID, 1, 1);

Type *Type::ty_char = new Type(TypeKind::TY_CHAR, 1, 1);
Type *Type::ty_short = new Type(TypeKind::TY_SHORT, 2, 2);
Type *Type::ty_int = new Type(TypeKind::TY_INT, 4, 4);
Type *Type::ty_long = new Type(TypeKind::TY_LONG, 8, 8);

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

static Type *get_common_type(Type *ty1, Type *ty2) {
  if (ty1->base) return Type::pointer_to(ty1->base);
  if (ty1->size == 8 || ty2->size == 8) return Type::ty_long;
  return Type::ty_int;
}

// For many binary operators, we implicitly promote operands so that
// both operands have the same type. Any integral type smaller than
// int is always promoted to int. If the type of one operand is larger
// than the other's (e.g. "long" vs. "int"), the smaller operand will
// be promoted to match with the other.
//
// This operation is called the "usual arithmetic conversion".
static void usual_arith_conv(Node **lhs, Node **rhs) {
  Type *ty = get_common_type((*lhs)->ty, (*rhs)->ty);
  *lhs = new_cast(*lhs, ty);
  *rhs = new_cast(*rhs, ty);
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
    case NodeKind::ND_NUM:
      node->ty = (node->val == (int)node->val) ? Type::ty_int : Type::ty_long;
      return;
    case NodeKind::ND_ADD:
    case NodeKind::ND_SUB:
    case NodeKind::ND_MUL:
    case NodeKind::ND_DIV:
      usual_arith_conv(&node->lhs, &node->rhs);
      node->ty = node->lhs->ty;
      return;
    case NodeKind::ND_NEG: {
      Type *ty = get_common_type(Type::ty_int, node->lhs->ty);
      node->lhs = new_cast(node->lhs, ty);
      node->ty = ty;
      return;
    }
    case NodeKind::ND_ASSIGN:
      if (node->lhs->ty->kind == TypeKind::TY_ARRAY) error_tok(node->lhs->tok, "not an lvalue");
      node->ty = node->lhs->ty;
      return;
    case NodeKind::ND_EQ:
    case NodeKind::ND_NE:
    case NodeKind::ND_LT:
    case NodeKind::ND_LE:
      usual_arith_conv(&node->lhs, &node->rhs);
      node->ty = Type::ty_int;
      return;
    case NodeKind::ND_FUNCALL:
      node->ty = Type::ty_long;
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

      if (node->lhs->ty->base->kind == TypeKind::TY_VOID)
        error_tok(node->tok, "dereferencing a void pointer");

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
