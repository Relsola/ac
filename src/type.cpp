#include "core.h"

Type *Type::ty_void = new Type(TypeKind::TY_VOID, 1, 1);
Type *Type::ty_bool = new Type(TypeKind::TY_BOOL, 1, 1);

Type *Type::ty_char = new Type(TypeKind::TY_CHAR, 1, 1);
Type *Type::ty_short = new Type(TypeKind::TY_SHORT, 2, 2);
Type *Type::ty_int = new Type(TypeKind::TY_INT, 4, 4);
Type *Type::ty_long = new Type(TypeKind::TY_LONG, 8, 8);

Type *Type::ty_uchar = new Type(TypeKind::TY_CHAR, 1, 1, true);
Type *Type::ty_ushort = new Type(TypeKind::TY_SHORT, 2, 2, true);
Type *Type::ty_uint = new Type(TypeKind::TY_INT, 4, 4, true);
Type *Type::ty_ulong = new Type(TypeKind::TY_LONG, 8, 8, true);

Type *Type::ty_float = new Type(TypeKind::TY_FLOAT, 4, 4);
Type *Type::ty_double = new Type(TypeKind::TY_DOUBLE, 8, 8);

Type *Type::copy_type(Type *ty) {
  Type *ret = new Type();
  *ret = *ty;
  return ret;
}

Type *Type::pointer_to(Type *base) {
  Type *ty = new Type(TypeKind::TY_PTR, 8, 8);
  ty->base = base;
  ty->is_unsigned = true;
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

Type *Type::enum_type() { return new Type(TypeKind::TY_ENUM, 4, 4); }

Type *Type::struct_type(void) { return new Type(TypeKind::TY_STRUCT, 0, 1); }

static Type *get_common_type(Type *ty1, Type *ty2) {
  if (ty1->base) return Type::pointer_to(ty1->base);

  if (ty1->kind == TypeKind::TY_FUNC) return Type::pointer_to(ty1);
  if (ty2->kind == TypeKind::TY_FUNC) return Type::pointer_to(ty2);

  if (ty1->kind == TypeKind::TY_DOUBLE || ty2->kind == TypeKind::TY_DOUBLE) return Type::ty_double;
  if (ty1->kind == TypeKind::TY_FLOAT || ty2->kind == TypeKind::TY_FLOAT) return Type::ty_float;

  if (ty1->size < 4) ty1 = Type::ty_int;
  if (ty2->size < 4) ty2 = Type::ty_int;

  if (ty1->size != ty2->size) return (ty1->size < ty2->size) ? ty2 : ty1;

  if (ty2->is_unsigned) return ty2;

  return ty1;
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
      node->ty = Type::ty_int;
      return;
    case NodeKind::ND_ADD:
    case NodeKind::ND_SUB:
    case NodeKind::ND_MUL:
    case NodeKind::ND_DIV:
    case NodeKind::ND_MOD:
    case NodeKind::ND_BITAND:
    case NodeKind::ND_BITOR:
    case NodeKind::ND_BITXOR:
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
      if (node->lhs->ty->kind != TypeKind::TY_STRUCT)
        node->rhs = new_cast(node->rhs, node->lhs->ty);
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
    case NodeKind::ND_NOT:
    case NodeKind::ND_LOGOR:
    case NodeKind::ND_LOGAND:
      node->ty = Type::ty_int;
      return;
    case NodeKind::ND_BITNOT:
    case NodeKind::ND_SHL:
    case NodeKind::ND_SHR:
      node->ty = node->lhs->ty;
      return;
    case NodeKind::ND_VAR:
      node->ty = node->var->ty;
      return;
    case NodeKind::ND_COND:
      if (node->then->ty->kind == TypeKind::TY_VOID || node->els->ty->kind == TypeKind::TY_VOID) {
        node->ty = Type::ty_void;
      } else {
        usual_arith_conv(&node->then, &node->els);
        node->ty = node->then->ty;
      }
      return;
    case NodeKind::ND_COMMA:
      node->ty = node->rhs->ty;
      return;
    case NodeKind::ND_MEMBER:
      node->ty = node->member->ty;
      return;
    case NodeKind::ND_ADDR: {
      Type *ty = node->lhs->ty;
      if (ty->kind == TypeKind::TY_ARRAY)
        node->ty = Type::pointer_to(ty->base);
      else
        node->ty = Type::pointer_to(ty);
      return;
    }
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
