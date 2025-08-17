// This file contains a recursive descent parser for C.
//
// Most functions in this file are named after the symbols they are
// supposed to read from an input token list. For example, stmt() is
// responsible for reading a statement from a token list. The function
// then construct an AST node representing a statement.
//
// Each function conceptually returns two values, an AST node and
// remaining part of the input tokens. Since C doesn't support
// multiple return values, the remaining tokens are returned to the
// caller via a pointer argument.
//
// Input tokens are represented by a linked list. Unlike many recursive
// descent parsers, we don't have the notion of the "input token stream".
// Most parsing functions don't change the global state of the parser.
// So it is very easy to lookahead arbitrary number of tokens in this
// parser.

#include "core.h"

// All local variable instances created during parsing are
// accumulated to this list.
static Obj *locals;
static Obj *globals;

static Type *declspec(Token **rest, Token *tok);
static Type *declarator(Token **rest, Token *tok, Type *ty);
static Node *declaration(Token **rest, Token *tok);
static Node *compound_stmt(Token **rest, Token *tok);
static Node *stmt(Token **rest, Token *tok);
static Node *expr_stmt(Token **rest, Token *tok);
static Node *expr(Token **rest, Token *tok);
static Node *assign(Token **rest, Token *tok);
static Node *equality(Token **rest, Token *tok);
static Node *relational(Token **rest, Token *tok);
static Node *add(Token **rest, Token *tok);
static Node *mul(Token **rest, Token *tok);
static Node *postfix(Token **rest, Token *tok);
static Node *unary(Token **rest, Token *tok);
static Node *primary(Token **rest, Token *tok);

// Find a local variable by name.
static Obj *find_var(Token *tok) {
  for (Obj *var = locals; var; var = var->next)
    if (strlen(var->name) == tok->len && !strncmp(tok->loc, var->name, tok->len)) return var;

  for (Obj *var = globals; var; var = var->next)
    if (strlen(var->name) == tok->len && !strncmp(tok->loc, var->name, tok->len)) return var;

  return NULL;
}

static Node *new_node(NodeKind kind, Token *tok) {
  Node *node = new Node();
  node->kind = kind;
  node->tok = tok;
  return node;
}

static Node *new_binary(NodeKind kind, Node *lhs, Node *rhs, Token *tok) {
  Node *node = new_node(kind, tok);
  node->lhs = lhs;
  node->rhs = rhs;
  return node;
}

static Node *new_unary(NodeKind kind, Node *expr, Token *tok) {
  Node *node = new_node(kind, tok);
  node->lhs = expr;
  return node;
}

static Node *new_num(int val, Token *tok) {
  Node *node = new_node(NodeKind::ND_NUM, tok);
  node->val = val;
  return node;
}

static Node *new_var_node(Obj *var, Token *tok) {
  Node *node = new_node(NodeKind::ND_VAR, tok);
  node->var = var;
  return node;
}

static Obj *new_lvar(char *name, Type *ty) {
  Obj *var = new Obj(name, ty);
  var->is_local = true;
  var->next = locals;
  locals = var;
  return var;
}

static Obj *new_gvar(char *name, Type *ty) {
  Obj *var = new Obj(name, ty);
  var->next = globals;
  globals = var;
  return var;
}

static char *get_ident(Token *tok) {
  if (tok->kind != TokenKind::TK_IDENT) error_tok(tok, "expected an identifier");
  return strndup(tok->loc, tok->len);
}

// declspec = "int"
static Type *declspec(Token **rest, Token *tok) {
  *rest = tok->skip("int");
  return Type::ty_int;
}

// func-params = (param ("," param)*)? ")"
// param       = declspec declarator
static Type *func_params(Token **rest, Token *tok, Type *ty) {
  Type head;
  Type *cur = &head;

  while (!tok->equal(")")) {
    if (cur != &head) tok = tok->skip(",");

    Type *basety = declspec(&tok, tok);
    Type *ty = declarator(&tok, tok, basety);
    cur = cur->next = Type::copy_type(ty);
  }

  ty = Type::func_type(ty);
  ty->params = head.next;
  *rest = tok->next;
  return ty;
}

// type-suffix = "(" func-params
//             | "[" num "]" type-suffix
//             | ε
static Type *type_suffix(Token **rest, Token *tok, Type *ty) {
  if (tok->equal("(")) return func_params(rest, tok->next, ty);

  if (tok->equal("[")) {
    int len = tok->next->get_number();
    tok = tok->next->next->skip("]");
    ty = type_suffix(rest, tok, ty);
    return Type::array_of(ty, len);
  }

  *rest = tok;
  return ty;
}

// declarator = "*"* ident
static Type *declarator(Token **rest, Token *tok, Type *ty) {
  while (tok->consume(&tok, "*")) ty = Type::pointer_to(ty);

  if (tok->kind != TokenKind::TK_IDENT) error_tok(tok, "expected a variable name");

  ty = type_suffix(rest, tok->next, ty);
  ty->name = tok;
  return ty;
}

// declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
static Node *declaration(Token **rest, Token *tok) {
  Type *basety = declspec(&tok, tok);

  Node head = {};
  Node *cur = &head;
  int i = 0;

  while (!tok->equal(";")) {
    if (i++ > 0) tok = tok->skip(",");

    Type *ty = declarator(&tok, tok, basety);
    Obj *var = new_lvar(get_ident(ty->name), ty);

    if (!tok->equal("=")) continue;

    Node *lhs = new_var_node(var, ty->name);
    Node *rhs = assign(&tok, tok->next);
    Node *node = new_binary(NodeKind::ND_ASSIGN, lhs, rhs, tok);
    cur = cur->next = new_unary(NodeKind::ND_EXPR_STMT, node, tok);
  }

  Node *node = new_node(NodeKind::ND_BLOCK, tok);
  node->body = head.next;
  *rest = tok->next;
  return node;
}

// stmt = "return" expr ";"
//      | "if" "(" expr ")" stmt ("else" stmt)?
//      | "for" "(" expr-stmt expr? ";" expr? ")" stmt
//      | "while" "(" expr ")" stmt
//      | "{" compound-stmt
//      | expr-stmt
static Node *stmt(Token **rest, Token *tok) {
  if (tok->equal("return")) {
    Node *node = new_node(NodeKind::ND_RETURN, tok);
    node->lhs = expr(&tok, tok->next);
    *rest = tok->skip(";");
    return node;
  }

  if (tok->equal("if")) {
    Node *node = new_node(NodeKind::ND_IF, tok);
    tok = tok->next->skip("(");
    node->cond = expr(&tok, tok);
    tok = tok->skip(")");
    node->then = stmt(&tok, tok);
    if (tok->equal("else")) node->els = stmt(&tok, tok->next);
    *rest = tok;
    return node;
  }

  if (tok->equal("for")) {
    Node *node = new_node(NodeKind::ND_FOR, tok);
    tok = tok->next->skip("(");

    node->init = expr_stmt(&tok, tok);

    if (!tok->equal(";")) node->cond = expr(&tok, tok);
    tok = tok->skip(";");

    if (!tok->equal(")")) node->inc = expr(&tok, tok);
    tok = tok->skip(")");

    node->then = stmt(rest, tok);
    return node;
  }

  if (tok->equal("while")) {
    Node *node = new_node(NodeKind::ND_FOR, tok);
    tok = tok->next->skip("(");
    node->cond = expr(&tok, tok);
    tok = tok->skip(")");
    node->then = stmt(rest, tok);
    return node;
  }

  if (tok->equal("{")) return compound_stmt(rest, tok->next);

  return expr_stmt(rest, tok);
}

// compound-stmt = (declaration | stmt)* "}"
static Node *compound_stmt(Token **rest, Token *tok) {
  Node *node = new_node(NodeKind::ND_BLOCK, tok);

  Node head = {};
  Node *cur = &head;
  while (!tok->equal("}")) {
    if (tok->equal("int"))
      cur = cur->next = declaration(&tok, tok);
    else
      cur = cur->next = stmt(&tok, tok);
    add_type(cur);
  }

  node->body = head.next;
  *rest = tok->next;
  return node;
}

// expr-stmt = expr? ";"
static Node *expr_stmt(Token **rest, Token *tok) {
  if (tok->equal(";")) {
    *rest = tok->next;
    return new_node(NodeKind::ND_BLOCK, tok);
  }

  Node *node = new_node(NodeKind::ND_EXPR_STMT, tok);
  node->lhs = expr(&tok, tok);
  *rest = tok->skip(";");
  return node;
}

// expr = assign
static Node *expr(Token **rest, Token *tok) { return assign(rest, tok); }

// assign = equality ("=" assign)?
static Node *assign(Token **rest, Token *tok) {
  Node *node = equality(&tok, tok);
  if (tok->equal("=")) node = new_binary(NodeKind::ND_ASSIGN, node, assign(&tok, tok->next), tok);
  *rest = tok;
  return node;
}

// equality = relational ("==" relational | "!=" relational)*
static Node *equality(Token **rest, Token *tok) {
  Node *node = relational(&tok, tok);

  for (;;) {
    Token *start = tok;

    if (tok->equal("==")) {
      node = new_binary(NodeKind::ND_EQ, node, relational(&tok, tok->next), start);
      continue;
    }

    if (tok->equal("!=")) {
      node = new_binary(NodeKind::ND_NE, node, relational(&tok, tok->next), start);
      continue;
    }

    *rest = tok;
    return node;
  }
}

// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
static Node *relational(Token **rest, Token *tok) {
  Node *node = add(&tok, tok);

  for (;;) {
    Token *start = tok;

    if (tok->equal("<")) {
      node = new_binary(NodeKind::ND_LT, node, add(&tok, tok->next), start);
      continue;
    }

    if (tok->equal("<=")) {
      node = new_binary(NodeKind::ND_LE, node, add(&tok, tok->next), start);
      continue;
    }

    if (tok->equal(">")) {
      node = new_binary(NodeKind::ND_LT, add(&tok, tok->next), node, start);
      continue;
    }

    if (tok->equal(">=")) {
      node = new_binary(NodeKind::ND_LE, add(&tok, tok->next), node, start);
      continue;
    }

    *rest = tok;
    return node;
  }
}

static Node *new_add(Node *lhs, Node *rhs, Token *tok) {
  add_type(lhs);
  add_type(rhs);

  // num + num
  if (lhs->ty->is_integer() && rhs->ty->is_integer())
    return new_binary(NodeKind::ND_ADD, lhs, rhs, tok);

  if (lhs->ty->base && rhs->ty->base) error_tok(tok, "invalid operands");

  // Canonicalize `num + ptr` to `ptr + num`.
  if (!lhs->ty->base && rhs->ty->base) {
    Node *tmp = lhs;
    lhs = rhs;
    rhs = tmp;
  }

  // ptr + num
  rhs = new_binary(NodeKind::ND_MUL, rhs, new_num(lhs->ty->base->size, tok), tok);
  return new_binary(NodeKind::ND_ADD, lhs, rhs, tok);
}

// Like `+`, `-` is overloaded for the pointer type.
static Node *new_sub(Node *lhs, Node *rhs, Token *tok) {
  add_type(lhs);
  add_type(rhs);

  // num - num
  if (lhs->ty->is_integer() && rhs->ty->is_integer())
    return new_binary(NodeKind::ND_SUB, lhs, rhs, tok);

  // ptr - num
  if (lhs->ty->base && rhs->ty->is_integer()) {
    rhs = new_binary(NodeKind::ND_MUL, rhs, new_num(lhs->ty->base->size, tok), tok);
    add_type(rhs);
    Node *node = new_binary(NodeKind::ND_SUB, lhs, rhs, tok);
    node->ty = lhs->ty;
    return node;
  }

  // ptr - ptr, which returns how many elements are between the two.
  if (lhs->ty->base && rhs->ty->base) {
    Node *node = new_binary(NodeKind::ND_SUB, lhs, rhs, tok);
    node->ty = Type::ty_int;
    return new_binary(NodeKind::ND_DIV, node, new_num(lhs->ty->base->size, tok), tok);
  }

  error_tok(tok, "invalid operands");
}

// add = mul ("+" mul | "-" mul)*
static Node *add(Token **rest, Token *tok) {
  Node *node = mul(&tok, tok);

  for (;;) {
    Token *start = tok;

    if (tok->equal("+")) {
      node = node = new_add(node, mul(&tok, tok->next), start);
      continue;
    }

    if (tok->equal("-")) {
      node = new_sub(node, mul(&tok, tok->next), start);
      continue;
    }

    *rest = tok;
    return node;
  }
}

// mul = unary ("*" unary | "/" unary)*
static Node *mul(Token **rest, Token *tok) {
  Node *node = unary(&tok, tok);

  for (;;) {
    Token *start = tok;

    if (tok->equal("*")) {
      node = new_binary(NodeKind::ND_MUL, node, unary(&tok, tok->next), start);
      continue;
    }

    if (tok->equal("/")) {
      node = new_binary(NodeKind::ND_DIV, node, unary(&tok, tok->next), start);
      continue;
    }

    *rest = tok;
    return node;
  }
}

// unary = ("+" | "-" | "*" | "&") unary
//       | primary
static Node *unary(Token **rest, Token *tok) {
  if (tok->equal("+")) return unary(rest, tok->next);

  if (tok->equal("-")) return new_unary(NodeKind::ND_NEG, unary(rest, tok->next), tok);

  if (tok->equal("&")) return new_unary(NodeKind::ND_ADDR, unary(rest, tok->next), tok);

  if (tok->equal("*")) return new_unary(NodeKind::ND_DEREF, unary(rest, tok->next), tok);

  return postfix(rest, tok);
}

// postfix = primary ("[" expr "]")*
static Node *postfix(Token **rest, Token *tok) {
  Node *node = primary(&tok, tok);

  while (tok->equal("[")) {
    // x[y] is short for *(x+y)
    Token *start = tok;
    Node *idx = expr(&tok, tok->next);
    tok = tok->skip("]");
    node = new_unary(NodeKind::ND_DEREF, new_add(node, idx, start), start);
  }
  *rest = tok;
  return node;
}

// funcall = ident "(" (assign ("," assign)*)? ")"
static Node *funcall(Token **rest, Token *tok) {
  Token *start = tok;
  tok = tok->next->next;

  Node head = {};
  Node *cur = &head;

  while (!tok->equal(")")) {
    if (cur != &head) {
      tok = tok->skip(",");
    }
    cur = cur->next = assign(&tok, tok);
  }

  *rest = tok->skip(")");

  Node *node = new_node(NodeKind::ND_FUNCALL, start);
  node->funcname = strndup(start->loc, start->len);
  node->args = head.next;
  return node;
}

// primary = "(" expr ")" | "sizeof" unary | ident func-args? | num
static Node *primary(Token **rest, Token *tok) {
  if (tok->equal("(")) {
    Node *node = expr(&tok, tok->next);
    *rest = tok->skip(")");
    return node;
  }

  if (tok->equal("sizeof")) {
    Node *node = unary(rest, tok->next);
    add_type(node);
    return new_num(node->ty->size, tok);
  }

  if (tok->kind == TokenKind::TK_IDENT) {
    if (tok->next->equal("(")) {
      return funcall(rest, tok);
    }

    // Variable
    Obj *var = find_var(tok);
    if (!var) error_tok(tok, "undefined variable");
    *rest = tok->next;
    return new_var_node(var, tok);
  }

  if (tok->kind == TokenKind::TK_NUM) {
    Node *node = new_num(tok->val, tok);
    *rest = tok->next;
    return node;
  }

  error_tok(tok, "expected an expression");
}

static void create_param_lvars(Type *param) {
  if (param) {
    create_param_lvars(param->next);
    new_lvar(get_ident(param->name), param);
  }
}

static Token *function(Token *tok, Type *basety) {
  Type *ty = declarator(&tok, tok, basety);

  Obj *fn = new_gvar(get_ident(ty->name), ty);
  fn->is_function = true;

  locals = nullptr;
  create_param_lvars(ty->params);
  fn->params = locals;

  tok = tok->skip("{");
  fn->body = compound_stmt(&tok, tok);
  fn->locals = locals;

  return tok;
}

static Token *global_variable(Token *tok, Type *basety) {
  bool first = true;

  while (!tok->consume(&tok, ";")) {
    if (!first) tok = tok->skip(",");

    first = false;

    Type *ty = declarator(&tok, tok, basety);
    new_gvar(get_ident(ty->name), ty);
  }

  return tok;
}

// Lookahead tokens and returns true if a given token is a start
// of a function definition or declaration.
static bool is_function(Token *tok) {
  if (tok->equal(";")) return false;

  Type dummy;
  Type *ty = declarator(&tok, tok, &dummy);
  return ty->kind == TypeKind::TY_FUNC;
}

// program = stmt*
Obj *parse(Token *tok) {
  globals = nullptr;

  while (tok->kind != TokenKind::TK_EOF) {
    Type *basety = declspec(&tok, tok);

    // Function
    if (is_function(tok)) {
      tok = function(tok, basety);
      continue;
    }

    // Global variable
    tok = global_variable(tok, basety);
  }

  return globals;
}
