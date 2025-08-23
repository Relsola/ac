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

// Scope for local or global variables.
struct VarScope {
  VarScope *next = nullptr;
  char *name = nullptr;
  Obj *var = nullptr;
};

// Scope for struct or union tags
struct TagScope {
  TagScope *next = nullptr;
  char *name = nullptr;
  Type *ty = nullptr;
};

// Represents a block scope.
struct Scope {
  Scope *next = nullptr;

  // C has two block scopes; one is for variables and the other is
  // for struct tags.
  VarScope *vars;
  TagScope *tags;
};

// All local variable instances created during parsing are
// accumulated to this list.
static Obj *locals;

// Likewise, global variables are accumulated to this list.
static Obj *globals;

static Scope *scope = new Scope();

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
static Type *struct_decl(Token **rest, Token *tok);
static Type *union_decl(Token **rest, Token *tok);
static Node *postfix(Token **rest, Token *tok);
static Node *unary(Token **rest, Token *tok);
static Node *primary(Token **rest, Token *tok);

static void enter_scope() {
  Scope *sc = new Scope();
  sc->next = scope;
  scope = sc;
}

static void leave_scope() { scope = scope->next; }

// Find a variable by name.
static Obj *find_var(Token *tok) {
  for (Scope *sc = scope; sc; sc = sc->next)
    for (VarScope *sc2 = sc->vars; sc2; sc2 = sc2->next)
      if (tok->equal(sc2->name)) return sc2->var;

  return nullptr;
}

static Type *find_tag(Token *tok) {
  for (Scope *sc = scope; sc; sc = sc->next)
    for (TagScope *sc2 = sc->tags; sc2; sc2 = sc2->next)
      if (tok->equal(sc2->name)) return sc2->ty;

  return nullptr;
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

static Node *new_num(int64_t val, Token *tok) {
  Node *node = new_node(NodeKind::ND_NUM, tok);
  node->val = val;
  return node;
}

static Node *new_var_node(Obj *var, Token *tok) {
  Node *node = new_node(NodeKind::ND_VAR, tok);
  node->var = var;
  return node;
}

static VarScope *push_scope(char *name, Obj *var) {
  VarScope *sc = new VarScope();
  sc->name = name;
  sc->var = var;
  sc->next = scope->vars;
  scope->vars = sc;
  return sc;
}

static Obj *new_var(char *name, Type *ty) {
  Obj *var = new Obj(name, ty);
  push_scope(name, var);
  return var;
}

static Obj *new_lvar(char *name, Type *ty) {
  Obj *var = new_var(name, ty);
  var->is_local = true;
  var->next = locals;
  locals = var;
  return var;
}

static Obj *new_gvar(char *name, Type *ty) {
  Obj *var = new_var(name, ty);
  var->next = globals;
  globals = var;
  return var;
}

static char *new_unique_name(void) {
  static int id = 0;
  return format(".L..%d", id++);
}

static Obj *new_anon_gvar(Type *ty) { return new_gvar(new_unique_name(), ty); }

static Obj *new_string_literal(char *p, Type *ty) {
  Obj *var = new_anon_gvar(ty);
  var->init_data = p;
  return var;
}

static char *get_ident(Token *tok) {
  if (tok->kind != TokenKind::TK_IDENT) error_tok(tok, "expected an identifier");
  return strndup(tok->loc, tok->len);
}

static void push_tag_scope(Token *tok, Type *ty) {
  TagScope *sc = new TagScope();
  sc->name = strndup(tok->loc, tok->len);
  sc->ty = ty;
  sc->next = scope->tags;
  scope->tags = sc;
}

// declspec = "char" | "short" | "int" | "long" | struct-decl | union-decl
static Type *declspec(Token **rest, Token *tok) {
  if (tok->equal("char")) {
    *rest = tok->next;
    return Type::ty_char;
  }

  if (tok->equal("int")) {
    *rest = tok->next;
    return Type::ty_int;
  }

  if (tok->equal("long")) {
    *rest = tok->next;
    return Type::ty_long;
  }

  if (tok->equal("struct")) return struct_decl(rest, tok->next);

  if (tok->equal("union")) return union_decl(rest, tok->next);

  error_tok(tok, "typename expected");
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

// Returns true if a given token represents a type.
static bool is_typename(Token *tok) {
  return tok->equal("char") || tok->equal("short") || tok->equal("int") || tok->equal("long") ||
         tok->equal("struct") || tok->equal("union");
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

  enter_scope();

  while (!tok->equal("}")) {
    if (is_typename(tok))
      cur = cur->next = declaration(&tok, tok);
    else
      cur = cur->next = stmt(&tok, tok);
    add_type(cur);
  }

  leave_scope();

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

// expr = assign ("," expr)?
static Node *expr(Token **rest, Token *tok) {
  Node *node = assign(&tok, tok);

  if (tok->equal(",")) return new_binary(NodeKind::ND_COMMA, node, expr(rest, tok->next), tok);

  *rest = tok;
  return node;
}

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

// struct-members = (declspec declarator (","  declarator)* ";")*
static void struct_members(Token **rest, Token *tok, Type *ty) {
  Member head;
  Member *cur = &head;

  while (!tok->equal("}")) {
    Type *basety = declspec(&tok, tok);
    int i = 0;

    while (!tok->consume(&tok, ";")) {
      if (i++) tok = tok->skip(",");

      Member *mem = new Member;
      mem->ty = declarator(&tok, tok, basety);
      mem->name = mem->ty->name;
      cur = cur->next = mem;
    }
  }

  *rest = tok->next;
  ty->members = head.next;
}

// struct-union-decl = ident? ("{" struct-members)?
static Type *struct_union_decl(Token **rest, Token *tok) {
  // Read a tag.
  Token *tag = nullptr;
  if (tok->kind == TokenKind::TK_IDENT) {
    tag = tok;
    tok = tok->next;
  }

  if (tag && !tok->equal("{")) {
    Type *ty = find_tag(tag);
    if (!ty) error_tok(tag, "unknown struct type");
    *rest = tok;
    return ty;
  }

  // Construct a struct object.
  Type *ty = new Type();
  ty->kind = TypeKind::TY_STRUCT;
  struct_members(rest, tok->next, ty);
  ty->align = 1;

  // Register the struct type if a name was given.
  if (tag) push_tag_scope(tag, ty);
  return ty;
}

// struct-decl = struct-union-decl
static Type *struct_decl(Token **rest, Token *tok) {
  Type *ty = struct_union_decl(rest, tok);
  ty->kind = TypeKind::TY_STRUCT;

  // Assign offsets within the struct to members.
  int offset = 0;
  for (Member *mem = ty->members; mem; mem = mem->next) {
    offset = align_to(offset, mem->ty->align);
    mem->offset = offset;
    offset += mem->ty->size;

    if (ty->align < mem->ty->align) ty->align = mem->ty->align;
  }
  ty->size = align_to(offset, ty->align);

  return ty;
}

// union-decl = struct-union-decl
static Type *union_decl(Token **rest, Token *tok) {
  Type *ty = struct_union_decl(rest, tok);
  ty->kind = TypeKind::TY_UNION;

  // If union, we don't have to assign offsets because they
  // are already initialized to zero. We need to compute the
  // alignment and the size though.
  for (Member *mem = ty->members; mem; mem = mem->next) {
    if (ty->align < mem->ty->align) ty->align = mem->ty->align;
    if (ty->size < mem->ty->size) ty->size = mem->ty->size;
  }
  ty->size = align_to(ty->size, ty->align);
  return ty;
}

static Member *get_struct_member(Type *ty, Token *tok) {
  for (Member *mem = ty->members; mem; mem = mem->next)
    if (mem->name->len == tok->len && !strncmp(mem->name->loc, tok->loc, tok->len)) return mem;
  error_tok(tok, "no such member");
}

static Node *struct_ref(Node *lhs, Token *tok) {
  add_type(lhs);
  if (lhs->ty->kind != TypeKind::TY_STRUCT && lhs->ty->kind != TypeKind::TY_UNION)
    error_tok(lhs->tok, "not a struct nor a union");

  Node *node = new_unary(NodeKind::ND_MEMBER, lhs, tok);
  node->member = get_struct_member(lhs->ty, tok);
  return node;
}

// postfix = primary ("[" expr "]" | "." ident | "->" ident)*
static Node *postfix(Token **rest, Token *tok) {
  Node *node = primary(&tok, tok);

  for (;;) {
    if (tok->equal("[")) {
      // x[y] is short for *(x+y)
      Token *start = tok;
      Node *idx = expr(&tok, tok->next);
      tok = tok->skip("]");
      node = new_unary(NodeKind::ND_DEREF, new_add(node, idx, start), start);
      continue;
    }

    if (tok->equal(".")) {
      node = struct_ref(node, tok->next);
      tok = tok->next->next;
      continue;
    }

    if (tok->equal("->")) {
      // x->y is short for (*x).y
      node = new_unary(NodeKind::ND_DEREF, node, tok);
      node = struct_ref(node, tok->next);
      tok = tok->next->next;
      continue;
    }

    *rest = tok;
    return node;
  }
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

// primary = "(" "{" stmt+ "}" ")"
//         | "(" expr ")"
//         | "sizeof" unary
//         | ident func-args?
//         | str
//         | num
static Node *primary(Token **rest, Token *tok) {
  if (tok->equal("(") && tok->next->equal("{")) {
    // This is a GNU statement expression.
    Node *node = new_node(NodeKind::ND_STMT_EXPR, tok);
    node->body = compound_stmt(&tok, tok->next->next)->body;
    *rest = tok->skip(")");
    return node;
  }

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

  if (tok->kind == TokenKind::TK_STR) {
    Obj *var = new_string_literal(tok->str, tok->ty);
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
  enter_scope();
  create_param_lvars(ty->params);
  fn->params = locals;

  tok = tok->skip("{");
  fn->body = compound_stmt(&tok, tok);
  fn->locals = locals;
  leave_scope();

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
