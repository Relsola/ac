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

// Scope for local variables, global variables, typedefs
// or enum constants
struct VarScope {
  VarScope *next = nullptr;
  char *name = nullptr;
  Obj *var = nullptr;
  Type *type_def = nullptr;
  Type *enum_ty;
  int enum_val;
};

// Scope for struct, union or enum tags
struct TagScope {
  TagScope *next = nullptr;
  char *name = nullptr;
  Type *ty = nullptr;
};

// Represents a block scope.
struct Scope {
  Scope *next = nullptr;

  // C has two block scopes; one is for variables/typedefs and
  // the other is for struct/union/enum tags.
  VarScope *vars = nullptr;
  TagScope *tags = nullptr;
};

// Variable attributes such as typedef or extern.
struct VarAttr {
  bool is_typedef = false;
  bool is_static = false;
  bool is_extern = false;
  int align = 0;
};

// This struct represents a variable initializer. Since initializers
// can be nested (e.g. `int x[2][2] = {{1, 2}, {3, 4}}`), this struct
// is a tree data structure.
struct Initializer {
  Initializer *next = nullptr;
  Type *ty = nullptr;
  Token *tok = nullptr;
  bool is_flexible = false;

  // If it's not an aggregate type and has an initializer,
  // `expr` has an initialization expression.
  Node *expr = nullptr;

  // If it's an initializer for an aggregate type (e.g. array or struct),
  // `children` has initializers for its children.
  Initializer **children = nullptr;
};

// For local variable initializer.
struct InitDesg {
  InitDesg *next = nullptr;
  int idx = 0;
  Member *member = nullptr;
  Obj *var = nullptr;
};

// All local variable instances created during parsing are
// accumulated to this list.
static Obj *locals = nullptr;

// Likewise, global variables are accumulated to this list.
static Obj *globals = nullptr;

static Scope *scope = new Scope();

// Points to the function object the parser is currently parsing.
static Obj *current_fn = nullptr;

// Lists of all goto statements and labels in the curent function.
static Node *gotos = nullptr;
static Node *labels = nullptr;

// Current "goto" and "continue" jump targets.
static char *brk_label = nullptr;
static char *cont_label = nullptr;

// Points to a node representing a switch if we are parsing
// a switch statement. Otherwise, NULL.
static Node *current_switch = nullptr;

static bool is_typename(Token *tok);
static Type *declspec(Token **rest, Token *tok, VarAttr *attr);
static Type *type_name(Token **rest, Token *tok);
static Type *enum_specifier(Token **rest, Token *tok);
static Type *type_suffix(Token **rest, Token *tok, Type *ty);
static Type *declarator(Token **rest, Token *tok, Type *ty);
static Node *declaration(Token **rest, Token *tok, Type *basety, VarAttr *attr);
static void initializer2(Token **rest, Token *tok, Initializer *init);
static Initializer *initializer(Token **rest, Token *tok, Type *ty, Type **new_ty);
static Node *lvar_initializer(Token **rest, Token *tok, Obj *var);
static void gvar_initializer(Token **rest, Token *tok, Obj *var);
static Node *compound_stmt(Token **rest, Token *tok);
static Node *stmt(Token **rest, Token *tok);
static Node *expr_stmt(Token **rest, Token *tok);
static Node *expr(Token **rest, Token *tok);
static int64_t eval(Node *node);
static int64_t eval(Node *node, char **label);
static int64_t eval_rval(Node *node, char **label);
static Node *assign(Token **rest, Token *tok);
static Node *logor(Token **rest, Token *tok);
static int64_t const_expr(Token **rest, Token *tok);
static Node *conditional(Token **rest, Token *tok);
static Node *logand(Token **rest, Token *tok);
static Node *bit_or(Token **rest, Token *tok);
static Node *bit_xor(Token **rest, Token *tok);
static Node *bit_and(Token **rest, Token *tok);
static Node *equality(Token **rest, Token *tok);
static Node *relational(Token **rest, Token *tok);
static Node *shift(Token **rest, Token *tok);
static Node *add(Token **rest, Token *tok);
static Node *new_add(Node *lhs, Node *rhs, Token *tok);
static Node *new_sub(Node *lhs, Node *rhs, Token *tok);
static Node *mul(Token **rest, Token *tok);
static Node *cast(Token **rest, Token *tok);
static Type *struct_decl(Token **rest, Token *tok);
static Type *union_decl(Token **rest, Token *tok);
static Node *postfix(Token **rest, Token *tok);
static Node *unary(Token **rest, Token *tok);
static Node *primary(Token **rest, Token *tok);
static Token *parse_typedef(Token *tok, Type *basety);
static bool is_function(Token *tok);
static Token *function(Token *tok, Type *basety, VarAttr *attr);
static Token *global_variable(Token *tok, Type *basety, VarAttr *attr);

static void enter_scope() {
  Scope *sc = new Scope();
  sc->next = scope;
  scope = sc;
}

static void leave_scope() { scope = scope->next; }

// Find a variable by name.
static VarScope *find_var(Token *tok) {
  for (Scope *sc = scope; sc; sc = sc->next)
    for (VarScope *sc2 = sc->vars; sc2; sc2 = sc2->next)
      if (tok->equal(sc2->name)) return sc2;

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

static Node *new_long(int64_t val, Token *tok) {
  Node *node = new_node(NodeKind::ND_NUM, tok);
  node->val = val;
  node->ty = Type::ty_long;
  return node;
}

static Node *new_var_node(Obj *var, Token *tok) {
  Node *node = new_node(NodeKind::ND_VAR, tok);
  node->var = var;
  return node;
}

Node *new_cast(Node *expr, Type *ty) {
  add_type(expr);

  Node *node = new Node();
  node->kind = NodeKind::ND_CAST;
  node->tok = expr->tok;
  node->lhs = expr;
  node->ty = Type::copy_type(ty);
  return node;
}

static VarScope *push_scope(char *name) {
  VarScope *sc = new VarScope();
  sc->name = name;
  sc->next = scope->vars;
  scope->vars = sc;
  return sc;
}

static Initializer *new_initializer(Type *ty, bool is_flexible) {
  Initializer *init = new Initializer();
  init->ty = ty;

  if (ty->kind == TypeKind::TY_ARRAY) {
    if (is_flexible && ty->size < 0) {
      init->is_flexible = true;
      return init;
    }

    init->children = new Initializer *[ty->array_len]();
    for (int i = 0; i < ty->array_len; i++) init->children[i] = new_initializer(ty->base, false);
    return init;
  }

  if (ty->kind == TypeKind::TY_STRUCT || ty->kind == TypeKind::TY_UNION) {
    // Count the number of struct members.
    int len = 0;
    for (Member *mem = ty->members; mem; mem = mem->next) len++;

    init->children = new Initializer *();

    for (Member *mem = ty->members; mem; mem = mem->next) {
      if (is_flexible && ty->is_flexible && !mem->next) {
        Initializer *child = new Initializer();
        child->ty = mem->ty;
        child->is_flexible = true;
        init->children[mem->idx] = child;
      } else {
        init->children[mem->idx] = new_initializer(mem->ty, false);
      }
    }

    return init;
  }

  return init;
}

static Obj *new_var(char *name, Type *ty) {
  Obj *var = new Obj(name, ty);
  var->align = ty->align;
  push_scope(name)->var = var;
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
  var->is_static = true;
  var->is_definition = true;
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

static Type *find_typedef(Token *tok) {
  if (tok->kind == TokenKind::TK_IDENT) {
    VarScope *sc = find_var(tok);
    if (sc) return sc->type_def;
  }
  return nullptr;
}

static void push_tag_scope(Token *tok, Type *ty) {
  TagScope *sc = new TagScope();
  sc->name = strndup(tok->loc, tok->len);
  sc->ty = ty;
  sc->next = scope->tags;
  scope->tags = sc;
}

// declspec = ("void" | "_Bool" | "char" | "short" | "int" | "long"
//             | "typedef" | "static" | "extern"
//             | "signed"
//             | struct-decl | union-decl | typedef-name
//             | enum-specifier)+
//
// The order of typenames in a type-specifier doesn't matter. For
// example, `int long static` means the same as `static long int`.
// That can also be written as `static long` because you can omit
// `int` if `long` or `short` are specified. However, something like
// `char int` is not a valid type specifier. We have to accept only a
// limited combinations of the typenames.
//
// In this function, we count the number of occurrences of each typename
// while keeping the "current" type object that the typenames up
// until that point represent. When we reach a non-typename token,
// we returns the current type object.
static Type *declspec(Token **rest, Token *tok, VarAttr *attr) {
  // We use a single integer as counters for all typenames.
  // For example, bits 0 and 1 represents how many times we saw the
  // keyword "void" so far. With this, we can use a switch statement
  // as you can see below.
  enum {
    VOID = 1 << 0,
    BOOL = 1 << 2,
    CHAR = 1 << 4,
    SHORT = 1 << 6,
    INT = 1 << 8,
    LONG = 1 << 10,
    OTHER = 1 << 12,
    SIGNED = 1 << 13,
  };

  Type *ty = Type::ty_int;
  int counter = 0;

  while (is_typename(tok)) {
    // Handle storage class specifiers.
    if (tok->equal("typedef") || tok->equal("static") || tok->equal("extern")) {
      if (!attr) error_tok(tok, "storage class specifier is not allowed in this context");

      if (tok->equal("typedef"))
        attr->is_typedef = true;
      else if (tok->equal("static"))
        attr->is_static = true;
      else
        attr->is_extern = true;

      if (attr->is_typedef && attr->is_static + attr->is_extern > 1)
        error_tok(tok, "typedef may not be used together with static or extern");
      tok = tok->next;
      continue;
    }

    if (tok->equal("_Alignas")) {
      if (!attr) error_tok(tok, "_Alignas is not allowed in this context");
      tok = tok->next->skip("(");

      if (is_typename(tok))
        attr->align = type_name(&tok, tok)->align;
      else
        attr->align = const_expr(&tok, tok);
      tok = tok->skip(")");
      continue;
    }

    // Handle user-defined types.
    Type *ty2 = find_typedef(tok);
    if (tok->equal("struct") || tok->equal("union") || tok->equal("enum") || ty2) {
      if (counter) break;

      if (tok->equal("struct")) {
        ty = struct_decl(&tok, tok->next);
      } else if (tok->equal("union")) {
        ty = union_decl(&tok, tok->next);
      } else if (tok->equal("enum")) {
        ty = enum_specifier(&tok, tok->next);
      } else {
        ty = ty2;
        tok = tok->next;
      }

      counter += OTHER;
      continue;
    }

    // Handle built-in types.
    if (tok->equal("void"))
      counter += VOID;
    else if (tok->equal("_Bool"))
      counter += BOOL;
    else if (tok->equal("char"))
      counter += CHAR;
    else if (tok->equal("short"))
      counter += SHORT;
    else if (tok->equal("int"))
      counter += INT;
    else if (tok->equal("long"))
      counter += LONG;
    else if (tok->equal("signed"))
      counter |= SIGNED;
    else
      unreachable();

    switch (counter) {
      case VOID:
        ty = Type::ty_void;
        break;
      case BOOL:
        ty = Type::ty_bool;
        break;
      case CHAR:
      case SIGNED + CHAR:
        ty = Type::ty_char;
        break;
      case SHORT:
      case SHORT + INT:
      case SIGNED + SHORT:
      case SIGNED + SHORT + INT:
        ty = Type::ty_short;
        break;
      case INT:
      case SIGNED:
      case SIGNED + INT:
        ty = Type::ty_int;
        break;
      case LONG:
      case LONG + INT:
      case LONG + LONG:
      case LONG + LONG + INT:
      case SIGNED + LONG:
      case SIGNED + LONG + INT:
      case SIGNED + LONG + LONG:
      case SIGNED + LONG + LONG + INT:
        ty = Type::ty_long;
        break;
      default:
        error_tok(tok, "invalid type");
    }

    tok = tok->next;
  }

  *rest = tok;
  return ty;
}

// func-params = ("void" | param ("," param)* ("," "...")?)? ")"
// param       = declspec declarator
static Type *func_params(Token **rest, Token *tok, Type *ty) {
  if (tok->equal("void") && tok->next->equal(")")) {
    *rest = tok->next->next;
    return Type::func_type(ty);
  }

  Type head;
  Type *cur = &head;
  bool is_variadic = false;

  while (!tok->equal(")")) {
    if (cur != &head) tok = tok->skip(",");

    if (tok->equal("...")) {
      is_variadic = true;
      tok = tok->next;
      tok->skip(")");
      break;
    }

    Type *ty2 = declspec(&tok, tok, nullptr);
    ty2 = declarator(&tok, tok, ty2);

    // "array of T" is converted to "pointer to T" only in the parameter
    // context. For example, *argv[] is converted to **argv by this.
    if (ty2->kind == TypeKind::TY_ARRAY) {
      Token *name = ty2->name;
      ty2 = Type::pointer_to(ty2->base);
      ty2->name = name;
    }

    cur = cur->next = Type::copy_type(ty2);
  }

  if (cur == &head) is_variadic = true;

  ty = Type::func_type(ty);
  ty->params = head.next;
  ty->is_variadic = is_variadic;
  *rest = tok->next;
  return ty;
}

// array-dimensions = const-expr? "]" type-suffix
static Type *array_dimensions(Token **rest, Token *tok, Type *ty) {
  if (tok->equal("]")) {
    ty = type_suffix(rest, tok->next, ty);
    return Type::array_of(ty, -1);
  }

  int sz = const_expr(&tok, tok);
  tok = tok->skip("]");
  ty = type_suffix(rest, tok, ty);
  return Type::array_of(ty, sz);
}

// type-suffix = "(" func-params
//             | "[" array-dimensions
//             | ε
static Type *type_suffix(Token **rest, Token *tok, Type *ty) {
  if (tok->equal("(")) return func_params(rest, tok->next, ty);

  if (tok->equal("[")) return array_dimensions(rest, tok->next, ty);

  *rest = tok;
  return ty;
}

// abstract-declarator = "*"* ("(" abstract-declarator ")")? type-suffix
static Type *abstract_declarator(Token **rest, Token *tok, Type *ty) {
  while (tok->equal("*")) {
    ty = Type::pointer_to(ty);
    tok = tok->next;
  }

  if (tok->equal("(")) {
    Token *start = tok;
    Type dummy = {};
    abstract_declarator(&tok, start->next, &dummy);
    tok = tok->skip(")");
    ty = type_suffix(rest, tok, ty);
    return abstract_declarator(&tok, start->next, ty);
  }

  return type_suffix(rest, tok, ty);
}

// type-name = declspec abstract-declarator
static Type *type_name(Token **rest, Token *tok) {
  Type *ty = declspec(&tok, tok, nullptr);
  return abstract_declarator(rest, tok, ty);
}

// declarator = "*"* ("(" ident ")" | "(" declarator ")" | ident) type-suffix
static Type *declarator(Token **rest, Token *tok, Type *ty) {
  while (tok->consume(&tok, "*")) ty = Type::pointer_to(ty);

  if (tok->equal("(")) {
    Token *start = tok;
    Type dummy = {};
    declarator(&tok, start->next, &dummy);
    tok = tok->skip(")");
    ty = type_suffix(rest, tok, ty);
    return declarator(&tok, start->next, ty);
  }

  if (tok->kind != TokenKind::TK_IDENT) error_tok(tok, "expected a variable name");

  ty = type_suffix(rest, tok->next, ty);
  ty->name = tok;
  return ty;
}

static bool is_end(Token *tok) {
  return tok->equal("}") || (tok->equal(",") && tok->next->equal("}"));
}

static bool consume_end(Token **rest, Token *tok) {
  if (tok->equal("}")) {
    *rest = tok->next;
    return true;
  }

  if (tok->equal(",") && tok->next->equal("}")) {
    *rest = tok->next->next;
    return true;
  }

  return false;
}

// enum-specifier = ident? "{" enum-list? "}"
//                | ident ("{" enum-list? "}")?
//
// enum-list      = ident ("=" num)? ("," ident ("=" num)?)* ","?
static Type *enum_specifier(Token **rest, Token *tok) {
  Type *ty = Type::enum_type();

  // Read a struct tag.
  Token *tag = nullptr;
  if (tok->kind == TokenKind::TK_IDENT) {
    tag = tok;
    tok = tok->next;
  }

  if (tag && !tok->equal("{")) {
    Type *ty = find_tag(tag);
    if (!ty) error_tok(tag, "unknown enum type");
    if (ty->kind != TypeKind::TY_ENUM) error_tok(tag, "not an enum tag");
    *rest = tok;
    return ty;
  }

  tok = tok->skip("{");

  // Read an enum-list.
  int i = 0;
  int val = 0;
  while (!consume_end(rest, tok)) {
    if (i++ > 0) tok = tok->skip(",");

    char *name = get_ident(tok);
    tok = tok->next;

    if (tok->equal("=")) val = const_expr(&tok, tok->next);

    VarScope *sc = push_scope(name);
    sc->enum_ty = ty;
    sc->enum_val = val++;
  }

  if (tag) push_tag_scope(tag, ty);
  return ty;
}

// declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
static Node *declaration(Token **rest, Token *tok, Type *basety, VarAttr *attr) {
  Node head = {};
  Node *cur = &head;
  int i = 0;

  while (!tok->equal(";")) {
    if (i++ > 0) tok = tok->skip(",");

    Type *ty = declarator(&tok, tok, basety);

    if (ty->kind == TypeKind::TY_VOID) error_tok(tok, "variable declared void");

    if (attr && attr->is_static) {
      // static local variable
      Obj *var = new_anon_gvar(ty);
      push_scope(get_ident(ty->name))->var = var;
      if (tok->equal("=")) gvar_initializer(&tok, tok->next, var);
      continue;
    }

    Obj *var = new_lvar(get_ident(ty->name), ty);
    if (attr && attr->align) var->align = attr->align;

    if (tok->equal("=")) {
      Node *expr = lvar_initializer(&tok, tok->next, var);
      cur = cur->next = new_unary(NodeKind::ND_EXPR_STMT, expr, tok);
    }

    if (var->ty->size < 0) error_tok(ty->name, "variable has incomplete type");
    if (var->ty->kind == TypeKind::TY_VOID) error_tok(ty->name, "variable declared void");
  }

  Node *node = new_node(NodeKind::ND_BLOCK, tok);
  node->body = head.next;
  *rest = tok->next;
  return node;
}

static Token *skip_excess_element(Token *tok) {
  if (tok->equal("{")) {
    tok = skip_excess_element(tok->next);
    return tok->skip("}");
  }

  assign(&tok, tok);
  return tok;
}

// string-initializer = string-literal
static void string_initializer(Token **rest, Token *tok, Initializer *init) {
  if (init->is_flexible)
    *init = *new_initializer(Type::array_of(init->ty->base, tok->ty->array_len), false);

  int len = std::min(init->ty->array_len, tok->ty->array_len);
  for (int i = 0; i < len; i++) init->children[i]->expr = new_num(tok->str[i], tok);
  *rest = tok->next;
}

static int count_array_init_elements(Token *tok, Type *ty) {
  Initializer *dummy = new_initializer(ty->base, false);
  int i = 0;

  for (; !consume_end(&tok, tok); i++) {
    if (i > 0) tok = tok->skip(",");
    initializer2(&tok, tok, dummy);
  }
  return i;
}

// array-initializer1 = "{" initializer ("," initializer)* ","? "}"
static void array_initializer1(Token **rest, Token *tok, Initializer *init) {
  tok = tok->skip("{");

  if (init->is_flexible) {
    int len = count_array_init_elements(tok, init->ty);
    *init = *new_initializer(Type::array_of(init->ty->base, len), false);
  }

  for (int i = 0; !consume_end(rest, tok); i++) {
    if (i > 0) tok = tok->skip(",");

    if (i < init->ty->array_len)
      initializer2(&tok, tok, init->children[i]);
    else
      tok = skip_excess_element(tok);
  }
}

// array-initializer2 = initializer ("," initializer)*
static void array_initializer2(Token **rest, Token *tok, Initializer *init) {
  if (init->is_flexible) {
    int len = count_array_init_elements(tok, init->ty);
    *init = *new_initializer(Type::array_of(init->ty->base, len), false);
  }

  for (int i = 0; i < init->ty->array_len && !is_end(tok); i++) {
    if (i > 0) tok = tok->skip(",");
    initializer2(&tok, tok, init->children[i]);
  }
  *rest = tok;
}

// struct-initializer1 = "{" initializer ("," initializer)* ","? "}"
static void struct_initializer1(Token **rest, Token *tok, Initializer *init) {
  tok = tok->skip("{");

  Member *mem = init->ty->members;

  while (!consume_end(rest, tok)) {
    if (mem != init->ty->members) tok = tok->skip(",");

    if (mem) {
      initializer2(&tok, tok, init->children[mem->idx]);
      mem = mem->next;
    } else {
      tok = skip_excess_element(tok);
    }
  }
}

// struct-initializer2 = initializer ("," initializer)*
static void struct_initializer2(Token **rest, Token *tok, Initializer *init) {
  bool first = true;

  for (Member *mem = init->ty->members; mem && !is_end(tok); mem = mem->next) {
    if (!first) tok = tok->skip(",");
    first = false;
    initializer2(&tok, tok, init->children[mem->idx]);
  }
  *rest = tok;
}

static void union_initializer(Token **rest, Token *tok, Initializer *init) {
  // Unlike structs, union initializers take only one initializer,
  // and that initializes the first union member.
  if (tok->equal("{")) {
    initializer2(&tok, tok->next, init->children[0]);
    tok->consume(&tok, ",");
    *rest = tok->skip("}");
  } else {
    initializer2(rest, tok, init->children[0]);
  }
}

// initializer = string-initializer | array-initializer
//             | struct-initializer | union-initializer
//             | assign
static void initializer2(Token **rest, Token *tok, Initializer *init) {
  if (init->ty->kind == TypeKind::TY_ARRAY && tok->kind == TokenKind::TK_STR) {
    string_initializer(rest, tok, init);
    return;
  }

  if (init->ty->kind == TypeKind::TY_ARRAY) {
    if (tok->equal("{"))
      array_initializer1(rest, tok, init);
    else
      array_initializer2(rest, tok, init);
    return;
  }

  if (init->ty->kind == TypeKind::TY_STRUCT) {
    if (tok->equal("{")) {
      struct_initializer1(rest, tok, init);
      return;
    }

    // A struct can be initialized with another struct. E.g.
    // `struct T x = y;` where y is a variable of type `struct T`.
    // Handle that case first.

    Node *expr = assign(rest, tok);
    add_type(expr);
    if (expr->ty->kind == TypeKind::TY_STRUCT) {
      init->expr = expr;
      return;
    }

    struct_initializer2(rest, tok, init);
    return;
  }

  if (init->ty->kind == TypeKind::TY_UNION) {
    union_initializer(rest, tok, init);
    return;
  }

  if (tok->equal("{")) {
    // An initializer for a scalar variable can be surrounded by
    // braces. E.g. `int x = {3};`. Handle that case.
    initializer2(&tok, tok->next, init);
    *rest = tok->skip("}");
    return;
  }

  init->expr = assign(rest, tok);
}

static Type *copy_struct_type(Type *ty) {
  ty = Type::copy_type(ty);

  Member head = {};
  Member *cur = &head;
  for (Member *mem = ty->members; mem; mem = mem->next) {
    Member *m = new Member();
    *m = *mem;
    cur = cur->next = m;
  }

  ty->members = head.next;
  return ty;
}

static Initializer *initializer(Token **rest, Token *tok, Type *ty, Type **new_ty) {
  Initializer *init = new_initializer(ty, true);
  initializer2(rest, tok, init);

  if ((ty->kind == TypeKind::TY_STRUCT || ty->kind == TypeKind::TY_UNION) && ty->is_flexible) {
    ty = copy_struct_type(ty);

    Member *mem = ty->members;
    while (mem->next) mem = mem->next;
    mem->ty = init->children[mem->idx]->ty;
    ty->size += mem->ty->size;

    *new_ty = ty;
    return init;
  }

  *new_ty = init->ty;
  return init;
}

static Node *init_desg_expr(InitDesg *desg, Token *tok) {
  if (desg->var) return new_var_node(desg->var, tok);

  if (desg->member) {
    Node *node = new_unary(NodeKind::ND_MEMBER, init_desg_expr(desg->next, tok), tok);
    node->member = desg->member;
    return node;
  }

  Node *lhs = init_desg_expr(desg->next, tok);
  Node *rhs = new_num(desg->idx, tok);
  return new_unary(NodeKind::ND_DEREF, new_add(lhs, rhs, tok), tok);
}

static Node *create_lvar_init(Initializer *init, Type *ty, InitDesg *desg, Token *tok) {
  if (ty->kind == TypeKind::TY_ARRAY) {
    Node *node = new_node(NodeKind::ND_NULL_EXPR, tok);
    for (int i = 0; i < ty->array_len; i++) {
      InitDesg desg2 = {desg, i};
      Node *rhs = create_lvar_init(init->children[i], ty->base, &desg2, tok);
      node = new_binary(NodeKind::ND_COMMA, node, rhs, tok);
    }
    return node;
  }

  if (ty->kind == TypeKind::TY_STRUCT && !init->expr) {
    Node *node = new_node(NodeKind::ND_NULL_EXPR, tok);

    for (Member *mem = ty->members; mem; mem = mem->next) {
      InitDesg desg2 = {desg, 0, mem};
      Node *rhs = create_lvar_init(init->children[mem->idx], mem->ty, &desg2, tok);
      node = new_binary(NodeKind::ND_COMMA, node, rhs, tok);
    }
    return node;
  }

  if (ty->kind == TypeKind::TY_UNION) {
    InitDesg desg2 = {desg, 0, ty->members};
    return create_lvar_init(init->children[0], ty->members->ty, &desg2, tok);
  }

  if (!init->expr) return new_node(NodeKind::ND_NULL_EXPR, tok);

  Node *lhs = init_desg_expr(desg, tok);
  return new_binary(NodeKind::ND_ASSIGN, lhs, init->expr, tok);
}

// A variable definition with an initializer is a shorthand notation
// for a variable definition followed by assignments. This function
// generates assignment expressions for an initializer. For example,
// `int x[2][2] = {{6, 7}, {8, 9}}` is converted to the following
// expressions:
//
//   x[0][0] = 6;
//   x[0][1] = 7;
//   x[1][0] = 8;
//   x[1][1] = 9;
static Node *lvar_initializer(Token **rest, Token *tok, Obj *var) {
  Initializer *init = initializer(rest, tok, var->ty, &var->ty);
  InitDesg desg = {nullptr, 0, nullptr, var};

  // If a partial initializer list is given, the standard requires
  // that unspecified elements are set to 0. Here, we simply
  // zero-initialize the entire memory region of a variable before
  // initializing it with user-supplied values.
  Node *lhs = new_node(NodeKind::ND_MEMZERO, tok);
  lhs->var = var;

  Node *rhs = create_lvar_init(init, var->ty, &desg, tok);
  return new_binary(NodeKind::ND_COMMA, lhs, rhs, tok);
}

static void write_buf(char *buf, uint64_t val, int sz) {
  if (sz == 1)
    *buf = val;
  else if (sz == 2)
    *(uint16_t *)buf = val;
  else if (sz == 4)
    *(uint32_t *)buf = val;
  else if (sz == 8)
    *(uint64_t *)buf = val;
  else
    unreachable();
}

static Relocation *write_gvar_data(Relocation *cur, Initializer *init, Type *ty, char *buf,
                                   int offset) {
  if (ty->kind == TypeKind::TY_ARRAY) {
    int sz = ty->base->size;
    for (int i = 0; i < ty->array_len; i++)
      cur = write_gvar_data(cur, init->children[i], ty->base, buf, offset + sz * i);
    return cur;
  }

  if (ty->kind == TypeKind::TY_STRUCT) {
    for (Member *mem = ty->members; mem; mem = mem->next)
      cur = write_gvar_data(cur, init->children[mem->idx], mem->ty, buf, offset + mem->offset);
    return cur;
  }

  if (ty->kind == TypeKind::TY_UNION)
    return write_gvar_data(cur, init->children[0], ty->members->ty, buf, offset);

  if (!init->expr) return cur;

  char *label = nullptr;
  uint64_t val = eval(init->expr, &label);

  if (!label) {
    write_buf(buf + offset, val, ty->size);
    return cur;
  }

  Relocation *rel = new Relocation();
  rel->offset = offset;
  rel->label = label;
  rel->addend = val;
  cur->next = rel;
  return cur->next;
}

// Initializers for global variables are evaluated at compile-time and
// embedded to .data section. This function serializes Initializer
// objects to a flat byte array. It is a compile error if an
// initializer list contains a non-constant expression.
static void gvar_initializer(Token **rest, Token *tok, Obj *var) {
  Initializer *init = initializer(rest, tok, var->ty, &var->ty);

  Relocation head = {};
  char *buf = new char[var->ty->size]();
  write_gvar_data(&head, init, var->ty, buf, 0);
  var->init_data = buf;
  var->rel = head.next;
}

// Returns true if a given token represents a type.
static bool is_typename(Token *tok) {
  static char *kw[] = {
      "void",
      "_Bool",
      "char",
      "short",
      "int",
      "long",
      "struct",
      "union",
      "enum",
      "typedef",
      "static",
      "extern",
      "_Alignas",
      "signed",
  };

  for (int i = 0; i < sizeof(kw) / sizeof(*kw); i++)
    if (tok->equal(kw[i])) return true;

  return find_typedef(tok);
  ;
}

// stmt = "return" expr? ";"
//      | "if" "(" expr ")" stmt ("else" stmt)?
//      | "switch" "(" expr ")" stmt
//      | "case" const-expr ":" stmt
//      | "default" ":" stmt
//      | "for" "(" expr-stmt expr? ";" expr? ")" stmt
//      | "while" "(" expr ")" stmt
//      | "do" stmt "while" "(" expr ")" ";"
//      | "goto" ident ";"
//      | "break" ";"
//      | "continue" ";"
//      | ident ":" stmt
//      | "{" compound-stmt
//      | expr-stmt
static Node *stmt(Token **rest, Token *tok) {
  if (tok->equal("return")) {
    Node *node = new_node(NodeKind::ND_RETURN, tok);
    if (tok->next->consume(rest, ";")) return node;

    Node *exp = expr(&tok, tok->next);
    *rest = tok->skip(";");

    add_type(exp);
    node->lhs = new_cast(exp, current_fn->ty->return_ty);
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

  if (tok->equal("switch")) {
    Node *node = new_node(NodeKind::ND_SWITCH, tok);
    tok = tok->next->skip("(");
    node->cond = expr(&tok, tok);
    tok = tok->skip(")");

    Node *sw = current_switch;
    current_switch = node;

    char *brk = brk_label;
    brk_label = node->brk_label = new_unique_name();

    node->then = stmt(rest, tok);

    current_switch = sw;
    brk_label = brk;
    return node;
  }

  if (tok->equal("case")) {
    if (!current_switch) error_tok(tok, "stray case");

    Node *node = new_node(NodeKind::ND_CASE, tok);
    int val = const_expr(&tok, tok->next);
    tok = tok->skip(":");
    node->label = new_unique_name();
    node->lhs = stmt(rest, tok);
    node->val = val;
    node->case_next = current_switch->case_next;
    current_switch->case_next = node;
    return node;
  }

  if (tok->equal("default")) {
    if (!current_switch) error_tok(tok, "stray default");

    Node *node = new_node(NodeKind::ND_CASE, tok);
    tok = tok->next->skip(":");
    node->label = new_unique_name();
    node->lhs = stmt(rest, tok);
    current_switch->default_case = node;
    return node;
  }

  if (tok->equal("for")) {
    Node *node = new_node(NodeKind::ND_FOR, tok);
    tok = tok->next->skip("(");

    enter_scope();

    char *brk = brk_label;
    char *cont = cont_label;
    brk_label = node->brk_label = new_unique_name();
    cont_label = node->cont_label = new_unique_name();

    if (is_typename(tok)) {
      Type *basety = declspec(&tok, tok, nullptr);
      node->init = declaration(&tok, tok, basety, nullptr);
    } else {
      node->init = expr_stmt(&tok, tok);
    }

    if (!tok->equal(";")) node->cond = expr(&tok, tok);
    tok = tok->skip(";");

    if (!tok->equal(")")) node->inc = expr(&tok, tok);
    tok = tok->skip(")");

    node->then = stmt(rest, tok);

    leave_scope();
    brk_label = brk;
    cont_label = cont;
    return node;
  }

  if (tok->equal("while")) {
    Node *node = new_node(NodeKind::ND_FOR, tok);
    tok = tok->next->skip("(");
    node->cond = expr(&tok, tok);
    tok = tok->skip(")");

    char *brk = brk_label;
    char *cont = cont_label;
    brk_label = node->brk_label = new_unique_name();
    cont_label = node->cont_label = new_unique_name();

    node->then = stmt(rest, tok);

    brk_label = brk;
    cont_label = cont;
    return node;
  }

  if (tok->equal("do")) {
    Node *node = new_node(NodeKind::ND_DO, tok);

    char *brk = brk_label;
    char *cont = cont_label;
    brk_label = node->brk_label = new_unique_name();
    cont_label = node->cont_label = new_unique_name();

    node->then = stmt(&tok, tok->next);

    brk_label = brk;
    cont_label = cont;

    tok = tok->skip("while");
    tok = tok->skip("(");
    node->cond = expr(&tok, tok);
    tok = tok->skip(")");
    *rest = tok->skip(";");
    return node;
  }

  if (tok->equal("goto")) {
    Node *node = new_node(NodeKind::ND_GOTO, tok);
    node->label = get_ident(tok->next);
    node->goto_next = gotos;
    gotos = node;
    *rest = tok->next->next->skip(";");
    return node;
  }

  if (tok->equal("break")) {
    if (!brk_label) error_tok(tok, "stray break");
    Node *node = new_node(NodeKind::ND_GOTO, tok);
    node->unique_label = brk_label;
    *rest = tok->next->skip(";");
    return node;
  }

  if (tok->equal("continue")) {
    if (!cont_label) error_tok(tok, "stray continue");
    Node *node = new_node(NodeKind::ND_GOTO, tok);
    node->unique_label = cont_label;
    *rest = tok->next->skip(";");
    return node;
  }

  if (tok->kind == TokenKind::TK_IDENT && tok->next->equal(":")) {
    Node *node = new_node(NodeKind::ND_LABEL, tok);
    node->label = strndup(tok->loc, tok->len);
    node->unique_label = new_unique_name();
    node->lhs = stmt(rest, tok->next->next);
    node->goto_next = labels;
    labels = node;
    return node;
  }

  if (tok->equal("{")) return compound_stmt(rest, tok->next);

  return expr_stmt(rest, tok);
}

// compound-stmt = (typedef | declaration | stmt)* "}"
static Node *compound_stmt(Token **rest, Token *tok) {
  Node *node = new_node(NodeKind::ND_BLOCK, tok);

  Node head = {};
  Node *cur = &head;

  enter_scope();

  while (!tok->equal("}")) {
    if (is_typename(tok) && !tok->next->equal(":")) {
      VarAttr attr = {};
      Type *basety = declspec(&tok, tok, &attr);

      if (attr.is_typedef) {
        tok = parse_typedef(tok, basety);
        continue;
      }

      if (is_function(tok)) {
        tok = function(tok, basety, &attr);
        continue;
      }

      if (attr.is_extern) {
        tok = global_variable(tok, basety, &attr);
        continue;
      }

      cur = cur->next = declaration(&tok, tok, basety, &attr);
    } else {
      cur = cur->next = stmt(&tok, tok);
    }
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

static int64_t eval(Node *node) { return eval(node, nullptr); }

// Evaluate a given node as a constant expression.
//
// A constant expression is either just a number or ptr+n where ptr
// is a pointer to a global variable and n is a positive/negative
// number. The latter form is accepted only as an initialization
// expression for a global variable.
static int64_t eval(Node *node, char **label) {
  add_type(node);

  switch (node->kind) {
    case NodeKind::ND_ADD:
      return eval(node->lhs, label) + eval(node->rhs);
    case NodeKind::ND_SUB:
      return eval(node->lhs, label) - eval(node->rhs);
    case NodeKind::ND_MUL:
      return eval(node->lhs) * eval(node->rhs);
    case NodeKind::ND_DIV:
      return eval(node->lhs) / eval(node->rhs);
    case NodeKind::ND_NEG:
      return -eval(node->lhs);
    case NodeKind::ND_MOD:
      return eval(node->lhs) % eval(node->rhs);
    case NodeKind::ND_BITAND:
      return eval(node->lhs) & eval(node->rhs);
    case NodeKind::ND_BITOR:
      return eval(node->lhs) | eval(node->rhs);
    case NodeKind::ND_BITXOR:
      return eval(node->lhs) ^ eval(node->rhs);
    case NodeKind::ND_SHL:
      return eval(node->lhs) << eval(node->rhs);
    case NodeKind::ND_SHR:
      return eval(node->lhs) >> eval(node->rhs);
    case NodeKind::ND_EQ:
      return eval(node->lhs) == eval(node->rhs);
    case NodeKind::ND_NE:
      return eval(node->lhs) != eval(node->rhs);
    case NodeKind::ND_LT:
      return eval(node->lhs) < eval(node->rhs);
    case NodeKind::ND_LE:
      return eval(node->lhs) <= eval(node->rhs);
    case NodeKind::ND_COND:
      return eval(node->cond) ? eval(node->then, label) : eval(node->els, label);
    case NodeKind::ND_COMMA:
      return eval(node->rhs, label);
    case NodeKind::ND_NOT:
      return !eval(node->lhs);
    case NodeKind::ND_BITNOT:
      return ~eval(node->lhs);
    case NodeKind::ND_LOGAND:
      return eval(node->lhs) && eval(node->rhs);
    case NodeKind::ND_LOGOR:
      return eval(node->lhs) || eval(node->rhs);
    case NodeKind::ND_CAST: {
      int64_t val = eval(node->lhs, label);
      if (node->ty->is_integer()) {
        switch (node->ty->size) {
          case 1:
            return (uint8_t)val;
          case 2:
            return (uint16_t)val;
          case 4:
            return (uint32_t)val;
        }
      }
      return val;
    }
    case NodeKind::ND_ADDR:
      return eval_rval(node->lhs, label);
    case NodeKind::ND_MEMBER:
      if (!label) error_tok(node->tok, "not a compile-time constant");
      if (node->ty->kind != TypeKind::TY_ARRAY) error_tok(node->tok, "invalid initializer");
      return eval_rval(node->lhs, label) + node->member->offset;
    case NodeKind::ND_VAR:
      if (!label) error_tok(node->tok, "not a compile-time constant");
      if (node->var->ty->kind != TypeKind::TY_ARRAY && node->var->ty->kind != TypeKind::TY_FUNC)
        error_tok(node->tok, "invalid initializer");
      *label = node->var->name;
      return 0;
    case NodeKind::ND_NUM:
      return node->val;
  }

  error_tok(node->tok, "not a compile-time constant");
}

static int64_t eval_rval(Node *node, char **label) {
  switch (node->kind) {
    case NodeKind::ND_VAR:
      if (node->var->is_local) error_tok(node->tok, "not a compile-time constant");
      *label = node->var->name;
      return 0;
    case NodeKind::ND_DEREF:
      return eval(node->lhs, label);
    case NodeKind::ND_MEMBER:
      return eval_rval(node->lhs, label) + node->member->offset;
  }

  error_tok(node->tok, "invalid initializer");
}

static int64_t const_expr(Token **rest, Token *tok) {
  Node *node = conditional(rest, tok);
  return eval(node);
}

// Convert `A op= B` to `tmp = &A, *tmp = *tmp op B`
// where tmp is a fresh pointer variable.
static Node *to_assign(Node *binary) {
  add_type(binary->lhs);
  add_type(binary->rhs);
  Token *tok = binary->tok;

  Obj *var = new_lvar("", Type::pointer_to(binary->lhs->ty));

  Node *expr1 = new_binary(NodeKind::ND_ASSIGN,
                           new_var_node(var, tok),
                           new_unary(NodeKind::ND_ADDR, binary->lhs, tok),
                           tok);

  Node *expr2 = new_binary(NodeKind::ND_ASSIGN,
                           new_unary(NodeKind::ND_DEREF, new_var_node(var, tok), tok),
                           new_binary(binary->kind,
                                      new_unary(NodeKind::ND_DEREF, new_var_node(var, tok), tok),
                                      binary->rhs,
                                      tok),
                           tok);

  return new_binary(NodeKind::ND_COMMA, expr1, expr2, tok);
}

// assign    = conditional (assign-op assign)?
// assign-op = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="
//           | "<<=" | ">>="
static Node *assign(Token **rest, Token *tok) {
  Node *node = conditional(&tok, tok);

  if (tok->equal("=")) node = new_binary(NodeKind::ND_ASSIGN, node, assign(&tok, tok->next), tok);

  if (tok->equal("+=")) return to_assign(new_add(node, assign(rest, tok->next), tok));

  if (tok->equal("-=")) return to_assign(new_sub(node, assign(rest, tok->next), tok));

  if (tok->equal("*="))
    return to_assign(new_binary(NodeKind::ND_MUL, node, assign(rest, tok->next), tok));

  if (tok->equal("/="))
    return to_assign(new_binary(NodeKind::ND_DIV, node, assign(rest, tok->next), tok));

  if (tok->equal("%="))
    return to_assign(new_binary(NodeKind::ND_MOD, node, assign(rest, tok->next), tok));

  if (tok->equal("&="))
    return to_assign(new_binary(NodeKind::ND_BITAND, node, assign(rest, tok->next), tok));

  if (tok->equal("|="))
    return to_assign(new_binary(NodeKind::ND_BITOR, node, assign(rest, tok->next), tok));

  if (tok->equal("^="))
    return to_assign(new_binary(NodeKind::ND_BITXOR, node, assign(rest, tok->next), tok));

  if (tok->equal("<<="))
    return to_assign(new_binary(NodeKind::ND_SHL, node, assign(rest, tok->next), tok));

  if (tok->equal(">>="))
    return to_assign(new_binary(NodeKind::ND_SHR, node, assign(rest, tok->next), tok));

  *rest = tok;
  return node;
}

// conditional = logor ("?" expr ":" conditional)?
static Node *conditional(Token **rest, Token *tok) {
  Node *cond = logor(&tok, tok);

  if (!tok->equal("?")) {
    *rest = tok;
    return cond;
  }

  Node *node = new_node(NodeKind::ND_COND, tok);
  node->cond = cond;
  node->then = expr(&tok, tok->next);
  tok = tok->skip(":");
  node->els = conditional(rest, tok);
  return node;
}

// logor = logand ("||" logand)*
static Node *logor(Token **rest, Token *tok) {
  Node *node = logand(&tok, tok);
  while (tok->equal("||")) {
    Token *start = tok;
    node = new_binary(NodeKind::ND_LOGOR, node, logand(&tok, tok->next), start);
  }
  *rest = tok;
  return node;
}

// logand = bitor ("&&" bitor)*
static Node *logand(Token **rest, Token *tok) {
  Node *node = bit_or(&tok, tok);
  while (tok->equal("&&")) {
    Token *start = tok;
    node = new_binary(NodeKind::ND_LOGAND, node, bit_or(&tok, tok->next), start);
  }
  *rest = tok;
  return node;
}

// bitor = bitxor ("|" bitxor)*
static Node *bit_or(Token **rest, Token *tok) {
  Node *node = bit_xor(&tok, tok);
  while (tok->equal("|")) {
    Token *start = tok;
    node = new_binary(NodeKind::ND_BITOR, node, bit_xor(&tok, tok->next), start);
  }
  *rest = tok;
  return node;
}

// bitxor = bitand ("^" bitand)*
static Node *bit_xor(Token **rest, Token *tok) {
  Node *node = bit_and(&tok, tok);
  while (tok->equal("^")) {
    Token *start = tok;
    node = new_binary(NodeKind::ND_BITXOR, node, bit_and(&tok, tok->next), start);
  }
  *rest = tok;
  return node;
}

// bitand = equality ("&" equality)*
static Node *bit_and(Token **rest, Token *tok) {
  Node *node = equality(&tok, tok);
  while (tok->equal("&")) {
    Token *start = tok;
    node = new_binary(NodeKind::ND_BITAND, node, equality(&tok, tok->next), start);
  }
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

// relational = shift ("<" shift | "<=" shift | ">" shift | ">=" shift)*
static Node *relational(Token **rest, Token *tok) {
  Node *node = shift(&tok, tok);

  for (;;) {
    Token *start = tok;

    if (tok->equal("<")) {
      node = new_binary(NodeKind::ND_LT, node, shift(&tok, tok->next), start);
      continue;
    }

    if (tok->equal("<=")) {
      node = new_binary(NodeKind::ND_LE, node, shift(&tok, tok->next), start);
      continue;
    }

    if (tok->equal(">")) {
      node = new_binary(NodeKind::ND_LT, shift(&tok, tok->next), node, start);
      continue;
    }

    if (tok->equal(">=")) {
      node = new_binary(NodeKind::ND_LE, shift(&tok, tok->next), node, start);
      continue;
    }

    *rest = tok;
    return node;
  }
}

// shift = add ("<<" add | ">>" add)*
static Node *shift(Token **rest, Token *tok) {
  Node *node = add(&tok, tok);

  for (;;) {
    Token *start = tok;

    if (tok->equal("<<")) {
      node = new_binary(NodeKind::ND_SHL, node, add(&tok, tok->next), start);
      continue;
    }

    if (tok->equal(">>")) {
      node = new_binary(NodeKind::ND_SHR, node, add(&tok, tok->next), start);
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
  rhs = new_binary(NodeKind::ND_MUL, rhs, new_long(lhs->ty->base->size, tok), tok);
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
    rhs = new_binary(NodeKind::ND_MUL, rhs, new_long(lhs->ty->base->size, tok), tok);
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

// mul = cast ("*" cast | "/" cast | "%" cast)*
static Node *mul(Token **rest, Token *tok) {
  Node *node = cast(&tok, tok);

  for (;;) {
    Token *start = tok;

    if (tok->equal("*")) {
      node = new_binary(NodeKind::ND_MUL, node, cast(&tok, tok->next), start);
      continue;
    }

    if (tok->equal("/")) {
      node = new_binary(NodeKind::ND_DIV, node, cast(&tok, tok->next), start);
      continue;
    }

    if (tok->equal("%")) {
      node = new_binary(NodeKind::ND_MOD, node, cast(&tok, tok->next), start);
      continue;
    }

    *rest = tok;
    return node;
  }
}

// cast = "(" type-name ")" cast | unary
static Node *cast(Token **rest, Token *tok) {
  if (tok->equal("(") && is_typename(tok->next)) {
    Token *start = tok;
    Type *ty = type_name(&tok, tok->next);
    tok = tok->skip(")");

    // compound literal
    if (tok->equal("{")) return unary(rest, start);

    // type cast
    Node *node = new_cast(cast(rest, tok), ty);
    node->tok = start;
    return node;
  }

  return unary(rest, tok);
}

// unary = ("+" | "-" | "*" | "&" | "!" | "~") cast
//       | ("++" | "--") unary
//       | primary
static Node *unary(Token **rest, Token *tok) {
  if (tok->equal("+")) return cast(rest, tok->next);

  if (tok->equal("-")) return new_unary(NodeKind::ND_NEG, cast(rest, tok->next), tok);

  if (tok->equal("&")) return new_unary(NodeKind::ND_ADDR, cast(rest, tok->next), tok);

  if (tok->equal("*")) return new_unary(NodeKind::ND_DEREF, cast(rest, tok->next), tok);

  if (tok->equal("!")) return new_unary(NodeKind::ND_NOT, cast(rest, tok->next), tok);

  // Read ++i as i+=1
  if (tok->equal("++")) return to_assign(new_add(unary(rest, tok->next), new_num(1, tok), tok));

  if (tok->equal("~")) return new_unary(NodeKind::ND_BITNOT, cast(rest, tok->next), tok);

  // Read --i as i-=1
  if (tok->equal("--")) return to_assign(new_sub(unary(rest, tok->next), new_num(1, tok), tok));

  return postfix(rest, tok);
}

// struct-members = (declspec declarator (","  declarator)* ";")*
static void struct_members(Token **rest, Token *tok, Type *ty) {
  Member head;
  Member *cur = &head;
  int idx = 0;

  while (!tok->equal("}")) {
    VarAttr attr = {};
    Type *basety = declspec(&tok, tok, &attr);
    bool first = true;

    while (!tok->consume(&tok, ";")) {
      if (!first) tok = tok->skip(",");
      first = false;

      Member *mem = new Member;
      mem->ty = declarator(&tok, tok, basety);
      mem->name = mem->ty->name;
      mem->idx = idx++;
      mem->align = attr.align ? attr.align : mem->ty->align;
      cur = cur->next = mem;
    }
  }

  // If the last element is an array of incomplete type, it's
  // called a "flexible array member". It should behave as if
  // if were a zero-sized array.
  if (cur != &head && cur->ty->kind == TypeKind::TY_ARRAY && cur->ty->array_len < 0) {
    cur->ty = Type::array_of(cur->ty->base, 0);
    ty->is_flexible = true;
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
    *rest = tok;

    Type *ty = find_tag(tag);
    if (ty) return ty;

    ty = Type::struct_type();
    ty->size = -1;
    push_tag_scope(tag, ty);
    return ty;
  }

  tok = tok->skip("{");

  // Construct a struct object.
  Type *ty = Type::struct_type();
  struct_members(rest, tok, ty);

  if (tag) {
    // If this is a redefinition, overwrite a previous type.
    // Otherwise, register the struct type.
    for (TagScope *sc = scope->tags; sc; sc = sc->next) {
      if (tag->equal(sc->name)) {
        *sc->ty = *ty;
        return sc->ty;
      }
    }

    push_tag_scope(tag, ty);
  }

  return ty;
}

// struct-decl = struct-union-decl
static Type *struct_decl(Token **rest, Token *tok) {
  Type *ty = struct_union_decl(rest, tok);
  ty->kind = TypeKind::TY_STRUCT;

  if (ty->size < 0) return ty;

  // Assign offsets within the struct to members.
  int offset = 0;
  for (Member *mem = ty->members; mem; mem = mem->next) {
    offset = align_to(offset, mem->align);
    mem->offset = offset;
    offset += mem->ty->size;

    if (ty->align < mem->align) ty->align = mem->align;
  }
  ty->size = align_to(offset, ty->align);

  return ty;
}

// union-decl = struct-union-decl
static Type *union_decl(Token **rest, Token *tok) {
  Type *ty = struct_union_decl(rest, tok);
  ty->kind = TypeKind::TY_UNION;

  if (ty->size < 0) return ty;

  // If union, we don't have to assign offsets because they
  // are already initialized to zero. We need to compute the
  // alignment and the size though.
  for (Member *mem = ty->members; mem; mem = mem->next) {
    if (ty->align < mem->align) ty->align = mem->align;
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

// Convert A++ to `(typeof A)((A += 1) - 1)`
static Node *new_inc_dec(Node *node, Token *tok, int addend) {
  add_type(node);
  return new_cast(new_add(to_assign(new_add(node, new_num(addend, tok), tok)),
                          new_num(-addend, tok),
                          tok),
                  node->ty);
}

// postfix = "(" type-name ")" "{" initializer-list "}"
//         | primary ("[" expr "]" | "." ident | "->" ident | "++" | "--")*
static Node *postfix(Token **rest, Token *tok) {
  if (tok->equal("(") && is_typename(tok->next)) {
    // Compound literal
    Token *start = tok;
    Type *ty = type_name(&tok, tok->next);
    tok = tok->skip(")");

    if (scope->next == nullptr) {
      Obj *var = new_anon_gvar(ty);
      gvar_initializer(rest, tok, var);
      return new_var_node(var, start);
    }

    Obj *var = new_lvar("", ty);
    Node *lhs = lvar_initializer(rest, tok, var);
    Node *rhs = new_var_node(var, tok);
    return new_binary(NodeKind::ND_COMMA, lhs, rhs, start);
  }

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

    if (tok->equal("++")) {
      node = new_inc_dec(node, tok, 1);
      tok = tok->next;
      continue;
    }

    if (tok->equal("--")) {
      node = new_inc_dec(node, tok, -1);
      tok = tok->next;
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

  VarScope *sc = find_var(start);
  if (!sc) error_tok(start, "implicit declaration of a function");
  if (!sc->var || sc->var->ty->kind != TypeKind::TY_FUNC) error_tok(start, "not a function");

  Type *ty = sc->var->ty;
  Type *param_ty = ty->params;

  Node head = {};
  Node *cur = &head;

  while (!tok->equal(")")) {
    if (cur != &head) {
      tok = tok->skip(",");
    }

    Node *arg = assign(&tok, tok);
    add_type(arg);

    if (!param_ty && !ty->is_variadic) error_tok(tok, "too many arguments");

    if (param_ty) {
      if (param_ty->kind == TypeKind::TY_STRUCT || param_ty->kind == TypeKind::TY_UNION)
        error_tok(arg->tok, "passing struct or union is not supported yet");
      arg = new_cast(arg, param_ty);
      param_ty = param_ty->next;
    }

    cur = cur->next = arg;
  }

  if (param_ty) error_tok(tok, "too few arguments");

  *rest = tok->skip(")");

  Node *node = new_node(NodeKind::ND_FUNCALL, start);
  node->funcname = strndup(start->loc, start->len);
  node->func_ty = ty;
  node->ty = ty->return_ty;
  node->args = head.next;
  return node;
}

// primary = "(" "{" stmt+ "}" ")"
//         | "(" expr ")"
//         | "sizeof" "(" type-name ")"
//         | "sizeof" unary
//         | "_Alignof" "(" type-name ")"
//         | "_Alignof" unary
//         | ident func-args?
//         | str
//         | num
static Node *primary(Token **rest, Token *tok) {
  Token *start = tok;

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

  if (tok->equal("sizeof") && tok->next->equal("(") && is_typename(tok->next->next)) {
    Type *ty = type_name(&tok, tok->next->next);
    *rest = tok->skip(")");
    return new_num(ty->size, start);
  }

  if (tok->equal("sizeof")) {
    Node *node = unary(rest, tok->next);
    add_type(node);
    return new_num(node->ty->size, tok);
  }

  if (tok->equal("_Alignof") && tok->next->equal("(") && is_typename(tok->next->next)) {
    Type *ty = type_name(&tok, tok->next->next);
    *rest = tok->skip(")");
    return new_num(ty->align, tok);
  }

  if (tok->equal("_Alignof")) {
    Node *node = unary(rest, tok->next);
    add_type(node);
    return new_num(node->ty->align, tok);
  }

  if (tok->kind == TokenKind::TK_IDENT) {
    // Function call
    if (tok->next->equal("(")) return funcall(rest, tok);

    // Variable or enum constant
    VarScope *sc = find_var(tok);
    if (!sc || (!sc->var && !sc->enum_ty)) error_tok(tok, "undefined variable");

    Node *node;
    if (sc->var)
      node = new_var_node(sc->var, tok);
    else
      node = new_num(sc->enum_val, tok);

    *rest = tok->next;
    return node;
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

static Token *parse_typedef(Token *tok, Type *basety) {
  bool first = true;

  while (!tok->consume(&tok, ";")) {
    if (!first) tok = tok->skip(",");
    first = false;

    Type *ty = declarator(&tok, tok, basety);
    push_scope(get_ident(ty->name))->type_def = ty;
  }
  return tok;
}

static void create_param_lvars(Type *param) {
  if (param) {
    create_param_lvars(param->next);
    new_lvar(get_ident(param->name), param);
  }
}

// This function matches gotos with labels.
//
// We cannot resolve gotos as we parse a function because gotos
// can refer a label that appears later in the function.
// So, we need to do this after we parse the entire function.
static void resolve_goto_labels(void) {
  for (Node *x = gotos; x; x = x->goto_next) {
    for (Node *y = labels; y; y = y->goto_next) {
      if (!strcmp(x->label, y->label)) {
        x->unique_label = y->unique_label;
        break;
      }
    }

    if (x->unique_label == nullptr) error_tok(x->tok->next, "use of undeclared label");
  }

  gotos = labels = nullptr;
}

static Token *function(Token *tok, Type *basety, VarAttr *attr) {
  Type *ty = declarator(&tok, tok, basety);

  Obj *fn = new_gvar(get_ident(ty->name), ty);
  fn->is_function = true;
  fn->is_definition = !tok->consume(&tok, ";");
  fn->is_static = attr->is_static;

  if (!fn->is_definition) return tok;

  current_fn = fn;
  locals = nullptr;
  enter_scope();
  create_param_lvars(ty->params);
  fn->params = locals;
  if (ty->is_variadic) fn->va_area = new_lvar("__va_area__", Type::array_of(Type::ty_char, 136));

  tok = tok->skip("{");
  fn->body = compound_stmt(&tok, tok);
  fn->locals = locals;
  leave_scope();
  resolve_goto_labels();

  return tok;
}

static Token *global_variable(Token *tok, Type *basety, VarAttr *attr) {
  bool first = true;

  while (!tok->consume(&tok, ";")) {
    if (!first) tok = tok->skip(",");

    first = false;

    Type *ty = declarator(&tok, tok, basety);
    Obj *var = new_gvar(get_ident(ty->name), ty);
    var->is_definition = !attr->is_extern;
    var->is_static = attr->is_static;
    if (attr->align) var->align = attr->align;

    if (tok->equal("=")) gvar_initializer(&tok, tok->next, var);
  }

  return tok;
}

// Lookahead tokens and returns true if a given token is a start
// of a function definition or declaration.
static bool is_function(Token *tok) {
  if (tok->equal(";")) return false;

  Type dummy = {};
  Type *ty = declarator(&tok, tok, &dummy);
  return ty->kind == TypeKind::TY_FUNC;
}

// program = (typedef | function-definition | global-variable)*
Obj *parse(Token *tok) {
  globals = nullptr;

  while (tok->kind != TokenKind::TK_EOF) {
    VarAttr attr = {};
    Type *basety = declspec(&tok, tok, &attr);

    // Typedef
    if (attr.is_typedef) {
      tok = parse_typedef(tok, basety);
      continue;
    }

    // Function
    if (is_function(tok)) {
      tok = function(tok, basety, &attr);
      continue;
    }

    // Global variable
    tok = global_variable(tok, basety, &attr);
  }

  return globals;
}
