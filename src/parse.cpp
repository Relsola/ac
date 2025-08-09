#include "core.h"

// All local variable instances created during parsing are
// accumulated to this list.
Obj *locals;

Obj *new_lvar(char *name) {
  Obj *var = new Obj;

  var->name = name;
  var->next = locals;
  locals = var;
  return var;
}

static Node *compound_stmt(Token **rest, Token *tok);
static Node *expr_stmt(Token **rest, Token *tok);
static Node *expr(Token **rest, Token *tok);
static Node *assign(Token **rest, Token *tok);
static Node *equality(Token **rest, Token *tok);
static Node *relational(Token **rest, Token *tok);
static Node *add(Token **rest, Token *tok);
static Node *mul(Token **rest, Token *tok);
static Node *unary(Token **rest, Token *tok);
static Node *primary(Token **rest, Token *tok);

// Find a local variable by name.
static Obj *find_var(Token *tok) {
  for (Obj *var = locals; var; var = var->next)
    if (strlen(var->name) == tok->len &&
        !strncmp(tok->loc, var->name, tok->len))
      return var;
  return NULL;
}

// stmt = "return" expr ";"
//      | "if" "(" expr ")" stmt ("else" stmt)?
//      | "for" "(" expr-stmt expr? ";" expr? ")" stmt
//      | "while" "(" expr ")" stmt
//      | "{" compound-stmt
//      | expr-stmt
static Node *stmt(Token **rest, Token *tok) {
  if (tok->equal("return")) {
    Node *node = new Node(Node::NodeKind::ND_RETURN, tok);
    node->lhs = expr(&tok, tok->next);
    *rest = tok->skip(";");
    return node;
  }

  if (tok->equal("if")) {
    Node *node = new Node(Node::NodeKind::ND_IF, tok);
    tok = tok->next->skip("(");
    node->cond = expr(&tok, tok);
    tok = tok->skip(")");
    node->then = stmt(&tok, tok);
    if (tok->equal("else")) node->els = stmt(&tok, tok->next);
    *rest = tok;
    return node;
  }

  if (tok->equal("for")) {
    Node *node = new Node(Node::NodeKind::ND_FOR, tok);
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
    Node *node = new Node(Node::NodeKind::ND_FOR, tok);
    tok = tok->next->skip("(");
    node->cond = expr(&tok, tok);
    tok = tok->skip(")");
    node->then = stmt(rest, tok);
    return node;
  }

  if (tok->equal("{")) return compound_stmt(rest, tok->next);

  return expr_stmt(rest, tok);
}

// compound-stmt = stmt* "}"
static Node *compound_stmt(Token **rest, Token *tok) {
  Node head = {};
  Node *cur = &head;
  while (!tok->equal("}")) cur = cur->next = stmt(&tok, tok);

  Node *node = new Node(Node::NodeKind::ND_BLOCK, tok);
  node->body = head.next;
  *rest = tok->next;
  return node;
}

// expr-stmt = expr? ";"
static Node *expr_stmt(Token **rest, Token *tok) {
  if (tok->equal(";")) {
    *rest = tok->next;
    return new Node(Node::NodeKind::ND_BLOCK, tok);
  }

  Node *node = new Node(Node::NodeKind::ND_EXPR_STMT, tok);
  node->lhs = expr(&tok, tok);
  *rest = tok->skip(";");
  return node;
}

// expr = assign
static Node *expr(Token **rest, Token *tok) { return assign(rest, tok); }

// assign = equality ("=" assign)?
static Node *assign(Token **rest, Token *tok) {
  Node *node = equality(&tok, tok);
  if (tok->equal("="))
    node =
        new Node(Node::NodeKind::ND_ASSIGN, node, assign(&tok, tok->next), tok);
  *rest = tok;
  return node;
}

// equality = relational ("==" relational | "!=" relational)*
static Node *equality(Token **rest, Token *tok) {
  Node *node = relational(&tok, tok);

  for (;;) {
    Token *start = tok;

    if (tok->equal("==")) {
      node = new Node(Node::NodeKind::ND_EQ, node, relational(&tok, tok->next),
                      start);
      continue;
    }

    if (tok->equal("!=")) {
      node = new Node(Node::NodeKind::ND_NE, node, relational(&tok, tok->next),
                      start);
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
      node = new Node(Node::NodeKind::ND_LT, node, add(&tok, tok->next), start);
      continue;
    }

    if (tok->equal("<=")) {
      node = new Node(Node::NodeKind::ND_LE, node, add(&tok, tok->next), start);
      continue;
    }

    if (tok->equal(">")) {
      node = new Node(Node::NodeKind::ND_LT, add(&tok, tok->next), node, start);
      continue;
    }

    if (tok->equal(">=")) {
      node = new Node(Node::NodeKind::ND_LE, add(&tok, tok->next), node, start);
      continue;
    }

    *rest = tok;
    return node;
  }
}

// add = mul ("+" mul | "-" mul)*
static Node *add(Token **rest, Token *tok) {
  Node *node = mul(&tok, tok);

  for (;;) {
    Token *start = tok;

    if (tok->equal("+")) {
      node =
          new Node(Node::NodeKind::ND_ADD, node, mul(&tok, tok->next), start);
      continue;
    }

    if (tok->equal("-")) {
      node =
          new Node(Node::NodeKind::ND_SUB, node, mul(&tok, tok->next), start);
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
      node =
          new Node(Node::NodeKind::ND_MUL, node, unary(&tok, tok->next), start);
      continue;
    }

    if (tok->equal("/")) {
      node =
          new Node(Node::NodeKind::ND_DIV, node, unary(&tok, tok->next), start);
      continue;
    }

    *rest = tok;
    return node;
  }
}

// unary = ("+" | "-") unary
//       | primary
static Node *unary(Token **rest, Token *tok) {
  if (tok->equal("+")) return unary(rest, tok->next);

  if (tok->equal("-"))
    return new Node(Node::NodeKind::ND_NEG, unary(rest, tok->next), tok);

  return primary(rest, tok);
}

// primary = "(" expr ")" | ident | num
static Node *primary(Token **rest, Token *tok) {
  if (tok->equal("(")) {
    Node *node = expr(&tok, tok->next);
    *rest = tok->skip(")");
    return node;
  }

  if (tok->kind == Token::TokenKind::TK_IDENT) {
    Obj *var = find_var(tok);
    if (!var) var = new_lvar(strndup(tok->loc, tok->len));
    *rest = tok->next;
    return new Node(var, tok);
  }

  if (tok->kind == Token::TokenKind::TK_NUM) {
    Node *node = new Node(tok->val, tok);
    *rest = tok->next;
    return node;
  }

  error_tok(tok, "expected an expression");
}

// program = stmt*
Function *parse(Token *tok) {
  tok = tok->skip("{");

  Function *prog;
  prog->body = compound_stmt(&tok, tok);
  prog->locals = locals;
  return prog;
}
