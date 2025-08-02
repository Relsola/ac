#include "core.h"

static Node *expr(Token **rest, Token *tok);
static Node *equality(Token **rest, Token *tok);
static Node *relational(Token **rest, Token *tok);
static Node *add(Token **rest, Token *tok);
static Node *mul(Token **rest, Token *tok);
static Node *unary(Token **rest, Token *tok);
static Node *primary(Token **rest, Token *tok);

// expr = equality
static Node *expr(Token **rest, Token *tok) { return equality(rest, tok); }

// equality = relational ("==" relational | "!=" relational)*
static Node *equality(Token **rest, Token *tok) {
  Node *node = relational(&tok, tok);

  for (;;) {
    if (tok->equal("==")) {
      node = new Node(Node::NodeKind::ND_EQ, node, relational(&tok, tok->next));
      continue;
    }

    if (tok->equal("!=")) {
      node = new Node(Node::NodeKind::ND_NE, node, relational(&tok, tok->next));
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
    if (tok->equal("<")) {
      node = new Node(Node::NodeKind::ND_LT, node, add(&tok, tok->next));
      continue;
    }

    if (tok->equal("<=")) {
      node = new Node(Node::NodeKind::ND_LE, node, add(&tok, tok->next));
      continue;
    }

    if (tok->equal(">")) {
      node = new Node(Node::NodeKind::ND_LT, add(&tok, tok->next), node);
      continue;
    }

    if (tok->equal(">=")) {
      node = new Node(Node::NodeKind::ND_LE, add(&tok, tok->next), node);
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
    if (tok->equal("+")) {
      node = new Node(Node::NodeKind::ND_ADD, node, mul(&tok, tok->next));
      continue;
    }

    if (tok->equal("-")) {
      node = new Node(Node::NodeKind::ND_SUB, node, mul(&tok, tok->next));
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
    if (tok->equal("*")) {
      node = new Node(Node::NodeKind::ND_MUL, node, unary(&tok, tok->next));
      continue;
    }

    if (tok->equal("/")) {
      node = new Node(Node::NodeKind::ND_DIV, node, unary(&tok, tok->next));
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
    return new Node(Node::NodeKind::ND_NEG, unary(rest, tok->next));

  return primary(rest, tok);
}

// primary = "(" expr ")" | num
static Node *primary(Token **rest, Token *tok) {
  if (tok->equal("(")) {
    Node *node = expr(&tok, tok->next);
    *rest = tok->skip(")");
    return node;
  }

  if (tok->kind == Token::TokenKind::TK_NUM) {
    Node *node = new Node(tok->val);
    *rest = tok->next;
    return node;
  }

  error_tok(tok, "expected an expression");
}

Node *parse(Token *tok) {
  Node *node = expr(&tok, tok);
  if (tok->kind != Token::TokenKind::TK_EOF) error_tok(tok, "extra token");
  return node;
}
