#include "core.h"

Token::Token(TokenKind kind, char *start, char *end)
    : kind(kind), loc(start), len(end - start){};

bool Token::equal(char *op) {
  return memcmp(this->loc, op, this->len) == 0 && op[this->len] == '\0';
};

Token *Token::skip(char *s) {
  if (!this->equal(s)) error_tok(this, "expected '%s'", s);

  return this->next;
}

int Token::get_number() {
  if (this->kind != TokenKind::TK_NUM) error_tok(this, "expected a number");

  return this->val;
}

bool Token::consume(Token **rest, char *str) {
  if (this->equal(str)) {
    *rest = this->next;
    return true;
  }

  *rest = this;
  return false;
}
