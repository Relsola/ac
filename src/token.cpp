#include "core.h"

Token *Token::skip(char *s) {
  if (!this->equal(s)) error_tok(this, "expected '%s'", s);

  return this->next;
}

long Token::get_number() {
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
