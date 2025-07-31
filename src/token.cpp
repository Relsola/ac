#include "token.h"

Token::Token(){};
Token::Token(TokenKind kind) : kind(kind){};
Token::Token(TokenKind kind, std::string punct) : kind(kind), punct(punct){};

bool Token::equal(char *op) { return this->punct == op; };

Token *Token::skip(char *s) {
  if (!this->equal(s)) error("expected '%s'", s);

  return this->next;
}

int Token::get_number() {
  if (this->kind != TokenKind::TK_NUM) error("expected a number");

  return this->val;
}
