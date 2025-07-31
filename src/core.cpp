#include "core.h"

Token *tokenize(char *p) {
  Token head;
  Token *cur = &head;

  while (*p) {
    if (isspace(*p)) {
      p++;
      continue;
    }

    if (isdigit(*p)) {
      cur = cur->next = new Token(Token::TokenKind::TK_NUM);
      cur->val = strtoul(p, &p, 10);
      continue;
    }

    if (*p == '+' || *p == '-') {
      std::string punct = std::string(p, 1);
      cur = cur->next = new Token(Token::TokenKind::TK_PUNCT, punct);
      p++;
      continue;
    }

    error("invalid token");
  }

  cur = cur->next = new Token(Token::TokenKind::TK_EOF);
  return head.next;
}
