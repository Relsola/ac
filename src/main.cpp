#include <cstdlib>
#include <iostream>

#include "core.h"

int main(int argc, char const* argv[]) {
  if (argc != 2) error("%s: invalid number of arguments", argv[0]);

  Token* tok = tokenize((char*)argv[1]);

  std::cout << "  .globl main\n";
  std::cout << "main:\n";

  std::cout << "  mov $" << tok->get_number() << ", %rax\n";
  tok = tok->next;

  while (tok->kind != Token::TokenKind::TK_EOF) {
    if (tok->equal("+")) {
      tok = tok->next;
      std::cout << "  add $" << tok->get_number() << ", %rax\n";
      tok = tok->next;
      continue;
    }

    tok = tok->skip("-");
    std::cout << "  sub $" << tok->get_number() << ", %rax\n";
    tok = tok->next;
  }

  std::cout << "  ret" << std::endl;
  return 0;
}
