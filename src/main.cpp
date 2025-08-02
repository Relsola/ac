#include <cstdlib>
#include <iostream>

#include "core.h"

int main(int argc, char const* argv[]) {
  if (argc != 2) error("%s: invalid number of arguments", argv[0]);

  Token* tok = tokenize((char*)argv[1]);
  Function* prog = parse(tok);
  codegen(prog);

  return 0;
}
