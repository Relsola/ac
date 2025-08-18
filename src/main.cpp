#include <cstdlib>
#include <iostream>

#include "core.h"

int main(int argc, char const* argv[]) {
  if (argc != 2) error("%s: invalid number of arguments", argv[0]);

  // Tokenize and parse.
  Token* tok = tokenize_file((char*)argv[1]);
  Obj* prog = parse(tok);

  // Traverse the AST to emit assembly.
  codegen(prog);

  return 0;
}
