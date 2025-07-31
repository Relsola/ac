#include <cstdlib>
#include <iostream>

int main(int argc, char const* argv[]) {
  if (argc != 2) {
    std::cerr << argv[0] << ": invalid number of arguments\n";
    return 1;
  }

  char* p = (char*)argv[1];

  std::cout << "  .globl main\n";
  std::cout << "main:\n";
  std::cout << "  mov $" << strtol(p, &p, 10) << ", %rax\n";

  while (*p) {
    if (*p == '+') {
      p++;
      std::cout << "  add $" << strtol(p, &p, 10) << ", %rax\n";
      continue;
    }

    if (*p == '-') {
      p++;
      std::cout << "  sub $" << strtol(p, &p, 10) << ", %rax\n";
      continue;
    }

    std::cout << "unexpected character: '" << *p << "'\n";

    return 1;
  }

  std::cout << "  ret" << std::endl;

  return 0;
}
