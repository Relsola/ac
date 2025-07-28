#include <iostream>

int main(int argc, char const* argv[]) {
  if (argc != 2) {
    std::cerr << argv[0] << ": invalid number of arguments\n";
    std::cerr << "Usage: " << argv[0] << " <number>\n";
    return 1;
  }

  std::cout << "  .globl main\n";
  std::cout << "main:\n";
  std::cout << "  mov $" << argv[1] << ", %rax\n";
  std::cout << "  ret\n";

  return 0;
}
