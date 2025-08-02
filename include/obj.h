#pragma once

class Obj {
 public:
  Obj *next;
  char *name;  // Variable name
  int offset;  // Offset from RBP
};
