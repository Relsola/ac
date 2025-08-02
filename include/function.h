#pragma once

#include "node.h"
#include "obj.h"

class Function {
 public:
  Node *body;
  Obj *locals;
  int stack_size;
};
