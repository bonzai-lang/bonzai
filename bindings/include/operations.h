#ifndef OPERATIONS_H
#define OPERATIONS_H

#include <module.h>
#include <value.h>

Value print(Module* mod, Value* args, int argc);
Value exit_with(Module* mod, Value* args, int argc);
Value mutable_value(Module* mod, Value* args, int argc);

#endif  // OPERATIONS_H