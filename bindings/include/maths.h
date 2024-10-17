#ifndef MATHS_H
#define MATHS_H

#include <module.h>
#include <value.h>

Value add_value(Module* mod, Value* args, int argc);
Value sub_value(Module* mod, Value* args, int argc);
Value mul_value(Module* mod, Value* args, int argc);
Value div_value(Module* mod, Value* args, int argc);

Value eq_value(Module* mod, Value* args, int argc);
Value lt_value(Module* mod, Value* args, int argc);
Value gt_value(Module* mod, Value* args, int argc);
Value lte_value(Module* mod, Value* args, int argc);
Value gte_value(Module* mod, Value* args, int argc);
Value neq_value(Module* mod, Value* args, int argc);

Value and_value(Module* mod, Value* args, int argc);
Value or_value(Module* mod, Value* args, int argc);
Value not_value(Module* mod, Value* args, int argc);

#endif  // MATHS_H