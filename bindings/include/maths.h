#ifndef MATHS_H
#define MATHS_H

#include <module.h>
#include <value.h>

Value add_(Module* mod, Value* args, int argc);
Value sub_(Module* mod, Value* args, int argc);
Value mul_(Module* mod, Value* args, int argc);
Value div_(Module* mod, Value* args, int argc);

Value eq_(Module* mod, Value* args, int argc);
Value lt_(Module* mod, Value* args, int argc);
Value gt_(Module* mod, Value* args, int argc);
Value lte_(Module* mod, Value* args, int argc);
Value gte_(Module* mod, Value* args, int argc);

Value and_(Module* mod, Value* args, int argc);
Value or_(Module* mod, Value* args, int argc);
Value not_(Module* mod, Value* args, int argc);

#endif  // MATHS_H