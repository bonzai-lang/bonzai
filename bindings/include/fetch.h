#ifndef FETCH_H
#define FETCH_H

#include <module.h>
#include <value.h>

Value fetch(Module* module, Value* args, int argc);
Value fetch_with(Module* module, Value* args, int argc);

#endif // FETCH_H