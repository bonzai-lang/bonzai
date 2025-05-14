#ifndef THREADING_H
#define THREADING_H

#include <module.h>
#include <value.h>

Value list_get(Module *module, Value list, uint32_t index);
Value call_function(struct Module *m, Value closure, int32_t argc, Value *argv);

#endif