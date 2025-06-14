#ifndef VALUES_H
#define VALUES_H

#include <value.h>
#include <module.h>

Value throwable_ok(Module* module, Value value);
Value throwable_error(Module* module, char* error);
Value make_tuple(Module* mod, Value a, Value b);
Value unit(Module* mod);
Value make_some(Module* mod, Value value);
Value make_none(Module* mod);

#define SET_LIST(list, index, value) \
  (GET_PTR(list)->as_ptr[index] = value)

#endif