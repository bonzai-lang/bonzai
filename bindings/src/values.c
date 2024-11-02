#include <values.h>

Value throwable_ok(Module* module, Value value) {
  Value* args = malloc(4 * sizeof(Value));

  args[0] = kNull;
  args[1] = MAKE_STRING_NON_GC(module, "Throwable");
  args[2] = MAKE_STRING_NON_GC(module, "Ok");
  args[3] = value;

  Value result = MAKE_LIST(module, args, 4);
  return result;
}

Value throwable_error(Module* module, char* error) {
  Value* args = malloc(4 * sizeof(Value));

  args[0] = kNull;
  args[1] = MAKE_STRING_NON_GC(module, "Throwable");
  args[2] = MAKE_STRING_NON_GC(module, "Error");
  args[3] = MAKE_STRING(module, error);

  Value result = MAKE_LIST(module, args, 4);
  return result;
}

Value make_tuple(Module* mod, Value a, Value b) {
  Value* tuple = malloc(5 * sizeof(Value));

  tuple[0] = kNull;
  tuple[1] = MAKE_STRING_NON_GC(mod, "Tuple");
  tuple[2] = MAKE_STRING_NON_GC(mod, "Tuple");
  tuple[3] = a;
  tuple[4] = b;

  return MAKE_LIST(mod, tuple, 5);
}