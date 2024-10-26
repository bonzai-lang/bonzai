#include <values.h>

Value throwable_ok(Module* module, Value value) {
  Value* args = malloc(4 * sizeof(Value));

  args[0] = kNull;
  args[1] = MAKE_STRING(module, strdup("Throwable"));
  args[2] = MAKE_STRING(module, strdup("Ok"));
  args[3] = value;

  Value result = MAKE_LIST(module, args, 4);
  return result;
}

Value throwable_error(Module* module, char* error) {
  Value* args = malloc(4 * sizeof(Value));

  args[0] = kNull;
  args[1] = MAKE_STRING(module, strdup("Throwable"));
  args[2] = MAKE_STRING(module, strdup("Error"));
  args[3] = MAKE_STRING(module, error);

  Value result = MAKE_LIST(module, args, 4);
  return result;
}