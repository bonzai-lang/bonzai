#include <value.h>
#include <module.h>
#include <operations.h>
#include <error.h>

Value print(Module* mod, Value* args, int argc) {
  ASSERT_ARGC("print", argc, 1);
  native_print(args[0]); printf("\n");

  return MAKE_INTEGER(0);
}

Value exit_with(Module* mod, Value* args, int argc) {
  ASSERT_ARGC("exit_with", argc, 1);
  ASSERT_TYPE("exit_with", args[0], TYPE_INTEGER);

  int code = GET_INT(args[0]);

  mod->is_terminated = true;
  pthread_exit(0);
  exit(code);
}

Value mutable_value(Module* mod, Value* args, int argc) {
  ASSERT_ARGC("mutable_value", argc, 1);
  ASSERT_TYPE("mutable_value", args[0], TYPE_MUTABLE);

  return GET_MUTABLE(args[0]);
}
