#include <module.h>
#include <value.h>
#include <call.h>
#include <error.h>
#include <debug.h>
#include <threading.h>
#include <gc.h>

void op_call(Module *module, Value callee, int32_t argc) {
  ASSERT_FMT(module->callstack < MAX_FRAMES, "Call stack overflow, reached %d", module->callstack);

  int16_t ipc = (int16_t) (callee & MASK_PAYLOAD_INT);
  int16_t local_space = (int16_t) ((callee >> 16) & MASK_PAYLOAD_INT);
  int16_t old_sp = module->stack->stack_pointer - argc;

  module->stack->stack_pointer += local_space - argc;

  int32_t new_pc = module->pc + 5;

  stack_push(module->stack, MAKE_FRAME(new_pc, old_sp, module->base_pointer));

  module->base_pointer = module->stack->stack_pointer - 1;
  module->callstack++;

  module->pc = ipc;
}

void op_native_call(Module *module, Value callee, int32_t argc) {
  ASSERT_TYPE("op_native_call", callee, TYPE_STRING);
  char* fun = GET_NATIVE(callee);

  Value* args = GC_malloc(sizeof(Value) * argc);

  // Pop args in reverse order
  for (int i = argc - 1; i >= 0; i--) {
    args[i] = stack_pop(module->stack);
  }

  if (strcmp(fun, "print") == 0) {
    native_print(args[0]);
    printf("\n");

    stack_push(module->stack, MAKE_INTEGER(argc));
  } else if (strcmp(fun, "exit") == 0) {
    printf("EXIT\n");
    module->is_terminated = true;
    pthread_exit(0);
  } else if (strcmp(fun, "+") == 0) {
    ASSERT_ARGC("+", argc, 2);
    ASSERT_TYPE("+", args[0], TYPE_INTEGER);
    ASSERT_TYPE("+", args[1], TYPE_INTEGER);

    stack_push(module->stack, MAKE_INTEGER(GET_INT(args[0]) + GET_INT(args[1])));
  } else if (strcmp(fun, "-") == 0) {
    ASSERT_ARGC("-", argc, 2);
    ASSERT_TYPE("-", args[0], TYPE_INTEGER);
    ASSERT_TYPE("-", args[1], TYPE_INTEGER);

    stack_push(module->stack, MAKE_INTEGER(GET_INT(args[0]) - GET_INT(args[1])));
  } else if (strcmp(fun, "*") == 0) {
    ASSERT_ARGC("*", argc, 2);
    ASSERT_TYPE("*", args[0], TYPE_INTEGER);
    ASSERT_TYPE("*", args[1], TYPE_INTEGER);

    stack_push(module->stack, MAKE_INTEGER(GET_INT(args[0]) * GET_INT(args[1])));
  } else if (strcmp(fun, "/") == 0) {
    ASSERT_ARGC("/", argc, 2);
    ASSERT_TYPE("/", args[0], TYPE_INTEGER);
    ASSERT_TYPE("/", args[1], TYPE_INTEGER);

    stack_push(module->stack, MAKE_INTEGER(GET_INT(args[0]) / GET_INT(args[1])));
  } else if (strcmp(fun, "==") == 0) {
    ASSERT_ARGC("==", argc, 2);

    stack_push(module->stack, MAKE_INTEGER(value_eq(args[0], args[1])));
  } else if (strcmp(fun, "<") == 0) {
    ASSERT_ARGC("<", argc, 2);
    ASSERT_TYPE("<", args[0], TYPE_INTEGER);
    ASSERT_TYPE("<", args[1], TYPE_INTEGER);

    stack_push(module->stack, MAKE_INTEGER(GET_INT(args[0]) < GET_INT(args[1])));
  } else if (strcmp(fun, ">") == 0) {
    ASSERT_ARGC(">", argc, 2);
    ASSERT_TYPE(">", args[0], TYPE_INTEGER);
    ASSERT_TYPE(">", args[1], TYPE_INTEGER);

    stack_push(module->stack, MAKE_INTEGER(GET_INT(args[0]) > GET_INT(args[1])));
  } else if (strcmp(fun, "<=") == 0) {
    ASSERT_ARGC("<=", argc, 2);
    ASSERT_TYPE("<=", args[0], TYPE_INTEGER);
    ASSERT_TYPE("<=", args[1], TYPE_INTEGER);

    stack_push(module->stack, MAKE_INTEGER(GET_INT(args[0]) <= GET_INT(args[1])));
  } else if (strcmp(fun, ">=") == 0) {
    ASSERT_ARGC(">=", argc, 2);
    ASSERT_TYPE(">=", args[0], TYPE_INTEGER);
    ASSERT_TYPE(">=", args[1], TYPE_INTEGER);

    stack_push(module->stack, MAKE_INTEGER(GET_INT(args[0]) >= GET_INT(args[1])));
  } else if (strcmp(fun, "&&") == 0) {
    ASSERT_ARGC("&&", argc, 2);
    ASSERT_TYPE("&&", args[0], TYPE_INTEGER);
    ASSERT_TYPE("&&", args[1], TYPE_INTEGER);

    stack_push(module->stack, MAKE_INTEGER(GET_INT(args[0]) && GET_INT(args[1])));
  } else if (strcmp(fun, "||") == 0) {
    ASSERT_ARGC("||", argc, 2);
    ASSERT_TYPE("||", args[0], TYPE_INTEGER);
    ASSERT_TYPE("||", args[1], TYPE_INTEGER);

    stack_push(module->stack, MAKE_INTEGER(GET_INT(args[0]) || GET_INT(args[1])));
  } else if (strcmp(fun, "not") == 0) {
    ASSERT_ARGC("not", argc, 1);
    ASSERT_TYPE("not", args[0], TYPE_INTEGER);

    stack_push(module->stack, MAKE_INTEGER(!GET_INT(args[0])));
  } else if (strcmp(fun, "value") == 0) {
    ASSERT_ARGC("value", argc, 1);
    ASSERT_TYPE("value", args[0], TYPE_MUTABLE);

    stack_push(module->stack, GET_MUTABLE(args[0]));
  } else {
    THROW_FMT("Unknown native function %s", fun);
  }

  module->pc += 5;
}

InterpreterFunc interpreter_table[] = { op_native_call, op_call };