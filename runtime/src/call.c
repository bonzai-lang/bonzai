#include <module.h>
#include <value.h>
#include <call.h>
#include <error.h>
#include <debug.h>
#include <threading.h>
#include <gc.h>
#include <library/http.h>

void op_call(Module *module, Value callee, int32_t argc) {
  ASSERT_FMT(module->callstack < MAX_FRAMES, "Call stack overflow, reached %d", module->callstack);

  int16_t ipc = (int16_t) (callee & MASK_PAYLOAD_INT);
  int16_t local_space = (int16_t) ((callee >> 16) & MASK_PAYLOAD_INT);
  int16_t old_sp = module->stack->stack_pointer - argc;

  module->stack->stack_pointer += local_space - argc;

  int32_t new_pc = module->pc + 5;

  stack_push(module->stack, MAKE_FRAME(module, new_pc, old_sp, module->base_pointer));

  module->base_pointer = module->stack->stack_pointer - 1;
  module->callstack++;

  module->pc = ipc;
}

void op_native_call(Module *module, Value callee, int32_t argc) {
  ASSERT_TYPE("op_native_call", callee, TYPE_STRING);
  char* fun = GET_NATIVE(callee);

  Value* args = /*test*/malloc(sizeof(Value) * argc);

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
  } else if (strcmp(fun, "listen_http") == 0) {
    ASSERT_ARGC("listen_http", argc, 1);
    ASSERT_TYPE("listen_http", args[0], TYPE_INTEGER);

    int port = GET_INT(args[0]);
    int server_socket = start_http_server(module, port);

    stack_push(module->stack, MAKE_INTEGER(server_socket));
  } else if (strcmp(fun, "accept_request") == 0) {
    ASSERT_ARGC("accept_request", argc, 1);
    ASSERT_TYPE("accept_request", args[0], TYPE_INTEGER);

    int server_socket = GET_INT(args[0]);
    int client_socket = accept_request(module, server_socket);

    stack_push(module->stack, MAKE_INTEGER(client_socket));
  } else if (strcmp(fun, "close_client") == 0) {
    ASSERT_ARGC("close_client", argc, 1);
    ASSERT_TYPE("close_client", args[0], TYPE_INTEGER);

    int client_socket = GET_INT(args[0]);
    close_client(module, client_socket);
  } else if (strcmp(fun, "get_buffer") == 0) {
    ASSERT_ARGC("get_buffer", argc, 1);
    ASSERT_TYPE("get_buffer", args[0], TYPE_INTEGER);

    int client_socket = GET_INT(args[0]);
    Value buffer = get_buffer(module, client_socket);

    stack_push(module->stack, buffer);
  } else if (strcmp(fun, "send_buffer") == 0) {
    ASSERT_ARGC("send_buffer", argc, 2);
    ASSERT_TYPE("send_buffer", args[0], TYPE_INTEGER);
    ASSERT_TYPE("send_buffer", args[1], TYPE_STRING);

    int client_socket = GET_INT(args[0]);
    send_buffer(module, client_socket, args[1]);
  } else {
    THROW_FMT("Unknown native function: %s", fun);
  }

  free(args);
  module->pc += 5;
}

InterpreterFunc interpreter_table[] = { op_native_call, op_call };