#include <call.h>
#include <debug.h>
#include <error.h>
#include <module.h>
#include <threading.h>
#include <value.h>

inline __attribute__((always_inline)) void op_call(Module* module, Value callee,
                                                  int32_t argc) {
  // ASSERT_FMT(module, module->callstack < MAX_FRAMES,
  //            "Call stack overflow, reached %d", module->callstack);

  ASSERT_TYPE(module, "op_call", callee, TYPE_FUNCTION);

  HeapValue* func = GET_PTR(callee);

  int32_t ipc = func->as_func.ip;
  int16_t local_space = func->as_func.local_space;
  int32_t old_sp = module->stack->stack_pointer - argc;

  module->stack->stack_pointer += local_space - argc;
  int32_t sp_before = module->stack->stack_pointer;

  int32_t new_pc = module->pc + 5;

  stack_push(module, MAKE_INTEGER(new_pc));
  stack_push(module, MAKE_INTEGER(old_sp));
  stack_push(module, MAKE_INTEGER(module->base_pointer));

  module->base_pointer = sp_before;
  module->callstack++;

  module->pc = ipc;
}

void* find_function(Module* module, struct Native callee) {
  if (module->native_handles != NULL &&
      module->native_handles[callee.addr] != NULL) {
    return module->native_handles[callee.addr];
  }

  for (size_t i = 0; i < module->num_handles; i++) {
    DLL lib = module->handles[i];
    void* addr = get_proc_address(lib, callee.name);

    if (addr == NULL) continue;

    module->native_handles[callee.addr] = addr;

    return addr;
  }

  return NULL;
}

void op_native_call(Module* module, Value callee, int32_t argc) {
  ASSERT_TYPE(module, "op_native_call", callee, TYPE_NATIVE);
  struct Native fun = GET_NATIVE(callee);

  Value* args = malloc(sizeof(Value) * argc);
  // Pop args in reverse order
  for (int i = argc - 1; i >= 0; i--) {
    args[i] = stack_pop(module);
    // mark_value(args[i]);
  }

  NativeFunction handler = find_function(module, fun);

  if (handler == NULL) {
    THROW_FMT(module, "Function %s not found", fun.name);
  }

  Value ret = handler(module, args, argc);

  stack_push(module, ret);

  module->gc->gc_enabled = true;

  free(args);
  module->pc += 5;
}

void direct_native_call(Module* module, struct Native fun, int32_t argc) {
  Value* args = malloc(sizeof(Value) * argc);

  pthread_mutex_lock(&module->gc->gc_mutex);
  module->gc->gc_enabled = false;
  pthread_mutex_unlock(&module->gc->gc_mutex);

  // Pop args in reverse order
  for (int i = argc - 1; i >= 0; i--) {
    args[i] = stack_pop(module);
  }

  NativeFunction handler = find_function(module, fun);

  if (handler == NULL) {
    THROW_FMT(module, "Function %s not found", fun.name);
  }

  Value ret = handler(module, args, argc);

  stack_push(module, ret);

  pthread_mutex_lock(&module->gc->gc_mutex);
  module->gc->gc_enabled = true;
  pthread_mutex_unlock(&module->gc->gc_mutex);

  free(args);
  module->pc += 5;
}

InterpreterFunc interpreter_table[] = {op_native_call, op_call};