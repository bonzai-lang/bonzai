#include <module.h>
#include <value.h>
#include <call.h>
#include <error.h>
#include <debug.h>
#include <threading.h>

void op_call(Module *module, Value callee, int32_t argc) {
  ASSERT_FMT(module, module->callstack < MAX_FRAMES, "Call stack overflow, reached %d", module->callstack);

  ASSERT_TYPE(module, "op_call", callee, TYPE_FUNCTION);

  int16_t ipc = (int16_t) (callee & MASK_PAYLOAD_INT);
  int16_t local_space = (int16_t) ((callee >> 16) & MASK_PAYLOAD_INT);
  int32_t old_sp = module->stack->stack_pointer - argc;

  module->stack->stack_pointer += local_space - argc;

  int32_t new_pc = module->pc + 5;

  stack_push(module, MAKE_INTEGER(new_pc));
  stack_push(module, MAKE_INTEGER(old_sp));
  stack_push(module, MAKE_INTEGER(module->base_pointer));

  module->base_pointer = module->stack->stack_pointer - 3;
  module->callstack++;

  module->pc = ipc;
}

void* find_function(Module* module, struct Native callee) {
  if (module->native_handles != NULL && module->native_handles[callee.addr] != NULL) {
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

void op_native_call(Module *module, Value callee, int32_t argc) {
  ASSERT_TYPE(module, "op_native_call", callee, TYPE_NATIVE);
  struct Native fun = GET_NATIVE(callee);


  Value* args = malloc(sizeof(Value) * argc);

  module->gc_enabled = false;
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

  module->gc_enabled = true;

  free(args);
  module->pc += 5;
}

InterpreterFunc interpreter_table[] = { op_native_call, op_call };