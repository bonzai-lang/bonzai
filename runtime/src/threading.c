#include <debug.h>
#include <error.h>
#include <interpreter.h>
#include <module.h>
#include <pthread.h>
#include <threading.h>
#include <unistd.h>
#include <value.h>

Value list_get(Module *mod, Value list, uint32_t idx) {
  // safe_point(mod);
  HeapValue *l = GET_PTR(list);
  if (idx < 0 || idx >= l->length) {
    THROW_FMT(mod, "Invalid index, received %d", idx);
  }

  return l->as_ptr[idx];
}

Value call_function(struct Module *m, Value closure, int32_t argc,
                    Value *argv) {
  Value callee = list_get(m, closure, 0);
  Value env = list_get(m, closure, 1);

  ASSERT_TYPE(m, "call_function", callee, TYPE_FUNCTION);
  ASSERT_TYPE(m, "call_function", env, TYPE_LIST);

  HeapValue *func = GET_PTR(callee);

  int32_t ipc = func->as_func.ip;
  uint16_t local_space = func->as_func.local_space;

  int32_t old_sp = m->stack->stack_pointer;

  // Push arguments in reverse order
  for (int i = argc - 1; i >= 0; i--) stack_push(m, argv[i]);
  stack_push(m, env);
  for (int i = 0; i < local_space - argc - 1; i++) stack_push(m, kNull);

  int32_t old_sp2 = m->stack->stack_pointer;
  int32_t new_pc = m->pc + 5;

  stack_push(m, MAKE_INTEGER(new_pc));
  stack_push(m, MAKE_INTEGER(old_sp));
  stack_push(m, MAKE_INTEGER(m->base_pointer));

  m->base_pointer = old_sp2;

  // mark_value(m, closure);

  bool old_gc_enabled = atomic_load(&m->gc->gc_enabled);

  atomic_store(&m->gc->gc_enabled, true);

  Value ret = run_interpreter(m, ipc, true, 1);

  atomic_store(&m->gc->gc_enabled, old_gc_enabled);

  return ret;
}


void* value_to_function(void* value) {
  struct thread_data_t* data = (struct thread_data_t*)value;

  Module* module = data->mod;
  Value function = data->function;

  Value ret = module->call_function(module, function, 0, NULL);
  
  Value* object_as_ptr = malloc(sizeof(Value));

  *object_as_ptr = ret;

  atomic_fetch_sub(&module->gc->thread_quantity, 1);

  return object_as_ptr;
}
