#include <module.h>
#include <pthread.h>
#include <value.h>

struct thread_data_t {
  Value function;
  Module* mod;
};

void* value_to_function(void* value) {
  struct thread_data_t* data = (struct thread_data_t*)value;

  Module* module = data->mod;
  Value function = data->function;

  Module* new_module = malloc(sizeof(Module));
  pthread_mutex_lock(&module->module_mutex);
  new_module->stack = stack_new();
  new_module->gc = module->gc;
  int sc = new_module->gc->stacks.stack_count;
  new_module->gc->stacks.stacks[sc] = new_module->stack;
  new_module->gc->stacks.stack_count++;
  // memcpy(new_module->stack->values, module->stack->values,
  //        GLOBALS_SIZE * sizeof(Value));

  for (int i = 0; i < GLOBALS_SIZE; i++) {
    new_module->stack->values[i] = module->stack->values[i];
  }

  new_module->instr_count = module->instr_count;
  new_module->instrs = module->instrs;
  new_module->constants = module->constants;
  new_module->argc = module->argc;
  new_module->argv = module->argv;
  new_module->handles = module->handles;
  new_module->num_handles = module->num_handles;
  new_module->native_handles = module->native_handles;
  new_module->is_terminated = 0;
  new_module->pc = module->pc;
  new_module->call_function = module->call_function;
  new_module->file = module->file;

  pthread_mutex_unlock(&module->module_mutex);
  new_module->callstack = 1;
  pthread_mutex_init(&new_module->module_mutex, NULL);

  // printf("Thread %p\n", new_module->stack->values);

  Value ret = new_module->call_function(new_module, function, 0, NULL);

  new_module->gc->stacks.stacks[sc] = NULL;
  rearrange_stacks(new_module);

  // Free the stack and the module
  free(new_module->stack->values);
  free(new_module->stack);

  free(new_module);

  free(data);

  Value* object_as_ptr = malloc(sizeof(Value));

  *object_as_ptr = ret;

  return object_as_ptr;
}

Value create_thread(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "create_thread", argc, 1);
  ASSERT_TYPE(mod, "create_thread", args[0], TYPE_LIST);

  pthread_t thread;
  struct thread_data_t* data = malloc(sizeof(struct thread_data_t));
  data->function = args[0];
  data->mod = mod;
  pthread_create(&thread, NULL, value_to_function, data);

  HeapValue* thread_value = allocate(mod, TYPE_API);
  thread_value->as_any = thread;
  thread_value->destructor = NULL;
  return MAKE_PTR(thread_value);
}

Value create_detached_thread(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "create_detached_thread", argc, 1);
  ASSERT_TYPE(mod, "create_detached_thread", args[0], TYPE_LIST);

  pthread_t thread;
  struct thread_data_t* data = malloc(sizeof(struct thread_data_t));
  data->function = args[0];
  data->mod = mod;
  pthread_create(&thread, NULL, value_to_function, data);
  pthread_detach(thread);

  HeapValue* thread_value = allocate(mod, TYPE_API);
  thread_value->as_any = thread;
  thread_value->destructor = NULL;
  return MAKE_PTR(thread_value);
}

Value wait_thread(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "wait_thread", argc, 1);
  // printf("Thread: "); debug_value(args[0]); printf("\n");
  ASSERT_TYPE(mod, "wait_thread", args[0], TYPE_API);

  HeapValue* thread_value = GET_PTR(args[0]);
  pthread_t thread = thread_value->as_any;
  atomic_store(&mod->stack->is_stopped, true);
  Value* thread_returned;
  pthread_join(thread, (void**)&thread_returned);
  atomic_store(&mod->stack->is_stopped, false);

  Value object = *thread_returned;

  free(thread_returned);

  return object;
}

Value lock(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "lock_mutex", argc, 1);

  if (!IS_PTR(args[0])) {
    THROW(mod, "Argument to lock_mutex must be a pointer");
  }

  HeapValue* mutex = GET_PTR(args[0]);

  pthread_mutex_lock(&mutex->mutex);

  return MAKE_INTEGER(0);
}

Value unlock(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "lock_mutex", argc, 1);

  if (!IS_PTR(args[0])) {
    THROW(mod, "Argument to lock_mutex must be a pointer");
  }

  HeapValue* mutex = GET_PTR(args[0]);

  pthread_mutex_unlock(&mutex->mutex);

  return MAKE_INTEGER(0);
}
