#include <debug.h>
#include <error.h>
#include <interpreter.h>
#include <module.h>
#include <pthread.h>
#include <threading.h>
#include <unistd.h>
#include <value.h>

Value list_get(Module* mod, Value list, uint32_t idx) {
  HeapValue *l = GET_PTR(list);
  if (idx < 0 || idx >= l->length) THROW_FMT(mod, "Invalid index, received %d", idx);

  return l->as_ptr[idx];
}

Value call_threaded(Module *new_module, Value callee, int32_t argc, Value *argv) {
  int16_t ipc = (int16_t)(callee & MASK_PAYLOAD_INT);
  int16_t local_space = (int16_t)((callee >> 16) & MASK_PAYLOAD_INT);
  int32_t old_sp = new_module->stack->stack_pointer;

  // Push arguments in reverse order
  for (int i = argc - 1; i >= 0; i--) stack_push(new_module, argv[i]);
  for (int i = 0; i < local_space - argc; i++) stack_push(new_module, MAKE_INTEGER(0));

  int32_t new_pc = new_module->pc + 5;

  new_module->gc_enabled = false;
  stack_push(new_module, MAKE_INTEGER(new_pc));
  stack_push(new_module, MAKE_INTEGER(old_sp));
  stack_push(new_module, MAKE_INTEGER(new_module->base_pointer));
  new_module->gc_enabled = true;

  new_module->base_pointer = new_module->stack->stack_pointer - 3;
  new_module->stack->stack_pointer++;

  Value ret = run_interpreter(new_module, ipc, true, new_module->callstack - 1);

  return ret;
}

void *actor_run(void *arg) {
  Actor *actor = (Actor *)arg;
  Module* module = actor->mod;

  Module* new_module = malloc(sizeof(Module));
  new_module->stack = stack_new();
  init_gc(new_module);
  pthread_mutex_lock(&module->module_mutex);
  memcpy(new_module->stack->values, module->stack->values, GLOBALS_SIZE * sizeof(Value));
  new_module->instr_count = module->instr_count;
  new_module->instrs = module->instrs;
  new_module->constants = module->constants;
  new_module->argc = module->argc;
  new_module->argv = module->argv;
  new_module->handles = module->handles;
  new_module->num_handles = module->num_handles;
  new_module->native_handles = module->native_handles;
  pthread_mutex_unlock(&module->module_mutex);
  new_module->callstack = 1;

  while (1) {
    Message *msg = dequeue(actor->queue);
    
    struct Event event = actor->event;
    Value *args = msg->args;
    int argc = msg->argc;
    int id = msg->name;

    Value *ons = event.ons;
    Value event_func = ons[id];

    call_threaded(new_module, event_func, argc, args);

    free(msg);
    free(args);

    if (new_module->is_terminated && actor->queue->head == NULL) {
      // Free the stack and the module
      free(new_module->stack->values);
      free(new_module->stack);
      free(new_module);
      free(actor->queue);
      free(actor);
      pthread_exit(0);
    }
  }

  // Free the stack and the module
  free(new_module->stack->values);
  free(new_module->stack);
  free(new_module);
  free(actor->queue);
  free(actor);
  return NULL;
}

Actor *create_actor(struct Event event, struct Module* mod) {
  Actor *actor = malloc(sizeof(Actor));
  actor->queue = create_message_queue();
  actor->event = event;
  actor->mod = mod;

  pthread_create(&actor->thread, NULL, actor_run, actor);
  
  mod->events[mod->event_count++] = actor;

  return actor;
}

void send_message(Actor *actor, int name, Value *args, int argc) {
  Message *msg = malloc(sizeof(Message));
  msg->args = args;
  msg->name = name;
  msg->argc = argc;
  msg->next = NULL;
  enqueue(actor->queue, msg);
}