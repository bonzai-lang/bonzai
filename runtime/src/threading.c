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
  if (idx < 0 || idx >= l->length) {
    THROW_FMT(mod, "Invalid index, received %d", idx);
  }

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

  stack_push(new_module, MAKE_INTEGER(new_pc));
  stack_push(new_module, MAKE_INTEGER(old_sp));
  stack_push(new_module, MAKE_INTEGER(new_module->base_pointer));

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
  pthread_mutex_lock(&module->module_mutex);
  new_module->gc = module->gc;
  new_module->gc->stacks.stacks[new_module->gc->stacks.stack_count++] = new_module->stack;
  memcpy(new_module->stack->values, module->stack->values, GLOBALS_SIZE * sizeof(Value));
  new_module->instr_count = module->instr_count;
  new_module->instrs = module->instrs;
  new_module->constants = module->constants;
  new_module->argc = module->argc;
  new_module->argv = module->argv;
  new_module->handles = module->handles;
  new_module->num_handles = module->num_handles;
  new_module->native_handles = module->native_handles;
  new_module->current_actor = actor;
  new_module->is_terminated = 0;
  pthread_mutex_unlock(&module->module_mutex);
  new_module->callstack = 1;
  
  while (true) {
    Message *msg = dequeue(actor->queue);
    
    struct Event event = actor->event;
    Value *args = msg->args;
    int argc = msg->argc;
    int id = msg->name;

    new_module->actor_args = args;

    Value *ons = event.ons;
    Value event_func = ons[id];

    free(msg);
    
    call_threaded(new_module, event_func, argc, args);

    // gc(new_module);
    free(args);
    
    if (new_module->is_terminated && actor->queue->head == NULL) {
      break;
    }
  }

  // Free the stack and the module
  gc(new_module);
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
  pthread_mutex_init(&actor->mutex, NULL);
  pthread_cond_init(&actor->cond, NULL);

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

  // for (int i = 0; i < argc; i++) {
  //   mark_value(args[i]);
  // }

  enqueue(actor->queue, msg);
}