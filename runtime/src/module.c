#include <debug.h>
#include <error.h>
#include <module.h>
#include <stdatomic.h>
#include <value.h>

__attribute__((always_inline)) inline struct Frame pop_frame(Module* mod) {
  Value pc = mod->stack->values[mod->base_pointer];
  Value sp = mod->stack->values[mod->base_pointer + 1];
  Value bp = mod->stack->values[mod->base_pointer + 2];

  ASSERT_TYPE(mod, "pop_frame", pc, TYPE_INTEGER);
  ASSERT_TYPE(mod, "pop_frame", sp, TYPE_INTEGER);
  ASSERT_TYPE(mod, "pop_frame", bp, TYPE_INTEGER);

  struct Frame frame = {
      .instruction_pointer = GET_INT(pc),
      .stack_pointer = GET_INT(sp),
      .base_ptr = GET_INT(bp),
  };

  mod->callstack--;

  return frame;
}

__attribute__((always_inline)) inline struct Frame pop_event_frame(
    Module* mod) {
  Value ev = mod->stack->values[mod->base_pointer];
  Value pc = mod->stack->values[mod->base_pointer + 1];
  Value sp = mod->stack->values[mod->base_pointer + 2];
  Value bp = mod->stack->values[mod->base_pointer + 3];

  ASSERT_TYPE(mod, "pop_event_frame", ev, TYPE_EVENT);
  ASSERT_TYPE(mod, "pop_event_frame", pc, TYPE_INTEGER);
  ASSERT_TYPE(mod, "pop_event_frame", sp, TYPE_INTEGER);
  ASSERT_TYPE(mod, "pop_event_frame", bp, TYPE_INTEGER);

  struct Frame frame = {
      .instruction_pointer = GET_INT(pc),
      .stack_pointer = GET_INT(sp),
      .base_ptr = GET_INT(bp),
      .ons_count = GET_PTR(ev)->as_event.ons_count,
      .function_ipc = GET_PTR(ev)->as_event.ipc,
  };

  mod->callstack--;

  return frame;
}

// Initialize the GC system and its worker thread
void init_gc(gc_t* gc, Module* mod) {
  // Initialize GC state
  gc->first_object = NULL;
  gc->max_objects = INIT_OBJECTS;  // Example default size
  gc->num_objects = 0;
  gc->gc_enabled = true;
  gc->gc_running = false;

  pthread_cond_init(&gc->gc_cond, NULL);
  pthread_mutex_init(&gc->gc_mutex, NULL);

  mod->gc = gc;
}

void jump_try_catch(struct Module* module) {
  int jmp = module->latest_try_catch[0][module->latest_try_catch_count - 1];
  int offset = module->latest_try_catch[1][module->latest_try_catch_count - 1];
  module->latest_try_catch_count--;
  module->pc += (jmp - offset) * 5;
}