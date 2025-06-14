#include <debug.h>
#include <error.h>
#include <module.h>
#include <stdatomic.h>
#include <value.h>

__attribute__((always_inline)) inline struct Frame pop_frame(Module* mod) {
  Value pc = mod->stack->values[mod->base_pointer];
  Value sp = mod->stack->values[mod->base_pointer + 1];
  Value bp = mod->stack->values[mod->base_pointer + 2];

  ASSERT_TYPE(mod, "pop_frame pc", pc, TYPE_INTEGER);
  ASSERT_TYPE(mod, "pop_frame sp", sp, TYPE_INTEGER);
  ASSERT_TYPE(mod, "pop_frame bp", bp, TYPE_INTEGER);

  struct Frame frame = {
      .instruction_pointer = GET_INT(pc),
      .stack_pointer = GET_INT(sp),
      .base_ptr = GET_INT(bp),
  };

  mod->callstack--;

  return frame;
}

// Initialize mark-sweep generation for old generation
void init_marksweep_generation(Generation* gen) {
  gen->first_object = NULL;
  gen->num_objects = 0;
  gen->max_objects = INIT_OBJECTS;
  gen->is_copying = false;
}

// Initialize the GC system and its worker thread
void init_gc(gc_t* gc, Module* mod) {
  // Initialize GC state
  init_marksweep_generation(&gc->young);
  init_marksweep_generation(&gc->old);

  atomic_store(&gc->gc_enabled, true);

  pthread_cond_init(&gc->gc_cond, NULL);
  pthread_mutex_init(&gc->gc_mutex, NULL);

  gc->stacks.stack_count = 0;
  gc->stacks.stack_capacity = STACKS_SIZE;
  gc->stacks.stacks = malloc(STACKS_SIZE * sizeof(Stack*));

  gc->remembered_set = NULL;

  atomic_store(&gc->gc_is_requested, false);

  atomic_store(&gc->thread_quantity, 1); // Count the main thread
  atomic_store(&gc->thread_stopped, 0);

  mod->gc = gc;
}
