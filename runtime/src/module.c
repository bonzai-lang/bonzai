#include <error.h>
#include <module.h>
#include <value.h>

Frame pop_frame(Module* mod) {
  Value pc = mod->stack->values[mod->base_pointer];
  Value sp = mod->stack->values[mod->base_pointer + 1];
  Value bp = mod->stack->values[mod->base_pointer + 2];

  ASSERT_TYPE(mod, "pop_frame", pc, TYPE_INTEGER);
  ASSERT_TYPE(mod, "pop_frame", sp, TYPE_INTEGER);
  ASSERT_TYPE(mod, "pop_frame", bp, TYPE_INTEGER);

  Frame frame = {
    .instruction_pointer = GET_INT(pc),
    .stack_pointer = GET_INT(sp),
    .base_ptr = GET_INT(bp),
  };

  mod->callstack--;

  return frame;
}

Frame pop_event_frame(Module* mod) {
  Value ev = mod->stack->values[mod->base_pointer];
  Value pc = mod->stack->values[mod->base_pointer + 1];
  Value sp = mod->stack->values[mod->base_pointer + 2];
  Value bp = mod->stack->values[mod->base_pointer + 3];

  ASSERT_TYPE(mod, "pop_frame", ev, TYPE_EVENT);
  ASSERT_TYPE(mod, "pop_frame", pc, TYPE_INTEGER);
  ASSERT_TYPE(mod, "pop_frame", sp, TYPE_INTEGER);
  ASSERT_TYPE(mod, "pop_frame", bp, TYPE_INTEGER);

  Frame frame = {
    .instruction_pointer = GET_INT(pc),
    .stack_pointer = GET_INT(sp),
    .base_ptr = GET_INT(bp),
    .ons_count = GET_PTR(ev)->as_event.ons_count,
    .function_ipc = GET_PTR(ev)->as_event.ipc,
  };

  mod->callstack--;

  return frame;
}

void init_gc(Module* mod) {
  mod->first_object = NULL;
  mod->max_objects = INIT_OBJECTS;
  mod->num_objects = 0;

  mod->gc_enabled = true;
}