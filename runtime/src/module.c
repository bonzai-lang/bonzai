#include <value.h>
#include <module.h>
#include <error.h>

Frame pop_frame(Module *mod) {
  Value clos_env = mod->stack->values[mod->base_pointer];

  ASSERT_FMT(mod, get_type(clos_env) == TYPE_FRAME, "Expected frame got %s", type_of(clos_env));

  HeapValue* clos_env_ptr = GET_PTR(clos_env);
  Frame frame = clos_env_ptr->as_frame;

  mod->callstack--;

  return frame;
}
