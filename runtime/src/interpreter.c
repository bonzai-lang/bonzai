#include <call.h>
#include <compare.h>
#include <debug.h>
#include <error.h>
#include <math.h>
#include <module.h>
#include <stdbool.h>
#include <threading.h>
#include <value.h>
#include <hashtable.h>

#define INCREASE_IP_BY(mod, x) (mod->pc += ((x) * 5));
#define INCREASE_IP(mod) INCREASE_IP_BY(mod, 1);

Value run_interpreter(Module *module, int32_t ipc, bool does_return,
                      int callstack) {
  Constants *constants = module->constants;
  int32_t *bytecode = module->instrs;
  module->pc = ipc;

#define op bytecode[module->pc]
#define i1 bytecode[module->pc + 1]
#define i2 bytecode[module->pc + 2]
#define i3 bytecode[module->pc + 3]
#define i4 bytecode[module->pc + 4]

#define UNKNOWN &&case_unknown

  void *jmp_table[] = {&&case_load_local,
                       &&case_store_local,
                       &&case_load_constant,
                       &&case_load_global,
                       &&case_store_global,
                       &&case_return,
                       &&case_compare,
                       &&case_update,
                       &&case_make_list,
                       &&case_list_get,
                       &&case_call,
                       &&case_call_global,
                       &&case_call_local,
                       &&case_jump_if_false,
                       &&case_jump_rel,
                       &&case_get_index,
                       &&case_special,
                       &&case_halt,
                       &&case_spawn,
                       UNKNOWN,
                       UNKNOWN,
                       &&case_make_function_and_store,
                       &&case_load_native,
                       &&case_unloc,
                       UNKNOWN,
                       &&case_make_mutable,
                       &&case_loc,
                       &&case_add,
                       &&case_sub,
                       &&case_mul,
                       &&case_div,
                       &&case_mod,
                       &&case_call_native,
                       UNKNOWN,
                       &&case_get_value,
                       &&case_get_record_access,
                       &&case_make_record,
                       &&case_jump,
                       UNKNOWN};

  goto *jmp_table[op];

case_load_local: {
  safe_point(module);
  int bp = module->base_pointer;
  Value value = module->stack->values[bp + i1];
  stack_push(module, value);
  INCREASE_IP(module);
  goto *jmp_table[op];
}

case_store_local: {
  safe_point(module);
  int bp = module->base_pointer;
  module->stack->values[bp + i1] = module->stack->values[module->stack->stack_pointer - 1];
  module->stack->values[module->stack->stack_pointer - 1] = kNull;

  module->stack->stack_pointer--;

  INCREASE_IP(module);
  goto *jmp_table[op];
}

case_load_constant: {
  safe_point(module);
  Value value = constants->values[i1];
  stack_push(module, value);
  INCREASE_IP(module);
  goto *jmp_table[op];
}

case_load_global: {
  safe_point(module);
  Value value = module->stack->values[i1];
  stack_push(module, value);
  INCREASE_IP(module);
  goto *jmp_table[op];
}

case_store_global: {
  safe_point(module);
  Value value = module->stack->values[module->stack->stack_pointer - 1];
  module->stack->values[i1] = value;

  module->stack->values[module->stack->stack_pointer - 1] = kNull;

  module->stack->stack_pointer--;

  INCREASE_IP(module);
  goto *jmp_table[op];
}

case_return: {
  safe_point(module);
  struct Frame fr = pop_frame(module);
  Value ret = module->stack->values[module->stack->stack_pointer - 1];

  module->stack->values[module->stack->stack_pointer - 1] = kNull;

  for (int i = module->stack->stack_pointer; i > fr.stack_pointer; i--) {
    module->stack->values[i - 1] = kNull;
  }

  module->stack->stack_pointer = fr.stack_pointer;
  module->base_pointer = fr.base_ptr;
  stack_push(module, ret);

  module->pc = fr.instruction_pointer;
  if (does_return && module->callstack == 0) {
    atomic_store(&module->stack->is_halted, true);
    return ret;
  }

  goto *jmp_table[op];
}

case_compare: {
  safe_point(module);
  Value a = module->stack->values[module->stack->stack_pointer - 2];
  Value b = module->stack->values[module->stack->stack_pointer - 1];

  module->stack->values[module->stack->stack_pointer - 1] = kNull;

  Value result = comparison_table[i1](module, a, b);
  module->stack->values[module->stack->stack_pointer - 2] = result;
  module->stack->stack_pointer--;
  INCREASE_IP(module);
  goto *jmp_table[op];
}

case_update: {
  safe_point(module);
  Value variable = module->stack->values[module->stack->stack_pointer - 1];
  Value value = module->stack->values[module->stack->stack_pointer - 2];

  module->stack->values[module->stack->stack_pointer - 1] = kNull;
  module->stack->values[module->stack->stack_pointer - 2] = kNull;

  ASSERT_TYPE(module, "update", variable, TYPE_MUTABLE);

  // trylock(&module->gc->gc_mutex);

  writeBarrier(module, GET_PTR(variable), value);
  GET_MUTABLE(variable) = value;

  // pthread_mutex_unlock(&module->gc->gc_mutex);

  module->stack->stack_pointer -= 2;

  INCREASE_IP(module);
  goto *jmp_table[op];
}

case_make_list: {
  safe_point(module);
  if (i1 == 0) {
    stack_push(module, EMPTY_LIST);
    INCREASE_IP(module);
    goto *jmp_table[op];
  }
  Value *list = malloc(i1 * sizeof(Value));

  // Loop in reverse order to pop values in the correct order
  for (int i = i1 - 1; i >= 0; i--) {
    list[i] = module->stack->values[module->stack->stack_pointer - i1 + i];
    module->stack->values[module->stack->stack_pointer - i1 + i] = kNull;
  }

  int sp = module->stack->stack_pointer - i1;

  Value value = MAKE_LIST(module, list, i1);
 
  module->stack->values[sp] = value;
  module->stack->stack_pointer = sp + 1;

  INCREASE_IP(module);
  goto *jmp_table[op];
}

case_list_get: {
  safe_point(module);
  Value list = module->stack->values[module->stack->stack_pointer - 1];
  uint32_t index = i1;

  ASSERT_TYPE(module, "list_get", list, TYPE_LIST);

  Value *list_ptr = GET_LIST(list);

  ASSERT_FMT(module, index >= 0 && index < GET_PTR(list)->length,
             "Index out of bounds, received %d", index);

  // stack_push(module, list_ptr[index]);
  module->stack->values[module->stack->stack_pointer - 1] = list_ptr[index];

  INCREASE_IP(module);
  goto *jmp_table[op];
}

case_call: {
  safe_point(module);
  Value callee = stack_pop(module);

  ASSERT(module, IS_FUN(callee) || IS_PTR(callee), "Invalid callee type");

  ValueType callee_type = get_type(callee);

  interpreter_table[callee_type == TYPE_FUNCTION](module, callee, i1);

  goto *jmp_table[op];
}

case_call_global: {
  safe_point(module);

  Value callee = module->stack->values[i1];

  ASSERT(module, IS_FUN(callee) || IS_PTR(callee), "Invalid callee type");
  ValueType callee_type = get_type(callee);

  interpreter_table[callee_type == TYPE_FUNCTION](module, callee, i2);

  goto *jmp_table[op];
}

case_call_local: {
  safe_point(module);
  Value callee = module->stack->values[module->base_pointer + i1];

  ASSERT(module, IS_FUN(callee) || IS_PTR(callee), "Invalid callee type");
  ValueType callee_type = get_type(callee);

  interpreter_table[callee_type == TYPE_FUNCTION](module, callee, i2);

  goto *jmp_table[op];
}

case_jump_if_false: {
  safe_point(module);
  Value value = module->stack->values[module->stack->stack_pointer - 1];
  if (GET_INT(value) == 0) {
    INCREASE_IP_BY(module, i1);
  } else {
    INCREASE_IP(module);
  }

  // Clear the value from the stack
  module->stack->values[module->stack->stack_pointer - 1] = kNull;

  module->stack->stack_pointer--;

  goto *jmp_table[op];
}

case_jump_rel: {
  safe_point(module);
  INCREASE_IP_BY(module, i1);
  goto *jmp_table[op];
}

case_get_index: {
  safe_point(module);
  Value index = module->stack->values[module->stack->stack_pointer - 1];
  Value list = module->stack->values[module->stack->stack_pointer - 2];

  ASSERT_TYPE(module, "get_index", list, TYPE_LIST);
  ASSERT_TYPE(module, "get_index", index, TYPE_INTEGER);

  Value *list_ptr = GET_LIST(list);
  uint32_t idx = GET_INT(index);

  ASSERT_FMT(module, idx >= 0 && idx < GET_PTR(list)->length,
             "Index out of bounds, received %d", idx);

  // Clear the values from the stack
  module->stack->values[module->stack->stack_pointer - 1] = kNull;
  module->stack->values[module->stack->stack_pointer - 2] = kNull;

  module->stack->values[module->stack->stack_pointer - 2] = list_ptr[idx];
  module->stack->stack_pointer--;

  INCREASE_IP(module);
  goto *jmp_table[op];
}

case_special: {
  safe_point(module);
  stack_push(module, kNull);
  INCREASE_IP(module);
  goto *jmp_table[op];
}

case_halt: {
  atomic_store(&module->stack->is_halted, true);
  atomic_store(&module->stack->is_stopped, true);
  atomic_fetch_sub(&module->gc->thread_quantity, 1);
  module->gc->stacks.stacks[0] = NULL;
  pthread_join(module->gc->gc_thread, NULL);
  module->is_terminated = true;
  
  return kNull;
}

case_spawn: {
  safe_point(module);
  Value function = module->stack->values[module->stack->stack_pointer - 1];

  ASSERT_TYPE(module, "spawn", function, TYPE_LIST);

  ASSERT_FMT(module, GET_PTR(function)->length >= 1,
             "Spawn function must have at least one argument");
  ASSERT_TYPE(module, "spawn", GET_PTR(function)->as_ptr[0], TYPE_FUNCTION);
  ASSERT_FMT(module, GET_PTR(function)->length <= 2,
             "Spawn function must have at most two arguments");

  Module *new_module = malloc(sizeof(Module));
  new_module->stack = stack_new();
  new_module->gc = module->gc;

  memcpy(new_module->stack->values, module->stack->values,
         GLOBALS_SIZE * sizeof(Value));

  // for (int i = 0; i < GLOBALS_SIZE; i++) {
  //   new_module->stack->values[i] =
  //       clone_value(new_module, module->stack->values[i]);
  // }

  trylock(&new_module->gc->gc_mutex);
  int sc = new_module->gc->stacks.stack_count;
  if (sc >= module->gc->stacks.stack_capacity) {
    module->gc->stacks.stack_capacity *= 1.5;
    module->gc->stacks.stacks =
        realloc(module->gc->stacks.stacks,
                module->gc->stacks.stack_capacity * sizeof(Stack *));

    if (module->gc->stacks.stacks == NULL) {
      pthread_mutex_unlock(&module->module_mutex);
      THROW(new_module, "Failed to allocate memory for stacks");
    }
  }

  new_module->gc->stacks.stacks[sc] = new_module->stack;
  atomic_store(&new_module->stack->stack_id, sc);
  atomic_fetch_add(&new_module->gc->thread_quantity, 1);
  new_module->gc->stacks.stack_count++;
  pthread_mutex_unlock(&new_module->gc->gc_mutex);

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
  // Clone the latest position
  for (int i = 0; i < module->latest_position_index; i++)
    new_module->latest_position[i][0] = module->latest_position[i][0];
  for (int i = 0; i < module->latest_position_index; i++)
    new_module->latest_position[i][1] = module->latest_position[i][1];
  for (int i = 0; i < module->latest_position_index; i++)
    new_module->file[i] = strdup(module->file[i]);
  new_module->latest_position_index = module->latest_position_index;

  new_module->callstack = 1;
  pthread_mutex_init(&new_module->module_mutex, NULL);

  pthread_t thread;

  struct thread_data_t *data = malloc(sizeof(struct thread_data_t));
  data->function = function;
  data->mod = new_module;

  pthread_create(&thread, NULL, value_to_function, data);

  Value thread_value = MAKE_THREAD(module, thread, function);
  writeBarrier(module, GET_PTR(thread_value), function);

  module->stack->values[module->stack->stack_pointer - 1] = thread_value;

  INCREASE_IP(module);
  goto *jmp_table[op];
}

case_make_function_and_store: {
  safe_point(module);
  int32_t new_pc = module->pc + 5;
  Value lambda = MAKE_FUNCTION(module, new_pc, i3);

  module->stack->values[i1] = lambda;

  INCREASE_IP_BY(module, i2 + 1);
  goto *jmp_table[op];
}

case_load_native: {
  safe_point(module);
  Value native = module->constants->values[i1];

  ASSERT_TYPE(module, "load_native", native, TYPE_STRING);

  char *name = GET_STRING(native);

  stack_push(module, MAKE_NATIVE(module, name, i1));
  INCREASE_IP(module);
  goto *jmp_table[op];
}

case_unloc: {
  safe_point(module);
  // printf("Unloc instruction at %d\n", module->pc);
  module->latest_position_index--;
  if (module->latest_position_index < 0) {
    THROW(module, "Unmatched unloc instruction");
  }

  INCREASE_IP(module);
  goto *jmp_table[op];
}

case_make_mutable: {
  safe_point(module);
  Value x = module->stack->values[module->stack->stack_pointer - 1];
  module->stack->values[module->stack->stack_pointer - 1] =
      MAKE_MUTABLE(module, x);
  INCREASE_IP(module);
  goto *jmp_table[op];
}

case_loc: {
  safe_point(module);

  if (module->latest_position_index >= 512) {
    module->latest_position_index = 511;
  }

  module->latest_position[module->latest_position_index][0] = i1;
  module->latest_position[module->latest_position_index][1] = i2;
  module->file[module->latest_position_index] = GET_STRING(constants->values[i3]);

  module->latest_position_index++;

  INCREASE_IP(module);
  goto *jmp_table[op];
}

case_add: {
  safe_point(module);
  Value b = module->stack->values[module->stack->stack_pointer - 1];
  Value a = module->stack->values[module->stack->stack_pointer - 2];

  module->stack->values[module->stack->stack_pointer - 1] = kNull;
  module->stack->values[module->stack->stack_pointer - 2] = kNull;

  ValueType ty = get_type(a);

  ASSERT_TYPE(module, "add", b, ty);

  switch (ty) {
    case TYPE_INTEGER:
      module->stack->values[module->stack->stack_pointer - 2] =
          MAKE_INTEGER(a + b);
      module->stack->stack_pointer--;
      break;

    case TYPE_FLOAT: {
      double f = GET_FLOAT(a) + GET_FLOAT(b);
      module->stack->values[module->stack->stack_pointer - 2] = MAKE_FLOAT(f);
      module->stack->stack_pointer--;
      break;
    }

    case TYPE_STRING: {
      char *s1 = GET_STRING(a);
      char *s2 = GET_STRING(b);

      char *s = malloc(strlen(s1) + strlen(s2) + 1);

      snprintf(s, strlen(s1) + strlen(s2) + 1, "%s%s", s1, s2);

      module->stack->values[module->stack->stack_pointer - 2] =
          MAKE_STRING(module, s);
      module->stack->stack_pointer--;
      break;
    }

    case TYPE_RECORD: {
      if (IS_EMPTY_RECORD(a)) {
        module->stack->values[module->stack->stack_pointer - 2] = b;
        module->stack->stack_pointer--;
        break;
      } else if (IS_EMPTY_RECORD(b)) {
        module->stack->values[module->stack->stack_pointer - 2] = a;
        module->stack->stack_pointer--;
        break;
      }
      
      module->stack->values[module->stack->stack_pointer - 2] =
          hash_concat(module, a, b);

      module->stack->stack_pointer--;
      break;
    }

    case TYPE_LIST: {
      if (IS_EMPTY_LIST(a)) {
        module->stack->values[module->stack->stack_pointer - 2] = b;
        module->stack->stack_pointer--;
        break;
      } else if (IS_EMPTY_LIST(b)) {
        module->stack->values[module->stack->stack_pointer - 2] = a;
        module->stack->stack_pointer--;
        break;
      }

      HeapValue *l1 = GET_PTR(a);
      HeapValue *l2 = GET_PTR(b);

      Value *list = malloc((l1->length + l2->length) * sizeof(Value));
      for (uint32_t i = 0; i < l1->length; i++) list[i] = l1->as_ptr[i];
      for (uint32_t i = 0; i < l2->length; i++) list[l1->length + i] = l2->as_ptr[i];

      module->stack->values[module->stack->stack_pointer - 2] =
          MAKE_LIST(module, list, l1->length + l2->length);

      module->stack->stack_pointer--;
      break;
    }

    default:
      THROW(module, "Unsupported type for +");
  }

  INCREASE_IP(module);
  goto *jmp_table[op];
}

case_sub: {
  safe_point(module);
  Value a = stack_pop(module);
  Value b = stack_pop(module);

  ValueType ty = get_type(a);

  ASSERT_TYPE(module, "sub", b, ty);

  switch (ty) {
    case TYPE_INTEGER:
      stack_push(module, MAKE_INTEGER(GET_INT(b) - GET_INT(a)));
      break;

    case TYPE_FLOAT: {
      double f = GET_FLOAT(b) - GET_FLOAT(a);
      stack_push(module, MAKE_FLOAT(f));
      break;
    }

    default:
      THROW(module, "Unsupported type for -");
  }

  INCREASE_IP(module);
  goto *jmp_table[op];
}

case_mul: {
  safe_point(module);
  Value a = stack_pop(module);
  Value b = stack_pop(module);

  ValueType ty = get_type(a);

  ASSERT_TYPE(module, "mul", b, ty);

  switch (ty) {
    case TYPE_INTEGER:
      stack_push(module, MAKE_INTEGER(GET_INT(b) * GET_INT(a)));
      break;

    case TYPE_FLOAT: {
      double f = GET_FLOAT(b) * GET_FLOAT(a);
      stack_push(module, MAKE_FLOAT(f));
      break;
    }

    default:
      THROW(module, "Unsupported type for *");
  }

  INCREASE_IP(module);
  goto *jmp_table[op];
}

case_div: {
  safe_point(module);
  Value a = stack_pop(module);
  Value b = stack_pop(module);

  ValueType ty = get_type(a);

  ASSERT_TYPE(module, "div", b, ty);

  switch (ty) {
    case TYPE_INTEGER:
      stack_push(module, MAKE_INTEGER(GET_INT(b) / GET_INT(a)));
      break;

    case TYPE_FLOAT: {
      double f = GET_FLOAT(b) / GET_FLOAT(a);
      stack_push(module, MAKE_FLOAT(f));
      break;
    }

    default:
      THROW(module, "Unsupported type for /");
  }

  INCREASE_IP(module);
  goto *jmp_table[op];
}

case_mod: {
  safe_point(module);
  Value b = stack_pop(module);
  Value a = stack_pop(module);

  ValueType ty = get_type(a);

  ASSERT_TYPE(module, "mod", b, ty);

  switch (ty) {
    case TYPE_INTEGER:
      stack_push(module, MAKE_INTEGER(GET_INT(a) % GET_INT(b)));
      break;

    case TYPE_FLOAT: {
      float f = fmod(GET_FLOAT(a), GET_FLOAT(b));
      stack_push(module, MAKE_FLOAT(f));
      break;
    }

    default:
      THROW(module, "Unsupported type for %%");
  }

  INCREASE_IP(module);
  goto *jmp_table[op];
}

case_call_native: {
  safe_point(module);
  Value native = module->constants->values[i1];

  direct_native_call(
      module, (struct Native){.name = GET_STRING(native), .addr = i1}, i2);
  goto *jmp_table[op];
}

case_get_value: {
  safe_point(module);
  Value value = module->stack->values[module->stack->stack_pointer - 1];

  ASSERT_TYPE(module, "get_value", value, TYPE_MUTABLE);

  module->stack->values[module->stack->stack_pointer - 1] = GET_MUTABLE(value);

  INCREASE_IP(module);
  goto *jmp_table[op];
}

case_get_record_access: {
  safe_point(module);
  Value record = module->stack->values[module->stack->stack_pointer - 1];
  Value access = module->constants->values[i1];

  ASSERT_TYPE(module, "get_record_access", record, TYPE_RECORD);
  ASSERT_TYPE(module, "get_record_access", access, TYPE_STRING);

  if (IS_EMPTY_RECORD(record)) {
    Value *none = malloc(3 * sizeof(Value));
    none[0] = kNull;
    none[1] = MAKE_STRING_NON_GC(module, "Optional");
    none[2] = MAKE_STRING_NON_GC(module, "None");

    module->stack->values[module->stack->stack_pointer - 1] =
        MAKE_LIST(module, none, 3);

    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  HeapValue *ptr = GET_PTR(record);
  char *name = GET_STRING(access);

  // A record is in the following format:
  // [null, Map, Map, [name1, value1, name2, value2, ...]]
  // We need to find the index of the name in the list
  // and return the corresponding value

  module->stack->values[module->stack->stack_pointer - 1] =
      hash_get(module, &ptr->as_record, name, GET_PTR(access)->length);

  INCREASE_IP(module);
  goto *jmp_table[op];
}

case_make_record: {
  safe_point(module);
  if (i1 == 0) {
    stack_push(module, EMPTY_RECORD);
    INCREASE_IP(module);
    goto *jmp_table[op];
  }
  Value* keys = malloc(i1 * sizeof(char*));
  Value *values = malloc(i1 * sizeof(Value));

  // Records are in the following format:
  // [key1, value1, key2, value2, ...]
  // We need to split the stack into keys and values
  // We can loop in reverse order to pop values in the correct order
  // There is i1 * 2 values to pop from the stack
  for (int i = (i1 * 2) - 1; i >= 0; i -= 2) {
    int idx = (i - 1) / 2;
    values[idx] =
        module->stack->values[module->stack->stack_pointer - i1 * 2 + i];
    keys[idx] = module->stack->values[module->stack->stack_pointer - i1 * 2 + i - 1];
    
    module->stack->values[module->stack->stack_pointer - i1 * 2 + i] = kNull;
    module->stack->values[module->stack->stack_pointer - i1 * 2 + i - 1] = kNull;
  }

  int sp = module->stack->stack_pointer - (i1 * 2);

  module->stack->values[sp] =
      MAKE_RECORD(module, keys, values, i1);

  free(keys);
  free(values);

  module->stack->stack_pointer = sp + 1;

  INCREASE_IP(module);
  goto *jmp_table[op];
}

case_jump: {
  safe_point(module);

  module->pc = i1 * 5;
  goto *jmp_table[op];
}

case_unknown:
  THROW_FMT(module, "Unknown opcode %d", op);
  exit(1);
}