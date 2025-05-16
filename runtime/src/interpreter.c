#include <call.h>
#include <compare.h>
#include <debug.h>
#include <error.h>
#include <math.h>
#include <module.h>
#include <stdbool.h>
#include <threading.h>
#include <value.h>

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
                       UNKNOWN,
                       UNKNOWN,
                       UNKNOWN,
                       &&case_make_function_and_store,
                       &&case_load_native,
                       UNKNOWN,
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
  module->stack->values[bp + i1] = stack_pop(module);

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

  module->stack->stack_pointer--;

  INCREASE_IP(module);
  goto *jmp_table[op];
}

case_return: {
  safe_point(module);
  struct Frame fr = pop_frame(module);
  Value ret = module->stack->values[module->stack->stack_pointer - 1];

  module->stack->stack_pointer = fr.stack_pointer;
  module->base_pointer = fr.base_ptr;
  stack_push(module, ret);

  module->pc = fr.instruction_pointer;
  if (does_return && module->callstack == 0) {
    return ret;
  }

  goto *jmp_table[op];
}

case_compare: {
  safe_point(module);
  Value a = module->stack->values[module->stack->stack_pointer - 2];
  Value b = module->stack->values[module->stack->stack_pointer - 1];

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

  ASSERT_TYPE(module, "update", variable, TYPE_MUTABLE);

  // pthread_mutex_lock(&ptr->mutex);

  GET_MUTABLE(variable) = value;

  // pthread_mutex_unlock(&ptr->mutex);

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

  // debug_value(list); printf("\n");

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
  pthread_join(module->gc->gc_thread, NULL);
  module->is_terminated = true;
  
  return kNull;
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
  module->latest_position[0] = i1;
  module->latest_position[1] = i2;
  module->file = GET_STRING(constants->values[i3]);

  INCREASE_IP(module);
  goto *jmp_table[op];
}

case_add: {
  safe_point(module);
  Value b = module->stack->values[module->stack->stack_pointer - 1];
  Value a = module->stack->values[module->stack->stack_pointer - 2];

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

      HeapValue *r1 = GET_PTR(a);
      HeapValue *r2 = GET_PTR(b);

      char **keys = malloc((r1->length + r2->length) * sizeof(char *));
      Value *values = malloc((r1->length + r2->length) * sizeof(Value));

      for (uint32_t i = 0; i < r1->length; i++) {
        keys[i] = r1->as_record.keys[i];
        values[i] = r1->as_record.values[i + 1];
      }
      for (uint32_t i = 0; i < r2->length; i++) {
        keys[r1->length + i] = r2->as_record.keys[i];
        values[r1->length + i] = r2->as_record.values[i + 1];
      }

      module->stack->values[module->stack->stack_pointer - 2] =
          MAKE_RECORD(module, keys, values, r1->length + r2->length);
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
      for (uint32_t i = 0; i < l2->length; i++)
        list[l1->length + i] = l2->as_ptr[i];

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
      stack_push(module, MAKE_INTEGER(GET_INT(b) % GET_INT(a)));
      break;

    case TYPE_FLOAT: {
      float f = fmod(GET_FLOAT(b), GET_FLOAT(a));
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
  bool found = false;
  for (int i = 0; i < ptr->length; i++) {
    if (strcmp(ptr->as_record.keys[i], name) == 0) {
      module->stack->values[module->stack->stack_pointer - 1] =
          ptr->as_record.values[i];
      found = true;
      break;
    }
  }

  if (!found) {
    Value* none = malloc(3 * sizeof(Value));
    none[0] = kNull;
    none[1] = MAKE_STRING_NON_GC(module, "Optional");
    none[2] = MAKE_STRING_NON_GC(module, "None");

    module->stack->values[module->stack->stack_pointer - 1] = MAKE_LIST(module, none, 3);
  }

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
  char** keys = malloc(i1 * sizeof(char*));
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
    keys[idx] = GET_STRING(
        module->stack->values[module->stack->stack_pointer - i1 * 2 + i - 1]);
  }

  int sp = module->stack->stack_pointer - (i1 * 2);

  module->stack->values[sp] =
      MAKE_RECORD(module, keys, values, i1);

  module->stack->stack_pointer = sp + 1;

  INCREASE_IP(module);
  goto *jmp_table[op];
}

case_unknown:
  printf("Unknown opcode: %d\n", op);
  exit(1);
}