#include <value.h>
#include <module.h>
#include <error.h>
#include <debug.h>
#include <compare.h>
#include <call.h>
#include <stdbool.h>
#include <threading.h>
#include <math.h>

// Increase IP might also check if gc is lock
#define INCREASE_IP_BY(mod, x) \
  if (mod->latest_try_catch_count > 0) { \
    mod->latest_try_catch[1][mod->latest_try_catch_count - 1] += 1; \
  } \
  (mod->pc += ((x) * 5));
#define INCREASE_IP(mod) INCREASE_IP_BY(mod, 1)

Value run_interpreter(Module *module, int32_t ipc, bool does_return, int callstack) {
  Constants constants = module->constants;
  int32_t* bytecode = module->instrs;
  module->pc = ipc;

  #define op bytecode[module->pc]
  #define i1 bytecode[module->pc + 1]
  #define i2 bytecode[module->pc + 2]
  #define i3 bytecode[module->pc + 3]
  #define i4 bytecode[module->pc + 4]

  #define UNKNOWN &&case_unknown

  void* jmp_table[] = { 
    &&case_load_local, &&case_store_local, &&case_load_constant, &&case_load_global,
    &&case_store_global, &&case_return, &&case_compare, &&case_update,
    &&case_make_list, &&case_list_get, &&case_call, &&case_call_global,
    &&case_call_local, &&case_jump_if_false, &&case_jump_rel, &&case_get_index,
    &&case_special, &&case_halt, &&case_spawn, &&case_event_on, 
    &&case_send, &&case_make_function_and_store, &&case_load_native, &&case_make_event,
    &&case_return_event, &&case_make_mutable, &&case_loc, &&case_add, &&case_sub,
    &&case_mul, &&case_div, &&case_mod, &&case_call_native, &&case_try_catch, 
    &&case_get_value, UNKNOWN
  };

  goto *jmp_table[op];

  case_load_local: {
    int bp = module->base_pointer;
    Value value = module->stack->values[bp + i1];
    stack_push(module, value);
    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_store_local: {
    int bp = module->base_pointer;
    module->stack->values[bp + i1] = stack_pop(module);

    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_load_constant: {
    Value value = constants.values[i1];
    stack_push(module, value);
    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_load_global: {
    Value value = module->stack->values[i1];
    stack_push(module, value);
    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_store_global: {
    Value value = stack_pop(module);
    module->stack->values[i1] = value;

    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_return: {
    Frame fr = pop_frame(module);
    Value ret = stack_pop(module);

    module->stack->stack_pointer = fr.stack_pointer;
    module->base_pointer = fr.base_ptr;
    stack_push(module, ret);

    module->pc = fr.instruction_pointer;

    if (does_return && callstack == module->callstack) {
      return ret;
    }

    goto *jmp_table[op];
  }


  case_compare: {
    Value a = stack_pop(module);
    Value b = stack_pop(module);

    stack_push(module, comparison_table[i1](module, b, a));
    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_update: {
    Value variable = stack_pop(module);
    Value value = stack_pop(module);

    ASSERT_TYPE(module, "update", variable, TYPE_MUTABLE);

    GET_MUTABLE(variable) = value;

    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_make_list: {
    Value* list = malloc(i1 * sizeof(Value));
    module->gc->gc_enabled = false;
    
    // Loop in reverse order to pop values in the correct order
    for (int i = i1 - 1; i >= 0; i--) {
      list[i] = stack_pop(module);
    }

    Value value = MAKE_LIST(module, list, i1);
    stack_push(module, value);

    module->gc->gc_enabled = true;
    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_list_get: {
    Value list = stack_pop(module);
    uint32_t index = i1;

    ASSERT_TYPE(module, "list_get", list, TYPE_LIST);

    Value* list_ptr = GET_LIST(list);

    ASSERT_FMT(module, index >= 0 && index < GET_PTR(list)->length, "Index out of bounds, received %d", index);

    stack_push(module, list_ptr[index]);

    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_call: {
    safe_point(module);

    Value callee = stack_pop(module);

    ASSERT(module, IS_FUN(callee) || IS_PTR(callee), "Invalid callee type");
    
    interpreter_table[(callee & MASK_SIGNATURE) == SIGNATURE_FUNCTION](module, callee, i1);

    goto *jmp_table[op];
  }

  case_call_global: {
    safe_point(module);

    Value callee = module->stack->values[i1];

    ASSERT(module, IS_FUN(callee) || IS_PTR(callee), "Invalid callee type");

    interpreter_table[(callee & MASK_SIGNATURE) == SIGNATURE_FUNCTION](module, callee, i2);

    goto *jmp_table[op];
  }

  case_call_local: {
    safe_point(module);

    Value callee = module->stack->values[module->base_pointer + i1];

    ASSERT(module, IS_FUN(callee) || IS_PTR(callee), "Invalid callee type");

    interpreter_table[(callee & MASK_SIGNATURE) == SIGNATURE_FUNCTION](module, callee, i2);

    goto *jmp_table[op];
  }

  case_jump_if_false: {
    safe_point(module);

    Value value = stack_pop(module);
    if (GET_INT(value) == 0) {
      INCREASE_IP_BY(module, i1);
    } else {
      INCREASE_IP(module);
    }
    goto *jmp_table[op];
  }

  case_jump_rel: {
    safe_point(module);

    INCREASE_IP_BY(module, i1);
    goto *jmp_table[op];
  }

  case_get_index: {
    Value index = stack_pop(module);
    Value list = stack_pop(module);

    ASSERT_TYPE(module, "get_index", list, TYPE_LIST);
    ASSERT_TYPE(module, "get_index", index, TYPE_INTEGER);

    Value* list_ptr = GET_LIST(list);
    uint32_t idx = GET_INT(index);
    
    ASSERT_FMT(module, idx >= 0 && idx < GET_PTR(list)->length, "Index out of bounds, received %d", idx);

    stack_push(module, list_ptr[idx]);

    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_special: {
    stack_push(module, kNull);
    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_halt: {
    module->is_terminated = true;

    for (int i = 0; i < module->event_count; i++) {
      pthread_join(module->events[i]->thread, NULL);
    }

    return kNull;
  }

  case_spawn: {
    Value event = stack_pop(module);

    ASSERT_TYPE(module, "spawn", event, TYPE_EVENT);

    HeapValue* ev = GET_PTR(event);

    int old_sp = module->stack->stack_pointer;

    stack_push(module, event);
    stack_push(module, MAKE_INTEGER(module->pc + 5));
    stack_push(module, MAKE_INTEGER(old_sp));
    stack_push(module, MAKE_INTEGER(module->base_pointer));

    module->base_pointer = old_sp;
    module->callstack++;
    module->pc = ev->as_event.ipc + 5;
    
    goto *jmp_table[op];
  }

  case_event_on: {
    int32_t new_pc = module->pc + 5;
    Value lambda = MAKE_FUNCTION(new_pc, i4);

    Value event_on = MAKE_EVENT_ON(module, i1, lambda);

    stack_push(module, event_on);

    INCREASE_IP_BY(module, i3 + 1);
    goto *jmp_table[op];
  }

  case_send: {
    int event_id = i2;
    int argsc = i1;

    Value* args = malloc(argsc * sizeof(Value));
    for (int i = 0; i < argsc; i++) {
      args[i] = stack_pop(module);
    }

    Value event = stack_pop(module);

    ASSERT_TYPE(module, "send", event, TYPE_EVENT);

    HeapValue* hp = GET_PTR(event);

    send_message(hp->as_event.actor, event_id, args, argsc);

    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_make_function_and_store: {
    int32_t new_pc = module->pc + 5;
    Value lambda = MAKE_FUNCTION(new_pc, i3);

    module->stack->values[i1] = lambda;

    INCREASE_IP_BY(module, i2 + 1);
    goto *jmp_table[op];
  }

  case_load_native: {
    Value native = module->constants.values[i1];

    ASSERT_TYPE(module, "load_native", native, TYPE_STRING);
    
    char* name = GET_STRING(native);
    
    stack_push(module, MAKE_NATIVE(module, name, i1));
    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_make_event: {
    Value ev = MAKE_EVENT(module, i1, module->pc);
    stack_push(module, ev);
    INCREASE_IP_BY(module, i2 + 1);
    goto *jmp_table[op];
  }

  case_return_event: {
    Frame fr = pop_event_frame(module);
    
    HeapValue* hp = allocate(module, TYPE_EVENT);
  
    hp->as_event.ons_count = fr.ons_count;
    hp->as_event.ipc = fr.function_ipc;

    // Pop ons in reverse order
    for (int i = fr.ons_count - 1; i >= 0; i--) {
      Value v = stack_pop(module);

      HeapValue* v_hp = GET_PTR(v);

      hp->as_event.ons[v_hp->as_event_on.id] = v_hp->as_event_on.func;
    }

    hp->as_event.actor = create_actor(hp->as_event, module);
    
    module->stack->stack_pointer = fr.stack_pointer;
    module->base_pointer = fr.base_ptr;
    
    stack_push(module, MAKE_PTR(hp));
    
    module->pc = fr.instruction_pointer;

    goto *jmp_table[op];
  }

  case_make_mutable: {
    module->gc->gc_enabled = false;
    Value x = stack_pop(module);
    stack_push(module, MAKE_MUTABLE(module, x));
    module->gc->gc_enabled = true;
    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_loc: {
    module->latest_position[0] = i1;
    module->latest_position[1] = i2;
    module->file = GET_STRING(constants.values[i3]);

    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_add: {
    Value b = stack_pop(module);
    Value a = stack_pop(module);

    ValueType ty = get_type(a);


    ASSERT_TYPE(module, "add", b, ty);

    switch (ty) {
      case TYPE_INTEGER: 
        stack_push(module, MAKE_INTEGER(a + b));
        break;
      
      case TYPE_FLOAT: {
        double f = GET_FLOAT(a) + GET_FLOAT(b);
        stack_push(module, MAKE_FLOAT(f));
        break;
      }

      case TYPE_STRING: {
        char* s1 = GET_STRING(a);
        char* s2 = GET_STRING(b);

        stack_push(module, MAKE_STRING_MULTIPLE(module, s1, s2));
        break;
      }

      case TYPE_LIST: {
        HeapValue* l1 = GET_PTR(a);
        HeapValue* l2 = GET_PTR(b);
        Value* list = malloc((l1->length + l2->length) * sizeof(Value));
        for (uint32_t i = 0; i < l1->length; i++) list[i] = l1->as_ptr[i];
        for (uint32_t i = 0; i < l2->length; i++) list[l1->length + i] = l2->as_ptr[i];
        stack_push(module, MAKE_LIST(module, list, l1->length + l2->length));
        break;
      }

      default: 
        THROW(module, "Unsupported type for +");
    }

    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_sub: {
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
    Value native = module->constants.values[i1];
    Value nat = MAKE_NATIVE(module, GET_STRING(native), i1);
    
    op_native_call(module, nat, i2);
    goto *jmp_table[op];
  }

  case_try_catch: {
    module->latest_try_catch[0][module->latest_try_catch_count] = i1 + 1;
    module->latest_try_catch[1][module->latest_try_catch_count] = 0;
    module->latest_try_catch_count++;

    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_get_value: {
    Value value = stack_pop(module);

    ASSERT_TYPE(module, "get_value", value, TYPE_MUTABLE);

    stack_push(module, GET_MUTABLE(value));

    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_unknown:
    printf("Unknown opcode: %d\n", op);
    exit(1);
}