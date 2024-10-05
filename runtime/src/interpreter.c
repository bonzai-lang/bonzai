#include <gc.h>
#include <value.h>
#include <module.h>
#include <error.h>
#include <debug.h>
#include <compare.h>
#include <call.h>
#include <stdbool.h>
#include <threading.h>

#define INCREASE_IP_BY(mod, x) (mod->pc += ((x) * 5))
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
    &&case_return_event, &&case_make_mutable, UNKNOWN
  };

  goto *jmp_table[op];

  case_load_local: {
    int bp = module->base_pointer;
    Value value = module->stack->values[bp + i1];
    stack_push(module->stack, value);
    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_store_local: {
    int bp = module->base_pointer;
    module->stack->values[bp + i1] = stack_pop(module->stack);

    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_load_constant: {
    Value value = constants.values[i1];
    stack_push(module->stack, value);
    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_load_global: {
    Value value = module->stack->values[i1];
    stack_push(module->stack, value);
    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_store_global: {
    Value value = stack_pop(module->stack);
    module->stack->values[i1] = value;

    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_return: {
    Frame fr = pop_frame(module);
    Value ret = stack_pop(module->stack);

    module->stack->stack_pointer = fr.stack_pointer;
    module->base_pointer = fr.base_ptr;
    stack_push(module->stack, ret);

    module->pc = fr.instruction_pointer;

    if (does_return && callstack == module->callstack) 
      return ret;

    goto *jmp_table[op];
  }


  case_compare: {
    Value a = stack_pop(module->stack);
    Value b = stack_pop(module->stack);

    stack_push(module->stack, comparison_table[i1](b, a));
    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_update: {
    Value variable = stack_pop(module->stack);
    Value value = stack_pop(module->stack);

    ASSERT_TYPE("update", variable, TYPE_MUTABLE);

    GET_MUTABLE(variable) = value;

    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_make_list: {
    Value* list = GC_malloc(i1 * sizeof(Value));

    // Loop in reverse order to pop values in the correct order
    for (int i = i1 - 1; i >= 0; i--) {
      list[i] = stack_pop(module->stack);
    }

    Value value = MAKE_LIST(list, i1);
    stack_push(module->stack, value);
    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_list_get: {
    Value list = stack_pop(module->stack);
    int index = i1;

    ASSERT_TYPE("list_get", list, TYPE_LIST);

    Value* list_ptr = GET_LIST(list);
    stack_push(module->stack, list_ptr[index]);

    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_call: {
    Value callee = stack_pop(module->stack);

    ASSERT(IS_FUN(callee) || IS_PTR(callee), "Invalid callee type");
    
    interpreter_table[(callee & MASK_SIGNATURE) == SIGNATURE_FUNCTION](module, callee, i1);

    goto *jmp_table[op];
  }

  case_call_global: {}
  case_call_local: {}

  case_jump_if_false: {
    Value value = stack_pop(module->stack);
    if (GET_INT(value) == 0) {
      INCREASE_IP_BY(module, i1);
    } else {
      INCREASE_IP(module);
    }
    goto *jmp_table[op];
  }

  case_jump_rel: {
    INCREASE_IP_BY(module, i1);
    goto *jmp_table[op];
  }

  case_get_index: {
    Value index = stack_pop(module->stack);
    Value list = stack_pop(module->stack);

    ASSERT_TYPE("get_index", list, TYPE_LIST);
    ASSERT_TYPE("get_index", index, TYPE_INTEGER);

    Value* list_ptr = GET_LIST(list);
    stack_push(module->stack, list_ptr[GET_INT(index)]);

    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_special: {}

  case_halt: {
    module->is_terminated = true;

    for (int i = 0; i < module->event_count; i++) {
      pthread_join(module->events[i]->thread, NULL);
    }

    return kNull;
  }

  case_spawn: {
    Value event = stack_pop(module->stack);

    ASSERT_TYPE("spawn", event, TYPE_EVENT);

    HeapValue* ev = GET_PTR(event);
  
    stack_push(module->stack, MAKE_EVENT_FRAME(module->pc + 5, module->stack->stack_pointer, module->base_pointer, ev->as_event.ons_count, ev->as_event.ipc));

    module->base_pointer = module->stack->stack_pointer - 1;
    module->callstack++;
    module->pc = ev->as_event.ipc + 5;
    
    goto *jmp_table[op];
  }

  case_event_on: {
    int32_t new_pc = module->pc + 5;
    Value lambda = MAKE_FUNCTION(new_pc, i4);

    Value event_on = MAKE_EVENT_ON(i1, lambda);

    stack_push(module->stack, event_on);

    INCREASE_IP_BY(module, i3 + 1);
    goto *jmp_table[op];
  }

  case_send: {
    int event_id = i2;
    int argsc = i1;

    Value* args = GC_malloc(argsc * sizeof(Value));
    for (int i = 0; i < argsc; i++) {
      args[i] = stack_pop(module->stack);
    }

    Value event = stack_pop(module->stack);

    ASSERT_TYPE("send", event, TYPE_EVENT);

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
    stack_push(module->stack, native);
    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_make_event: {
    Value ev = MAKE_EVENT(i1, module->pc);
    stack_push(module->stack, ev);
    INCREASE_IP_BY(module, i2 + 1);
    goto *jmp_table[op];
  }

  case_return_event: {
    Frame fr = pop_frame(module);
    
    HeapValue* hp = GC_malloc(sizeof(HeapValue));
  
    hp->type = TYPE_EVENT;
    hp->as_event.ons_count = fr.ons_count;
    hp->as_event.ipc = fr.function_ipc;

    // Pop ons in reverse order
    for (int i = fr.ons_count - 1; i >= 0; i--) {
      Value v = stack_pop(module->stack);

      HeapValue* v_hp = GET_PTR(v);

      hp->as_event.ons[v_hp->as_event_on.id] = v_hp->as_event_on.func;
    }

    hp->as_event.actor = create_actor(hp->as_event, module);
    stack_push(module->stack, MAKE_PTR(hp));
    
    module->pc = fr.instruction_pointer;

    goto *jmp_table[op];
  }

  case_make_mutable: {
    Value x = stack_pop(module->stack);
    stack_push(module->stack, MAKE_MUTABLE(x));
    INCREASE_IP(module);
    goto *jmp_table[op];
  }

  case_unknown:
    printf("Unknown opcode: %d\n", op);
    exit(1);
}