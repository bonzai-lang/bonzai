#include <error.h>
#include <module.h>
#include <stdio.h>
#include <string.h>
#include <value.h>
#include <math.h>

void mark_value(Value value) {
  if (!IS_PTR(value)) return;

  HeapValue* ptr = GET_PTR(value);

  if (ptr->is_marked || ptr->is_constant) return;

  ptr->is_marked = true;
  if (ptr->type == TYPE_LIST) {
    for (uint32_t i = 0; i < ptr->length; i++) {
      mark_value(ptr->as_ptr[i]);
    }
  } else if (ptr->type == TYPE_MUTABLE) {
    mark_value(*(ptr->as_ptr));
  } else if (ptr->type == TYPE_EVENT) {
    for (int i = 0; i < ptr->as_event.ons_count; i++) {
      mark_value(ptr->as_event.ons[i]);
    }
  } else if (ptr->type == TYPE_EVENT_ON) {
    mark_value(ptr->as_event_on.func);
  }
}

void mark_all(struct Module* vm) {
  for (int i = 0; i < vm->stack->stack_pointer; i++) {
    // printf("Marking stack value %d\n", i);
    mark_value(vm->stack->values[i]);
  }
}

void sweep(struct Module* vm) {
  HeapValue** object = &vm->first_object;
  while (*object) {
    if (!(*object)->is_marked && !(*object)->is_constant) {
      /* This object wasn't reached, so remove it from the list
         and free it. */
      HeapValue* unreached = *object;

      switch (unreached->type) {
        case TYPE_LIST: case TYPE_MUTABLE: {
          free(unreached->as_ptr);
          break;
        }

        case TYPE_STRING: {
          if (!unreached->is_constant) {
            free(unreached->as_string);
          }
          break;
        }

        default: break;
      }

      free(unreached);

      *object = unreached->next;
      vm->num_objects--;
    } else {
      /* This object was reached, so unmark it (for the next GC)
         and move on to the next. */
      (*object)->is_marked = 0;
      object = &(*object)->next;
    }
  }
}

void gc(struct Module* vm) {
  // int numObjects = vm->num_objects;

  mark_all(vm);
  sweep(vm);

  vm->max_objects = vm->num_objects < INIT_OBJECTS ? INIT_OBJECTS : vm->num_objects * 2;
  
  // printf("Collected %d objects, %d remaining.\n", numObjects - vm->num_objects,
  //        vm->num_objects);
}

void force_sweep(struct Module* vm) {
  HeapValue* object = vm->first_object;
  while (object) {
    HeapValue* next = object->next;
    switch (object->type) {
      case TYPE_LIST: case TYPE_MUTABLE: {
        free(object->as_ptr);
        break;
      }

      case TYPE_STRING: {
        if (!object->is_constant) {
          free(object->as_string);
        }
        break;
      }

      default: break;
    }
    free(object);
    object = next;
  }
  vm->first_object = NULL;
  vm->num_objects = 0;
}

HeapValue* allocate(struct Module* mod, ValueType type) {
  if (mod->num_objects == mod->max_objects) {
    if (mod->gc_enabled) {
      gc(mod);
    } else {
      mod->max_objects += 2;
    }
  }

  HeapValue* v = malloc(sizeof(HeapValue));

  v->type = type;
  v->is_marked = false;
  v->next = mod->first_object;
  v->is_constant = false;

  mod->first_object = v;
  mod->num_objects++;

  // printf("Allocated %d objects, %d remaining.\n", mod->num_objects, mod->max_objects - mod->num_objects);

  return v;
}

Value MAKE_STRING(struct Module* mod, char* x) {
  HeapValue* v = allocate(mod, TYPE_STRING);
  v->as_string = x;
  v->length = strlen(x);

  return MAKE_PTR(v);
}

Value MAKE_STRING_NON_GC(struct Module* mod, char* x) {
  HeapValue* v = allocate(mod, TYPE_STRING);
  v->as_string = x;
  v->length = strlen(x);
  v->is_constant = true;

  return MAKE_PTR(v);
}

Value MAKE_LIST(struct Module* mod, Value* x, uint32_t len) {
  HeapValue* v = allocate(mod, TYPE_LIST);
  v->as_ptr = x;
  v->length = len;

  return MAKE_PTR(v);
}

Value MAKE_MUTABLE(struct Module* mod, Value x) {
  HeapValue* v = allocate(mod, TYPE_MUTABLE);

  v->as_ptr = malloc(sizeof(Value));
  *v->as_ptr = x;
  v->length = 1;

  return MAKE_PTR(v);
}

Value MAKE_EVENT(struct Module* mod, uint32_t ons_count, uint32_t ipc) {
  HeapValue* v = allocate(mod, TYPE_EVENT);
  v->as_event.ons_count = ons_count;
  v->as_event.ipc = ipc;

  return MAKE_PTR(v);
}

Value MAKE_FRAME(struct Module* mod, int32_t ip, int32_t sp, int32_t bp) {
  HeapValue* v = allocate(mod, TYPE_FRAME);

  v->as_frame.instruction_pointer = ip;
  v->as_frame.stack_pointer = sp;
  v->as_frame.base_ptr = bp;
  v->as_frame.ons_count = 0;

  return MAKE_PTR(v);
}

Value MAKE_EVENT_FRAME(struct Module* mod, int32_t ip, int32_t sp, int32_t bp,
                       int32_t ons_count, int function_ipc) {
  HeapValue* v = allocate(mod, TYPE_FRAME);

  v->as_frame.instruction_pointer = ip;
  v->as_frame.stack_pointer = sp;
  v->as_frame.base_ptr = bp;
  v->as_frame.ons_count = ons_count;
  v->as_frame.function_ipc = function_ipc;

  return MAKE_PTR(v);
}

Value MAKE_EVENT_ON(struct Module* mod, int id, Value func) {
  HeapValue* v = allocate(mod, TYPE_EVENT_ON);

  v->as_event_on.id = id;
  v->as_event_on.func = func;

  return MAKE_PTR(v);
}

ValueType get_type(Value value) {
  uint64_t signature = value & MASK_SIGNATURE;
  if ((~value & MASK_EXPONENT) != 0) return TYPE_FLOAT;

  // Check for encoded pointer
  if (signature == SIGNATURE_POINTER) {
    HeapValue* ptr = GET_PTR(value);
    return ptr->type;
  }

  // Short encoded types
  switch (signature) {
    case SIGNATURE_NAN:
      return TYPE_UNKNOWN;
    case SIGNATURE_SPECIAL:
      return TYPE_SPECIAL;
    case SIGNATURE_INTEGER:
      return TYPE_INTEGER;
    case SIGNATURE_FUNCTION:
      return TYPE_FUNCTION;
    case SIGNATURE_FUNCENV:
      return TYPE_FUNCENV;
  }

  return TYPE_UNKNOWN;
}

char* type_of(Value value) {
  switch (get_type(value)) {
    case TYPE_INTEGER:
      return "integer";
    case TYPE_FUNCTION:
      return "function";
    case TYPE_FUNCENV:
      return "function_env";
    case TYPE_FLOAT:
      return "float";
    case TYPE_STRING:
      return "string";
    case TYPE_LIST:
      return "list";
    case TYPE_SPECIAL:
      return "special";
    case TYPE_MUTABLE:
      return "mutable";
    case TYPE_UNKNOWN:
      return "unknown";
    case TYPE_API:
      return "api";
    case TYPE_EVENT:
      return "event";
    case TYPE_FRAME:
      return "frame";
    case TYPE_EVENT_ON:
      return "event_on";
  }
}

Stack* stack_new() {
  Stack* stack = malloc(sizeof(Stack));
  stack->stack_pointer = GLOBALS_SIZE;
  stack->stack_capacity = MAX_STACK_SIZE;
  stack->values = malloc(stack->stack_capacity * sizeof(Value));
  return stack;
}

void stack_push(Module* mod, Value value) {
  Stack* stack = mod->stack;
  if (stack->stack_pointer >= stack->stack_capacity) {
    stack->stack_capacity *= 2;
    stack->values = realloc(stack->values, stack->stack_capacity * sizeof(Value));

    if (!stack->values) {
      THROW(mod, "Failed to allocate memory for stack");
    }
  }

  stack->values[stack->stack_pointer++] = value;
}

Value stack_pop(Module* mod) { 
  Stack* stack = mod->stack;
  if (stack->stack_pointer <= 0) {
    THROW(mod, "Stack underflow");
  }
  return stack->values[--stack->stack_pointer];
}

void enqueue(MessageQueue* queue, Message* msg) {
  pthread_mutex_lock(&queue->mutex);
  if (queue->tail) {
    queue->tail->next = msg;
  } else {
    queue->head = msg;
  }
  queue->tail = msg;
  pthread_cond_signal(&queue->cond);
  pthread_mutex_unlock(&queue->mutex);
}

Message* dequeue(MessageQueue* queue) {
  pthread_mutex_lock(&queue->mutex);
  while (!queue->head) {
    pthread_cond_wait(&queue->cond, &queue->mutex);
  }
  Message* msg = queue->head;
  queue->head = msg->next;
  if (!queue->head) {
    queue->tail = NULL;
  }
  pthread_mutex_unlock(&queue->mutex);
  return msg;
}

MessageQueue* create_message_queue() {
  MessageQueue* queue = malloc(sizeof(MessageQueue));
  queue->head = NULL;
  queue->tail = NULL;
  pthread_mutex_init(&queue->mutex, NULL);
  pthread_cond_init(&queue->cond, NULL);
  return queue;
}

void print_message_queue(MessageQueue* queue) {
  Message* msg = queue->head;
  while (msg) {
    printf("%d ", msg->name);
    msg = msg->next;
  }
  printf("\n");
}

int value_eq(Module* mod, Value a, Value b) {
  ValueType ty = get_type(a);
  ASSERT_TYPE(mod, "==", b, ty);

  switch (ty) {
    case TYPE_INTEGER:
      return GET_INT(a) == GET_INT(b);
    case TYPE_FLOAT:
      return GET_FLOAT(a) == GET_FLOAT(b);
    case TYPE_STRING:
      return strcmp(GET_STRING(a), GET_STRING(b)) == 0;
    case TYPE_LIST: {
      HeapValue* list_a = GET_PTR(a);
      HeapValue* list_b = GET_PTR(b);
      if (list_a->length != list_b->length) {
        return 0;
      }
      for (uint32_t i = 0; i < list_a->length; i++) {
        if (!value_eq(mod, list_a->as_ptr[i], list_b->as_ptr[i])) {
          return 0;
        }
      }
      return 1;
    }
    case TYPE_MUTABLE:
      return value_eq(mod, GET_MUTABLE(a), GET_MUTABLE(b));
    case TYPE_SPECIAL:
      return 1;
    case TYPE_FUNCTION:
      return 1;
    case TYPE_FUNCENV:
      return 1;
    case TYPE_UNKNOWN:
    default:
      return 0;
  }
}

void debug_value(Value v) {
  switch (get_type(v)) {
    case TYPE_INTEGER:
      printf("%d", (int) GET_INT(v));
      break;
    
    case TYPE_FLOAT:
      printf("%f", GET_FLOAT(v));
      break;

    case TYPE_STRING:
      printf("%s", GET_STRING(v));
      break;
    
    case TYPE_LIST: {
      HeapValue* list = GET_PTR(v);
      printf("[");
      for (uint32_t i = 0; i < list->length; i++) {
        debug_value(list->as_ptr[i]);
        if (i < list->length - 1) {
          printf(", ");
        }
      }
      printf("]");
      break;
    }

    case TYPE_MUTABLE:
      printf("Mutable(");
      debug_value(GET_MUTABLE(v));
      printf(")");
      break;

    case TYPE_SPECIAL:
      printf("Special");
      break;
    
    case TYPE_FUNCTION:
      printf("Function");
      break;
    
    case TYPE_FUNCENV:
      printf("FunctionEnv");
      break;
    
    case TYPE_UNKNOWN:
    default:
      printf("Unknown");
      break;
  }
}