#include <debug.h>
#include <error.h>
#include <module.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <threading.h>
#include <value.h>
#include <unistd.h>

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))

void mark_value(Module* mod, Value value) {
  if (!IS_PTR(value)) return;

  HeapValue* ptr = GET_PTR(value);

  if (ptr->is_marked) return;

  // printf("Marking object (%p): ", ptr);
  // debug_value(value);
  // printf("\n");

  ptr->is_marked = true;
  if (ptr->type == TYPE_LIST) {
    for (uint32_t i = 0; i < ptr->length; i++) {
      mark_value(mod, ptr->as_ptr[i]);
    }
  } else if (ptr->type == TYPE_MUTABLE) {
    mark_value(mod, *(ptr->as_ptr));
  } else if (ptr->type == TYPE_RECORD) {
    for (uint32_t i = 0; i < ptr->length; i++) {
      mark_value(mod, ptr->as_record.values[i]);
    }
  } else if (ptr->type == TYPE_API && ptr->mark) {
    ptr->mark(mod, ptr);
  }
}

void free_value(struct Module* mod, HeapValue* unreached) {
  if (unreached->type == TYPE_LIST || unreached->type == TYPE_MUTABLE) {
    free(unreached->as_ptr);
  } else if (unreached->type == TYPE_STRING) {
    if (!unreached->is_constant) {
      free(unreached->as_string);
    }
  } else if (unreached->type == TYPE_API && unreached->destructor) {
    unreached->destructor(mod, unreached);
  }
  free(unreached);
}

bool are_all_threads_stopped(struct Module* vm) {
  for (int i = 0; i < vm->gc->stacks.stack_count; i++) {
    Stack* stack = vm->gc->stacks.stacks[i];
    if (!stack) continue;

    if (atomic_load(&stack->is_halted)) {
      continue;
    }

    if (!atomic_load(&stack->is_stopped)) {
      return false;
    }
  }
  return true;
}

bool is_at_least_one_programs_running(struct Module* vm) {
  // printf("sc: %d\n", vm->gc->stacks.stack_count);
  for (int i = 0; i < vm->gc->stacks.stack_count; i++) {
    Stack* stack = vm->gc->stacks.stacks[i];
    if (!stack) continue;

    if (!atomic_load(&stack->is_halted)) {
      return true;
    }
  }
  return false;
}

void gc_free(struct Module* mod, Value value) {
  if (!IS_PTR(value)) return;

  HeapValue* ptr = GET_PTR(value);

  if (ptr->is_marked) return;

  free_value(mod, ptr);
}

void mark_all(struct Module* vm) {
  for (int i = 0; i < vm->argc; i++) {
    mark_value(vm, vm->argv[i]);
  }

  for (int i = 0; i < vm->gc->stacks.stack_count; i++) {
    Stack* stack = vm->gc->stacks.stacks[i];

    // printf("Marking stack %d (sp: %d)\n", i, stack->stack_pointer);

    // DEBUG_STACK_FROM(stack, 0);
    if (!stack || !stack->values) continue;
    pthread_mutex_lock(&stack->mutex);

    for (int j = 0; j <= stack->stack_capacity; j++) {
      if (j <= stack->stack_pointer) {
        Value value = stack->values[j];
        if (value == kNull) continue;
        mark_value(vm, value);

        continue;
      }

      stack->values[j] = kNull;
    }

    pthread_mutex_unlock(&stack->mutex);
  }
}

static void sweep(struct Module* vm) {
  HeapValue* previous = NULL;
  HeapValue* object = vm->gc->first_object;
  while (object != NULL) {
    if (object->is_marked == true) {
      object->is_marked = false;
      previous = object;
      object = object->next;
    } else {
      HeapValue* unreached = object;
      object = object->next;
      if (previous != NULL) {
        previous->next = object;
      } else {
        vm->gc->first_object = object;
      }

      // printf("Unreachable object (%p): ", unreached);
      // debug_value(MAKE_PTR(unreached));
      // printf("\n");

      free_value(vm, unreached);
      vm->gc->num_objects--;
    }
  }
}

void gc(struct Module* vm) {
  // printf("Requested from %p\n", vm->stack->values);
  gc_t* gc_ = vm->gc;
  // int numObjects = gc_->num_objects;

  mark_all(vm);
  sweep(vm);

  pthread_mutex_lock(&gc_->gc_mutex);
  gc_->max_objects =
      gc_->num_objects < INIT_OBJECTS ? INIT_OBJECTS : gc_->num_objects * 2;
  pthread_mutex_unlock(&gc_->gc_mutex);

  // printf("Collected %d objects, %d max objects (sp: %d)\n",
  //        numObjects - gc_->num_objects, gc_->max_objects,
  //        vm->stack->stack_pointer);
  atomic_store(&gc_is_requested, false);
}

void force_sweep(struct Module* vm) {
  gc_t* gc = vm->gc;
  HeapValue* object = gc->first_object;
  while (object) {
    HeapValue* next = object->next;
    free_value(vm, object);
    object = next;
  }
  gc->first_object = NULL;
  gc->num_objects = 0;
}

void request_gc(struct Module* vm) {
  if (vm->gc->gc_enabled) {
    atomic_store(&gc_is_requested, true);
  }
}

void* gc_thread(void* data) {
  struct Module* vm = (struct Module*)data;
  while (is_at_least_one_programs_running(vm)) {
    if (atomic_load(&gc_is_requested)
        && are_all_threads_stopped(vm)
        && vm->gc->gc_enabled
      ) {
      gc(vm);
    }
  }

  // printf("Stopping GC thread\n");

  return NULL;
}

pthread_t start_gc(struct Module* vm) {
  pthread_t gc_thread_id;
  pthread_create(&gc_thread_id, NULL, gc_thread, vm);

  return gc_thread_id;
}

HeapValue* allocate(struct Module* mod, ValueType type) {
  gc_t* gc_ = mod->gc;
  int numObjects = gc_->num_objects;
  int maxObjects = gc_->max_objects;

  if (numObjects > maxObjects) {
    if (gc_->gc_enabled) {
      atomic_store(&gc_is_requested, true);
    } else {
      gc_->max_objects += 2;
    }
  }

  HeapValue* v = malloc(sizeof(HeapValue));

  v->type = type;
  v->is_marked = false;
  v->next = gc_->first_object;
  v->is_constant = false;
  pthread_mutex_init(&v->mutex, NULL);
  pthread_cond_init(&v->cond, NULL);

  pthread_mutex_lock(&gc_->gc_mutex);
  gc_->first_object = v;
  gc_->num_objects++;
  pthread_mutex_unlock(&gc_->gc_mutex);

  return v;
}

Value MAKE_STRING_MULTIPLE(struct Module* mod, ...) {
  va_list args;
  va_start(args, mod);

  int len = 0;

  for (char* str = va_arg(args, char*); str != NULL;
       str = va_arg(args, char*)) {
    len += strlen(str);
  }

  va_end(args);

  char* str = malloc(len + 1);

  va_start(args, mod);

  for (char* s = va_arg(args, char*); s != NULL; s = va_arg(args, char*)) {
    strcat(str, s);
  }

  va_end(args);

  HeapValue* v = allocate(mod, TYPE_STRING);
  v->as_string = str;
  v->length = len;
  v->is_constant = false;

  return MAKE_PTR(v);
}

Value MAKE_STRING(struct Module* mod, char* x) {
  HeapValue* v = allocate(mod, TYPE_STRING);
  v->as_string = x;
  v->length = strlen(x);
  v->is_constant = false;

  return MAKE_PTR(v);
}

Value MAKE_RECORD(struct Module* mod, char** keys, Value* values,
                  int size) {
  HeapValue* v = allocate(mod, TYPE_RECORD);
  v->as_record.keys = keys;
  v->as_record.values = values;
  v->length = size;

  return MAKE_PTR(v);
}

Value MAKE_STRING_NON_GC(struct Module* mod, char* x) {
  HeapValue* v = allocate(mod, TYPE_STRING);
  v->as_string = x;
  v->length = strlen(x);
  v->is_constant = true;
  v->is_marked = true;

  return MAKE_PTR(v);
}

Value MAKE_NATIVE(struct Module* mod, char* name, int addr) {
  HeapValue* v = allocate(mod, TYPE_NATIVE);
  v->as_native.name = name;
  v->as_native.addr = addr;

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

Value MAKE_FUNCTION(struct Module* mod, int32_t ip, uint16_t local_space) {
  HeapValue* v = allocate(mod, TYPE_FUNCTION);

  v->as_func.ip = ip;
  v->as_func.local_space = local_space;

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
    case TYPE_RECORD:
      return "record";
    case TYPE_NATIVE:
      return "native";
  }
}

Stack* stack_new() {
  Stack* stack = malloc(sizeof(Stack));

  stack->stack_pointer = GLOBALS_SIZE;
  stack->stack_capacity = MAX_STACK_SIZE;
  stack->values = calloc(stack->stack_capacity, sizeof(Value));

  atomic_store(&stack->is_stopped, false);
  atomic_store(&stack->is_halted, false);

  pthread_mutex_init(&stack->mutex, NULL);

  stack->thread = pthread_self();

  return stack;
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
      printf("%d", (int)GET_INT(v));
      break;

    case TYPE_FLOAT:
      printf("%f", GET_FLOAT(v));
      break;

    case TYPE_STRING:
      printf("%s", GET_STRING(v));
      break;

    case TYPE_LIST: {
      if (IS_EMPTY_LIST(v)) {
        printf("[]");
        break;
      }

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

    case TYPE_NATIVE: {
      printf("Native(%s)", GET_NATIVE(v).name);
      break;
    }

    case TYPE_RECORD: {
      if (IS_EMPTY_RECORD(v)) {
        printf("{}");
        break;
      }

      HeapValue* record = GET_PTR(v);
      printf("{ ");
      for (int i = 0; i < record->length; i++) {
        printf("%s: ", record->as_record.keys[i]);
        debug_value(record->as_record.values[i]);
        if (i < record->length - 1) {
          printf(", ");
        }
      }
      printf(" }");
      break;
    }

    case TYPE_API: {
      printf("API(%p)", GET_PTR(v)->as_any);
      break;
    }

    case TYPE_UNKNOWN:
    default:
      printf("Unknown");
      break;
  }
}

Value clone_value(Module* mod, Value value) {
  switch (get_type(value)) {
    case TYPE_INTEGER:
      return MAKE_INTEGER(GET_INT(value));
    case TYPE_FLOAT:
      return MAKE_FLOAT(GET_FLOAT(value));
    case TYPE_STRING: {
      HeapValue* str = GET_PTR(value);
      char* new_str = malloc(str->length + 1);
      strcpy(new_str, str->as_string);
      return MAKE_STRING(mod, new_str);
    }

    case TYPE_RECORD: {
      if (IS_EMPTY_RECORD(value)) {
        return EMPTY_RECORD;
      }

      HeapValue* record = GET_PTR(value);
      char** keys = malloc(record->length * sizeof(char*));
      Value* values = malloc(record->length * sizeof(Value));
      for (uint32_t i = 0; i < record->length; i++) {
        keys[i] = record->as_record.keys[i];
        values[i] = clone_value(mod, record->as_record.values[i]);
      }
      return MAKE_RECORD(mod, keys, values, record->length);
    }

    case TYPE_LIST: {
      if (IS_EMPTY_LIST(value)) {
        return EMPTY_LIST;
      }

      HeapValue* list = GET_PTR(value);
      Value* new_list = malloc(list->length * sizeof(Value));
      for (uint32_t i = 0; i < list->length; i++) {
        new_list[i] = clone_value(mod, list->as_ptr[i]);
      }
      return MAKE_LIST(mod, new_list, list->length);
    }
    case TYPE_MUTABLE:
      return MAKE_MUTABLE(mod, clone_value(mod, GET_MUTABLE(value)));
    case TYPE_SPECIAL:
      return MAKE_SPECIAL();
    case TYPE_FUNCTION:
      return MAKE_FUNCTION(mod, GET_PTR(value)->as_func.ip,
                           GET_PTR(value)->as_func.local_space);
    case TYPE_NATIVE:
      return MAKE_NATIVE(mod, GET_NATIVE(value).name, GET_NATIVE(value).addr);
    case TYPE_UNKNOWN:
    default:
      return value;
  }
}

void rearrange_stacks(Module* mod) {
  #define st mod->gc->stacks
  int new_size = 0;

  for (int i = 0; i < st.stack_count; i++) {
    if (st.stacks[i] == NULL) continue;
    new_size++;
  }

  Stack** new_stacks = calloc(new_size, sizeof(Stack*));
  if (new_stacks == NULL) {
    perror("Failed to allocate memory for new stacks");
    exit(EXIT_FAILURE);
  }

  int j = 0;
  for (int i = 0; i < st.stack_count; i++) {
    if (st.stacks[i] == NULL) continue;
    new_stacks[j++] = st.stacks[i];
  }

  // Free the old stacks
  free(st.stacks);
  st.stacks = new_stacks;
  st.stack_count = new_size;
}
