#include <debug.h>
#include <error.h>
#include <execinfo.h>
#include <module.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <threading.h>
#include <unistd.h>
#include <value.h>
#include <hashtable.h>

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))

void mark_all(struct Module* vm);

static int young_gc_count = 0;

void mark_record(struct Module* mod, struct HashTable ht, bool should_mark) {
  void (*mark_fn)(struct Module*, Value) = should_mark ? mark_value : unmark_value;
  for (uint32_t i = 0; i < HASHTABLE_SIZE; i++) {
    struct HashNode* node = ht.nodes[i];

    if (!node) continue;

    while (node != NULL) {
      mark_fn(mod, node->value);
      node = node->next;
    }
  }
}

void mark_value(struct Module* mod, Value value) {
  if (!IS_PTR(value)) return;

  HeapValue* ptr = GET_PTR(value);

  if (!ptr) return;

  if (ptr->is_marked) {
    return;
  }

  ptr->is_marked = true;

  switch (ptr->type) {
    case TYPE_LIST: {
      for (uint32_t i = 0; i < ptr->length; i++) {
        mark_value(mod, ptr->as_ptr[i]);
      }
      break;
    }
    case TYPE_THREAD: {
      mark_value(mod, ptr->as_event.function);
      break;
    }
    case TYPE_MUTABLE: {
      mark_value(mod, *(ptr->as_ptr));
      break;
    }
    case TYPE_RECORD: {
      mark_record(mod, ptr->as_record, true);
      break;
    }
    case TYPE_API: {
      if (ptr->mark) {
        ptr->mark(mod, ptr, false);
      }
      break;
    }
    default:
      break;
  }
}

void unmark_value(Module* mod, Value value) {
  if (!IS_PTR(value)) return;

  HeapValue* ptr = GET_PTR(value);

  if (!ptr) {
    return;
  }

  if (!ptr->is_marked) {
    return;
  }

  ptr->is_marked = false;

  switch (ptr->type) {
    case TYPE_LIST:
      for (uint32_t i = 0; i < ptr->length; i++) {
        unmark_value(mod, ptr->as_ptr[i]);
      }
      break;
    case TYPE_MUTABLE:
      unmark_value(mod, *(ptr->as_ptr));
      break;
    case TYPE_RECORD:
      mark_record(mod, ptr->as_record, false);
      break;
    case TYPE_THREAD: {
      unmark_value(mod, ptr->as_event.function);
      break;
    }
    case TYPE_API:
      if (ptr->mark) {
        ptr->mark(mod, ptr, true);
      }
      break;
    default:
      break;
  }
}

void free_value(struct Module* mod, HeapValue* unreached) {
  if (!unreached) return;

  if (unreached->type == TYPE_LIST || unreached->type == TYPE_MUTABLE) {
    free(unreached->as_ptr);
  } else if (unreached->type == TYPE_STRING) {
    if (!unreached->is_constant) {
      free(unreached->as_string);
    }
  } else if (unreached->type == TYPE_API && unreached->destructor) {
    unreached->destructor(mod, unreached);
  } else if (unreached->type == TYPE_RECORD) {
    for (uint32_t i = 0; i < HASHTABLE_SIZE; i++) {
      struct HashNode* node = unreached->as_record.nodes[i];
      while (node != NULL) {
        struct HashNode* next = node->next;
        free(node);
        node = next;
      }
    }
  }

  free(unreached);
}

bool are_all_threads_stopped(struct Module* vm) {
  return atomic_load(&vm->gc->thread_quantity) ==
         atomic_load(&vm->gc->thread_stopped);

}

void trylock_(pthread_mutex_t* mutex) {
  while (pthread_mutex_trylock(mutex) != 0) {
    usleep(1000);  // Sleep for 1ms to avoid busy waiting
  }
}

bool is_at_least_one_programs_running(struct Module* vm) {
  return atomic_load(&vm->gc->thread_quantity) > 0;
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

  // Also mark any young objects reachable from the remembered set
  for (HeapValue* obj = vm->gc->remembered_set; obj != NULL;
       obj = obj->next_remembered) {
    mark_value(vm, MAKE_PTR(obj));
  }

  for (int i = 0; i < vm->gc->stacks.stack_count; i++) {
    Stack* stack = vm->gc->stacks.stacks[i];

    if (!stack || !stack->values) continue;
    trylock(&stack->mutex);
    Value* values = stack->values;
    int stack_pointer = stack->stack_pointer;
    pthread_mutex_unlock(&stack->mutex);

    for (int j = 0; j <= stack_pointer; j++) {
      Value value = values[j];
      if (value == kNull) continue;
      mark_value(vm, value);
    }
  }
}

// Forward declaration for new gc_young
void gc_young(struct Module* mod);
void gc_old(struct Module* mod);

void gc(struct Module* mod) {
  gc_t* gc_ = mod->gc;

  mark_all(mod);
  gc_young(mod);

  young_gc_count++;

  if (gc_->old.num_objects >= gc_->old.max_objects || young_gc_count >= 3) {
    gc_old(mod);
    young_gc_count = 0;
  } else {
    HeapValue* obj = gc_->old.first_object;
    while (obj != NULL) {
      obj->is_marked = false;
      obj = obj->next;
    }
  }

  gc_->young.max_objects = gc_->young.num_objects < INIT_OBJECTS
                               ? INIT_OBJECTS
                               : gc_->young.num_objects * 2;
  gc_->old.max_objects = gc_->old.num_objects < INIT_OBJECTS
                             ? INIT_OBJECTS
                             : gc_->old.num_objects * 2;

  rearrange_stacks(mod);
  atomic_store(&gc_->gc_is_requested, false);
}

void force_sweep(struct Module* mod) {
  gc_t* gc = mod->gc;
  HeapValue* object = gc->young.first_object;
  while (object) {
    HeapValue* next = object->next;
    free_value(mod, object);
    object = next;
  }
  gc->young.first_object = NULL;
  gc->young.num_objects = 0;

  object = gc->old.first_object;
  while (object) {
    HeapValue* next = object->next;
    free_value(mod, object);
    object = next;
  }
  gc->old.first_object = NULL;
  gc->old.num_objects = 0;
}

void request_gc(struct Module* vm) {
  if (atomic_load(&vm->gc->gc_enabled)) {
    atomic_store(&vm->gc->gc_is_requested, true);
  }
}

void* gc_thread(void* data) {
  struct Module* vm = (struct Module*)data;
  while (is_at_least_one_programs_running(vm)) {
    if (atomic_load(&vm->gc->gc_is_requested) &&
        atomic_load(&vm->gc->gc_enabled) && are_all_threads_stopped(vm)) {
      gc(vm);
    } else {
      usleep(1000);
    }
  }

  return NULL;
}

pthread_t start_gc(struct Module* vm) {
  pthread_t gc_thread_id;
  pthread_create(&gc_thread_id, NULL, gc_thread, vm);

  return gc_thread_id;
}

HeapValue* allocate(struct Module* mod, ValueType type) {
  gc_t* gc_ = mod->gc;

  if (gc_->young.num_objects >= gc_->young.max_objects) {
    if (atomic_load(&gc_->gc_enabled)) {
      atomic_store(&gc_->gc_is_requested, true);
    } else {
      gc_->young.max_objects += 2;
    }
  }

  safe_point(mod);

  HeapValue* v = malloc(sizeof(HeapValue));

  v->type = type;
  v->is_marked = false;
  v->is_constant = false;
  v->in_remembered_set = false;
  v->survival_count = 0;
  v->generation = GEN_YOUNG;
  v->next_remembered = NULL;
  v->next = gc_->young.first_object;
  pthread_mutex_init(&v->mutex, NULL);
  pthread_cond_init(&v->cond, NULL);

  // Insert in young generation
  // if (atomic_load(&gc_->gc_enabled)) {
  //   trylock(&gc_->gc_mutex);
  // }
  gc_->young.first_object = v;
  gc_->young.num_objects++;

  // if (atomic_load(&gc_->gc_enabled)) {
  //   pthread_mutex_unlock(&gc_->gc_mutex);
  // }

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

Value MAKE_RECORD(struct Module* mod, Value* keys, Value* values, int size) {
  HeapValue* v = allocate(mod, TYPE_RECORD);
  v->as_record = hash_init();
  for (int i = 0; i < size; i++) {
    if (keys[i] == kNull) {
      THROW_FMT(mod, "Record keys cannot be null");
    }

    char* key = GET_STRING(keys[i]);
    size_t key_length = GET_PTR(keys[i])->length;
    if (key_length == 0) {
      THROW_FMT(mod, "Record keys cannot be empty");
    }

    hash_set(&v->as_record, key, values[i], key_length);
  }

  return MAKE_PTR(v);
}

Value MAKE_STRING_NON_GC(struct Module* mod, char* x) {
  HeapValue* v = allocate(mod, TYPE_STRING);
  v->as_string = x;
  v->length = strlen(x);
  v->is_constant = true;

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

Value MAKE_THREAD(struct Module* mod, pthread_t thread, Value function) {
  HeapValue* v = allocate(mod, TYPE_THREAD);
  v->as_event.function = function;
  v->as_event.thread = thread;

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
    case TYPE_THREAD:
      return "thread";
    case TYPE_FRAME:
      return "frame";
    case TYPE_EVENT_ON:
      return "event_on";
    case TYPE_RECORD:
      return "record";
    case TYPE_NATIVE:
      return "native";
    case TYPE_CHAR:
      return "char";
  }
}

Stack* stack_new() {
  Stack* stack = malloc(sizeof(Stack));

  stack->stack_pointer = GLOBALS_SIZE;
  stack->stack_capacity = MAX_STACK_SIZE;
  stack->values = calloc(stack->stack_capacity + 1, sizeof(Value));
  stack->thread = pthread_self();

  atomic_store(&stack->is_stopped, false);
  atomic_store(&stack->is_halted, false);

  pthread_mutex_init(&stack->mutex, NULL);

  return stack;
}

int value_eq(Module* mod, Value a, Value b) {
  ValueType ty = get_type(a);
  ASSERT_TYPE(mod, "==", b, ty);

  switch (ty) {
    case TYPE_INTEGER:
      return GET_INT(a) == GET_INT(b);
    case TYPE_CHAR:
      return GET_CHAR(a) == GET_CHAR(b);
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

struct RecordAsArray record_to_array(Value record) {
  struct RecordAsArray result;
  if (IS_EMPTY_RECORD(record)) {
    result.keys = NULL;
    result.values = NULL;
    result.length = 0;
    return result;
  }

  HeapValue* rec_ptr = GET_PTR(record);
  result.length = rec_ptr->length;
  result.keys = malloc(result.length * sizeof(char*));
  result.values = malloc(result.length * sizeof(Value));

  uint32_t i = 0;
  // loop through the record nodes
  for (i = 0; i < rec_ptr->length; i++) {
    struct HashNode* node = rec_ptr->as_record.nodes[i];
    
    while (node) {
      if (i >= result.length) {
        result.keys = realloc(result.keys, (i * 1.5) * sizeof(char*));
        result.values = realloc(result.values, (i * 1.5) * sizeof(Value));
        result.length = (int)(i * 1.5);
      }
      result.keys[i] = node->key;
      result.values[i] = node->value;
      node = node->next;
      i++;
    }
  }

  return result;
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

    case TYPE_CHAR: {
      printf("'%c'", GET_CHAR(v));
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
        
      }
      printf(" }");
      break;
    }

    case TYPE_API: {
      printf("API(%p)", GET_PTR(v)->as_any);
      break;
    }

    case TYPE_THREAD: {
      printf("Thread(%p, ", (void*)GET_PTR(v)->as_event.thread);
      debug_value(GET_PTR(v)->as_event.function);
      printf(")");
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
      char* new_str = strdup(str->as_string);
      return MAKE_STRING(mod, new_str);
    }

    case TYPE_RECORD: {
      if (IS_EMPTY_RECORD(value)) {
        return EMPTY_RECORD;
      }

      HeapValue* record = GET_PTR(value);
      Value* keys = malloc(record->length * sizeof(char*));
      Value* values = malloc(record->length * sizeof(Value));
      for (uint32_t i = 0; i < record->length; i++) {
        struct HashNode* node = record->as_record.nodes[i];
        
        while (node) {
          keys[i] = MAKE_STRING_NON_GC(mod, node->key);
          values[i] = clone_value(mod, node->value);
          node = node->next;
        }
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
    case TYPE_THREAD:
      return MAKE_THREAD(mod, GET_PTR(value)->as_event.thread,
                         clone_value(mod, GET_PTR(value)->as_event.function));
    case TYPE_API:
    case TYPE_FUNCENV:
    case TYPE_FRAME:
    case TYPE_EVENT_ON:
    case TYPE_CHAR:
    case TYPE_UNKNOWN:
      return value;
  }
}

void rearrange_stacks(Module* mod) {
  int new_size = 0;

  trylock(&mod->gc->gc_mutex);
  for (int i = 0; i < mod->gc->stacks.stack_count; i++) {
    if (mod->gc->stacks.stacks[i] == NULL) continue;
    if (atomic_load(&mod->gc->stacks.stacks[i]->is_halted)) continue;
    new_size++;
  }

  int size = new_size < STACKS_SIZE ? STACKS_SIZE : new_size;

  Stack** new_stacks = malloc(size * sizeof(Stack*));
  if (new_stacks == NULL) {
    perror("Failed to allocate memory for new stacks");
    exit(EXIT_FAILURE);
  }

  int j = 0;
  for (int i = 0; i < mod->gc->stacks.stack_count; i++) {
    if (mod->gc->stacks.stacks[i] == NULL) continue;
    if (atomic_load(&mod->gc->stacks.stacks[i]->is_halted)) continue;

    new_stacks[j] = mod->gc->stacks.stacks[i];
    atomic_store(&new_stacks[j]->stack_id, j);
    j += 1;
  }

  // Free the old stacks
  free(mod->gc->stacks.stacks);

  // Update the stacks in the gc structure
  mod->gc->stacks.stacks = new_stacks;
  mod->gc->stacks.stack_count = j;
  mod->gc->stacks.stack_capacity = size;
  pthread_mutex_unlock(&mod->gc->gc_mutex);
}

// Young generation collection and promotion
void gc_young(Module* mod) {
  HeapValue* prev = NULL;
  HeapValue* obj = mod->gc->young.first_object;

  while (obj != NULL) {
    if (obj->is_marked) {
      obj->is_marked = false;
      if (obj->generation == GEN_YOUNG) {
        obj->survival_count++;
      }
      if (obj->survival_count >= 5) {
        HeapValue* promoted = obj;
        if (prev)
          prev->next = obj->next;
        else
          mod->gc->young.first_object = obj->next;
        obj = obj->next;

        promoted->next = mod->gc->old.first_object;
        promoted->generation = GEN_OLD;
        promoted->survival_count = 0;
        mod->gc->old.first_object = promoted;
        mod->gc->young.num_objects--;
        mod->gc->old.num_objects++;
        mark_value(mod, MAKE_PTR(promoted));
        continue;
      }
      prev = obj;
      obj = obj->next;
    } else {
      HeapValue* unreached = obj;
      obj = obj->next;
      if (prev)
        prev->next = obj;
      else
        mod->gc->young.first_object = obj;
      free_value(mod, unreached);
      mod->gc->young.num_objects--;
    }
  }

  // Reset the remembered set
  HeapValue* remembered = mod->gc->remembered_set;
  while (remembered != NULL) {
    HeapValue* next = remembered->next_remembered;
    remembered->next_remembered = NULL;
    remembered = next;
  }
  mod->gc->remembered_set = NULL;
}

void gc_old(Module* mod) {
  HeapValue* prev = NULL;
  HeapValue* obj = mod->gc->old.first_object;
  while (obj != NULL) {
    if (obj->is_marked) {
      obj->is_marked = false;
      prev = obj;
      obj = obj->next;
    } else {
      HeapValue* unreached = obj;
      obj = obj->next;
      if (prev)
        prev->next = obj;
      else
        mod->gc->old.first_object = obj;
      free_value(mod, unreached);
      mod->gc->old.num_objects--;
    }
  }
}

void writeBarrier(Module* mod, HeapValue* parent, Value child) {
  if (!parent || !IS_PTR(child)) return;

  HeapValue* child_ptr = GET_PTR(child);

  // Old generation writes to young generation
  if (parent->generation == GEN_OLD && child_ptr->generation == GEN_YOUNG) {

    // If not already in remembered set, add it
    if (!parent->in_remembered_set) {
      parent->in_remembered_set = true;
      parent->next_remembered = mod->gc->remembered_set;
      mod->gc->remembered_set = parent;
    }
  }
}

__attribute__((__always_inline__)) inline Value stack_pop(Module* mod) {
  Stack* stack = mod->stack;
  Value* values = stack->values;

  Value value = values[--stack->stack_pointer];
  values[stack->stack_pointer] = kNull;  // Clear the popped value

  // pthread_mutex_unlock(&stack->mutex);

  return value;
}

void safe_point(Module* mod) {
  if (atomic_load(&mod->gc->gc_is_requested) && 
      atomic_load(&mod->gc->gc_enabled) &&
      !atomic_load(&mod->stack->is_halted) &&
      !atomic_load(&mod->stack->is_stopped)) {
    atomic_store(&mod->stack->is_stopped, true);
    atomic_fetch_add(&mod->gc->thread_stopped, 1);
  } else return;

  while (atomic_load(&mod->gc->gc_is_requested) && atomic_load(&mod->gc->gc_enabled)) {
    if (atomic_load(&mod->stack->is_halted) ||
        !atomic_load(&mod->stack->is_stopped)) {
      atomic_store(&mod->stack->is_stopped, false);
      break;
    } else {
      usleep(1000);  // Sleep for a short duration to avoid busy-waiting
    }
  }

  atomic_store(&mod->stack->is_stopped, false);
  atomic_fetch_sub(&mod->gc->thread_stopped, 1);
}