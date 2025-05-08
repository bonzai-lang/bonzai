#include <debug.h>
#include <error.h>
#include <module.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <threading.h>
#include <value.h>

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))

void mark_value(Value value) {
  if (!IS_PTR(value)) return;

  HeapValue* ptr = GET_PTR(value);

  if (ptr->is_marked) return;

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

void gc_free(struct Module* mod, Value value) {
  if (!IS_PTR(value)) return;

  HeapValue* ptr = GET_PTR(value);

  if (ptr->is_marked) return;

  free_value(mod, ptr);
}

void mark_all(struct Module* vm) {
  for (int i = 0; i < vm->argc; i++) {
    mark_value(vm->argv[i]);
  }

  for (int i = 0; i < vm->gc->stacks.stack_count; i++) {
    Stack* stack = vm->gc->stacks.stacks[i];

    // printf("Marking stack %d (sp: %d)\n", i, stack->stack_pointer);
    if (!stack || !stack->values) continue;
    pthread_mutex_lock(&stack->mutex);

    for (int j = 0; j <= stack->stack_capacity; j++) {
      if (j <= stack->stack_pointer) {
        mark_value(stack->values[j]);

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

      free_value(vm, unreached);
      vm->gc->num_objects--;
    }
  }
}

void gc(struct Module* vm) {
  // printf("Requested GC from thread %ld\n", pthread_self());
  gc_t* gc_ = vm->gc;
  pthread_mutex_lock(&gc_->gc_mutex);
  vm->gc->gc_running = true;
  // int numObjects = gc_->num_objects;
  pthread_mutex_unlock(&gc_->gc_mutex);

  mark_all(vm);
  sweep(vm);

  pthread_mutex_lock(&gc_->gc_mutex);
  gc_->max_objects =
      gc_->num_objects < INIT_OBJECTS ? INIT_OBJECTS : gc_->num_objects * 2;

  // printf("Collected %d objects, %d max objects (sp: %d)\n",
  //        numObjects - gc_->num_objects, gc_->max_objects,
  //        vm->stack->stack_pointer);

  pthread_cond_signal(&gc_->gc_cond);

  vm->gc->gc_running = false;
  pthread_mutex_unlock(&gc_->gc_mutex);
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
  gc_t* gc_ = vm->gc;
  if (gc_->num_objects >= gc_->max_objects) {
    stop_the_world(vm, true);
    gc(vm);
    stop_the_world(vm, false);
  }
}

HeapValue* allocate(struct Module* mod, ValueType type) {
  gc_t* gc_ = mod->gc;
  pthread_mutex_lock(&gc_->gc_mutex);
  while (gc_->gc_running) {
    pthread_cond_wait(&gc_->gc_cond, &gc_->gc_mutex);
  }
  int numObjects = gc_->num_objects;
  int maxObjects = gc_->max_objects;
  pthread_mutex_unlock(&gc_->gc_mutex);

  if (numObjects >= maxObjects) {
    if (gc_->gc_enabled) {
      pthread_mutex_lock(&gc_->gc_mutex);
      int is_running = gc_->gc_running;
      pthread_mutex_unlock(&gc_->gc_mutex);

      if (!is_running) {
        stop_the_world(mod, true);
        gc(mod);
        stop_the_world(mod, false);
      }
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

  gc_->first_object = v;
  gc_->num_objects++;

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
    case TYPE_NATIVE:
      return "native";
  }
}

Stack* stack_new() {
  Stack* stack = malloc(sizeof(Stack));
  stack->stack_pointer = GLOBALS_SIZE;
  stack->stack_capacity = MAX_STACK_SIZE;
  stack->values = calloc(stack->stack_capacity, sizeof(Value));
  pthread_mutex_init(&stack->mutex, NULL);
  return stack;
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
      printf("%d", (int)GET_INT(v));
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

    case TYPE_NATIVE: {
      printf("Native(%s)", GET_NATIVE(v).name);
      break;
    }

    case TYPE_UNKNOWN:
    default:
      printf("Unknown");
      break;
  }
}

void safe_point(Module* mod) {
  pthread_mutex_lock(&mod->gc->gc_mutex);
  while (mod->gc->gc_running) {
    pthread_cond_wait(&mod->gc->gc_cond, &mod->gc->gc_mutex);
  }
  pthread_mutex_unlock(&mod->gc->gc_mutex);
}

void stop_the_world(Module* mod, bool stop) {
  if (!stop) {
    pthread_cond_signal(&mod->gc->gc_cond);
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
    case TYPE_LIST: {
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
