#include <error.h>
#include <stdio.h>
#include <string.h>
#include <value.h>

void native_print(Value value) {
  if (value == 0) {
    printf("null");
    return;
  }
  ValueType val_type = get_type(value);
  switch (val_type) {
    case TYPE_INTEGER:
      printf("%d", (int32_t) value);
      break;
    case TYPE_SPECIAL:
      printf("<special>");
      break;
    case TYPE_FLOAT:
      printf("%f", GET_FLOAT(value));
      break;
    case TYPE_STRING:
      printf("%s", GET_STRING(value));
      break;
    case TYPE_LIST: {
      HeapValue* list = GET_PTR(value);
      printf("[");
      if (list->length == 0) {
        printf("]");
        break;
      }
      for (uint32_t i = 0; i < list->length; i++) {
        native_print(list->as_ptr[i]);
        if (i < list->length - 1) {
          printf(", ");
        }
      }
      printf("]");
      break;
    }
    case TYPE_MUTABLE: {
      printf("<mutable ");
      native_print(GET_MUTABLE(value));
      printf(">");
      break;
    }
    case TYPE_FUNCTION: {
      printf("<function>");
      break;
    }
    case TYPE_FUNCENV: {
      printf("<funcenv>");
      break;
    }

    case TYPE_EVENT: {
      printf("<event>");
      break;
    }

    case TYPE_FRAME: {
      printf("<frame>");
      break;
    }

    case TYPE_UNKNOWN: default: {
      printf("<unknown>");
      break;
    }
  }
}

Value MAKE_STRING(ugc_t *gc, char* x) {
  HeapValue* v = malloc(sizeof(HeapValue));
  ugc_register(gc, &v->header);
  v->length = strlen(x);
  v->type = TYPE_STRING;
  v->as_string = x;
  return MAKE_PTR(v);
}

Value MAKE_LIST(ugc_t *gc, Value* x, uint32_t len) {
  HeapValue* v = malloc(sizeof(HeapValue));
  ugc_register(gc, &v->header);
  v->length = len;
  v->type = TYPE_LIST;
  v->as_ptr = x;
  return MAKE_PTR(v);
}

Value MAKE_MUTABLE(ugc_t *gc, Value x) {
  HeapValue* v = malloc(sizeof(HeapValue));
  ugc_register(gc, &v->header);
  v->length = 1;
  v->type = TYPE_MUTABLE;
  v->as_ptr = malloc(sizeof(Value));
  *v->as_ptr = x;
  return MAKE_PTR(v);
}

Value MAKE_EVENT(ugc_t *gc, uint32_t ons_count, uint32_t lets_count, uint32_t ipc) {
  HeapValue* v = malloc(sizeof(HeapValue));
  ugc_register(gc, &v->header);
  v->length = 0;
  v->type = TYPE_EVENT;
  v->as_event.ons_count = ons_count;
  v->as_event.lets_count = lets_count;
  v->as_event.ipc = ipc;
  return MAKE_PTR(v);
}

Value MAKE_FRAME(ugc_t *gc, int32_t ip, int32_t sp, int32_t bp) {
  HeapValue* v = malloc(sizeof(HeapValue));
  ugc_register(gc, &v->header);

  v->type = TYPE_FRAME;
  v->as_frame.instruction_pointer = ip;
  v->as_frame.stack_pointer = sp;
  v->as_frame.base_ptr = bp;
  return MAKE_PTR(v);
}

Value MAKE_EVENT_FRAME(ugc_t *gc, int32_t ip, int32_t sp, int32_t bp, int32_t ons_count, int32_t lets_count, int function_ipc) {
  HeapValue* v = malloc(sizeof(HeapValue));
  ugc_register(gc, &v->header);

  v->type = TYPE_FRAME;
  v->as_frame.instruction_pointer = ip;
  v->as_frame.stack_pointer = sp;
  v->as_frame.base_ptr = bp;
  v->as_frame.ons_count = ons_count;
  v->as_frame.lets_count = lets_count;
  v->as_frame.function_ipc = function_ipc;
  return MAKE_PTR(v);
}

Value MAKE_EVENT_ON(ugc_t *gc, int id, Value func) {
  HeapValue* v = malloc(sizeof(HeapValue));
  ugc_register(gc, &v->header);

  v->type = TYPE_EVENT_ON;
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
  Stack* stack = malloc(MAX_STACK_SIZE * sizeof(Value));
  stack->stack_pointer = 12;
  return stack;
}

void stack_push(Stack* stack, Value value) {
  stack->values[stack->stack_pointer++] = value;
}

Value stack_pop(Stack* stack) {
  return stack->values[--stack->stack_pointer];
}

void enqueue(MessageQueue *queue, Message *msg) {
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

Message* dequeue(MessageQueue *queue) {
    pthread_mutex_lock(&queue->mutex);
    while (!queue->head) {
        pthread_cond_wait(&queue->cond, &queue->mutex);
    }
    Message *msg = queue->head;
    queue->head = msg->next;
    if (!queue->head) {
        queue->tail = NULL;
    }
    pthread_mutex_unlock(&queue->mutex);
    return msg;
}

MessageQueue* create_message_queue() {
    MessageQueue *queue = malloc(sizeof(MessageQueue));
    queue->head = NULL;
    queue->tail = NULL;
    pthread_mutex_init(&queue->mutex, NULL);
    pthread_cond_init(&queue->cond, NULL);
    return queue;
}

void print_message_queue(MessageQueue *queue) {
    Message *msg = queue->head;
    while (msg) {
        printf("%d ", msg->name);
        msg = msg->next;
    }
    printf("\n");
}