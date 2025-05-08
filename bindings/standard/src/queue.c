#include <module.h>
#include <pthread.h>
#include <value.h>
#include <values.h>

typedef struct message_t {
  Value value;
  struct message_t* next;  // Pointer to the next message in the queue
} message_t;

typedef struct queue_t {
  message_t* head;          // Head of the message queue
  message_t* tail;          // Tail of the message queue
  pthread_mutex_t mutex;    // Mutex to protect access to the queue
  pthread_cond_t cond;      // Condition variable to notify waiting threads
} queue_t;

void queue_finalizer(Module* mod, HeapValue* value) {
  (void) mod;

  queue_t* queue = (queue_t*)value->as_any;
  message_t* current = queue->head;
  while (current != NULL) {
    message_t* next = current->next;
    free(current);
    current = next;
  }
  pthread_mutex_destroy(&queue->mutex);
  pthread_cond_destroy(&queue->cond);
  free(queue);
}

Value queue_create(Module* mod, Value* args, int argc) {
  (void) args;
  (void) argc;

  queue_t* queue = malloc(sizeof(queue_t));
  queue->head = NULL;
  queue->tail = NULL;
  pthread_mutex_init(&queue->mutex, NULL);
  pthread_cond_init(&queue->cond, NULL);

  HeapValue* hp = allocate(mod, TYPE_API);

  hp->destructor = queue_finalizer;
  hp->as_any = queue;

  return MAKE_PTR(hp);
}

Value queue_enqueue(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "queue_enqueue", argc, 2);
  ASSERT_TYPE(mod, "queue_enqueue", args[0], TYPE_API);

  HeapValue* hp = GET_PTR(args[0]);
  queue_t* queue = (queue_t*)hp->as_any;

  message_t* msg = malloc(sizeof(message_t));
  msg->value = args[1];
  msg->next = NULL;

  pthread_mutex_lock(&queue->mutex);

  if (queue->head == NULL) {
    queue->head = msg;
    queue->tail = msg;
  } else {
    queue->tail->next = msg;
    queue->tail = msg;
  }

  pthread_cond_signal(&queue->cond);
  pthread_mutex_unlock(&queue->mutex);

  return unit(mod);
}

bool should_stop(Value value) {
  return GET_MUTABLE(value) == MAKE_INTEGER(0);
}

Value queue_dequeue(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "queue_dequeue", argc, 2);
  ASSERT_TYPE(mod, "queue_dequeue", args[0], TYPE_API);
  ASSERT_TYPE(mod, "queue_dequeue", args[1], TYPE_MUTABLE);

  HeapValue* hp = GET_PTR(args[0]);
  queue_t* queue = (queue_t*)hp->as_any;

  if (should_stop(args[1])) {
    pthread_mutex_unlock(&queue->mutex);
    return make_none(mod);
  }

  pthread_mutex_lock(&queue->mutex);

  while (queue->head == NULL) {
    pthread_cond_wait(&queue->cond, &queue->mutex);
  }

  message_t* msg = queue->head;
  queue->head = msg->next;

  pthread_mutex_unlock(&queue->mutex);

  Value value = msg->value;
  free(msg);

  return make_some(mod, value);
}

Value queue_destroy(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "queue_destroy", argc, 1);
  ASSERT_TYPE(mod, "queue_destroy", args[0], TYPE_API);

  HeapValue* hp = GET_PTR(args[0]);
  queue_t* queue = (queue_t*)hp->as_any;

  message_t* current = queue->head;
  while (current != NULL) {
    message_t* next = current->next;
    free(current);
    current = next;
  }

  pthread_mutex_destroy(&queue->mutex);
  pthread_cond_destroy(&queue->cond);
  free(queue);

  return kNull;
}

Value queue_show(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "queue_show", argc, 1);
  ASSERT_TYPE(mod, "queue_show", args[0], TYPE_API);

  HeapValue* hp = GET_PTR(args[0]);
  queue_t* queue = (queue_t*)hp->as_any;

  message_t* current = queue->head;
  while (current != NULL) {
    debug_value(current->value); printf(", ");
    current = current->next;
  }
  if (queue->head != NULL) {
    printf("\n");
  }

  return unit(mod);
}