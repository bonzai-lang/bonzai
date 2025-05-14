#include <module.h>
#include <pthread.h>
#include <value.h>
#include <values.h>

typedef struct __node_t {
  Value value;
  struct __node_t* next;
} node_t;

typedef struct __queue_t {
  node_t* head;
  node_t* tail;
  pthread_mutex_t head_lock;
  pthread_mutex_t tail_lock;
} queue_t;

void queue_finalizer(Module* mod, HeapValue* value) {
  (void) mod;

  queue_t* queue = (queue_t*)value->as_any;
  node_t* current = queue->head;
  while (current != NULL) {
    node_t* next = current->next;
    free(current);
    current = next;
  }
  pthread_mutex_destroy(&queue->head_lock);
  pthread_mutex_destroy(&queue->tail_lock);
  free(queue);
}

void queue_mark(Module* mod, HeapValue* value) {
  queue_t* queue = (queue_t*)value->as_any;
  node_t* current = queue->head;
  while (current != NULL) {
    mark_value(mod, current->value);
    current = current->next;
  }
}

Value queue_create(Module* mod, Value* args, int argc) {
  (void) args;
  (void) argc;

  queue_t* q = malloc(sizeof(queue_t));

  node_t* dummy = malloc(sizeof(node_t));
  dummy->value = kNull;
  dummy->next = NULL;

  q->head = dummy;
  q->tail = dummy;
  pthread_mutex_init(&q->head_lock, NULL);
  pthread_mutex_init(&q->tail_lock, NULL);

  HeapValue* hp = allocate(mod, TYPE_API);

  hp->destructor = queue_finalizer;
  hp->mark = queue_mark;
  hp->as_any = q;

  return MAKE_PTR(hp);
}

Value queue_enqueue(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "queue_enqueue", argc, 2);
  ASSERT_TYPE(mod, "queue_enqueue", args[0], TYPE_API);

  HeapValue* hp = GET_PTR(args[0]);
  queue_t* q = (queue_t*)hp->as_any;

  node_t* new_node = malloc(sizeof(node_t));
  new_node->value = args[1];
  new_node->next = NULL;

  pthread_mutex_lock(&q->tail_lock);
  q->tail->next = new_node;
  q->tail = new_node;
  pthread_mutex_unlock(&q->tail_lock);

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
  queue_t* q = (queue_t*)hp->as_any;

  pthread_mutex_lock(&q->head_lock);
  node_t* new_head = q->head->next;
  if (new_head == NULL) {
    pthread_mutex_unlock(&q->head_lock);
    return make_none(mod);
  }

  node_t* old_head = q->head;
  q->head = new_head;
  Value ret_value = new_head->value;
  pthread_mutex_unlock(&q->head_lock);
  free(old_head);

  return make_some(mod, ret_value);
}

Value queue_destroy(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "queue_destroy", argc, 1);
  ASSERT_TYPE(mod, "queue_destroy", args[0], TYPE_API);

  HeapValue* hp = GET_PTR(args[0]);
  queue_t* queue = (queue_t*)hp->as_any;

  node_t* current = queue->head;
  while (current != NULL) {
    node_t* next = current->next;
    free(current);
    current = next;
  }

  pthread_mutex_destroy(&queue->head_lock);
  pthread_mutex_destroy(&queue->tail_lock);

  free(queue);

  free(hp);
  return unit(mod);
}

Value queue_show(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "queue_show", argc, 1);
  ASSERT_TYPE(mod, "queue_show", args[0], TYPE_API);

  HeapValue* hp = GET_PTR(args[0]);
  queue_t* queue = (queue_t*)hp->as_any;

  node_t* current = queue->head;
  while (current != NULL) {
    debug_value(current->value); printf(", ");
    current = current->next;
  }
  if (queue->head != NULL) {
    printf("\n");
  }

  return unit(mod);
}