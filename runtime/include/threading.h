#ifndef THREADING_H
#define THREADING_H

#include <module.h>
#include <value.h>

void *routine(void *ptr);
void start_new_routine(Module *module, struct Event *event);

typedef struct Actor {
  MessageQueue *queue;
  pthread_t thread;
  struct Event event;
  struct Module *mod;

  pthread_cond_t cond;
  pthread_mutex_t mutex;

  struct Actor **children;
  struct Actor *parent;
  int children_count;
} Actor;

void *actor_run(void *arg);
Actor *create_actor(struct Event event, struct Module *mod);
void send_message(Actor *actor, int name, Value *args, int argc);

Value list_get(Module *module, Value list, uint32_t index);
Value call_function(struct Module *m, Value closure, int32_t argc, Value *argv);

#endif