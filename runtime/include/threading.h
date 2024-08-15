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
  struct Module* mod;
} Actor;

void *actor_run(void *arg);
Actor *create_actor(struct Event event, struct Module* mod);
void send_message(Actor *actor, int name, Value *args, int argc);

#endif