#include <pthread.h>
#include <stdatomic.h>
#include <stdlib.h>
#include "actor.h"
#include <stdio.h>

// Enqueue a message into the actor's mailbox
void send_message(struct actor_t* actor, message_t message) {
    mailbox_t* node = malloc(sizeof(mailbox_t));
    node->message = message;
    node->next = NULL;

    pthread_mutex_lock(&actor->lock);
    if (actor->mailbox == NULL) {
        actor->mailbox = node;
    } else {
        mailbox_t* tail = actor->mailbox;
        while (tail->next) tail = tail->next;
        tail->next = node;
    }
    pthread_cond_signal(&actor->cond); // Notify the actor thread
    pthread_mutex_unlock(&actor->lock);
}

// Dequeue a message from the actor's mailbox
message_t dequeue_message(struct actor_t* actor) {
    pthread_mutex_lock(&actor->lock);
    while (actor->mailbox == NULL && atomic_load(&actor->running)) {
        pthread_cond_wait(&actor->cond, &actor->lock);
    }

    if (!atomic_load(&actor->running) && actor->mailbox == NULL) {
        pthread_mutex_unlock(&actor->lock);
        return (message_t){.type = -1, .data = NULL}; // No more messages
    }

    mailbox_t* node = actor->mailbox;
    actor->mailbox = node->next;
    pthread_mutex_unlock(&actor->lock);

    message_t message = node->message;
    free(node);
    return message;
}

void* actor_thread(void* arg) {
    struct actor_t* actor = (struct actor_t*)arg;

    while (atomic_load(&actor->running)) {
        message_t message = dequeue_message(actor);
        if (message.type != -1) {
          actor->lambda->fun(actor->lambda->env, actor, message);
        } else {
          pthread_exit(actor->thread);
          break;
        }
    }

    return NULL;
}

// Create a new actor
struct actor_t* create_actor(struct lambda_t* behavior) {
    struct actor_t* actor = malloc(sizeof(struct actor_t));
    actor->mailbox = NULL;
    pthread_mutex_init(&actor->lock, NULL);
    pthread_cond_init(&actor->cond, NULL);
    atomic_store(&actor->running, 1);
    actor->lambda = behavior;

    pthread_create(&actor->thread, NULL, actor_thread, actor);
    return actor;
}

// Stop and clean up an actor
void stop_actor(struct actor_t* actor) {
    atomic_store(&actor->running, 0);
    pthread_cond_broadcast(&actor->cond); // Wake up the thread
    pthread_join(actor->thread, NULL);

    // Clean up remaining messages
    while (actor->mailbox) {
        mailbox_t* node = actor->mailbox;
        actor->mailbox = node->next;
        free(node);
    }

    pthread_mutex_destroy(&actor->lock);
    pthread_cond_destroy(&actor->cond);
    free(actor);
}

void waitFor(struct actor_t* actor) {
  pthread_join(actor->thread, NULL);
}

void make_unit() {
  return;
}

void print_int(int n) {
  printf("%d\n", n);
}