#include <pthread.h>
#include <stdatomic.h>

// Message structure
typedef struct message_t {
    int type;
    void* data;
    char* name;
} message_t;

// Node for the message queue
typedef struct mailbox_t {
    message_t message;
    struct mailbox_t* next;
} mailbox_t;

typedef struct lambda_t {
  void* env;
  void (*fun)(void*, struct actor_t*, message_t);
};

// Actor structure
typedef struct actor_t {
    pthread_t thread;
    mailbox_t* mailbox;   // Linked list for messages
    pthread_mutex_t lock;   // Mutex to protect the mailbox
    pthread_cond_t cond;    // Condition variable for new messages
    atomic_int running;     // Flag to keep the actor running
    struct lambda_t *lambda; // Actor behavior function
};

void stop_actor(struct actor_t* actor);
void* actor_thread(void* arg);
struct actor_t* create_actor(struct lambda_t*);
message_t dequeue_message(struct actor_t* actor);

void send_message(struct actor_t* actor, message_t message);

void make_unit();
void print_int(int);