#ifndef VALUE_H
#define VALUE_H

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>

#include <gc.h>

typedef uint64_t Value;

#define GLOBALS_SIZE 1024
#define MAX_STACK_SIZE GLOBALS_SIZE * 32
#define VALUE_STACK_SIZE MAX_STACK_SIZE - GLOBALS_SIZE
#define BASE_POINTER GLOBALS_SIZE

// Masks for important segments of a float value
#define MASK_SIGN 0x8000000000000000
#define MASK_EXPONENT 0x7ff0000000000000
#define MASK_QUIET 0x0008000000000000
#define MASK_TYPE 0x0007000000000000
#define MASK_SIGNATURE 0xffff000000000000
#define MASK_PAYLOAD_PTR 0x0000ffffffffffff
#define MASK_PAYLOAD_INT 0x00000000ffffffff

// Type IDs for short encoded types
#define MASK_TYPE_NAN 0x0000000000000000
#define MASK_TYPE_SPECIAL 0x0001000000000000
#define MASK_TYPE_INTEGER 0x0002000000000000
#define MASK_TYPE_STRING 0x0003000000000000
#define MASK_TYPE_FUNCTION 0x0004000000000000
#define MASK_TYPE_FUNCENV 0x0005000000000000

// Constant short encoded values
#define kNaN (MASK_EXPONENT | MASK_QUIET)
#define kNull (kNaN | MASK_TYPE_SPECIAL)

// Signatures of encoded types
#define SIGNATURE_NAN kNaN
#define SIGNATURE_SPECIAL kNull
#define SIGNATURE_INTEGER (kNaN | MASK_TYPE_INTEGER)
#define SIGNATURE_STRING (kNaN | MASK_TYPE_STRING)
#define SIGNATURE_FUNCTION (kNaN | MASK_TYPE_FUNCTION)
#define SIGNATURE_FUNCENV (kNaN | MASK_TYPE_FUNCENV)
#define SIGNATURE_POINTER (kNaN | MASK_SIGN)

typedef int32_t reg;

Value equal(Value x, Value y);
char* constructor_name(Value x);
void native_print(Value value);

// The type of the stored value
typedef enum {
  TYPE_INTEGER = 0,
  TYPE_FLOAT,
  TYPE_STRING,
  TYPE_LIST,
  TYPE_SPECIAL,
  TYPE_MUTABLE,
  TYPE_FUNCTION,
  TYPE_FUNCENV,
  TYPE_UNKNOWN,
  TYPE_API,
  TYPE_EVENT,
  TYPE_FRAME,
  TYPE_EVENT_ON,
} ValueType;

#define GET_PTR(x) ((HeapValue*)((x) & MASK_PAYLOAD_PTR))
#define GET_STRING(x) GET_PTR(x)->as_string
#define GET_LIST(x) GET_PTR(x)->as_ptr
#define GET_MUTABLE(x) *(GET_PTR(x)->as_ptr)

#define GET_INT(x) ((x) & MASK_PAYLOAD_INT)
#define GET_FLOAT(x) (*(double*)(&(x)))
#define GET_ADDRESS(x) GET_INT(x)
#define GET_NATIVE(x) GET_STRING(x)
#define GET_NTH_ELEMENT(x, n) ((x >> (n * 16)) & MASK_PAYLOAD_INT)

typedef struct Message {
    int name;
    Value* args;
    int argc;
    struct Message *next;   // Pointer to the next message in the queue
} Message;

typedef struct MessageQueue {
    Message *head;   // Head of the message queue
    Message *tail;   // Tail of the message queue
    pthread_mutex_t mutex;  // Mutex to protect access to the queue
    pthread_cond_t cond;    // Condition variable to notify waiting threads
} MessageQueue;


void enqueue(MessageQueue *queue, Message *msg);
Message* dequeue(MessageQueue *queue);
MessageQueue* create_message_queue();
void print_message_queue(MessageQueue *queue);

struct Event {
  int ons_count;
  Value ons[256];

  int ipc;

  int lets_count;
  Value *lets;

  struct Module *mod;
  struct Actor* actor;
};

typedef struct Stack {
  Value values[MAX_STACK_SIZE];
  int16_t stack_pointer;
} Stack;

#define MAX_FRAMES 1024

typedef struct {
  reg instruction_pointer;
  int32_t stack_pointer;
  int32_t base_ptr;

  int lets_count;
  int ons_count;
  int function_ipc;
} Frame;

struct EventOn {
  int id;
  Value func;
};

// Container type for values
typedef struct {
	ugc_header_t header;
  ValueType type;
  uint32_t length;

  union {
    char* as_string;
    Value* as_ptr;
    void* as_any;
    struct Event as_event;
    struct EventOn as_event_on;
    Frame as_frame;
  };
} HeapValue;

#define IS_PTR(x) (((x) & MASK_SIGNATURE) == SIGNATURE_POINTER)
#define IS_FUN(x) (((x) & MASK_SIGNATURE) == SIGNATURE_FUNCTION)

#define MAKE_INTEGER(x) (SIGNATURE_INTEGER | (uint32_t)(x))
#define MAKE_FLOAT(x) (*(Value*)(&(x)))
#define MAKE_PTR(x) (SIGNATURE_POINTER | (uint64_t)(x))

#define MAKE_FUNCTION(x, y) \
  (SIGNATURE_FUNCTION | (uint16_t)(x) | ((uint16_t)(y) << 16))

Value MAKE_MUTABLE(ugc_t *gc, Value x);
Value MAKE_STRING(ugc_t *gc, char* x);
Value MAKE_LIST(ugc_t *gc, Value* x, uint32_t len);
Value MAKE_EVENT(ugc_t *gc, uint32_t ons_count, uint32_t lets_count, uint32_t ipc);
Value MAKE_FRAME(ugc_t *gc, int32_t ip, int32_t sp, int32_t bp);
Value MAKE_EVENT_FRAME(ugc_t *gc, int32_t ip, int32_t sp, int32_t bp, int32_t ons_count, int32_t lets_count, int function_ipc);
Value MAKE_EVENT_ON(ugc_t *gc, int id, Value func);

#define MAKE_SPECIAL() kNull
#define MAKE_ADDRESS(x) MAKE_INTEGER(x)
#define MAKE_NATIVE(x) MAKE_STRING(x, strlen(x))

ValueType get_type(Value value);
char* type_of(Value value);

Stack* stack_new();
void stack_push(Stack* stack, Value value);
Value stack_pop(Stack* stack);

#endif  // VALUE_H
