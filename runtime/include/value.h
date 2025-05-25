#ifndef VALUE_H
#define VALUE_H

#include <error.h>
#include <pthread.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef uint64_t Value;

#define INIT_POS 512
#define INIT_OBJECTS 2048
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
#define MASK_TYPE_FUNCTION 0x0003000000000000
#define MASK_TYPE_EMPTY_LIST 0x0004000000000000
#define MASK_TYPE_EMPTY_RECORD 0x0005000000000000
#define MASK_TYPE_CHAR 0x0006000000000000

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
#define SIGNATURE_EMPTY_LIST (kNaN | MASK_TYPE_EMPTY_LIST)
#define SIGNATURE_EMPTY_RECORD (kNaN | MASK_TYPE_EMPTY_RECORD)
#define SIGNATURE_CHAR (kNaN | MASK_TYPE_CHAR)

typedef int32_t reg;

Value equal(Value x, Value y);
char* constructor_name(Value x);

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
  TYPE_NATIVE,
  TYPE_RECORD,
  TYPE_CHAR
} ValueType;

#define GET_PTR(x) ((HeapValue*)((x) & MASK_PAYLOAD_PTR))
#define GET_STRING(x) GET_PTR(x)->as_string
#define GET_LIST(x) GET_PTR(x)->as_ptr
#define GET_MUTABLE(x) *(GET_PTR(x)->as_ptr)

#define GET_INT(x) ((x) & MASK_PAYLOAD_INT)
#define GET_CHAR(x) ((char)((x) & MASK_PAYLOAD_INT))
#define GET_FLOAT(x) (*(double*)(&(x)))
#define GET_ADDRESS(x) GET_INT(x)
#define GET_NATIVE(x) GET_PTR(x)->as_native
#define GET_NTH_ELEMENT(x, n) ((x >> (n * 16)) & MASK_PAYLOAD_INT)

struct Event {
  int ons_count;
  Value ons[256];

  int ipc;

  struct Module* mod;
  struct Actor* actor;
};

typedef struct Stack {
  Value* values;
  int32_t stack_pointer;
  int32_t stack_capacity;
  pthread_mutex_t mutex;

  atomic_bool is_stopped;
  atomic_bool is_halted;

  pthread_t thread;
} Stack;

#define MAX_FRAMES 16384

struct Frame {
  reg instruction_pointer;
  int32_t stack_pointer;
  int32_t base_ptr;

  int ons_count;
  int function_ipc;
};

struct EventOn {
  int id;
  Value func;
};

struct Native {
  char* name;
  int addr;
};

struct Function {
  int ip;
  int local_space;
};

struct Record {
  char** keys;
  Value* values;
};

enum GenerationTag {
  GEN_YOUNG,
  GEN_OLD
};

// Container type for values
typedef struct HeapValue {
  ValueType type;
  uint32_t length;
  bool is_marked, is_constant, in_remembered_set;
  int survival_count;
  enum GenerationTag generation;
  struct HeapValue* next_remembered;
  struct HeapValue* next;
  void (*destructor)(struct Module*, struct HeapValue*);
  void (*mark)(struct Module*, struct HeapValue*);
  pthread_mutex_t mutex;
  pthread_cond_t cond;

  union {
    char* as_string;
    Value* as_ptr;
    void* as_any;
    struct Record as_record;
    struct Event as_event;
    struct EventOn as_event_on;
    struct Frame as_frame;
    struct Native as_native;
    struct Function as_func;
  };
} HeapValue;

typedef struct {
  Stack** stacks;
  int stack_count;
  int stack_capacity;
} stacks_t;;

typedef struct {
  HeapValue* first_object;
  int num_objects;
  int max_objects;
} Generation;

typedef struct {
  Generation young;
  Generation old;

  bool gc_enabled;
  stacks_t stacks;
  pthread_t gc_thread;
  pthread_cond_t gc_cond;
  pthread_mutex_t gc_mutex;

  HeapValue* remembered_set;
} gc_t;

#define IS_PTR(x) (((x) & MASK_SIGNATURE) == SIGNATURE_POINTER)
#define IS_FUN(x) (((x) & MASK_SIGNATURE) == SIGNATURE_FUNCTION)
#define IS_EMPTY_LIST(x) (((x) & MASK_SIGNATURE) == SIGNATURE_EMPTY_LIST)
#define IS_EMPTY_RECORD(x) (((x) & MASK_SIGNATURE) == SIGNATURE_EMPTY_RECORD)
#define IS_CHAR(x) (((x) & MASK_SIGNATURE) == SIGNATURE_CHAR)

#define MAKE_INTEGER(x) (SIGNATURE_INTEGER | (uint32_t)(x))
#define MAKE_FLOAT(x) (*(Value*)(&(x)))
#define MAKE_PTR(x) (SIGNATURE_POINTER | (uint64_t)(x))
#define MAKE_CHAR(x) (SIGNATURE_CHAR | (uint32_t)(x))
#define EMPTY_LIST (SIGNATURE_EMPTY_LIST | 0)
#define EMPTY_RECORD (SIGNATURE_EMPTY_RECORD | 0)

// #define MAKE_FUNCTION(x, y) \
//   (SIGNATURE_FUNCTION | (uint32_t)(x) | ((uint16_t)(y) << 32))

Value MAKE_MUTABLE(struct Module* mod, Value x);
Value MAKE_STRING(struct Module* mod, char* string);
Value MAKE_STRING_MULTIPLE(struct Module* mod, ...);
Value MAKE_LIST(struct Module* mod, Value* x, uint32_t len);
Value MAKE_EVENT(struct Module* mod, uint32_t ons_count, uint32_t ipc);
Value MAKE_FRAME(struct Module* mod, int32_t ip, int32_t sp, int32_t bp);
Value MAKE_EVENT_FRAME(struct Module* mod, int32_t ip, int32_t sp, int32_t bp,
                       int32_t ons_count, int function_ipc);
Value MAKE_NATIVE(struct Module* mod, char* name, int addr);
Value MAKE_EVENT_ON(struct Module* mod, int id, Value func);
Value MAKE_STRING_NON_GC(struct Module* mod, char* x);
Value MAKE_FUNCTION(struct Module* mod, int32_t ip, uint16_t local_space);
Value MAKE_RECORD(struct Module* mod, char** keys, Value* values,
                  int size);
// void gc(struct Module* vm);
void force_sweep(struct Module* vm);
HeapValue* allocate(struct Module* mod, ValueType type);
void mark_value(struct Module* mod, Value value);
Value clone_value(struct Module* mod, Value value);
void request_gc(struct Module* vm);
void free_value(struct Module* mod, HeapValue* unreached);
void safe_point(struct Module* mod);
pthread_t start_gc(struct Module* vm);
void rearrange_stacks(struct Module* mod);
bool is_at_least_one_programs_running(struct Module* vm);

static atomic_bool gc_is_requested;
static atomic_int gc_type;
static atomic_bool gc_is_running;

#define MAKE_SPECIAL() kNull
#define MAKE_ADDRESS(x) MAKE_INTEGER(x)

char* type_of(Value value);

Stack* stack_new();

int value_eq(struct Module* mod, Value a, Value b);

void debug_value(Value v);

#define stack_push(module, value)                                        \
  do {                                                                   \
    if (module->stack->stack_pointer >= module->stack->stack_capacity) { \
      printf("Reallocating stack");                                      \
      module->stack->stack_capacity *= 1.25;                             \
      module->stack->values =                                            \
          realloc(module->stack->values,                                 \
                  module->stack->stack_capacity * sizeof(Value));        \
      if (!module->stack->values) {                                      \
        THROW(module, "Failed to allocate memory for stack");            \
      }                                                                  \
    }                                                                    \
    module->stack->values[module->stack->stack_pointer++] = value;       \
  } while (0)

Value stack_pop(struct Module* module);

inline static ValueType get_type(Value value) {
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
    case SIGNATURE_EMPTY_LIST:
      return TYPE_LIST;
    case SIGNATURE_EMPTY_RECORD:
      return TYPE_RECORD;
    case SIGNATURE_CHAR: 
      return TYPE_CHAR;
  }

  return TYPE_UNKNOWN;
}

inline static char* type_to_str(ValueType t) {
  switch (t) {
    case TYPE_INTEGER:
      return "integer";
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
    case TYPE_FUNCTION:
      return "function";
    case TYPE_FUNCENV:
      return "function_env";
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
    case TYPE_RECORD:
      return "record";
    case TYPE_CHAR: 
      return "char";
  }
}

void safe_point(struct Module* mod);
void stop_the_world(struct Module* mod, bool stop);
void writeBarrier(struct Module* mod, HeapValue* parent, Value child);

#endif  // VALUE_H
