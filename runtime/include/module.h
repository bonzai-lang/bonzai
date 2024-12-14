#ifndef MODULE_H
#define MODULE_H

#include <library.h>
#include <stdlib.h>
#include <value.h>
#include <stdbool.h>
#include <pthread.h>

typedef int Position[2];

#define STACKS_SIZE 32

typedef struct {
  Value* values;
  int32_t size;
} Constants;

typedef struct Module {
  int32_t instr_count;
  int32_t *instrs;

  int32_t base_pointer;
  int32_t spawn_pointer;
  int32_t callstack;

  struct Actor* current_actor;

  Constants constants;
  Stack *stack;

  size_t num_handles;
  DLL* handles;

  int32_t argc;
  Value *argv;

  int32_t pc;
  Value (*call_function)(struct Module *m, Value callee, int32_t argc, Value* argv);
  
  int event_count;
  struct Actor* events[1024];

  bool is_terminated;

  pthread_mutex_t module_mutex;
  
  Position latest_position;
  const char* file;

  void** native_handles;
  
  gc_t* gc;
} Module;

typedef Value (*NativeFunction)(struct Module *m, Value *args, int argc);

Frame pop_frame(Module *mod);
Frame pop_event_frame(Module* mod);
void init_gc(gc_t* gc, Module* mod);

#endif  // MODULE_H