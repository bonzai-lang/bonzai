#ifndef MODULE_H
#define MODULE_H

#include <library.h>
#include <stdlib.h>
#include <value.h>
#include <stdbool.h>
#include <pthread.h>

typedef struct {
  Value* values;
  int32_t size;
} Constants;

typedef struct Module {
  int32_t instr_count;
  int32_t *instrs;

  int32_t base_pointer;
  int32_t callstack;

  Constants constants;
  Stack *stack;
  DLL* handles;

  int32_t argc;
  Value *argv;

  ugc_t *gc;
  int32_t pc;
  Value (*call_function)(struct Module *m, Value callee, int32_t argc, Value* argv);
  
  int event_count;
  struct Actor* events[1024];

  bool is_terminated;

  pthread_mutex_t module_mutex;
} Module;

typedef Value (*Native)(int argc, struct Module *m, Value *args);

Frame pop_frame(Module *mod);

#endif  // MODULE_H