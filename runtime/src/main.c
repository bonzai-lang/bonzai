#define UGC_IMPLEMENTATION
#include <deserialize.h>
#include <gc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <interpreter.h>
#include <error.h>
#include <threading.h>

int main(int argc, char* argv[]) {
  // GC_
  if (argc < 2) THROW_FMT("Usage: %s <file>\n", argv[0]);
  FILE* file = fopen(argv[1], "rb");

  Module module;
  module.stack = stack_new();

  module.argc = argc;
  Value* args = /*test*/malloc(argc * sizeof(Value));
  for (int i = 0; i < argc; i++) 
    args[i] = MAKE_STRING(&module, argv[i]);

  pthread_mutex_init(&module.module_mutex, NULL);

  if (file == NULL) {
    fprintf(stderr, "Failed to open file\n");
    return 1;
  }

  deserialize(&module, file);

  run_interpreter(&module, 0, false, 0);

  fclose(file);
  force_sweep(&module);
  free(module.stack->values);
  free(module.stack);
  free(module.instrs);

  for (int i = 0; i < module.constants.size; i++) {
    free_constant(module.constants.values[i]);
  }

  free(module.constants.values);

  for (int i = 0; i < argc; i++) {
    if (IS_PTR(args[i])) {
      HeapValue* hp = GET_PTR(args[i]);
      free(hp);
    }
  }
  free(args);
  return 0;
}