#define UGC_IMPLEMENTATION
#include <deserialize.h>
#include <gc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <interpreter.h>

int main() {
  GC_init();

  Module module;
  module.stack = stack_new();

  pthread_mutex_init(&module.module_mutex, NULL);

  FILE* file = fopen("example/factorial.cst.bin", "rb");
  if (file == NULL) {
    fprintf(stderr, "Failed to open file\n");
    return 1;
  }

  deserialize(&module, file);

  run_interpreter(&module, 0, false, 0);

  fclose(file);
  free(module.stack->values);
  free(module.stack);
  free(module.instrs);
  return 0;
}