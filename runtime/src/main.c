#define UGC_IMPLEMENTATION
#include <deserialize.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <interpreter.h>
#include <error.h>
#include <threading.h>

typedef struct LibraryOption {
  char* name;
  struct LibraryOption* next;
  DLL library;
} LibraryOption;

int count_libraries(LibraryOption* head) {
  int count = 0;
  LibraryOption* current = head;
  while (current != NULL) {
    count++;
    current = current->next;
  }
  return count;
}

LibraryOption* get_libraries(int argc, char* argv[]) {
  LibraryOption* head = NULL;
  LibraryOption* current = NULL;
  for (int i = 0; i < argc; i++) {
    if (strcmp(argv[i], "-l") == 0) {
      LibraryOption* lib = malloc(sizeof(LibraryOption));
      lib->name = argv[i + 1];
      lib->library = load_library(lib->name);
      // printf("Loaded library %s (%p)\n", lib->name, lib->library);
      lib->next = NULL;
      if (head == NULL) {
        head = lib;
        current = lib;
      } else {
        current->next = lib;
        current = lib;
      }
    }
  }
  return head;
}

void free_libraries(LibraryOption* head) {
  LibraryOption* current = head;
  while (current != NULL) {
    free_library(current->library);
    LibraryOption* next = current->next;
    free(current);
    current = next;
  }
}

int main(int argc, char* argv[]) {
  Module module;
  module.file = NULL;
  module.latest_position[0] = 0;
  module.latest_position[1] = 0;

  gc_t* gc = malloc(sizeof(gc_t));
  init_gc(gc, &module);

  if (argc < 2) { THROW_FMT((&module), "Usage: %s <file>", argv[0]); }
  FILE* file = fopen(argv[1], "rb");

  if (file == NULL) {
    THROW_FMT((&module), "Could not open file %s", argv[1]);
    return 1;
  }
  
  // Loading libraries
  LibraryOption* libraries = get_libraries(argc, argv);
  int num_libs = count_libraries(libraries);
  DLL* libs = malloc(sizeof(DLL) * num_libs);

  int i = 0;
  LibraryOption* current = libraries;
  while (current != NULL) {
    libs[i] = current->library;
    i++;
    current = current->next;
  }

  module.stack = stack_new();
  module.gc->stacks.stacks[module.gc->stacks.stack_count++] = module.stack;
  module.handles = libs;
  module.num_handles = num_libs;
  module.current_actor = NULL;
  module.latest_try_catch_count = 0;

  pthread_mutex_init(&module.module_mutex, NULL);

  deserialize(&module, file);

  // Collecting and passing arguments to the module
  module.argc = argc;
  Value* args = /*test*/malloc(argc * sizeof(Value));
  for (int i = 0; i < argc; i++) {
    args[i] = MAKE_STRING_NON_GC((&module), argv[i]);
  }
  module.argv = args;

  run_interpreter(&module, 0, false, 0);

  // pthread_join(gc_thread, NULL);

  // Closing and freeing resources
  fclose(file);

  for (int i = 0; i < module.constants.size; i++) {
    free_constant(module.constants.values[i]);
  }

  force_sweep(&module);
  free(module.stack->values);
  free(module.stack);
  free(module.instrs);
  free(module.constants.values);
  free(module.native_handles);
  free(gc->stacks.stacks);
  free(gc);

  free(args);
  free_libraries(libraries);
  free(libs);

  return 0;
}