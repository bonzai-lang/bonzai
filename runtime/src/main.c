#include <deserialize.h>
#include <error.h>
#include <interpreter.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <threading.h>
#include <value.h>

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
  module.latest_position_index = 0;

  gc_t* gc = malloc(sizeof(gc_t));
  init_gc(gc, &module);

  if (argc < 2) {
    THROW_FMT((&module), "Usage: %s <file>", argv[0]);
  }
  FILE* file = fopen(argv[1], "rb");

  if (file == NULL) {
    THROW_FMT((&module), "Could not open file %s", argv[1]);
    return 1;
  }

  // Loading libraries
  LibraryOption* libraries = get_libraries(argc, argv);
  int num_libs = count_libraries(libraries);
  DLL* libs = malloc(sizeof(DLL) * num_libs);

  LibraryOption* current = libraries;
  for (int i = 0; i < num_libs; i++) {
    libs[i] = current->library;
    current = current->next;
  }

  module.stack = stack_new();
  module.handles = libs;
  module.call_function = call_function;
  module.gc->stacks.stacks[module.gc->stacks.stack_count] = module.stack;
  atomic_store(&module.stack->stack_id, module.gc->stacks.stack_count);
  module.gc->stacks.stack_count++;
  module.num_handles = num_libs;
  module.current_actor = NULL;
  module.events = malloc(sizeof(struct Actor*) * 1024);
  module.event_capacity = 1024;

  pthread_mutex_init(&module.module_mutex, NULL);

  deserialize(&module, file);

  // Collecting and passing arguments to the module
  module.argc = argc;
  Value* args = /*test*/ malloc(argc * sizeof(Value));
  for (int i = 0; i < argc; i++) {
    args[i] = MAKE_STRING_NON_GC((&module), argv[i]);
  }
  module.argv = args;

  pthread_t id = start_gc(&module);

  module.gc->gc_thread = id;

  run_interpreter(&module, 0, false, 0);

  // Closing and freeing resources
  fclose(file);

  force_sweep(&module);

  for (int i = 0; i < module.constants->size; i++) {
    free_constant(module.constants->values[i]);
  }
  free(module.stack->values);
  free(module.stack);
  free(module.instrs);
  free(module.constants->values);
  free(module.native_handles);
  free(module.events);
  free(gc->stacks.stacks);
  free(gc);
  free(module.latest_position);
  free(module.file);

  free(args);
  free_libraries(libraries);
  free(libs);
  free(module.constants);

  return 0;
}