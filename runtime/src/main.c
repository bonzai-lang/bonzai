#define UGC_IMPLEMENTATION
#include <deserialize.h>
#include <gc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <interpreter.h>

static void scan_gc_obj(ugc_t* gc, ugc_header_t* header) {
  if (header == NULL)  // Scan the root set
  {
    // ugc_visit needs to be called on each pointer in the stack and global
    // environment

    Module* runtime = gc->userdata;

    // Scan the stack
    for (int16_t i = 0; i < runtime->stack->stack_pointer; ++i) {
      Value v = runtime->stack->values[i];
      if (v && IS_PTR(v)) {
        HeapValue* hp = GET_PTR(v);

        ugc_visit(gc, &hp->header);
      }
    }
  } else  // Scan the given object
  {
    // ugc_visit needs to be called on each external reference of this
    // object.

    HeapValue* obj = (HeapValue*)header;

    if (obj->type == TYPE_LIST) {
      for (uint32_t i = 0; i < obj->length; i++) {
        if (IS_PTR(obj->as_ptr[i])) {
          HeapValue* hp = GET_PTR(obj->as_ptr[i]);
          ugc_visit(gc, &hp->header);
        }
      }
    } else if (obj->type == TYPE_MUTABLE) {
      if (IS_PTR(*obj->as_ptr)) {
        HeapValue* hp = GET_PTR(*obj->as_ptr);
        ugc_visit(gc, &hp->header);
      }
    } else if (obj->type == TYPE_STRING) {
      // Nothing to do
    }

    // if(obj->external_ref) { ugc_visit(gc, obj->external_ref); }
  }
}

static void free_gc_obj(ugc_t* gc, ugc_header_t* header) {
  // HeapValue* runtime = gc->userdata;
  free(header);
}

int main() {
  ugc_t gc;
  ugc_init(&gc, scan_gc_obj, free_gc_obj);

  Module module;
  module.stack = stack_new();
  module.gc = &gc;
  gc.userdata = &module;

  pthread_mutex_init(&module.module_mutex, NULL);

  FILE* file = fopen("example/main.cst.bin", "rb");
  if (file == NULL) {
    fprintf(stderr, "Failed to open file\n");
    return 1;
  }

  deserialize(&module, file);

  run_interpreter(&module, 0, false, 0);

  fclose(file);
  ugc_release_all(&gc);
  free(module.stack->values);
  return 0;
}