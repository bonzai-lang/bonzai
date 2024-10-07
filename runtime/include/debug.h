#ifndef DEBUG_H
#define DEBUG_H

#define DEBUG 1

#if DEBUG
#include <value.h>
#include <stdio.h>

#define DEBUG_PRINT printf
#define DEBUG_PRINTLN(...) \
  do {                     \
    printf(__VA_ARGS__);   \
    printf("\n");          \
  } while (0)
#define DEBUG_STACK(stack)                           \
  do {                                               \
    printf("[");                                     \
    for (int i = 0; i < stack->stack_pointer; i++) { \
      native_print(stack->values[i]);                \
      if (i < stack->stack_pointer - 1) {            \
        printf(", ");                                \
      }                                              \
    }                                                \
    printf("]\n");                                   \
  } while (0)

#define DEBUG_STACK_FROM(stack, idx)                  \
  do {                                               \
    printf("[");                                     \
    for (int i = idx; i < stack->stack_pointer; i++) { \
      native_print(stack->values[i]);                \
      if (i < stack->stack_pointer - 1) {            \
        printf(", ");                                \
      }                                              \
    }                                                \
    printf("]\n");                                   \
  } while (0)

#else
#define DEBUG_PRINT(...)
#define DEBUG_PRINTLN(...)
#define DEBUG_STACK(...)
#define DEBUG_STACK_FROM(...)
#endif

#endif  // DEBUG_H