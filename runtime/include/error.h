#ifndef ERROR_H
#define ERROR_H

#include <stdlib.h>
#include <color.h>

#define ENABLE_ASSERTIONS 1

#define THROW(module, message)                                      \
  printf("%s[error]: %s", BRED, COLOR_RESET);                       \
  printf(message);                                                  \
  printf("\n");                                                     \
  if (module->file && module->latest_position[0] != 0 &&            \
      module->latest_position[1] != 0) {                            \
    for (int i = module->latest_position_index - 1; i >= module->latest_position_index - 5; i--) {        \
      if (i >= module->latest_position_index || !module->file[i]) break;                \
      printf("   %s- at %s:%d:%d\n", BLK, module->file[i],              \
             module->latest_position[i][0], module->latest_position[i][1]); \
    }                                                               \
  }                                                                 \
  printf("   %sin %s:%d\n", BLK, __func__, __LINE__);               \
  printf("   %sat IPC %d", BLK, module->pc / 5);                    \
  printf("\n");                                                     \
  exit(EXIT_FAILURE);

#define THROW_FMT(module, ...)                                              \
  printf("%s[error]: %s", BRED, COLOR_RESET);                               \
  printf(__VA_ARGS__);                                                      \
  printf("\n");                                                             \
  if (module->file && module->latest_position[0] != 0 &&                    \
      module->latest_position[1] != 0) {                                    \
    for (int i = module->latest_position_index - 1;                         \
         i >= module->latest_position_index - 5; i--) {                     \
      if (i >= module->latest_position_index || !module->file[i]) break;    \
      printf("  %s- at %s:%d:%d\n", BLK, module->file[i],                   \
             module->latest_position[i][0], module->latest_position[i][1]); \
    }                                                                       \
  }                                                                         \
  printf("  %s- in %s:%d\n", BLK, __func__, __LINE__);                      \
  printf("  %s- at IPC %d", BLK, module->pc / 5);                           \
  printf("\n");                                                             \
  exit(EXIT_FAILURE);

#if ENABLE_ASSERTIONS

#define ASSERT(module, condition, message) \
  if (!(condition)) {              \
    THROW(module, message);                \
  }

#define ASSERT_FMT(module, condition, ...) \
  if (!(condition)) {              \
    THROW_FMT(module, __VA_ARGS__);        \
  }

#define ASSERT_TYPE(module, func, v, t) \
  ASSERT_FMT(module, get_type(v) == t, "%s expected %s, but got %s", func, type_to_str(t), type_of(v))

#define ASSERT_ARGC(module, func, argc, n) \
  ASSERT_FMT(module, argc == (n + 1), "%s expected %d arguments, but got %d", func, n + 1, argc)

#else
#define ASSERT(condition, message)
#define ASSERT_FMT(condition, ...)
#define ASSERT_TYPE(func, v, t)
#define ASSERT_ARGC(func, argc, n)
#endif

#endif  // ERROR_H