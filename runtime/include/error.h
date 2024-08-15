#ifndef ERROR_H
#define ERROR_H

#include <stdlib.h>

#define ENABLE_ASSERTIONS 1

#define THROW(message)                       \
  do {                                       \
    printf(message);                         \
    printf(" at %s:%d", __FILE__, __LINE__); \
    printf("\n");                            \
    exit(EXIT_FAILURE);                      \
  } while (0);

#define THROW_FMT(...)   \
  do {                   \
    printf(__VA_ARGS__); \
    printf(" at %s:%d", __FILE__, __LINE__); \
    printf("\n");        \
    exit(EXIT_FAILURE);  \
  } while (0);

#if ENABLE_ASSERTIONS

#define ASSERT(condition, message) \
  if (!(condition)) {              \
    THROW(message);                \
  }

#define ASSERT_FMT(condition, ...) \
  if (!(condition)) {              \
    THROW_FMT(__VA_ARGS__);        \
  }

#define ASSERT_TYPE(func, v, t) \
  ASSERT_FMT(get_type(v) == t, "%s expected %s, but got %s", func, type_of(t), type_of(v))

#define ASSERT_ARGC(func, argc, n) \
  ASSERT_FMT(argc == n, "%s expected %d arguments, but got %d", func, n, argc)

#else
#define ASSERT(condition, message)
#define ASSERT_FMT(condition, ...)
#define ASSERT_TYPE(func, v, t)
#define ASSERT_ARGC(func, argc, n)
#endif

#endif  // ERROR_H