#include <error.h>
#include <module.h>
#include <operations.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>
#include <value.h>

void print_map_values(Value map) {
  if (!IS_PTR(map)) return;

  HeapValue* map_ptr = GET_PTR(map);
  
  if (map_ptr->type != TYPE_LIST) return;

  if (map_ptr->length == 0) {
    printf("{}");
    return;
  }

  printf("{ ");

  for (uint32_t i = 0; i < map_ptr->length; i++) {
    Value tuple = map_ptr->as_ptr[i];

    if (!IS_PTR(tuple)) return;

    HeapValue* tuple_ptr = GET_PTR(tuple);

    if (tuple_ptr->type != TYPE_LIST) return;
    if (tuple_ptr->length != 5) return;

    print_with_level(tuple_ptr->as_ptr[3], 0);
    printf(": ");
    print_with_level(tuple_ptr->as_ptr[4], 1);

    if (i < map_ptr->length - 1) {
      printf(", ");
    }
  }

  printf(" }");
}

void print_with_level(Value value, int level) {
  if (value == 0) {
    printf("null");
    return;
  }
  ValueType val_type = get_type(value);
  switch (val_type) {
    case TYPE_INTEGER:
      printf("%d", (int32_t)value);
      break;
    case TYPE_SPECIAL:
      printf("<special>");
      break;
    case TYPE_FLOAT:
      printf("%f", GET_FLOAT(value));
      break;
    case TYPE_STRING:
      if (level == 0) {
        printf("%s", GET_STRING(value));
      } else {
        printf("\"%s\"", GET_STRING(value));
      }
      break;
    case TYPE_LIST: {
      HeapValue* list = GET_PTR(value);

      if (list->length == 0) {
        printf("[]");
        break;
      }

      if (list->as_ptr[0] == kNull) {
        char* data_name = GET_STRING(list->as_ptr[1]);

        if (strcmp(data_name, "Map") == 0) {
          print_map_values(list->as_ptr[3]);
          break;
        }

        printf("%s::%s(", GET_STRING(list->as_ptr[1]),
               GET_STRING(list->as_ptr[2]));

        for (uint32_t i = 3; i < list->length; i++) {
          print_with_level(list->as_ptr[i], level + 1);
          if (i < list->length - 1) {
            printf(", ");
          }
        }

        printf(")");
      } else {
        printf("[");

        for (uint32_t i = 0; i < list->length; i++) {
          print_with_level(list->as_ptr[i], level + 1);
          if (i < list->length - 1) {
            printf(", ");
          }
        }

        printf("]");
      }

      break;
    }
    case TYPE_MUTABLE: {
      printf("<mutable ");
      print_with_level(GET_MUTABLE(value), level + 1);
      printf(">");
      break;
    }
    case TYPE_FUNCTION: {
      printf("<function>");
      break;
    }
    case TYPE_FUNCENV: {
      printf("<funcenv>");
      break;
    }

    case TYPE_EVENT: {
      printf("<event>");
      break;
    }

    case TYPE_FRAME: {
      printf("<frame>");
      break;
    }

    case TYPE_UNKNOWN:
    default: {
      printf("<unknown>");
      break;
    }
  }
}

Value print(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "print", argc, 1);
  print_with_level(args[0], 0);
  printf("\n");

  return MAKE_INTEGER(0);
}

Value exit_with(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "exit_with", argc, 1);
  ASSERT_TYPE(mod, "exit_with", args[0], TYPE_INTEGER);

  int code = GET_INT(args[0]);

  mod->is_terminated = true;
  pthread_exit(0);
  exit(code);
}

Value mutable_value(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "mutable_value", argc, 1);
  ASSERT_TYPE(mod, "mutable_value", args[0], TYPE_MUTABLE);

  return GET_MUTABLE(args[0]);
}

Value execute_command(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "execute_command", argc, 1);
  ASSERT_TYPE(mod, "execute_command", args[0], TYPE_STRING);

  char* command = GET_STRING(args[0]);
  int ret = system(command);

  return MAKE_INTEGER(ret);
}

Value get_args(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "get_args", argc, 0);

  Value* argv = malloc(mod->argc * sizeof(Value));
  for (int i = 0; i < mod->argc; i++) {
    argv[i] = mod->argv[i];
  }

  return MAKE_LIST(mod, argv, mod->argc);
}

Value slice(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "slice", argc, 3);
  ASSERT_TYPE(mod, "slice", args[0], TYPE_LIST);
  ASSERT_TYPE(mod, "slice", args[1], TYPE_INTEGER);
  ASSERT_TYPE(mod, "slice", args[2], TYPE_INTEGER);

  HeapValue* list = GET_PTR(args[0]);
  uint32_t start = GET_INT(args[1]);
  uint32_t end = GET_INT(args[2]);

  if (start < 0 || end < 0 || start > list->length || end > list->length) {
    THROW(mod, "Index out of bounds");
  }

  Value* new_list = malloc((end - start) * sizeof(Value));
  for (uint32_t i = start; i < end; i++) {
    new_list[i - start] = list->as_ptr[i];
  }

  return MAKE_LIST(mod, new_list, end - start);
}

Value get_cwd(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "get_cwd", argc, 0);

  char* cwd = malloc(1024);
  if (getcwd(cwd, 1024) == NULL) {
    THROW(mod, "Could not get current working directory");
  }

  return MAKE_STRING(mod, cwd);
}

bool is_not_directory(const char* path) {
  struct stat path_stat;
  stat(path, &path_stat);
  return !S_ISDIR(path_stat.st_mode);
}

Value file_exists(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "file_exists", argc, 1);
  ASSERT_TYPE(mod, "file_exists", args[0], TYPE_STRING);

  char* filename = GET_STRING(args[0]);
  FILE* file = fopen(filename, "r");
  if (file == NULL) {
    return MAKE_INTEGER(0);
  }

  fclose(file);
  return MAKE_INTEGER(is_not_directory(filename));
}

Value get_env(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "get_env", argc, 1);
  ASSERT_TYPE(mod, "get_env", args[0], TYPE_STRING);

  char* env = getenv(GET_STRING(args[0]));
  if (env == NULL) {
    char* empty = malloc(1);
    empty[0] = '\0';
    return MAKE_STRING(mod, empty);
  }

  return MAKE_STRING(mod, strdup(env));
}

Value length(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "length", argc, 1);

  switch (get_type(args[0])) {
    case TYPE_STRING:
      return MAKE_INTEGER(strlen(GET_STRING(args[0])));
    case TYPE_LIST:
      return MAKE_INTEGER(GET_PTR(args[0])->length);
    default:
      THROW_FMT(mod, "Unsupported type for length, received %s",
                type_of(args[0]));
  }
}

Value execute_command_silent(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "execute_command_silent", argc, 1);
  ASSERT_TYPE(mod, "execute_command_silent", args[0], TYPE_STRING);

  char* command = GET_STRING(args[0]);
  char devnull[] = " &> /dev/null";
  char* new_command = malloc(strlen(command) + strlen(devnull) + 1);

  strcpy(new_command, command);
  strcat(new_command, devnull);

  int ret = system(new_command);

  return MAKE_INTEGER(ret);
}

Value panic(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "panic", argc, 1);
  ASSERT_TYPE(mod, "panic", args[0], TYPE_STRING);

  THROW_FMT(mod, "%s", (GET_STRING(args[0])));
}

Value explode(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "explode", argc, 1);
  ASSERT_TYPE(mod, "explode", args[0], TYPE_STRING);

  char* str = GET_STRING(args[0]);
  uint32_t len = strlen(str);

  Value* list = malloc(len * sizeof(Value));
  for (uint32_t i = 0; i < len; i++) {
    char* c = malloc(2 * sizeof(char));
    c[0] = str[i];
    c[1] = '\0';

    list[i] = MAKE_STRING(mod, c);
  }

  return MAKE_LIST(mod, list, len);
}

Value implode(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "implode", argc, 1);
  ASSERT_TYPE(mod, "implode", args[0], TYPE_LIST);

  HeapValue* list = GET_PTR(args[0]);
  char* str = malloc(list->length + 1);

  for (uint32_t i = 0; i < list->length; i++) {
    str[i] = GET_STRING(list->as_ptr[i])[0];
  }

  str[list->length] = '\0';

  return MAKE_STRING(mod, str);
}

Value sliceFrom(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "sliceFrom", argc, 2);
  ASSERT_TYPE(mod, "sliceFrom", args[0], TYPE_LIST);
  ASSERT_TYPE(mod, "sliceFrom", args[1], TYPE_INTEGER);

  HeapValue* list = GET_PTR(args[0]);
  uint32_t start = GET_INT(args[1]);

  if (start < 0 || start > list->length) {
    THROW(mod, "Index out of bounds");
  }

  Value* new_list = malloc((list->length - start) * sizeof(Value));
  for (uint32_t i = start; i < list->length; i++) {
    new_list[i - start] = list->as_ptr[i];
  }

  return MAKE_LIST(mod, new_list, list->length - start);
}

Value toString(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "toString", argc, 1);

  ValueType ty = get_type(args[0]);

  switch (ty) {
    case TYPE_INTEGER: {
      char* str = malloc(12);
      sprintf(str, "%d", (int)GET_INT(args[0]));
      return MAKE_STRING(mod, str);
    }
    case TYPE_FLOAT: {
      char* str = malloc(32);
      sprintf(str, "%f", GET_FLOAT(args[0]));
      return MAKE_STRING(mod, str);
    }
    case TYPE_STRING: {
      return args[0];
    }
    case TYPE_LIST: {
      HeapValue* list = GET_PTR(args[0]);
      char* str = malloc(2 * list->length + 2);
      str[0] = '[';

      for (uint32_t i = 0; i < list->length; i++) {
        char* item = GET_STRING(toString(mod, &list->as_ptr[i], 1));
        strcat(str, item);
        if (i < list->length - 1) {
          strcat(str, ", ");
        }
      }

      strcat(str, "]");
      return MAKE_STRING(mod, str);
    }
    case TYPE_SPECIAL: {
      return MAKE_STRING(mod, "<special>");
    }
    case TYPE_MUTABLE: {
      HeapValue* mut = GET_PTR(args[0]);
      return toString(mod, mut->as_ptr, 1);
    }
    case TYPE_UNKNOWN: {
      return MAKE_STRING(mod, "unknown");
    }
    default: {
      THROW(mod, "Unsupported type for toString");
    }
  }
}

Value is_whitespace(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "is_whitespace", argc, 1);
  ASSERT_TYPE(mod, "is_whitespace", args[0], TYPE_STRING);

  char chr = GET_STRING(args[0])[0];

  return MAKE_INTEGER(chr == ' ' || chr == '\t' || chr == '\n' || chr == '\r');
}

Value is_digit(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "is_digit", argc, 1);
  ASSERT_TYPE(mod, "is_digit", args[0], TYPE_STRING);

  char chr = GET_STRING(args[0])[0];

  return MAKE_INTEGER(chr >= '0' && chr <= '9');
}

Value is_alpha(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "is_alpha", argc, 1);
  ASSERT_TYPE(mod, "is_alpha", args[0], TYPE_STRING);

  char chr = GET_STRING(args[0])[0];

  return MAKE_INTEGER((chr >= 'a' && chr <= 'z') || (chr >= 'A' && chr <= 'Z'));
}

Value is_alphanumeric(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "is_alphanumeric", argc, 1);
  ASSERT_TYPE(mod, "is_alphanumeric", args[0], TYPE_STRING);

  char chr = GET_STRING(args[0])[0];

  return MAKE_INTEGER((chr >= '0' && chr <= '9') ||
                      (chr >= 'a' && chr <= 'z') || (chr >= 'A' && chr <= 'Z'));
}

Value toInt(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "toInt", argc, 1);

  ValueType ty = get_type(args[0]);

  switch (ty) {
    case TYPE_INTEGER: {
      return args[0];
    }
    case TYPE_FLOAT: {
      return MAKE_INTEGER((int)GET_FLOAT(args[0]));
    }
    case TYPE_STRING: {
      return MAKE_INTEGER(atoi(GET_STRING(args[0])));
    }
    default: {
      THROW(mod, "Unsupported type for toInt");
    }
  }
}

Value toFloat(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "toFloat", argc, 1);

  ValueType ty = get_type(args[0]);

  switch (ty) {
    case TYPE_INTEGER: {
      double f = (double)GET_INT(args[0]);
      return MAKE_FLOAT(f);
    }
    case TYPE_FLOAT: {
      return args[0];
    }
    case TYPE_STRING: {
      double f = atof(GET_STRING(args[0]));
      return MAKE_FLOAT(f);
    }
    default: {
      THROW(mod, "Unsupported type for toFloat");
    }
  }
}

Value randomValue(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "randomValue", argc, 0);
  struct timespec ts;
  clock_gettime(CLOCK_REALTIME, &ts);

  // Combine seconds and nanoseconds to get a more unique seed
  unsigned int seed = ts.tv_sec ^ ts.tv_nsec;

  int f = rand_r(&seed);
  return MAKE_INTEGER(f);
}

Value itof(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "itof", argc, 1);
  ASSERT_TYPE(mod, "itof", args[0], TYPE_INTEGER);

  double f = (double)GET_INT(args[0]);
  return MAKE_FLOAT(f);
}

Value ftoi(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "ftoi", argc, 1);
  ASSERT_TYPE(mod, "ftoi", args[0], TYPE_FLOAT);

  int i = (int)GET_FLOAT(args[0]);
  return MAKE_INTEGER(i);
}
