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

  for (uint32_t i = 0; i < map_ptr->length; i += 2) {
    Value key = map_ptr->as_ptr[i];
    Value value = map_ptr->as_ptr[i + 1];

    char* key_name = GET_STRING(key);

    printf("%s: ", key_name);
    print_with_level(value, 1);

    if (i < map_ptr->length - 2) {
      printf(", ");
    }
  }

  printf(" }");
}

int starts_with(const char* str, const char* delim) {
  while (*delim) {
    if (*str != *delim) return 0;
    str++;
    delim++;
  }
  return 1;
}

Value split(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "split", argc, 2);
  ASSERT_TYPE(mod, "split", args[0], TYPE_STRING);
  ASSERT_TYPE(mod, "split", args[1], TYPE_STRING);

  char* string = GET_STRING(args[0]);
  char* delim = GET_STRING(args[1]);

  uint32_t len = strlen(string);
  uint32_t delim_len = strlen(delim);

  uint32_t count = 0;
  for (uint32_t i = 0; i < len; i++) {
    if (starts_with(&string[i], delim)) {
      count++;
      i += delim_len - 1;
    }
  }
  count++; // For the last token
  if (count == 0) {
    return EMPTY_LIST;
  }

  Value* parts = malloc(count * sizeof(Value));
  for (uint32_t i = 0; i < count; i++) {
    parts[i] = kNull;
  }

  int i = 0, token_start = 0, j = 0;

  while (i <= len) {
    // Check if current position matches the delimiter or end of string
    if (starts_with(&string[i], delim) || string[i] == '\0') {
      // Print the token
      char* token = malloc((i - token_start + 1) * sizeof(char));
      strncpy(token, &string[token_start], i - token_start);
      token[i - token_start] = '\0';

      Value token_value = MAKE_STRING(mod, token);
      parts[j++] = token_value;

      i += delim_len;
      token_start = i;
    } else {
      i++;
    }
  }

  return MAKE_LIST(mod, parts, count);
}

Value getIndex(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "getIndex", argc, 2);
  ASSERT_TYPE(mod, "getIndex", args[0], TYPE_STRING);
  ASSERT_TYPE(mod, "getIndex", args[1], TYPE_INTEGER);

  char* str = GET_STRING(args[0]);
  uint32_t index = GET_INT(args[1]);

  if (index < 0 || index >= strlen(str)) {
    THROW(mod, "Index out of bounds");
  }

  char* c = malloc(2 * sizeof(char));
  c[0] = str[index];
  c[1] = '\0';

  return MAKE_STRING(mod, c);
}

Value getIndexChar(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "getIndex", argc, 2);
  ASSERT_TYPE(mod, "getIndex", args[0], TYPE_STRING);
  ASSERT_TYPE(mod, "getIndex", args[1], TYPE_INTEGER);

  char* str = GET_STRING(args[0]);
  uint32_t index = GET_INT(args[1]);

  if (index < 0 || index >= strlen(str)) {
    THROW_FMT(mod, "Index out of bounds, received %d", index);
  }

  return MAKE_CHAR(str[index]);
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

    case TYPE_RECORD: {
      HeapValue* record = GET_PTR(value);
      if (IS_EMPTY_RECORD(value) || record->length == 0) {
        printf("{}");
        break;
      }

      printf("{ ");
      for (uint32_t i = 0; i < record->length; i++) {
        char* key_name = record->as_record.keys[i];
        Value key_value = record->as_record.values[i];

        printf("%s: ", key_name);
        print_with_level(key_value, level + 1);
        if (i < record->length - 1) {
          printf(", ");
        }
      }
      printf(" }");
      break;
    }

    case TYPE_LIST: {
      HeapValue* list = GET_PTR(value);

      if (IS_EMPTY_LIST(value) || list->length == 0) {
        printf("[]");
        break;
      }

      if (list->as_ptr[0] == kNull) {
        char* data_name = GET_STRING(list->as_ptr[1]);

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

    case TYPE_API: {
      printf("<api>");
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

  free(args);
  free(mod->actor_args);
  pthread_exit(NULL);
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

Value list_equals_string(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "list_equals_string", argc, 2);
  ASSERT_TYPE(mod, "list_equals_string", args[0], TYPE_LIST);
  ASSERT_TYPE(mod, "list_equals_string", args[1], TYPE_STRING);

  HeapValue* list = GET_PTR(args[0]);
  char* str = GET_STRING(args[1]);

  if (list->length != strlen(str)) {
    return MAKE_INTEGER(0);
  }

  for (uint32_t i = 0; i < list->length; i++) {
    if (GET_STRING(list->as_ptr[i])[0] != str[i]) {
      return MAKE_INTEGER(0);
    }
  }

  return MAKE_INTEGER(1);
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
    case TYPE_LIST: {
        if (IS_EMPTY_LIST(args[0])) return MAKE_INTEGER(0);

        return MAKE_INTEGER(GET_PTR(args[0])->length);
    }
    default:
      THROW_FMT(mod, "Unsupported type for length, received %s",
                type_of(args[0]));
  }

  return MAKE_INTEGER(0);
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

Value panic_(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "panic", argc, 1);
  ASSERT_TYPE(mod, "panic", args[0], TYPE_STRING);

  THROW_FMT(mod, "%s", (GET_STRING(args[0])));

  return MAKE_INTEGER(0);
}

Value explode(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "explode", argc, 1);
  ASSERT_TYPE(mod, "explode", args[0], TYPE_STRING);

  char* str = GET_STRING(args[0]);
  uint32_t len = strlen(str);

  Value* list = malloc(len * sizeof(Value));
  for (uint32_t i = 0; i < len; i++) {
    list[i] = MAKE_CHAR(str[i]);
  }

  return MAKE_LIST(mod, list, len);
}

Value implode(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "implode", argc, 1);
  ASSERT_TYPE(mod, "implode", args[0], TYPE_LIST);

  if (IS_EMPTY_LIST(args[0])) {
    return MAKE_STRING_NON_GC(mod, "");
  }

  HeapValue* list = GET_PTR(args[0]);

  char* str = malloc(list->length + 1);

  for (uint32_t i = 0; i < list->length; i++) {
    char c = GET_CHAR(list->as_ptr[i]);
    str[i] = c;
  }

  str[list->length] = '\0';

  return MAKE_STRING(mod, str);
}

Value sliceFrom(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "sliceFrom", argc, 2);
  ASSERT_TYPE(mod, "sliceFrom", args[0], TYPE_LIST);
  ASSERT_TYPE(mod, "sliceFrom", args[1], TYPE_INTEGER);

  HeapValue* list = GET_PTR(args[0]);
  int32_t start = GET_INT(args[1]);

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
    case TYPE_CHAR: {
      char* str = malloc(2);
      str[0] = GET_CHAR(args[0]);
      str[1] = '\0';
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
      if (IS_EMPTY_LIST(args[0])) {
        return MAKE_STRING_NON_GC(mod, "[]");
      }

      HeapValue* list = GET_PTR(args[0]);
      char* str = malloc(1);
      str[0] = '[';

      for (uint32_t i = 0; i < list->length; i++) {
        char* item = GET_STRING(toString(mod, (Value[]){list->as_ptr[i], EMPTY_RECORD}, 2));

        str = realloc(str, strlen(str) + strlen(item) + 3);

        strcat(str, item);
        if (i < list->length - 1) {
          strcat(str, ", ");
        }
      }

      str = realloc(str, strlen(str) + 2);

      strcat(str, "]");
      return MAKE_STRING(mod, str);
    }

    case TYPE_RECORD: {
      if (IS_EMPTY_RECORD(args[0])) {
        return MAKE_STRING_NON_GC(mod, "{}");
      }

      HeapValue* record = GET_PTR(args[0]);
      char* str = malloc(1);
      str[0] = '{';

      for (uint32_t i = 0; i < record->length; i++) {
        char* key_name = record->as_record.keys[i];
        Value key_value = record->as_record.values[i];

        char* item = GET_STRING(toString(mod, (Value[]){key_value, EMPTY_RECORD}, 2));

        str = realloc(str, strlen(str) + strlen(key_name) + strlen(item) + 5);

        strcat(str, key_name);
        strcat(str, ": ");
        strcat(str, item);
        if (i < record->length - 1) {
          strcat(str, ", ");
        }
      }

      str = realloc(str, strlen(str) + 2);

      strcat(str, "}");
      return MAKE_STRING(mod, str);
    }

    case TYPE_SPECIAL: {
      char* str = malloc(10);
      sprintf(str, "<special>");

      return MAKE_STRING(mod, str);
    }
    case TYPE_MUTABLE: {
      HeapValue* mut = GET_PTR(args[0]);
      return toString(mod, (Value[2]){*(mut->as_ptr), EMPTY_RECORD}, 2);
    }
    case TYPE_UNKNOWN:
    default: {
      char* str = malloc(10);
      sprintf(str, "<unknown>");

      return MAKE_STRING(mod, str);
    }
  }
}

Value is_whitespace(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "is_whitespace", argc, 1);
  ASSERT_TYPE(mod, "is_whitespace", args[0], TYPE_CHAR);

  char chr = GET_CHAR(args[0]);

  return MAKE_INTEGER(chr == ' ' || chr == '\t' || chr == '\n' || chr == '\r');
}

Value is_digit(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "is_digit", argc, 1);
  ASSERT_TYPE(mod, "is_digit", args[0], TYPE_CHAR);

  char chr = GET_CHAR(args[0]);

  return MAKE_INTEGER(chr >= '0' && chr <= '9');
}

Value is_alpha(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "is_alpha", argc, 1);
  ASSERT_TYPE(mod, "is_alpha", args[0], TYPE_CHAR);

  char chr = GET_CHAR(args[0]);

  return MAKE_INTEGER((chr >= 'a' && chr <= 'z') || (chr >= 'A' && chr <= 'Z'));
}

Value is_alphanumeric(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "is_alphanumeric", argc, 1);
  ASSERT_TYPE(mod, "is_alphanumeric", args[0], TYPE_CHAR);

  char chr = GET_CHAR(args[0]);

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

Value wait_time(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "wait", argc, 1);
  ASSERT_TYPE(mod, "wait", args[0], TYPE_INTEGER);

  int ms = GET_INT(args[0]);
  usleep(ms * 1000);

  return MAKE_INTEGER(0);
}

Value make_unit(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "unit", argc, 0);
  return kNull;
}

Value read_file(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "read_file", argc, 1);
  ASSERT_TYPE(mod, "read_file", args[0], TYPE_STRING);

  char* filename = GET_STRING(args[0]);
  FILE* file = fopen(filename, "r");
  if (file == NULL) {
    THROW(mod, "Could not open file");
  }

  fseek(file, 0, SEEK_END);
  long length = ftell(file);
  fseek(file, 0, SEEK_SET);

  char* buffer = malloc(length + 1);
  fread(buffer, 1, length, file);
  buffer[length] = '\0';

  fclose(file);

  return MAKE_STRING(mod, buffer);
}

Value write_file(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "write_file", argc, 2);
  ASSERT_TYPE(mod, "write_file", args[0], TYPE_STRING);
  ASSERT_TYPE(mod, "write_file", args[1], TYPE_STRING);

  char* filename = GET_STRING(args[0]);
  char* data = GET_STRING(args[1]);

  FILE* file = fopen(filename, "w");
  if (file == NULL) {
    THROW(mod, "Could not open file");
  }

  fwrite(data, 1, strlen(data), file);
  fclose(file);

  return MAKE_INTEGER(0);
}

Value run_gc(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "run_gc", argc, 0);

  atomic_store(&gc_is_requested, 1);

  return MAKE_INTEGER(0);
}

Value free_gc_value(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "free_gc_value", argc, 1);

  if (!IS_PTR(args[0])) return MAKE_INTEGER(0);

  free_value(mod, GET_PTR(args[0]));

  return MAKE_INTEGER(0);
}

Value create_mutex(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "create_mutex", argc, 1);

  HeapValue* mutex_v = allocate(mod, TYPE_API);
  mutex_v->as_ptr = malloc(sizeof(Value));

  *mutex_v->as_ptr = args[0];

  return MAKE_PTR(mutex_v);
}

Value lock_mutex(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "lock_mutex", argc, 1);
  ASSERT_TYPE(mod, "lock_mutex", args[0], TYPE_API);

  HeapValue* mutex_v = GET_PTR(args[0]);
  pthread_mutex_lock(&mutex_v->mutex);

  return *((Value*) mutex_v->as_ptr);
}

Value unlock_mutex(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "unlock_mutex", argc, 1);
  ASSERT_TYPE(mod, "unlock_mutex", args[0], TYPE_API);

  HeapValue* mutex_v = GET_PTR(args[0]);
  pthread_mutex_unlock(&mutex_v->mutex);

  return kNull;
}

Value wait_mutex(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "wait_mutex", argc, 1);
  ASSERT_TYPE(mod, "wait_mutex", args[0], TYPE_API);

  HeapValue* mutex_v = GET_PTR(args[0]);

  while (*((Value*) mutex_v->as_any) == kNull) {
    pthread_cond_wait(&mutex_v->cond, &mutex_v->mutex);
  }

  return kNull;
}

Value signal_mutex(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "signal_mutex", argc, 1);
  ASSERT_TYPE(mod, "signal_mutex", args[0], TYPE_API);

  HeapValue* mutex_v = GET_PTR(args[0]);
  pthread_cond_signal(&mutex_v->cond);

  return kNull;
}

Value destroy_mutex(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "destroy_mutex", argc, 1);
  ASSERT_TYPE(mod, "destroy_mutex", args[0], TYPE_API);

  HeapValue* mutex_v = GET_PTR(args[0]);
  pthread_mutex_destroy(&mutex_v->mutex);
  free(mutex_v->as_any);
  free(mutex_v);

  return kNull;
}

Value show_mutex(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "show_mutex", argc, 1);
  ASSERT_TYPE(mod, "show_mutex", args[0], TYPE_API);

  HeapValue* mutex_v = GET_PTR(args[0]);
  printf("<mutex ");
  debug_value(*((Value*) mutex_v->as_any));
  printf(">\n");

  return kNull;
}

Value write_mutex(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "write_mutex", argc, 2);
  ASSERT_TYPE(mod, "write_mutex", args[0], TYPE_API);

  HeapValue* mutex_v = GET_PTR(args[0]);
  *((Value*) mutex_v->as_any) = args[1];

  return kNull;
}

Value slice_string(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "slice_string", argc, 3);
  ASSERT_TYPE(mod, "slice_string", args[0], TYPE_STRING);
  ASSERT_TYPE(mod, "slice_string", args[1], TYPE_INTEGER);
  ASSERT_TYPE(mod, "slice_string", args[2], TYPE_INTEGER);

  char* str = GET_STRING(args[0]);
  int start = GET_INT(args[1]);
  int end = GET_INT(args[2]);

  if (start < 0 || end < 0 || end > strlen(str)) {
    THROW_FMT(mod, "Invalid string slice, start: %d, end: %d, length: %zu",
              start, end, strlen(str));
  }

  char* sliced = strndup(str + start, end - start);
  Value result = MAKE_STRING(mod, sliced);

  return result;
}