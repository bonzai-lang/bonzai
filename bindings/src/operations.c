#include <value.h>
#include <module.h>
#include <operations.h>
#include <error.h>
#include <unistd.h>
#include <sys/stat.h>

Value print(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "print", argc, 1);
  native_print(args[0]); printf("\n");

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
    return MAKE_STRING(mod, "");
  }

  return MAKE_STRING(mod, env);
}

Value length(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "length", argc, 1);
  
  switch (get_type(args[0])) {
    case TYPE_STRING:
      return MAKE_INTEGER(strlen(GET_STRING(args[0])));
    case TYPE_LIST:
      return MAKE_INTEGER(GET_PTR(args[0])->length);
    default:
      THROW_FMT(mod, "Unsupported type for length, received %s", type_of(args[0]));
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