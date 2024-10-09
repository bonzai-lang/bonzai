#ifndef OPERATIONS_H
#define OPERATIONS_H

#include <module.h>
#include <value.h>

Value print(Module* mod, Value* args, int argc);
Value exit_with(Module* mod, Value* args, int argc);
Value mutable_value(Module* mod, Value* args, int argc);
Value execute_command(Module* mod, Value* args, int argc);
Value get_args(Module* mod, Value* args, int argc);
Value slice(Module* mod, Value* args, int argc);
Value get_cwd(Module* mod, Value* args, int argc);
Value file_exists(Module* mod, Value* args, int argc);
Value get_env(Module* mod, Value* args, int argc);
Value length(Module* mod, Value* args, int argc);
Value execute_command_silent(Module* mod, Value* args, int argc);

#endif  // OPERATIONS_H