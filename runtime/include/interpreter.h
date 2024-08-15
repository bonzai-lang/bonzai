#ifndef INTERPRETER_H
#define INTERPRETER_H

#include <module.h>
#include <stdbool.h>

Value run_interpreter(Module *module, int ipc, bool does_return, int callstack);

#endif