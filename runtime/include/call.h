#ifndef CALL_H
#define CALL_H

#include <value.h>
#include <module.h>

void op_call(Module *module, Value callee, int32_t argc);
void op_native_call(Module *module, Value callee, int32_t argc);
typedef void (*InterpreterFunc)(Module*, Value, int32_t);

extern InterpreterFunc interpreter_table[2];

#endif