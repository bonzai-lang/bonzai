#ifndef BASE64_H
#define BASE64_H

#include <module.h>
#include <value.h>

Value toBase64(Module* module, Value* args, int argc);
Value fromBase64(Module* module, Value* args, int argc);

#endif // BASE64_H
