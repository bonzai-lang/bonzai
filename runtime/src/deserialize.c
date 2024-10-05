#include <assert.h>
#include <error.h>
#include <deserialize.h>
#include <module.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <value.h>

Value deserialize_value(ugc_t *gc, FILE* file) {
  Value value;

  uint8_t type;
  fread(&type, sizeof(uint8_t), 1, file);

  switch (type) {
    case TYPE_INTEGER: {
      int32_t int_value;
      fread(&int_value, sizeof(int32_t), 1, file);
      value = MAKE_INTEGER(int_value);
      break;
    }
    case TYPE_FLOAT: {
      double float_value;
      fread(&float_value, sizeof(double), 1, file);
      value = MAKE_FLOAT(float_value);
      break;
    }

    case TYPE_STRING: {
      int32_t length;
      fread(&length, sizeof(int32_t), 1, file);

      char* string_value = malloc(length + 1);
      fread(string_value, sizeof(char), length, file);
      string_value[length] = '\0';

      value = MAKE_STRING(gc, string_value);
      break;
    }

    default:
      THROW_FMT("Invalid value type, received %d", type);
  }

  return value;
}

Constants deserialize_constants(ugc_t *gc, FILE* file) {
  Constants constants;

  int32_t constant_count;
  fread(&constant_count, sizeof(int32_t), 1, file);

  constants.size = constant_count;
  constants.values = malloc(constant_count * sizeof(Value));
  for (int32_t i = 0; i < constant_count; i++) {
    constants.values[i] = deserialize_value(gc, file);
  }

  assert(constants.values != NULL);

  return constants;
}

void deserialize(Module *mod, FILE* file) {
  Constants constants_ = deserialize_constants(mod->gc, file);

  int32_t instr_count;
  fread(&instr_count, sizeof(int32_t), 1, file);
  
  int32_t* instrs = malloc(instr_count * 5 * sizeof(int32_t));
  fread(instrs, sizeof(int32_t), instr_count * 5, file);

  mod->base_pointer = BASE_POINTER;
  mod->instr_count = instr_count;
  mod->instrs = instrs;
  mod->constants = constants_;
  mod->callstack = 0;
  mod->is_terminated = false;
}
