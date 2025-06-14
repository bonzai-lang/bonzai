#include <assert.h>
#include <deserialize.h>
#include <error.h>
#include <module.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <value.h>

Value deserialize_value(Module* mod, FILE* file) {
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

      char* string_value = /*test*/ malloc(length + 1);
      fread(string_value, sizeof(char), length, file);
      string_value[length] = '\0';

      HeapValue* hp = /*test*/ malloc(sizeof(HeapValue));
      hp->type = TYPE_STRING;
      hp->as_string = string_value;
      hp->length = length;
      hp->is_constant = true;

      value = MAKE_PTR(hp);
      break;
    }

    case 3: /* Type Char */ {
      int32_t int_value;
      fread(&int_value, sizeof(int32_t), 1, file);
      value = MAKE_CHAR(int_value);
      break;
    }

    default: {
      THROW_FMT(mod, "Invalid value type, received %d", type);
    }
  }

  return value;
}

void free_constant(Value v) {
  if (!IS_PTR(v)) return;

  HeapValue* hp = GET_PTR(v);

  if (hp == NULL) return;

  if (hp->type == TYPE_STRING) {
    free(hp->as_string);
  }

  free(hp);
}

Constants deserialize_constants(Module* mod, FILE* file) {
  Constants constants;

  int32_t constant_count;
  fread(&constant_count, sizeof(int32_t), 1, file);

  constants.size = constant_count;
  constants.values = malloc(constant_count * sizeof(Value));
  for (int32_t i = 0; i < constant_count; i++) {
    constants.values[i] = deserialize_value(mod, file);
  }

  assert(constants.values != NULL);

  return constants;
}

void deserialize(Module* mod, FILE* file) {
  mod->callstack = 0;
  mod->is_terminated = false;
  mod->base_pointer = BASE_POINTER;

  Constants constants_ = deserialize_constants(mod, file);

  int32_t instr_count;
  fread(&instr_count, sizeof(int32_t), 1, file);

  int32_t* instrs = malloc(instr_count * 5 * sizeof(int32_t));
  fread(instrs, sizeof(int32_t), instr_count * 5, file);

  mod->instr_count = instr_count;
  mod->instrs = instrs;
  mod->constants = malloc(sizeof(Constants));
  mod->constants->size = constants_.size;
  mod->constants->values = constants_.values;

  mod->native_handles = calloc(constants_.size, sizeof(void*));
}
