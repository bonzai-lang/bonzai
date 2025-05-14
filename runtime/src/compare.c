#include <value.h>
#include <compare.h>
#include <error.h>
#include <module.h>

Value compare_eq(Module *mod, Value a, Value b) {
  ValueType a_type = get_type(a);
  ASSERT_FMT(mod, a_type == get_type(b), "Cannot compare values of different types: %s and %s", type_of(a), type_of(b));

  switch (a_type) {
    case TYPE_INTEGER:
      return MAKE_INTEGER(a == b);
    case TYPE_FLOAT:
      return MAKE_INTEGER(GET_FLOAT(a) == GET_FLOAT(b));
    case TYPE_STRING: {
      HeapValue* a_ptr = GET_PTR(a);
      HeapValue* b_ptr = GET_PTR(b);

      size_t a_len = strlen(a_ptr->as_string);
      size_t b_len = strlen(b_ptr->as_string);

      if (a_len != b_len) return MAKE_INTEGER(0);

      return MAKE_INTEGER(strcmp(a_ptr->as_string, b_ptr->as_string) == 0);
    }
    case TYPE_FUNCTION: case TYPE_FUNCENV: case TYPE_MUTABLE: {
      return MAKE_INTEGER(a == b);
    }
    
    case TYPE_RECORD: {
      if (IS_EMPTY_RECORD(a) && IS_EMPTY_RECORD(b)) return MAKE_INTEGER(1);
      if (IS_EMPTY_RECORD(a) || IS_EMPTY_RECORD(b)) return MAKE_INTEGER(0);

      HeapValue* a_ptr = GET_PTR(a);
      HeapValue* b_ptr = GET_PTR(b);

      if (a_ptr->length != b_ptr->length) return MAKE_INTEGER(0);

      for (uint32_t i = 0; i < a_ptr->length; i++) {
        if (strcmp(a_ptr->as_record.keys[i], b_ptr->as_record.keys[i]) != 0) return MAKE_INTEGER(0);
        if (!compare_eq(mod, a_ptr->as_record.values[i + 1], b_ptr->as_record.values[i + 1])) return MAKE_INTEGER(0);
      }

      return MAKE_INTEGER(1);
    }

    case TYPE_LIST: {
      if (IS_EMPTY_LIST(a) && IS_EMPTY_LIST(b)) return MAKE_INTEGER(1);
      if (IS_EMPTY_LIST(a) || IS_EMPTY_LIST(b)) return MAKE_INTEGER(0);

      HeapValue* a_ptr = GET_PTR(a);
      HeapValue* b_ptr = GET_PTR(b);

      if (a_ptr->length != b_ptr->length) return MAKE_INTEGER(0);

      for (uint32_t i = 0; i < a_ptr->length; i++) {
        if (!compare_eq(mod, a_ptr->as_ptr[i], b_ptr->as_ptr[i])) return MAKE_INTEGER(0);
      }

      return MAKE_INTEGER(1);
    }
    case TYPE_SPECIAL: return MAKE_INTEGER(1);
    default:
      THROW_FMT(mod, "Cannot compare values of type %s", type_of(a));
  }
}

Value compare_and(Module *module, Value a, Value b) {
  ASSERT(module, get_type(a) == TYPE_INTEGER && get_type(b) == TYPE_INTEGER, "Expected integers");
  return MAKE_INTEGER(GET_INT(a) && GET_INT(b));
}

Value compare_or(Module* mod, Value a, Value b) {
  ASSERT(mod, get_type(a) == TYPE_INTEGER && get_type(b) == TYPE_INTEGER, "Expected integers");
  return MAKE_INTEGER(GET_INT(a) || GET_INT(b));
}

Value compare_gt(Module* mod, Value a, Value b) {
  ASSERT(mod, get_type(a) == TYPE_INTEGER && get_type(b) == TYPE_INTEGER, "Expected integers");
  return MAKE_INTEGER(GET_INT(a) > GET_INT(b));
}

Value compare_lt(Module* mod, Value a, Value b) {
  ASSERT(mod, get_type(a) == TYPE_INTEGER && get_type(b) == TYPE_INTEGER, "Expected integers");
  return MAKE_INTEGER(GET_INT(a) < GET_INT(b));
}

Value compare_ne(Module* mod, Value a, Value b) {
  ASSERT_TYPE(mod, "compare_ne", a, get_type(b));

  return MAKE_INTEGER(!GET_INT(compare_eq(mod, a, b)));
}

Value compare_ge(Module* mod, Value a, Value b) {
  ASSERT(mod, get_type(a) == TYPE_INTEGER && get_type(b) == TYPE_INTEGER, "Expected integers");
  return MAKE_INTEGER(GET_INT(a) >= GET_INT(b));
}

Value compare_le(Module* mod, Value a, Value b) {
  ASSERT(mod, get_type(a) == TYPE_INTEGER && get_type(b) == TYPE_INTEGER, "Expected integers");
  return MAKE_INTEGER(GET_INT(a) <= GET_INT(b));
}

ComparisonFun comparison_table[] = { compare_lt, compare_gt, compare_eq, compare_ne, compare_le, compare_ge, compare_and, compare_or };