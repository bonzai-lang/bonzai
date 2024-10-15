#include <value.h>
#include <module.h>
#include <maths.h>
#include <error.h>

Value add_value(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "+", argc, 2);
  ValueType ty = get_type(args[1]);
  ASSERT_TYPE(mod, "+", args[0], ty);

  switch (ty) {
    case TYPE_INTEGER: 
      return MAKE_INTEGER(GET_INT(args[0]) + GET_INT(args[1]));
    
    case TYPE_FLOAT: {
      double f = GET_FLOAT(args[0]) + GET_FLOAT(args[1]);
      return MAKE_FLOAT(f);
    }
    
    case TYPE_STRING: {
      char* s = malloc(strlen(GET_STRING(args[0])) + strlen(GET_STRING(args[1])) + 1);
      strcpy(s, GET_STRING(args[0]));
      strcat(s, GET_STRING(args[1]));
      return MAKE_STRING(mod, s);
    }

    case TYPE_LIST: {
      HeapValue* l1 = GET_PTR(args[0]);
      HeapValue* l2 = GET_PTR(args[1]);
      Value* list = malloc((l1->length + l2->length) * sizeof(Value));
      for (uint32_t i = 0; i < l1->length; i++) list[i] = l1->as_ptr[i];
      for (uint32_t i = 0; i < l2->length; i++) list[l1->length + i] = l2->as_ptr[i];
      return MAKE_LIST(mod, list, l1->length + l2->length);
    }

    default: 
      THROW(mod, "Unsupported type for +");
  }
}

Value sub_value(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "-", argc, 2);
  ValueType ty = get_type(args[1]);
  ASSERT_TYPE(mod, "-", args[0], ty);

  switch (ty) {
    case TYPE_INTEGER: 
      return MAKE_INTEGER(GET_INT(args[0]) - GET_INT(args[1]));
    
    case TYPE_FLOAT: {
      double f = GET_FLOAT(args[0]) - GET_FLOAT(args[1]);
      return MAKE_FLOAT(f);
    }

    default: 
      THROW(mod, "Unsupported type for -");
  }
}

Value mul_value(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "*", argc, 2);
  ValueType ty = get_type(args[1]);
  ASSERT_TYPE(mod, "*", args[0], ty);

  switch (ty) {
    case TYPE_INTEGER: 
      return MAKE_INTEGER(GET_INT(args[0]) * GET_INT(args[1]));
    
    case TYPE_FLOAT: {
      double f = GET_FLOAT(args[0]) * GET_FLOAT(args[1]);
      return MAKE_FLOAT(f);
    }

    default: 
      THROW(mod, "Unsupported type for *");
  }
}

Value div_value(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "/", argc, 2);
  ValueType ty = get_type(args[1]);
  ASSERT_TYPE(mod, "/", args[0], ty);

  switch (ty) {
    case TYPE_INTEGER: 
      return MAKE_INTEGER(GET_INT(args[0]) / GET_INT(args[1]));
    
    case TYPE_FLOAT: {
      double f = GET_FLOAT(args[0]) / GET_FLOAT(args[1]);
      return MAKE_FLOAT(f);
    }

    default: 
      THROW(mod, "Unsupported type for /");
  }
}

Value eq_value(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "==", argc, 2);
  ValueType ty = get_type(args[1]);
  ASSERT_TYPE(mod, "==", args[0], ty);

  switch (ty) {
    case TYPE_INTEGER: 
      return MAKE_INTEGER(GET_INT(args[0]) == GET_INT(args[1]));
    
    case TYPE_FLOAT: 
      return MAKE_INTEGER(GET_FLOAT(args[0]) == GET_FLOAT(args[1]));
    
    case TYPE_STRING: 
      return MAKE_INTEGER(strcmp(GET_STRING(args[0]), GET_STRING(args[1])) == 0);
    
    case TYPE_LIST: {
      HeapValue* l1 = GET_PTR(args[0]);
      HeapValue* l2 = GET_PTR(args[1]);
      if (l1->length != l2->length) return MAKE_INTEGER(0);
      for (uint32_t i = 0; i < l1->length; i++) {
        if (!value_eq(mod, l1->as_ptr[i], l2->as_ptr[i])) return MAKE_INTEGER(0);
      }
      return MAKE_INTEGER(1);
    }

    default: return MAKE_INTEGER(args[0] == args[1]);
  }
}

Value lt_value(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "<", argc, 2);
  ValueType ty = get_type(args[1]);
  ASSERT_TYPE(mod, "<", args[0], ty);

  switch (ty) {
    case TYPE_INTEGER: 
      return MAKE_INTEGER(GET_INT(args[0]) < GET_INT(args[1]));
    
    case TYPE_FLOAT: 
      return MAKE_INTEGER(GET_FLOAT(args[0]) < GET_FLOAT(args[1]));

    default:
      THROW(mod, "Unsupported type for <");
  }
}

Value gt_value(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, ">", argc, 2);
  ValueType ty = get_type(args[1]);
  ASSERT_TYPE(mod, ">", args[0], ty);

  switch (ty) {
    case TYPE_INTEGER: 
      return MAKE_INTEGER(GET_INT(args[0]) > GET_INT(args[1]));
    
    case TYPE_FLOAT: 
      return MAKE_INTEGER(GET_FLOAT(args[0]) > GET_FLOAT(args[1]));
    
    default:
      THROW(mod, "Unsupported type for >");
  }
}

Value lte_value(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "<=", argc, 2);
  ValueType ty = get_type(args[1]);
  ASSERT_TYPE(mod, "<=", args[0], ty);

  switch (ty) {
    case TYPE_INTEGER: 
      return MAKE_INTEGER(GET_INT(args[0]) <= GET_INT(args[1]));
    
    case TYPE_FLOAT: 
      return MAKE_INTEGER(GET_FLOAT(args[0]) <= GET_FLOAT(args[1]));

    default:
      THROW(mod, "Unsupported type for <=");
  }
}

Value gte_value(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, ">=", argc, 2);
  ValueType ty = get_type(args[1]);
  ASSERT_TYPE(mod, ">=", args[0], ty);

  switch (ty) {
    case TYPE_INTEGER: 
      return MAKE_INTEGER(GET_INT(args[0]) >= GET_INT(args[1]));
    
    case TYPE_FLOAT: 
      return MAKE_INTEGER(GET_FLOAT(args[0]) >= GET_FLOAT(args[1]));
    
    default:
      THROW(mod, "Unsupported type for >=");
  }
}

Value and_value(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "&&", argc, 2);
  ValueType ty = get_type(args[1]);
  ASSERT_TYPE(mod, "&&", args[0], ty);

  switch (ty) {
    case TYPE_INTEGER: 
      return MAKE_INTEGER(GET_INT(args[0]) && GET_INT(args[1]));
    
    default:
      THROW(mod, "Unsupported type for &&");
  }
}

Value or_value(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "||", argc, 2);
  ValueType ty = get_type(args[1]);
  ASSERT_TYPE(mod, "||", args[0], ty);

  switch (ty) {
    case TYPE_INTEGER: 
      return MAKE_INTEGER(GET_INT(args[0]) || GET_INT(args[1]));
    
    default:
      THROW(mod, "Unsupported type for ||");
  }
}

Value not_value(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "not", argc, 1);
  ValueType ty = get_type(args[0]);
  ASSERT_TYPE(mod, "not", args[0], ty);

  switch (ty) {
    case TYPE_INTEGER: 
      return MAKE_INTEGER(!GET_INT(args[0]));
    
    default:
      THROW(mod, "Unsupported type for not");
  }
}