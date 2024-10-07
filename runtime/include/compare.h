#ifndef COMPARE_H
#define COMPARE_H

#include <value.h>
#include <module.h>

typedef Value (*ComparisonFun)(Module*, Value, Value);

Value compare_eq(Module* mod, Value a, Value b);
Value compare_and(Module* mod, Value a, Value b);
Value compare_or(Module* mod, Value a, Value b);
Value compare_gt(Module* mod, Value a, Value b);

ComparisonFun comparison_table[7];

#endif