#ifndef COMPARE_H
#define COMPARE_H

#include <value.h>

typedef Value (*ComparisonFun)(Value, Value);

Value compare_eq(Value a, Value b);
Value compare_and(Value a, Value b);
Value compare_or(Value a, Value b);
Value compare_gt(Value a, Value b);

ComparisonFun comparison_table[7];

#endif