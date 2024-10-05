#ifndef DESERIALIZER_H
#define DESERIALIZER_H

#include <module.h>
#include <stdio.h>

void deserialize(Module *mod, FILE *file);
void free_constant(Value v);

#endif  // DESERIALIZER_H