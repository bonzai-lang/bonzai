//
// Created by Thomas Vergne on 02/11/2024.
//

#ifndef SQL_H
#define SQL_H

#include <module.h>

Value psql_connect(Module* module, Value* args, int argc);
Value psql_query(Module* module, Value* args, int argc);
Value psql_close_connection(Module* module, Value* args, int argc);

#endif // SQL_H
