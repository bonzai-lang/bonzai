#ifndef HTTP_H
#define HTTP_H

#include <module.h>

Value start_http_server(Module *module, Value* args, int argc);
Value accept_request(Module *module, Value* args, int argc);
Value close_client(Module *module, Value* args, int argc);
Value get_buffer(Module *module, Value* args, int argc);
Value send_buffer(Module *module, Value* args, int argc);
Value close_server(Module *module, Value* args, int argc);

#endif  // HTTP_H