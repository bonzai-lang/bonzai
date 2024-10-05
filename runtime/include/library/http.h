#ifndef HTTP_H
#define HTTP_H

#include <module.h>

Value start_http_server(Module *module, int port);
Value accept_request(Module *module, Value request);

#endif  // HTTP_H