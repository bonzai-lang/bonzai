#ifndef HTTP_H
#define HTTP_H

#include <module.h>

Value start_http_server(Module *module, int port);
Value accept_request(Module *module, int server_socket);
Value close_client(Module *module, int client_socket);
Value get_buffer(Module *module, int client_socket);
Value send_buffer(Module *module, int client_socket, Value buffer);
Value close_server(Module *module, int server_socket);

#endif  // HTTP_H