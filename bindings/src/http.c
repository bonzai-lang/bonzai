#include <http.h>
#include <value.h>
#include <error.h>

#ifdef _WIN32
#include <winsock2.h>
#include <ws2tcpip.h>
#pragma comment(lib, "ws2_32.lib")
#else
#include <arpa/inet.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#endif

#define BUFFER_SIZE 1024

Value start_http_server(Module *module, Value* args, int argc) {
  ASSERT_ARGC(module, "start_http_server", argc, 1);
  ASSERT_TYPE(module, "start_http_server", args[0], TYPE_INTEGER);

  int port = GET_INT(args[0]);
  int server_socket;
  struct sockaddr_in server_addr;

  server_socket = socket(AF_INET, SOCK_STREAM, 0);
  if (server_socket < 0) { THROW(module, "Socket creation failed"); }

  memset(&server_addr, 0, sizeof(server_addr));
  server_addr.sin_family = AF_INET;
  server_addr.sin_addr.s_addr = INADDR_ANY;
  server_addr.sin_port = htons(port);

  if (bind(server_socket, (struct sockaddr*)&server_addr, sizeof(server_addr)) <
      0) {
    close(server_socket);
    THROW(module, "Bind failed");
  }

  if (listen(server_socket, 10) < 0) {
    close(server_socket);
    THROW(module, "Listen failed");
  }

  return MAKE_INTEGER(server_socket);
}

Value accept_request(Module *module, Value* args, int argc) {
  ASSERT_ARGC(module, "accept_request", argc, 1);
  ASSERT_TYPE(module, "accept_request", args[0], TYPE_INTEGER);

  int server_socket = GET_INT(args[0]);
  struct sockaddr_in client_addr;
  socklen_t client_len = sizeof(client_addr);
  int client_socket;

  client_socket = accept(server_socket, (struct sockaddr*)&client_addr, &client_len);
  
  if (client_socket < 0) {
    close(server_socket);
    THROW(module, "Accept failed");
  }

  return MAKE_INTEGER(client_socket);
}

Value close_client(Module *module, Value* args, int argc) {
  ASSERT_ARGC(module, "close_client", argc, 1);
  ASSERT_TYPE(module, "close_client", args[0], TYPE_INTEGER);

  int client_socket = GET_INT(args[0]);
  close(client_socket);

  return MAKE_INTEGER(0);
}

Value get_buffer(Module *module, Value* args, int argc) {
  ASSERT_ARGC(module, "get_buffer", argc, 1);
  ASSERT_TYPE(module, "get_buffer", args[0], TYPE_INTEGER);

  int client_socket = GET_INT(args[0]);
  HeapValue* buffer = allocate(module, TYPE_STRING);
  buffer->length = BUFFER_SIZE;
  int bytes_read = recv(client_socket, buffer->as_string, BUFFER_SIZE - 1, 0);

  if (bytes_read < 0) {
    return MAKE_STRING(module, "");
  }

  buffer->as_string[bytes_read] = '\0';

  return MAKE_PTR(buffer);
}

Value get_path(Module *module, Value* args, int argc) {
  ASSERT_ARGC(module, "get_path", argc, 1);
  ASSERT_TYPE(module, "get_path", args[0], TYPE_INTEGER);

  int client_socket = GET_INT(args[0]);

  char *buffer = malloc(BUFFER_SIZE);

  int bytes_read = recv(client_socket, buffer, BUFFER_SIZE - 1, 0);

  if (bytes_read < 0) {
    return MAKE_STRING(module, "");
  }

  buffer[bytes_read] = '\0';
  strtok(buffer, " ");

  char *path = strtok(NULL, " ");

  if (path == NULL) {
    return MAKE_STRING(module, "");
  }

  return MAKE_STRING(module, path);
}

Value send_buffer(Module *module, Value* args, int argc) {
  ASSERT_ARGC(module, "send_buffer", argc, 2);
  ASSERT_TYPE(module, "send_buffer", args[0], TYPE_INTEGER);
  ASSERT_TYPE(module, "send_buffer", args[1], TYPE_STRING);

  int client_socket = GET_INT(args[0]);
  char *buf = GET_STRING(args[1]);

  char* response = /*test*/malloc(strlen(buf) + 100);
  sprintf(response,
          "HTTP/1.1 200 OK\r\n"
          "Content-Type: text/plain\r\n"
          "Content-Length: %zu\r\n"
          "\r\n"
          "%s",
          strlen(buf), buf);

  int bytes_sent = send(client_socket, response, strlen(response), 0);

  if (bytes_sent < 0) {
    return MAKE_INTEGER(0);
  }

  free(response);

  return MAKE_INTEGER(bytes_sent);
}

Value redirect_to(Module* module, Value* args, int argc) {
  ASSERT_ARGC(module, "send_buffer", argc, 2);
  ASSERT_TYPE(module, "send_buffer", args[0], TYPE_INTEGER);
  ASSERT_TYPE(module, "send_buffer", args[1], TYPE_STRING);

  int client_socket = GET_INT(args[0]);
  char *buf = GET_STRING(args[1]);

  char* response = /*test*/malloc(strlen(buf) + 100);
  sprintf(response,
    "HTTP/1.1 302 Found\r\n"
    "Location: %s\r\n"
    "Content-Type: text/html\r\n"
    "Content-Length: 54\r\n"
    "Connection: close\r\n"
    "\r\n"
    "<html><body><h1>302 Found</h1><p>Redirecting...</p></body></html>", buf);

  int bytes_sent = send(client_socket, response, strlen(response), 0);

  if (bytes_sent < 0) {
    return MAKE_INTEGER(0);
  }

  free(response);

  return MAKE_INTEGER(bytes_sent);
}

Value send_buffer_with(Module *module, Value* args, int argc) {
  ASSERT_ARGC(module, "send_buffer", argc, 2);
  ASSERT_TYPE(module, "send_buffer", args[0], TYPE_INTEGER);
  ASSERT_TYPE(module, "send_buffer", args[1], TYPE_STRING);

  int client_socket = GET_INT(args[0]);
  char *buf = GET_STRING(args[1]);

  int bytes_sent = send(client_socket, buf, strlen(buf), 0);

  if (bytes_sent < 0) {
    return MAKE_INTEGER(0);
  }

  return MAKE_INTEGER(bytes_sent);
}

Value close_server(Module *module, Value* args, int argc) {
  ASSERT_ARGC(module, "close_server", argc, 1);
  ASSERT_TYPE(module, "close_server", args[0], TYPE_INTEGER);

  int server_socket = GET_INT(args[0]);
  close(server_socket);

  return MAKE_INTEGER(0);
}