#include <library/http.h>
#include <value.h>
#include <gc.h>
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

Value start_http_server(Module *module, int port) {
  int server_socket;
  struct sockaddr_in server_addr;

  server_socket = socket(AF_INET, SOCK_STREAM, 0);
  if (server_socket < 0) {
    perror("Socket creation failed");
    exit(EXIT_FAILURE);
  }

  memset(&server_addr, 0, sizeof(server_addr));
  server_addr.sin_family = AF_INET;
  server_addr.sin_addr.s_addr = INADDR_ANY;
  server_addr.sin_port = htons(port);

  if (bind(server_socket, (struct sockaddr*)&server_addr, sizeof(server_addr)) <
      0) {
    perror("Bind failed");
    close(server_socket);
    exit(EXIT_FAILURE);
  }

  if (listen(server_socket, 10) < 0) {
    perror("Listen failed");
    close(server_socket);
    exit(EXIT_FAILURE);
  }

  return MAKE_INTEGER(server_socket);
}

Value accept_request(Module *module, int server_socket) {
  struct sockaddr_in client_addr;
  socklen_t client_len = sizeof(client_addr);
  int client_socket;

  client_socket = accept(server_socket, (struct sockaddr*)&client_addr, &client_len);
  
  if (client_socket < 0) {
    perror("Accept failed");
    close(server_socket);
    exit(EXIT_FAILURE);
  }

  return MAKE_INTEGER(client_socket);
}

Value close_client(Module *module, int client_socket) {
  close(client_socket);
}

Value get_buffer(Module *module, int client_socket) {
  HeapValue* buffer = allocate(module, TYPE_STRING);
  buffer->length = BUFFER_SIZE;
  int bytes_read = recv(client_socket, buffer->as_string, BUFFER_SIZE - 1, 0);

  if (bytes_read < 0) {
    perror("recv failed");
    return MAKE_STRING(module, "");
  }

  buffer->as_string[bytes_read] = '\0';

  return MAKE_PTR(buffer);
}

Value send_buffer(Module *module, int client_socket, Value buffer) {
  ASSERT_TYPE("send_buffer", buffer, TYPE_STRING);
  char *buf = GET_STRING(buffer);

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
    perror("send failed");
    return MAKE_INTEGER(0);
  }

  free(response);

  return MAKE_INTEGER(bytes_sent);
}

Value close_server(Module *module, int server_socket) {
  close(server_socket);
}