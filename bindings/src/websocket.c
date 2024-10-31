#include <arpa/inet.h>
#include <error.h>
#include <module.h>
#include <netdb.h>
#include <openssl/bio.h>
#include <openssl/buffer.h>
#include <openssl/evp.h>
#include <openssl/rand.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/select.h>
#include <time.h>
#include <unistd.h>
#include <value.h>
#include <websocket.h>

#define BUFFER_SIZE 4096

// Generate a base64-encoded 16-byte random key for Sec-WebSocket-Key
char* generate_websocket_key() {
  unsigned char random_bytes[16];
  if (RAND_bytes(random_bytes, sizeof(random_bytes)) != 1) {
    fprintf(stderr, "Error generating random bytes\n");
    return NULL;
  }

  // Base64 encode the random bytes
  BIO *bio, *b64;
  BUF_MEM* buffer_ptr;

  b64 = BIO_new(BIO_f_base64());
  bio = BIO_new(BIO_s_mem());
  BIO_push(b64, bio);
  BIO_set_flags(b64, BIO_FLAGS_BASE64_NO_NL);  // Ignore newlines

  BIO_write(b64, random_bytes, sizeof(random_bytes));
  BIO_flush(b64);
  BIO_get_mem_ptr(b64, &buffer_ptr);

  char* base64_key = (char*)malloc((buffer_ptr->length + 1) * sizeof(char));
  memcpy(base64_key, buffer_ptr->data, buffer_ptr->length);
  base64_key[buffer_ptr->length] = '\0';

  BIO_free_all(b64);
  return base64_key;
}

void perform_websocket_handshake(int sockfd, char* server_address,
                                 int server_port, char* server_path) {
  char buffer[BUFFER_SIZE];
  char* websocket_key = generate_websocket_key();

  // Send handshake request
  snprintf(buffer, sizeof(buffer),
           "GET %s HTTP/1.1\r\n"
           "Host: %s\r\n"
           "Upgrade: websocket\r\n"
           "Connection: Upgrade\r\n"
           "Sec-WebSocket-Key: %s\r\n"
           "Sec-WebSocket-Version: 13\r\n\r\n",
           server_path, server_address, websocket_key);

  if (send(sockfd, buffer, strlen(buffer), 0) < 0) {
    perror("Error sending handshake");
    exit(EXIT_FAILURE);
  }

  // Receive server response
  if (recv(sockfd, buffer, sizeof(buffer), 0) < 0) {
    perror("Error receiving handshake response");
    exit(EXIT_FAILURE);
  }
}

int init_websocket(char* server_address, int server_port) {
  struct sockaddr_in server_addr;
  struct hostent* server;

  // Resolve hostname
  server = gethostbyname(server_address);
  if (!server) {
    fprintf(stderr, "Error resolving hostname\n");
    return 1;
  }

  // Create socket
  int sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd < 0) {
    perror("Socket creation failed");
    return 1;
  }

  // Configure server address struct
  memset(&server_addr, 0, sizeof(server_addr));
  server_addr.sin_family = AF_INET;
  server_addr.sin_port = htons(server_port);
  memcpy(&server_addr.sin_addr.s_addr, server->h_addr_list[0],
         server->h_length);

  // Connect to the server
  if (connect(sockfd, (struct sockaddr*)&server_addr, sizeof(server_addr)) <
      0) {
    perror("Connection failed");
    close(sockfd);
    return 1;
  }

  return sockfd;
}

void send_text_frame(int sockfd, const char* message) {
  size_t message_len = strlen(message);
  unsigned char frame[2 + BUFFER_SIZE];
  int frame_size = 0;

  // Frame header
  frame[0] = 0x81;  // FIN bit set, text frame opcode (0x1)
  if (message_len <= 125) {
    frame[1] = 0x80 | message_len;  // Masked, length <= 125
    frame_size = 2;
  } else if (message_len <= 65535) {
    frame[1] = 0x80 | 126;  // Masked, 2-byte length
    frame[2] = (message_len >> 8) & 0xFF;
    frame[3] = message_len & 0xFF;
    frame_size = 4;
  } else {
    fprintf(stderr, "Message too long\n");
    return;
  }

  // Mask key
  unsigned char mask_key[4];
  srand((unsigned int)time(NULL));
  for (int i = 0; i < 4; i++) {
    mask_key[i] = rand() % 256;
  }
  memcpy(frame + frame_size, mask_key, 4);
  frame_size += 4;

  // Masked payload
  for (size_t i = 0; i < message_len; i++) {
    frame[frame_size + i] = message[i] ^ mask_key[i % 4];
  }
  frame_size += message_len;

  // Send the frame
  if (send(sockfd, frame, frame_size, 0) < 0) {
    perror("Error sending frame");
  }
}

char* receive_frame(int sockfd) {
  unsigned char buffer[BUFFER_SIZE];
  int bytes_received = recv(sockfd, buffer, sizeof(buffer), 0);
  if (bytes_received < 0) {
    perror("Error receiving frame");
    return NULL;
  }

  // Check the opcode and extract the payload length
  unsigned char opcode = buffer[0] & 0x0F;
  size_t payload_len = buffer[1] & 0x7F;
  size_t offset = 2;

  if (payload_len == 126) {
    payload_len = (buffer[2] << 8) | buffer[3];
    offset += 2;
  }

  // Unmask the payload if needed
  if (buffer[1] & 0x80) {
    unsigned char mask_key[4];
    memcpy(mask_key, buffer + offset, 4);
    offset += 4;

    for (size_t i = 0; i < payload_len; i++) {
      buffer[offset + i] ^= mask_key[i % 4];
    }
  }

  char* res = (char*)malloc(payload_len + 1);
  memcpy(res, buffer + offset, payload_len);
  res[payload_len] = '\0';

  return res;
}

Value initWebsocket(Module* module, Value* args, int argc) {
  ASSERT_ARGC(module, "initWebsocket", argc, 2);
  ASSERT_TYPE(module, "initWebsocket", args[0], TYPE_STRING);
  ASSERT_TYPE(module, "initWebsocket", args[1], TYPE_INTEGER);

  char* server_address = GET_STRING(args[0]);
  int server_port = GET_INT(args[1]);

  int sockfd = init_websocket(server_address, server_port);

  return MAKE_INTEGER(sockfd);
}

Value sendTextFrame(Module* module, Value* args, int argc) {
  ASSERT_ARGC(module, "sendTextFrame", argc, 2);
  ASSERT_TYPE(module, "sendTextFrame", args[0], TYPE_INTEGER);
  ASSERT_TYPE(module, "sendTextFrame", args[1], TYPE_STRING);

  int sockfd = GET_INT(args[0]);
  const char* message = GET_STRING(args[1]);

  send_text_frame(sockfd, message);

  return MAKE_INTEGER(0);
}

Value receiveFrame(Module* module, Value* args, int argc) {
  ASSERT_ARGC(module, "receiveFrame", argc, 1);
  ASSERT_TYPE(module, "receiveFrame", args[0], TYPE_INTEGER);

  int sockfd = GET_INT(args[0]);
  char* res = receive_frame(sockfd);

  return MAKE_STRING(module, res);
}

Value closeWebsocket(Module* module, Value* args, int argc) {
  ASSERT_ARGC(module, "closeWebsocket", argc, 1);
  ASSERT_TYPE(module, "closeWebsocket", args[0], TYPE_INTEGER);

  int sockfd = GET_INT(args[0]);
  close(sockfd);

  return MAKE_INTEGER(0);
}

Value performHandshake(Module* module, Value* args, int argc) {
  ASSERT_ARGC(module, "performHandshake", argc, 4);
  ASSERT_TYPE(module, "performHandshake", args[0], TYPE_INTEGER);
  ASSERT_TYPE(module, "performHandshake", args[1], TYPE_STRING);
  ASSERT_TYPE(module, "performHandshake", args[2], TYPE_STRING);
  ASSERT_TYPE(module, "performHandshake", args[3], TYPE_INTEGER);

  int sockfd = GET_INT(args[0]);
  char* server_path = GET_STRING(args[1]);
  char* server_address = GET_STRING(args[2]);
  int server_port = GET_INT(args[3]);

  perform_websocket_handshake(sockfd, server_address, server_port, server_path);

  return MAKE_INTEGER(0);
}