#include <arpa/inet.h>
#include <error.h>
#include <netdb.h>
#include <value.h>
#include <openssl/bio.h>
#include <openssl/buffer.h>
#include <openssl/err.h>
#include <openssl/rand.h>
#include <openssl/ssl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <websocket.h>

#define BUFFER_SIZE 4096

// Initialize OpenSSL library
void initialize_openssl() {
  SSL_load_error_strings();
  OpenSSL_add_ssl_algorithms();
}

// Clean up OpenSSL library
void cleanup_openssl() { EVP_cleanup(); }

SSL_CTX* create_context(Module* module) {
  const SSL_METHOD* method;
  SSL_CTX* ctx;

  method = SSLv23_client_method();  // Use SSL/TLS client method
  ctx = SSL_CTX_new(method);
  if (!ctx) {
    THROW(module, "Unable to create SSL context");
  }

  return ctx;
}

char* generate_websocket_key(Module* module) {
  unsigned char random_bytes[16];
  if (RAND_bytes(random_bytes, sizeof(random_bytes)) != 1) {
    THROW(module, "Error generating random bytes");
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

void perform_websocket_handshake(SSL *ssl, int sockfd, char *server_address,
                                 int server_port, char *server_path, int use_ssl,
                                 Module* module) {
  char buffer[BUFFER_SIZE];
  char *websocket_key = generate_websocket_key(module);
  if (!websocket_key) {
    THROW(module, "Error generating WebSocket key");
  }

  // Prepare the WebSocket handshake request
  snprintf(buffer, sizeof(buffer),
           "GET %s HTTP/1.1\r\n"
           "Host: %s\r\n"
           "Upgrade: websocket\r\n"
           "Connection: Upgrade\r\n"
           "Sec-WebSocket-Key: %s\r\n"
           "Sec-WebSocket-Version: 13\r\n\r\n",
           server_path, server_address, websocket_key);

  free(websocket_key);  // Free the generated WebSocket key

  // Send the handshake request
  int bytes_sent;
  if (use_ssl) {
    bytes_sent = SSL_write(ssl, buffer, strlen(buffer));
    if (bytes_sent <= 0) {
      THROW(module, "Error sending SSL handshake");
    }
  } else {
    bytes_sent = send(sockfd, buffer, strlen(buffer), 0);
    if (bytes_sent < 0) {
      THROW(module, "Error sending TCP handshake");
    }
  }

  // Receive the server response
  int bytes_received;
  if (use_ssl) {
    bytes_received = SSL_read(ssl, buffer, sizeof(buffer) - 1);
    if (bytes_received <= 0) {
      THROW(module, "Error receiving SSL handshake response");
    }
  } else {
    bytes_received = recv(sockfd, buffer, sizeof(buffer) - 1, 0);
    if (bytes_received < 0) {
      THROW(module, "Error receiving TCP handshake response");
    }
  }

  buffer[bytes_received] = '\0';  // Null-terminate the received response
}

SSL* init_websocket(char* server_address, int server_port, SSL_CTX* ctx, int use_ssl, int *sockfd_out, Module* module) {
  struct sockaddr_in server_addr;
  struct hostent* server;

  // Resolve hostname
  server = gethostbyname(server_address);
  if (!server) {
    THROW(module, "Error resolving hostname");
  }

  // Create socket
  int sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd < 0) {
    THROW(module, "Error creating socket");
  }

  // Configure server address struct
  memset(&server_addr, 0, sizeof(server_addr));
  server_addr.sin_family = AF_INET;
  server_addr.sin_port = htons(server_port);
  memcpy(&server_addr.sin_addr.s_addr, server->h_addr_list[0], server->h_length);

  // Connect to the server
  if (connect(sockfd, (struct sockaddr*)&server_addr, sizeof(server_addr)) < 0) {
    THROW(module, "Error connecting to server");
  }

  if (use_ssl) {
    // Initialize SSL if secure connection is requested
    SSL *ssl = SSL_new(ctx);
    if (!ssl) {
      close(sockfd);
      THROW(module, "Error creating SSL structure");
    }

    SSL_set_fd(ssl, sockfd);

    // Establish SSL connection
    if (SSL_connect(ssl) <= 0) {
      SSL_free(ssl);
      close(sockfd);

      THROW(module, "Error establishing SSL connection");
    }

    *sockfd_out = sockfd;  // Save sockfd for cleanup later
    return ssl;            // Return SSL pointer for secure connection
  } else {
    *sockfd_out = sockfd;
    return NULL;  // Return NULL for unsecure connection
  }
}

void send_text_frame(SSL* ssl, int sockfd, const char* message, int use_ssl, Module* module) {
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
    THROW(module, "Message too long");
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

  // Send the frame based on whether SSL is in use
  int bytes_sent;
  if (use_ssl) {
    bytes_sent = SSL_write(ssl, frame, frame_size);
    if (bytes_sent <= 0) {
      THROW(module, "Error sending SSL frame");
    }
  } else {
    bytes_sent = send(sockfd, frame, frame_size, 0);
    if (bytes_sent < 0) {
      THROW(module, "Error sending TCP frame");
    }
  }
}

char* receive_frame(SSL* ssl, int sockfd, int use_ssl, Module* module) {
  unsigned char buffer[BUFFER_SIZE];
  int bytes_received;

  if (use_ssl) {
    bytes_received = SSL_read(ssl, buffer, sizeof(buffer));
    if (bytes_received <= 0) {
      THROW(module, "Error receiving SSL frame");
    }
  } else {
    bytes_received = recv(sockfd, buffer, sizeof(buffer), 0);
    if (bytes_received < 0) {
      THROW(module, "Error receiving TCP frame");
    }
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
  if (!res) {
    THROW(module, "Error allocating memory for frame payload");
  }
  memcpy(res, buffer + offset, payload_len);
  res[payload_len] = '\0';

  return res;
}

void cleanup_ssl_connection(SSL* ssl) {
  SSL_shutdown(ssl);
  SSL_free(ssl);
}

struct websocket_ctx {
  SSL* ssl;
  int sockfd;
  int use_ssl;
};

Value initWebsocket(Module* module, Value* args, int argc) {
  ASSERT_ARGC(module, "initWebsocket", argc, 3);
  ASSERT_TYPE(module, "initWebsocket", args[0], TYPE_STRING);
  ASSERT_TYPE(module, "initWebsocket", args[1], TYPE_INTEGER);
  ASSERT_TYPE(module, "initWebsocket", args[2], TYPE_INTEGER);

  char* server_address = GET_STRING(args[0]);
  int server_port = GET_INT(args[1]);
  int use_ssl = GET_INT(args[2]);

  // Initialize OpenSSL
  initialize_openssl();
  SSL_CTX* ctx = create_context(module);
  int sockfd;
  SSL* ssl = init_websocket(server_address, server_port, ctx, use_ssl, &sockfd, module);

  struct websocket_ctx* ws_ctx = (struct websocket_ctx*)malloc(sizeof(struct websocket_ctx));
  ws_ctx->ssl = ssl;
  ws_ctx->sockfd = sockfd;
  ws_ctx->use_ssl = use_ssl;

  HeapValue* ssl_ptr = allocate(module, TYPE_API);
  ssl_ptr->as_any = ws_ctx;

  // Return the SSL pointer wrapped in a suitable type
  return MAKE_PTR(ssl_ptr);
}

Value sendTextFrame(Module* module, Value* args, int argc) {
  ASSERT_ARGC(module, "sendTextFrame", argc, 2);
  ASSERT_TYPE(module, "sendTextFrame", args[0], TYPE_API);
  ASSERT_TYPE(module, "sendTextFrame", args[1], TYPE_STRING);

  struct websocket_ctx* ssl = GET_PTR(args[0])->as_any;
  const char* message = GET_STRING(args[1]);

  // Use SSL_write for secure sending
  send_text_frame(ssl->ssl, ssl->sockfd, message, ssl->use_ssl, module);

  return kNull;
}

Value receiveFrame(Module* module, Value* args, int argc) {
  ASSERT_ARGC(module, "receiveFrame", argc, 1);
  ASSERT_TYPE(module, "receiveFrame", args[0], TYPE_API);

  struct websocket_ctx* ssl = GET_PTR(args[0])->as_any;
  char* res = receive_frame(ssl->ssl, ssl->sockfd, ssl->use_ssl, module);

  return MAKE_STRING(module, res);
}

Value closeWebsocket(Module* module, Value* args, int argc) {
  ASSERT_ARGC(module, "closeWebsocket", argc, 1);
  ASSERT_TYPE(module, "closeWebsocket", args[0], TYPE_API);

  SSL* ssl = GET_PTR(args[0])->as_any;

  // Gracefully shutdown SSL connection
  cleanup_ssl_connection(ssl);
  cleanup_openssl();

  free(ssl);

  return kNull;
}

Value performHandshake(Module* module, Value* args, int argc) {
  ASSERT_ARGC(module, "performHandshake", argc, 4);
  ASSERT_TYPE(module, "performHandshake", args[0], TYPE_API);
  ASSERT_TYPE(module, "performHandshake", args[1], TYPE_STRING);
  ASSERT_TYPE(module, "performHandshake", args[2], TYPE_STRING);
  ASSERT_TYPE(module, "performHandshake", args[3], TYPE_INTEGER);

  struct websocket_ctx* ssl = GET_PTR(args[0])->as_any;
  char* server_path = GET_STRING(args[1]);
  char* server_address = GET_STRING(args[2]);
  int server_port = GET_INT(args[3]);

  perform_websocket_handshake(ssl->ssl, ssl->sockfd, server_address, server_port, server_path, ssl->use_ssl, module);

  return kNull;
}