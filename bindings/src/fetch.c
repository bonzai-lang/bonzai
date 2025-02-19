#include <fetch.h>
#include <error.h>
#include <curl/curl.h>
#include <values.h>

struct MemoryStruct {
  char* headers;
  size_t headers_size;

  char *memory;
  size_t size;
  Module* mod;
};

size_t WriteMemoryCallback(void *contents, size_t size, size_t nmemb, void *userp) {
  size_t realsize = size * nmemb;
  struct MemoryStruct *mem = (struct MemoryStruct *)userp;

  mem->memory = realloc(mem->memory, mem->size + realsize + 1);
  if (mem->memory == NULL) {
    THROW(mem->mod, "Not enough memory (realloc returned NULL)");
  }

  memcpy(&(mem->memory[mem->size]), contents, realsize);
  mem->size += realsize;
  mem->memory[mem->size] = 0;

  return realsize;
}

size_t write_headers_callback(void *ptr, size_t size, size_t nmemb, void *data) {
    size_t total_size = size * nmemb;
    struct MemoryStruct *resp = (struct MemoryStruct *)data;

    // Reallocate memory to hold the new headers
    char *new_headers = realloc(resp->headers, resp->headers_size + total_size + 1);
    if (new_headers == NULL) {
        fprintf(stderr, "Failed to allocate memory for headers\n");
        return 0;
    }
    resp->headers = new_headers;

    // Copy the new header data to the headers buffer
    memcpy(resp->headers + resp->headers_size, ptr, total_size);
    resp->headers_size += total_size;
    resp->headers[resp->headers_size] = '\0';  // Null-terminate

    return total_size;
}


// Fetch is a native function that fetches a URL and returns the content as a string.
Value fetch(Module* module, Value *args, int argc) {
  ASSERT_ARGC(module, "fetch", argc, 1);
  ASSERT_TYPE(module, "fetch", args[0], TYPE_STRING);

  CURL *curl;
  CURLcode res;
  char *url = GET_STRING(args[0]);

  curl_global_sslset(CURLSSLBACKEND_OPENSSL, NULL, NULL);

  curl = curl_easy_init();
  if (curl) {
    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);

    // Write the response to a string
    struct MemoryStruct chunk;
    chunk.memory = malloc(1);
    chunk.size = 0;
    chunk.headers = malloc(1);
    chunk.headers_size = 0;
    chunk.mod = module;

    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteMemoryCallback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&chunk);
    curl_easy_setopt(curl, CURLOPT_HEADERFUNCTION, write_headers_callback);
    curl_easy_setopt(curl, CURLOPT_HEADERDATA, &chunk);

    res = curl_easy_perform(curl);
    if (res != CURLE_OK) {
      char* error = strdup(curl_easy_strerror(res));

      free(chunk.memory);
      curl_easy_cleanup(curl);
      return throwable_error(module, error);
    }

    curl_easy_cleanup(curl);

    return throwable_ok(module, make_tuple(
      module,
      MAKE_STRING(module, chunk.memory),
      MAKE_STRING(module, chunk.headers)
    ));
  }
  
  curl_easy_cleanup(curl);
  return throwable_error(module, "Failed to initialize curl");
}

Value fetch_with(Module* module, Value* args, int argc) {
  ASSERT_ARGC(module, "fetch_with", argc, 2);
  ASSERT_TYPE(module, "fetch_with", args[0], TYPE_STRING);
  ASSERT_TYPE(module, "fetch_with", args[1], TYPE_STRING);

  CURL *curl;
  CURLcode res;
  char *url = GET_STRING(args[0]);
  char* headers = GET_STRING(args[1]);

  curl = curl_easy_init();

  if (curl) {
    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);

    struct curl_slist *chunk = NULL;
    chunk = curl_slist_append(chunk, headers);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, chunk);

    // Write the response to a string
    struct MemoryStruct chunk_;
    chunk_.memory = malloc(1);
    chunk_.size = 0;
    chunk_.headers = malloc(1);
    chunk_.headers_size = 0;
    chunk_.mod = module;

    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteMemoryCallback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&chunk_);
    curl_easy_setopt(curl, CURLOPT_HEADERFUNCTION, write_headers_callback);
    curl_easy_setopt(curl, CURLOPT_HEADERDATA, &chunk_);

    res = curl_easy_perform(curl);
    if (res != CURLE_OK) {
      char* error = strdup(curl_easy_strerror(res));

      free(chunk_.memory);
      curl_easy_cleanup(curl);
      return throwable_error(module, error);
    }

    curl_easy_cleanup(curl);

    return throwable_ok(module, make_tuple(
      module,
      MAKE_STRING(module, chunk_.memory),
      MAKE_STRING(module, chunk_.headers)
    ));
  }

  return throwable_error(module, "Failed to initialize curl");
}

Value post_data(Module* module, Value* args, int argc) {
  ASSERT_ARGC(module, "post_data", argc, 3);
  ASSERT_TYPE(module, "post_data", args[0], TYPE_STRING);
  ASSERT_TYPE(module, "post_data", args[1], TYPE_STRING);
  ASSERT_TYPE(module, "post_data", args[2], TYPE_STRING);

  CURL *curl;
  CURLcode res;
  char *url = GET_STRING(args[0]);
  char *data = GET_STRING(args[1]);
  char *headers = GET_STRING(args[2]);

  curl = curl_easy_init();

  if (curl) {
    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, data);

    struct curl_slist *chunk = NULL;
    chunk = curl_slist_append(chunk, headers);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, chunk);

    // Write the response to a string
    struct MemoryStruct chunk_;
    chunk_.memory = malloc(1);
    chunk_.size = 0;
    chunk_.headers = malloc(1);
    chunk_.headers_size = 0;
    chunk_.mod = module;

    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteMemoryCallback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&chunk_);
    curl_easy_setopt(curl, CURLOPT_HEADERFUNCTION, write_headers_callback);
    curl_easy_setopt(curl, CURLOPT_HEADERDATA, &chunk_);

    res = curl_easy_perform(curl);
    if (res != CURLE_OK) {
      char* error = strdup(curl_easy_strerror(res));

      free(chunk_.memory);
      curl_easy_cleanup(curl);
      return throwable_error(module, error);
    }

    curl_easy_cleanup(curl);

    return throwable_ok(module, make_tuple(
      module,
      MAKE_STRING(module, chunk_.memory),
      MAKE_STRING(module, chunk_.headers)
    ));
  }

  return throwable_error(module, "Failed to initialize curl");
}