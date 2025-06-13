#ifndef HASHTABLE_H
#define HASHTABLE_H

#include <stdint.h>
#include <stdbool.h>

typedef uint64_t Value;
#define HASHTABLE_SIZE 32
struct Module;

struct HashNode {
  const char* key;
  Value value;
  struct HashNode* next;
};

struct HashTable {
  struct HashNode* nodes[HASHTABLE_SIZE];
  uint32_t size;
  uint32_t count;
};

static uint32_t hash(const char* key, int count);
void hash_set(struct HashTable* ht, char* key, Value value, int key_length);
Value hash_get(struct Module* mod, struct HashTable* ht, char* key,
               int key_length);
Value hash_concat(struct Module* module, Value a, Value b);
bool hash_eq(struct Module* mod, Value a, Value b);
struct HashTable hash_init();

#endif // HASHTABLE_H