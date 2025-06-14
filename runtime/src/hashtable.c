#include <stdint.h>
#include <value.h>
#include <module.h>
#include <hashtable.h>
#include <compare.h>

#define hash_func meiyan

static inline uint32_t meiyan(const char* key, int count) {
  typedef uint32_t* P;
  uint32_t h = 0x811c9dc5;
  while (count >= 8) {
    h = (h ^ ((((*(P)key) << 5) | ((*(P)key) >> 27)) ^ *(P)(key + 4))) *
        0xad3e7;
    count -= 8;
    key += 8;
  }
#define tmp                            \
  h = (h ^ *(uint16_t*)key) * 0xad3e7; \
  key += 2;
  if (count & 4) {
    tmp tmp
  }
  if (count & 2) {
    tmp
  }
  if (count & 1) {
    h = (h ^ *key) * 0xad3e7;
  }
#undef tmp
  return h ^ (h >> 16);
}

static inline uint32_t hash(const char* key, int count) {
  return meiyan(key, count) % 32;
}

struct HashTable hash_init() {
  struct HashTable ht;
  ht.size = 0;
  ht.count = 0;
  for (int i = 0; i < HASHTABLE_SIZE; i++) {
    ht.nodes[i] = NULL;
  }
  return ht;
}

void hash_set(struct HashTable* ht, char* key, Value value, int key_length) {
  uint32_t h = hash(key, key_length) % HASHTABLE_SIZE;
  struct HashNode* new_node = malloc(sizeof(struct HashNode));
  new_node->key = key;
  new_node->value = value;
  new_node->next = NULL;

  if (ht->nodes[h] == NULL) {
    ht->nodes[h] = new_node;
  } else {
    new_node->next = ht->nodes[h];
    ht->nodes[h] = new_node;
  }
  ht->count++;
}

Value hash_get(struct Module* mod, struct HashTable* ht, char* key, int key_length) {
  uint32_t h = hash(key, key_length) % HASHTABLE_SIZE;
  struct HashNode* current = ht->nodes[h];

  while (current != NULL) {
    if (strncmp(current->key, key, key_length) == 0) {
      return current->value;
    }
    current = current->next;
  }
  
  Value* none_value = malloc(3 * sizeof(Value));
  none_value[0] = kNull;
  none_value[1] = MAKE_STRING_NON_GC(mod, "Optional");
  none_value[2] = MAKE_STRING_NON_GC(mod, "None");

  return MAKE_LIST(mod, none_value, 3);
}

Value hash_concat(Module* module, Value a, Value b) {
  if (IS_EMPTY_RECORD(a)) return b;
  if (IS_EMPTY_RECORD(b)) return a;

  HeapValue* rec_a = GET_PTR(a);
  HeapValue* rec_b = GET_PTR(b);

  struct HashTable ht = hash_init();

  for (uint32_t i = 0; i < rec_a->length; i++) {
    struct HashNode* node = rec_a->as_record.nodes[i];
    while (node) {
      hash_set(&ht, node->key, node->value, strlen(node->key));
      node = node->next;
    }
  }
 
  for (uint32_t i = 0; i < rec_b->length; i++) {
    struct HashNode* node = rec_b->as_record.nodes[i];
    while (node) {
      hash_set(&ht, node->key, node->value, strlen(node->key));
      node = node->next;
    }
  }

  HeapValue* new_record = allocate(module, TYPE_RECORD);
  new_record->as_record = ht;
  new_record->length = ht.size;

  return MAKE_PTR(new_record);
}

bool hash_eq(struct Module* mod, Value a, Value b) {
  if (IS_EMPTY_RECORD(a) && IS_EMPTY_RECORD(b)) return true;
  if (IS_EMPTY_RECORD(a) || IS_EMPTY_RECORD(b)) return false;

  HeapValue* rec_a = GET_PTR(a);
  HeapValue* rec_b = GET_PTR(b);

  if (rec_a->length != rec_b->length) return false;

  for (uint32_t i = 0; i < rec_a->length; i++) {
    struct HashNode* node_a = rec_a->as_record.nodes[i];
    struct HashNode* node_b = rec_b->as_record.nodes[i];

    while (node_a && node_b) {
      if (strcmp(node_a->key, node_b->key) != 0 ||
          !compare_eq(mod, node_a->value, node_b->value)) {
        return false;
      }
      node_a = node_a->next;
      node_b = node_b->next;
    }

    if (node_a || node_b) return false; // One is longer than the other
  }

  return true;
}
