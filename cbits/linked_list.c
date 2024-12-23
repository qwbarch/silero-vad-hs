#include "linked_list.h"

struct LinkedList *create_list() {
  struct LinkedList *list = malloc(sizeof(struct LinkedList));
  list->head_node = NULL;
  list->length = 0;
  return list;
}

void release_list(struct LinkedList *list, void (*free_value)(void *)) {
  struct Node *current_node = list->head_node;
  while (current_node) {
    struct Node *next_node = current_node->next_node;
    if (current_node->value && free_value) {
      free_value(current_node->value);
    }
    free(current_node);
    current_node = next_node;
  }
  free(list);
}

void push_list(struct LinkedList *list, void *value) {
  struct Node *next_node = list->head_node;
  list->length++;
  list->head_node = malloc(sizeof(struct Node));
  list->head_node->next_node = next_node;
  list->head_node->value = value;
}

void *pop_list(struct LinkedList *list) {
  if (!list->head_node)
    return NULL;
  void *value = list->head_node->value;
  struct Node *next_node = list->head_node->next_node;
  list->length--;
  free(list->head_node);
  list->head_node = next_node;
  return value;
}

void reverse_list(struct LinkedList *list) {
  struct Node *previous_node = NULL;
  struct Node *current_node = list->head_node;
  struct Node *next_node = NULL;

  while (current_node != NULL) {
    next_node = current_node->next_node;
    current_node->next_node = previous_node;
    previous_node = current_node;
    current_node = next_node;
  }

  list->head_node = previous_node;
}
