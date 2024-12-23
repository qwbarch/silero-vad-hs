#ifndef LINKED_LIST_H
#define LINKED_LIST_H

#include "stdlib.h"

struct Node {
  struct Node *next_node;
  void *value;
};

/** A singly-linked list. */
struct LinkedList {
  struct Node *head_node;
  int length;
};

struct LinkedList *create_list();

/**
 * Free the list, as well all values it holds.
 * The callback should release any allocated memory that the value holds.
 * The callback must handle freeing the value pointer as well.
 */
void release_list(struct LinkedList *list, void (*release_value)(void*));

/** Push the value to the front of the list. */
void push_list(struct LinkedList *list, void *value);

/**
 * Remove the value at the front of the list, freeing its memory.
 */
void *pop_list(struct LinkedList *list);

void reverse_list(struct LinkedList *list);

#endif
