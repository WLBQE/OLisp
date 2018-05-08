#include <stdarg.h>
#include <stdlib.h>

struct list {
	void* element;
	struct list* next;
};

struct list* make_list(int len, ...) {
	va_list args;
	struct list* begin = NULL;
	struct list** current = &begin;
	va_start(args, len);
	for (int i = 0; i < len; ++i) {
		*current = malloc(sizeof(struct list));
		(**current).element = va_arg(args, void*);
		current = &((**current).next);
		*current = NULL;
	}
	va_end(args);
	return begin;
}

struct list* cons(void* hd, struct list* tl) {
	struct list* new_node = malloc(sizeof(struct list));
	new_node->element = hd;
	new_node->next = tl;
	return new_node;
}

void* car(struct list* lst) {
	return lst->element;
}

struct list* cdr(struct list* lst) {
	return lst->next;
}

struct list* append(int len, ...) {
	va_list args;
	struct list* begin = NULL;
	struct list** current = &begin;
	va_start(args, len);
	for (int i = 0; i < len - 1; ++i) {
		struct list* l = va_arg(args, struct list*);
		while (l) {
			*current = malloc(sizeof(struct list));
			(**current).element = l->element;
			current = &((**current).next);
			*current = NULL;
			l = l->next;
		}
	}
	*current = va_arg(args, struct list*);
	va_end(args);
	return begin;
}
