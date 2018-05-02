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
		(**current).element = va_arg(args, int*);
		current = &((**current).next);
	}
	va_end(args);
	return begin;
}

struct list* list_cons(void* hd, struct list* tl) {
	struct list* new_list = malloc(sizeof(struct list));
	new_list->element = hd;
	new_list->next = tl;
	return new_list;
}

void* list_car(struct list* lst) {
	return lst->element;
}

struct list* list_cdr(struct list* lst) {
	return lst->next;
}

struct list* list_append(struct list* l1, struct list* l2) {
	struct list** current = &l1;
	while (*current) {
		current = &((**current).next);
	}
	*current = l2;
	return l1;
}
