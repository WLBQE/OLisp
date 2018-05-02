#include <stdarg.h>
#include <stdlib.h>

struct list {
	void* element;
	struct list* next;
};

void* make_list(int len, ...) {
	va_list args;
	struct list* begin = NULL;
	struct list** now = &begin;
	va_start(args, len);
	for (int i = 0; i < len; ++i) {
		*now = malloc(sizeof(struct list));
		(**now).element = va_arg(args, int*);
		now = &((**now).next);
	}
	va_end(args);
	return begin;
}
