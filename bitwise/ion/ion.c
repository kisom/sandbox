#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>



// stretchy buffers
static size_t
buf_len(void *buf)
{
	return *(size_t *)(buf - (2 * sizeof(size_t)));
}

static void
buf_set_len(void *buf, size_t len)
{
	*(size_t *)(buf - (2 * sizeof(size_t))) = len;
}

static size_t
buf_cap(void *buf)
{
	return *(size_t *)(buf - sizeof(size_t));
}

static size_t
buf_set_cap(void *buf, size_t cap)
{
	*(size_t *)(buf - sizeof(size_t)) = cap;
}

static const size_t buf_overhead = 2 * sizeof(size_t);

static void
buf_allocate(void **buf, size_t nelem, size_t size)
{
	size_t	len = 0, cap = nelem * size;
	if (*buf == NULL) {
		*buf = malloc(buf_overhead + cap);
		*buf += buf_overhead;
	}
	else {
		len = buf_len(*buf);
		assert(len < cap);
		void *new_buf = malloc(buf_overhead + cap);
		new_buf += buf_overhead;
		memcpy(new_buf, *buf, cap);
		free(*buf);
		*buf = new_buf;
	}

	buf_set_len(*buf, len);
	buf_set_cap(*buf, cap);
}

static bool
buf_must_grow(void *buf)
{
	return (buf == NULL) || (buf_len(buf) == buf_cap(buf));
}

static const size_t buf_initial_length = 32;

#define buf_push(buf, x)	{ \
	if ((void *)buf == NULL) { buf_allocate((void **)&(buf), buf_initial_length, sizeof((x))); } \
	if (buf_must_grow((void *)(buf))) { buf_allocate((void **)&(buf), 2 * buf_cap((void *)(buf)), 1); } \
	(buf)[buf_len((buf))] = (x); \
	buf_set_len((void *)(buf), buf_len((void *)(buf)) + 1); \
} \

static void
buf_free(void *buf)
{
	buf = (void *)buf - buf_overhead;
	free(buf);
}

/*
 * 1. NULL? → allocate with default size (32?)
 * 2. 
 */

int
main(int argc, char *argv[])
{
	int	*buf = NULL;
	buf_push(buf, 47); // std::vector::push_back
	// now buf actually points to a buffer
	buf_push(buf, 1234);

	printf("buf length: %lu / cap: %lu\n", buf_len(buf), buf_cap(buf));
	for (int i = 0; i < buf_len(buf); i++) {
		printf("%d\n", buf[i]);
	}

	for (int i = 0; i < 1024; i++) {
		buf_push(buf, i+1);
	}
	printf("buf length: %lu / cap: %lu\n", buf_len(buf), buf_cap(buf));

	buf_free(buf);
	return 0;
}