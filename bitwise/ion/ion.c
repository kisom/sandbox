#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX(a, b) a < b ? b : a

// stretchy buffers
typedef struct _BufHeader {
	size_t	len;
	size_t	cap;
	char	buf[0]; // C99
} BufHeader;

#define buf__hdr(b)	(BufHeader *)((char *)buf - offsetof(BufHeader, buf))
#define buf_fits(b, n)	((buf_len(b) + n) <= buf_cap(b))
#define buf__fit(b, n) (buf__fits(b, n) ? 0 : (b) = (buf_grow(b, 2*buf_cap(b), sizeof(*b))))

#define buf_len(b)	((b) ? buf__hdr(b)->len : 0)
#define buf_cap(b)	((b) ? buf__hdr(b)->cap : 0)
#define buf_push(b, x)	(buf__fit((b), 1), (b)[buf_len(b)++] = (x))

void *
buf__grow(const void *buf, size_t nlen, size_t esize)
{
	size_t ncap = MAX(1 + 2*buf_cap(buf), new_len);
	assert(nlen <= ncap);
	size_t nsize = ncap * esize;
	BufHdr *nhdr;
	
	if (buf) {
		nhdr = realloc(buf__hdr(buf), nsize)
	}
	else {
		nhdr = malloc(new_size)
		nhdr->len = 0;
	}

	nhdr.cap = ncap;
	return nhdr;
}

int
main(int argc, char *argv[])
{
	int	*buf = NULL;
	buf_push(buf, 47); // std::vector::push_back
	// now buf actually points to a buffer
	buf_push(buf, 1234);

	printf("buf length: %lu / cap: %lu\n",
		(unsigned long)buf_len(buf),
		(unsigned long)buf_cap(buf));
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