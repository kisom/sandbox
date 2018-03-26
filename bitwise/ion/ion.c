#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX(a, b) ((a) < (b) ? (b) : (a))

// stretchy buffers
typedef struct _BufHeader {
	size_t	len;
	size_t	cap;
	char	buf[0]; // C99
} BufHeader;

void *buf__grow(const void *, size_t, size_t);
#define buf__hdr(b)	((BufHeader *)((char *)(b) - offsetof(BufHeader, buf)))
#define buf__fits(b, n)	((buf_len(b) + n) <= buf_cap(b))
#define buf__fit(b, n) (buf__fits(b, n) ? 0 : ((b) = (buf__grow((b), buf_len(b) + (n), sizeof(*(b))))))

#define buf_len(b)	((b) ? buf__hdr(b)->len : 0)
#define buf_cap(b)	((b) ? buf__hdr(b)->cap : 0)
#define buf_push(b, x)	(buf__fit((b), 1), (b)[buf_len((b))] = (x), buf__hdr((b))->len++)
#define buf_free(b)	((b) ? free(buf__hdr((b))) : 0)

#define prlu(m, v)	(printf("%s: %lu\n", m, (unsigned long)(v)))

void *
buf__grow(const void *buf, size_t nlen, size_t esize)
{
	size_t ncap = MAX(1 + 2*buf_cap(buf), nlen);
	assert(nlen <= ncap);
	size_t nsize = offsetof(BufHeader, buf) + ncap * esize;
	BufHeader *nhdr;
	
	if (buf) {
		nhdr = realloc(buf__hdr(buf), nsize);
	}
	else {
		nhdr = malloc(nsize);
		nhdr->len = 0;
	}

	nhdr->cap = ncap;
	return nhdr->buf;
}

static void
buf_test(void)
{
	int	*stretchy = NULL;
	for (int i = 0; i < 1024; i++) {
		buf_push(stretchy, i);
	}

	for (int i = 0; i < buf_len(stretchy); i++) {
		assert(stretchy[i] == i);
	}

	buf_free(stretchy);
	printf("OK\n");
}
typedef enum TokenKind {
	TOKEN_INT = 128,
	TOKEN_NAME,
	// ...
} TokenKind;

typedef struct Token {
	TokenKind	kind;
	// ...
} Token;

const char *stream;

Token token;
void next_token(void) {
	switch (*stream) {
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
		while (isdigit(*stream)) {
			stream++;
		}
		token.kind = TOKEN_INT;
		break;
	default:
		token.kind = *stream++;
		break;
	}
}

void lex_test()
{
	char source[] = "+()123456+994";
	stream = source;
	next_token();
	while (token.kind) {
		printf("TOKEN: %d\n", token.kind);
		next_token();
	}
}

int
main(int argc, char *argv[])
{
	buf_test();
	lex_test();
	return 0;
}