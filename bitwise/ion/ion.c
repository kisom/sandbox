#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef RELEASE
#define tprint(...)	do { fprintf(stderr, __VA_ARGS__); } while (0)
#else
#define tprint(...)	do {} while (0)
#endif
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

#define prlu(m, v)	(tprint("%s: %lu\n", m, (unsigned long)(v)))

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

	tprint("buf_test\n");
	for (int i = 0; i < 1024; i++) {
		buf_push(stretchy, i);
	}

	for (int i = 0; i < buf_len(stretchy); i++) {
		assert(stretchy[i] == i);
	}

	buf_free(stretchy);
	tprint("OK\n");
}
typedef enum TokenKind {
	TOKEN_INT = 128,
	TOKEN_NAME,
	// ...
} TokenKind;

typedef struct Token {
	TokenKind	kind;
	union {
		uint64_t	val;
		struct {
			const char *start;
			const char *end;
		};
	};
	// ...
} Token;

const char *stream;

Token token;
void next_token(void) {
	switch (*stream) {
	/* Numeric literal */
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9': {
		uint64_t val = 0; /* reset the token's value */
		while (isdigit(*stream)) {
			val *= 10;
			val += (*stream - '0');
			stream++;
		}
		token.kind = TOKEN_INT;
		token.val = val;
		break;
	}

	/* Identifier */
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'f':
	case 'g':
	case 'h':
	case 'i':
	case 'j':
	case 'k':
	case 'l':
	case 'm':
	case 'n':
	case 'o':
	case 'p':
	case 'q':
	case 'r':
	case 's':
	case 't':
	case 'u':
	case 'v':
	case 'w':
	case 'x':
	case 'y':
	case 'z':
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'F':
	case 'G':
	case 'H':
	case 'I':
	case 'J':
	case 'K':
	case 'L':
	case 'M':
	case 'N':
	case 'O':
	case 'P':
	case 'Q':
	case 'R':
	case 'S':
	case 'T':
	case 'U':
	case 'V':
	case 'W':
	case 'X':
	case 'Y':
	case 'Z':
	case '_': {
		const char *start = stream++;
		while (isalnum(*stream) || *stream == '_') {
			stream++;
		}

		token.kind = TOKEN_NAME;
		token.start = start;
		token.end = stream;
		break;
	}
	default:
		token.kind = *stream++;
		break;
	}
}

void
print_token(void)
{
	tprint("TOKEN: ");
		switch (token.kind) {
		case TOKEN_INT:
			tprint("INT VALUE: %lu", token.val);
			break;
		case TOKEN_NAME:
			tprint("NAME VALUE: %.*s", (int)(token.end-token.start), token.start);
			break;
		default:
			tprint("'%c'", token.kind);
			break;
		}
	tprint("\n");
}

void
lex_test(void)
{
	char source[] = "+()123456+IDDQD,994_id3kfa";
	tprint("lex_test\n");
	stream = source;
	next_token();
	while (token.kind) {
		print_token();
		next_token();
	}
	tprint("OK\n");
}

int
main(int argc, char *argv[])
{
	buf_test();
	lex_test();
	return 0;
}