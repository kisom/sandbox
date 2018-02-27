#include "defs.h"
#include "parser.h"
#include "stack.h"

#include <ctype.h>
#include <string.h>

static void
reset(struct Token *t)
{
	t->token = nullptr;
	t->length = 0;
}

bool
match_token(const char *a, const size_t alen,
	    const char *b, const size_t blen)
{
	if (alen != blen) {
		return false;
	}

	for (size_t i = 0; i < alen; i++) {
		if (a[i] == b[i]) {
			continue;
		}

		if (!isalpha(a[i]) || !isalpha(b[i])) {
			return false;
		}
		
		if ((a[i] ^ 0x20) == b[i]) {
			continue;
		}
		
		if (a[i] == (b[i] ^ 0x20)) {
			continue;
		}
		
		return false;
	}
	return true;
}

PARSE_RESULT
parse_next(const char *buf, const size_t length, size_t *offset,
	   struct Token *token)
{
	size_t	 cursor = *offset;

	// Clear the token.
	reset(token);

	if (cursor == length) {
		return PARSE_EOB;
	}

	while (cursor <= length) {
		if (buf[cursor] != ' ') {
			if (buf[cursor] != '\t') {
				break;
			}
		}

		cursor++;
	}

	if (cursor == length) {
		return PARSE_EOB;
	}

	token->token = (char *)buf + cursor;
	while ((token->length <= MAX_TOKEN_LENGTH) && (cursor < length)) {
		if (buf[cursor] != ' ') {
			if (buf[cursor] != '\t') {
				cursor++;
				token->length++;
				continue;
			}
		}
		cursor++;
		break;
	}

	if (token->length > MAX_TOKEN_LENGTH) {
		reset(token);
		return PARSE_LEN;
	}

	*offset = cursor;
	return PARSE_OK;
}

bool
parse_num(struct Token *token, KF_INT *n)
{
	KF_INT	tmp = 0;
	uint8_t i = 0;
	bool    sign = false;

	if (token->length == 0) {
		return false;
	}

	if (token->token[i] == '-') {
		if (token->length == 1) {
			return false;
		}
		i++;
		sign = true;
	}

	while (i < token->length) {
		if (token->token[i] < '0') {
			return false;
		}

		if (token->token[i] > '9') {
			return false;
		}

		tmp *= 10;
		tmp += (uint8_t)(token->token[i] - '0');
		i++;
	}

	if (sign) {
		tmp *= -1;
	}

	*n = tmp;
	return true;
}