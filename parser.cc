#include "defs.h"
#include "parser.h"

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

	return memcmp(a, b, alen) == 0;
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