#ifndef __KF_PARSER_H__
#define __KF_PARSER_H__

#include "defs.h"
#include "stack.h"

struct Token {
	char	*token;
	uint8_t	 length;
};

typedef enum _PARSE_RESULT_ : uint8_t {
	PARSE_OK = 0,
	PARSE_EOB = 1, // end of buffer
	PARSE_LEN = 2, // token is too long
	PARSE_FAIL = 3 // catch-all error
} PARSE_RESULT;

bool		match_token(const char *, const size_t, const char *, const size_t);
PARSE_RESULT	parse_next(const char *, const size_t, size_t *, struct Token *);

// TODO(kyle): investigate a better return value, e.g. to differentiate between
// stack failures and parse failures.
bool		parse_num(struct Token *, KF_INT *);


#endif // __KF_PARSER_H__
