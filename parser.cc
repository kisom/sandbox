#include "defs.h"
#include "parser.h"

int
parse_next(const char *buf, const size_t length, size_t *offset,
	   struct Token *token)
{
	size_t	start = *offset;
	bool	ok = false;

	// TODO(skip past whitespace)
	// TODO(find next EOC)
	if (!ok) {
		*offset = start;
	}
	return -1;
}