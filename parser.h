#ifndef __KF_PARSER_H__
#define __KF_PARSER_H__

#include "defs.h"

struct Token {
	char	*token;
	uint8_t	 length;
};

int	parse_next(const char *, const size_t, size_t *, struct Token *);


#endif // __KF_PARSER_H__
