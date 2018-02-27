#ifndef __KF_DICT_H__
#define __KF_DICT_H__

#include "defs.h"
#include "parser.h"
#include "system.h"
#include "word.h"

typedef enum _LOOKUP_ : uint8_t {
	LOOKUP_OK = 0,	     // Lookup executed properly.
	LOOKUP_NOTFOUND = 1, // The token isn't in the dictionary.
	LOOKUP_FAILED = 2    // The word failed to execute.
} LOOKUP;

void	init_dict(System *);
LOOKUP	lookup(struct Token *, System *);



#endif // __KF_DICT_H__