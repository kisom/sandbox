#ifndef __KF_DICTIONARY_H__
#define __KF_DICTIONARY_H__


#include "defs.h"

struct entry {
	char	word[MAX_WORD_LEN];
	void	(*fun)(void);
	struct entry *next;
};


#endif // __KF_DICTIONARY_H__