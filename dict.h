#ifndef __KF_DICT_H__
#define __KF_DICT_H__

#include "defs.h"
#include "parser.h"
#include "system.h"
#include "word.h"

void	init_dict(System *);
void	reset_system(System *);
bool	lookup(struct Token *, System *);



#endif // __KF_DICT_H__