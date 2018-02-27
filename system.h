#ifndef __KF_CORE_H__
#define __KF_CORE_H__

#include "defs.h"
#include "io.h"
#include "stack.h"

typedef struct _System {
	Stack<KF_INT>	 dstack;
	IO		*interface;
	struct Word	*dict;
} System;


#endif // __KF_CORE_H__