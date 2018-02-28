#ifndef __KF_CORE_H__
#define __KF_CORE_H__

#include "defs.h"
#include "io.h"
#include "stack.h"

typedef enum _SYS_STATUS : uint8_t {
	STATUS_OK = 0,
	STATUS_STACK_OVERFLOW = 1,
	STATUS_STACK_UNDERFLOW = 2,
	STATUS_EXECUTION_FAILURE = 3,
	STATUS_UNKNOWN_WORD = 4
} SYS_STATUS;

class Word;

typedef struct _System {
	Stack<KF_INT>	 dstack;
	IO		*interface;
	Word		*dict;
	SYS_STATUS	 status;
} System;

void	system_clear_error(System *sys);
void	system_write_status(System *sys);

#endif // __KF_CORE_H__