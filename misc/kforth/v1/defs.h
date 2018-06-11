#ifndef __KF_DEFS_H__
#define __KF_DEFS_H__

#ifdef __linux__
#include "linux/defs.h"
#else
typedef int KF_INT;
typedef long KF_LONG;
constexpr uint8_t STACK_SIZE = 16;
#endif

constexpr size_t	MAX_TOKEN_LENGTH = 16;
constexpr size_t dshift = (sizeof(KF_INT) * 8) - 1;

static inline KF_INT
mask(size_t bits)
{
	KF_INT m = 0;

	for (size_t i = 0; i < bits; i++) {
		m += 1 << i;
	}
	
	return m;
}

#endif // __KF_DEFS_H__