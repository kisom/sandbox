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

#endif // __KF_DEFS_H__