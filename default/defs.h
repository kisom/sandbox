#ifndef __KF_PLATFORM_DEFAULT_H__
#define __KF_PLATFORM_DEFAULT_H__

#include <stdint.h>

typedef int KF_INT;
typedef uintptr_t KF_ADDR;

constexpr static size_t STACK_SIZE = 48 / sizeof(KF_INT);
constexpr static size_t	DICT_SIZE  = 4096;
constexpr static size_t	ARENA_SIZE = 256;


#endif // __KF_PLATFORM_DEFAULT_H__