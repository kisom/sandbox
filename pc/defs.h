#ifndef __KF_PLATFORM_PC_H__
#define __KF_PLATFORM_PC_H__

#include <stdint.h>

typedef int32_t KF_INT;
typedef uintptr_t KF_ADDR;

constexpr static size_t STACK_SIZE = 65535;
constexpr static size_t	DICT_SIZE  = 65535;
constexpr static size_t	ARENA_SIZE = 65535;


#endif // __KF_PLATFORM_PC_H__