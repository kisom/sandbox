#ifndef __KF_LINUX_DEFS_H__
#define __KF_LINUX_DEFS_H__

#include <stddef.h>
#include <stdint.h>

typedef int32_t KF_INT;
typedef uint32_t KF_UINT;
typedef int64_t	KF_LONG;

typedef uintptr_t KF_ADDR;
constexpr uint8_t STACK_SIZE = 128;
constexpr size_t ARENA_SIZE = 65535;

#endif