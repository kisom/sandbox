#include "defs.h"
#include "eval.h"

#include <string.h>

void
cwexec(uintptr_t entry)
{
	uintptr_t	target = 0;
	uintptr_t	codeword = 0;

	memcpy(&codeword, (void *)entry, sizeof(uintptr_t));	
	memcpy(&target, (void *)(entry + sizeof(uintptr_t)), sizeof(uintptr_t));	
	((void(*)(uintptr_t))codeword)(target);
}

void
nexec(uintptr_t target)
{
	((void(*)(void))target)();
}
