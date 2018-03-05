#include "defs.h"
#include "eval.h"
#include "stack.h"
#include "word.h"

#include <stdio.h>
#include <string.h>

void
hello(void)
{
	printf("hello, world\n");
}

int
main(void)
{
	dstack_push(2);
	dstack_push(3);

	uint8_t	arena[128] = {0};
	uintptr_t arena_p = (uintptr_t)arena;
	store_native(arena, "hello", 5, hello);

	cwexec(arena_p);

	printf("finished\n");
}
