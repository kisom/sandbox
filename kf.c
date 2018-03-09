#include "defs.h"
#include "eval.h"
#include "stack.h"
#include "word.h"

#include <stdio.h>
#include <stdlib.h>
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

	append_native_word("hello", 5, hello);
	uintptr_t	hwb = 0;

	if (!lookup("hello", 5, &hwb)) {
		fprintf(stderr, "failed to lookup 'hello'\n");
		exit(1);
	}
	printf("hello: 0x%lx\n", (unsigned long)hwb);
	if (!execute("hello", 5)) {
		fprintf(stderr, "failed to execute 'hello'\n");
		exit(1);
	}

	printf("finished\n");
}
