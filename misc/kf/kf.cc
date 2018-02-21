#include "io.h"
#ifdef __linux__
#include "linux.h"
#endif // __linux__

static void
repl(IO &interface)
{
	static char lbuf[81];

	while (true) {
		interface.rdln(lbuf, 80);
	}

	return;
}

int
main(void)
{
#ifdef __linux__
	Console interface;
#endif
	repl(interface);
	return 0;
}