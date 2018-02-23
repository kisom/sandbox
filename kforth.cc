#include "io.h"
#include "parser.h"

#ifdef __linux__
#include "linux.h"
#endif // __linux__

static char     ok[] = "ok.\n";

static void
interpreter(IO &interface)
{
	static size_t buflen = 0;
	static char linebuf[81];

	while (true) {
		buflen = interface.rdbuf(linebuf, 80, true, '\n');
		interface.wrln(linebuf, buflen);
		interface.wrbuf(ok, 4);
	}
}

static char	banner[] = "kforth interpreter\n";
const size_t	bannerlen = 19;

int
main(void)
{
#ifdef __linux__
	Console interface;
#endif
	interface.wrbuf(banner, bannerlen);
	interpreter(interface);
	return 0;
}