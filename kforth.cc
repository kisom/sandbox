#include "io.h"
#include "parser.h"

#include <stdlib.h>

#ifdef __linux__
#include "linux.h"
#endif // __linux__

static char     ok[] = "ok.\n";
static char	bye[] = "bye";

static bool
parser(IO &interface, const char *buf, const size_t buflen)
{
	static size_t		offset = 0;
	static struct Token	token;
	static PARSE_RESULT	result = PARSE_FAIL;

	offset = 0;

	// reset token
	token.token = nullptr;
	token.length = 0;

	while ((result = parse_next(buf, buflen, &offset, &token)) == PARSE_OK) {
		interface.wrbuf((char *)"token: ", 7);
		interface.wrbuf(token.token, token.length);
		interface.wrln((char *)".", 1);

		// Temporary hack until the interpreter is working further.
		if (match_token(token.token, token.length, bye, 3)) {
			interface.wrln((char *)"Goodbye!", 8);
			exit(0);
		}
	}

	switch (result) {
	case PARSE_EOB:
		interface.wrbuf(ok, 4);
		return true;
	case PARSE_LEN:
		interface.wrln((char *)"parse error: token too long", 27);
		return false;
	case PARSE_FAIL:
		interface.wrln((char *)"parser failure", 14);
		return false;
	default:
		interface.wrln((char *)"*** the world is broken ***", 27);
		exit(1);
	}
}

static void
interpreter(IO &interface)
{
	static size_t buflen = 0;
	static char linebuf[81];

	while (true) {
		interface.wrch('?');
		interface.wrch(' ');
		buflen = interface.rdbuf(linebuf, 80, true, '\n');
		parser(interface, linebuf, buflen);
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