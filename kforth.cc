#include "dict.h"
#include "io.h"
#include "parser.h"
#include "system.h"

#include <stdlib.h>
#include <string.h>

#ifdef __linux__
#include "linux.h"
#endif // __linux__

static System	sys;
#ifndef TRACE_STACK
#define TRACE_STACK false
#endif

static bool
parser(const char *buf, const size_t buflen)
{
	static size_t		offset = 0;
	static struct Token	token;
	static PARSE_RESULT	result = PARSE_FAIL;

	offset = 0;

	// reset token
	token.token = nullptr;
	token.length = 0;

	while ((result = parse_next(buf, buflen, &offset, &token)) == PARSE_OK) {
		if (!lookup(&token, &sys)) {
			break;
		}
	}

	system_write_status(&sys);
	sys.interface->newline();
	switch (result) {
	case PARSE_OK:
		return false;
	case PARSE_EOB:
		return true;
	case PARSE_LEN:
		sys.interface->wrln((char *)"parse error: token too long", 27);
		return false;
	case PARSE_FAIL:
		sys.interface->wrln((char *)"parser failure", 14);
		return false;
	default:
		sys.interface->wrln((char *)"*** the world is broken ***", 27);
		exit(1);
	}
}

static void
interpreter()
{
	static size_t buflen = 0;
	static char linebuf[81];

	while (true) {
		if (TRACE_STACK) {
			write_dstack(sys.interface, sys.dstack);
			sys.interface->newline();
		}
		sys.interface->wrch('?');
		sys.interface->wrch(' ');
		buflen = sys.interface->rdbuf(linebuf, 80, true, '\n');
		parser(linebuf, buflen);
	}
}

static char	banner[] = "kforth interpreter";
const size_t	bannerlen = 18;

int
main(void)
{
	reset_system(&sys);
	init_dict(&sys);
#ifdef __linux__
	Console interface;
	sys.interface = &interface;
#endif
	sys.interface->wrln(banner, bannerlen);
	interpreter();
	return 0;
}