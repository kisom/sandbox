#include "dict.h"
#include "io.h"
#include "parser.h"
#include "system.h"

#include <stdlib.h>
#include <string.h>

#ifdef __linux__
#include "linux.h"
#endif // __linux__

static char     ok[] = "ok.\n";
static System	sys;


static void
write_dstack()
{
	KF_INT	tmp;
	sys.interface->wrch('<');
	for (size_t i = 0; i < sys.dstack.size(); i++) {
		if (i > 0) {
			sys.interface->wrch(' ');
		}

		sys.dstack.get(i, tmp);
		write_num(sys.interface, tmp);
	}
	sys.interface->wrch('>');
}

static bool
parser(const char *buf, const size_t buflen)
{
	static size_t		offset = 0;
	static struct Token	token;
	static PARSE_RESULT	result = PARSE_FAIL;
	static LOOKUP		lresult = LOOKUP_FAILED;
	static bool		stop = false;

	offset = 0;

	// reset token
	token.token = nullptr;
	token.length = 0;

	while ((result = parse_next(buf, buflen, &offset, &token)) == PARSE_OK) {
		lresult = lookup(&token, &sys);
		switch (lresult) {
		case LOOKUP_OK:
			continue;
		case LOOKUP_NOTFOUND:
			sys.interface->wrln((char *)"word not found", 15);
			stop = true;
			break;
		case LOOKUP_FAILED:
			sys.interface->wrln((char *)"execution failed", 17);
			stop = true;
			break;
		default:
			sys.interface->wrln((char *)"*** the world is broken ***", 27);
			exit(1);
		}
		
		if (stop) {
			stop = false;
			break;
		}
	}

	switch (result) {
	case PARSE_OK:
		return false;
	case PARSE_EOB:
		sys.interface->wrbuf(ok, 4);
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
		write_dstack();
		sys.interface->wrch('\n');
		sys.interface->wrch('?');
		sys.interface->wrch(' ');
		buflen = sys.interface->rdbuf(linebuf, 80, true, '\n');
		parser(linebuf, buflen);
	}
}

static char	banner[] = "kforth interpreter\n";
const size_t	bannerlen = 19;

int
main(void)
{
	init_dict(&sys);
#ifdef __linux__
	Console interface;
	sys.interface = &interface;
#endif
	sys.interface->wrbuf(banner, bannerlen);
	interpreter();
	return 0;
}