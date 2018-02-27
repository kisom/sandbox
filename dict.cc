#include "defs.h"
#include "dict.h"
#include "stack.h"
#include "system.h"
#include "word.h"

#include <string.h>

static bool
add(System *sys)
{
	KF_INT	a = 0;
	KF_INT	b = 0;
	if (!sys->dstack.pop(&a)) {
		return false;
	}
	
	if (!sys->dstack.pop(&b)) {
		return false;
	}
	
	a += b;
	return sys->dstack.push(a);
}

static bool
sub(System *sys)
{
	KF_INT	a = 0;
	KF_INT	b = 0;
	if (!sys->dstack.pop(&a)) {
		return false;
	}
	
	if (!sys->dstack.pop(&b)) {
		return false;
	}
	
	b -= a;
	return sys->dstack.push(b);
}

static bool
mul(System *sys)
{
	KF_INT	a = 0;
	KF_INT	b = 0;
	if (!sys->dstack.pop(&a)) {
		return false;
	}
	
	if (!sys->dstack.pop(&b)) {
		return false;
	}
	
	b *= a;
	return sys->dstack.push(b);
}

static bool
div(System *sys)
{
	KF_INT	a = 0;
	KF_INT	b = 0;
	if (!sys->dstack.pop(&a)) {
		return false;
	}
	
	if (!sys->dstack.pop(&b)) {
		return false;
	}
	
	b /= a;
	return sys->dstack.push(b);
}

static bool
swap(System *sys)
{
	KF_INT	a = 0;
	KF_INT	b = 0;
	if (!sys->dstack.pop(&a)) {
		return false;
	}
	
	if (!sys->dstack.pop(&b)) {
		return false;
	}
	
	if (!sys->dstack.push(a)) {
		return false;
	}
	
	return sys->dstack.push(b);
}

static bool
rot(System *sys)
{
	KF_INT	a = 0;
	KF_INT	b = 0;
	KF_INT	c = 0;
	if (!sys->dstack.pop(&a)) {
		return false;
	}
	
	if (!sys->dstack.pop(&b)) {
		return false;
	}
	
	if (!sys->dstack.pop(&c)) {
		return false;
	}
	
	if (!sys->dstack.push(b)) {
		return false;
	}
	
	if (!sys->dstack.push(a)) {
		return false;
	}
	
	return sys->dstack.push(c);
}

// TODO: print multiple per line
static bool
definitions(System *sys)
{
	Word	*cursor = sys->dict;
	char	 buf[MAX_TOKEN_LENGTH];
	char	 line[72];
	size_t	 buflen = 0, linelen = 0;
	bool	 ready = false;

	while (cursor != nullptr) {
		if (ready) {
			ready = false;
			sys->interface->wrln(line, linelen);
			linelen = 0;
			continue;
		}

		cursor->getname(buf, &buflen);

		// TODO: get rid of magic numbers
		if ((buflen + linelen) > 72) {
			ready = true;
			continue;
		}
		memcpy(line + linelen, buf, buflen);
		linelen += buflen;

		if (linelen < 71) {
			line[linelen++] = ' ';
		}
		else {
			ready = true;
		}
		cursor = cursor->next();
	}

	sys->interface->wrln(line, linelen);
	return true;
}

void
init_dict(System *sys)
{
	sys->dict = nullptr;
	sys->dict = new Builtin((const char *)"DEFINITIONS", 11, sys->dict, definitions);
	sys->dict = new Builtin((const char *)"+", 1, sys->dict, add);
	sys->dict = new Builtin((const char *)"-", 1, sys->dict, sub);
	sys->dict = new Builtin((const char *)"*", 1, sys->dict, mul);
	sys->dict = new Builtin((const char *)"/", 1, sys->dict, div);
	sys->dict = new Builtin((const char *)"SWAP", 4, sys->dict, swap);
	sys->dict = new Builtin((const char *)"ROT", 3, sys->dict, rot);
}


LOOKUP
lookup(struct Token *token, System *sys)
{
	Word	*cursor = sys->dict;
	KF_INT	 n;
	
	if (parse_num(token, &n)) {
		if (sys->dstack.push(n)) {
			return LOOKUP_OK;
		}
		return LOOKUP_FAILED;
	}
	
	while (cursor != nullptr) {
		if (cursor->match(token)) {
			if (cursor->eval(sys)) {
				return LOOKUP_OK;
			}
			return LOOKUP_FAILED;
		}
		cursor = cursor->next();
	}
	
	return LOOKUP_NOTFOUND;
}