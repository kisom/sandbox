#include "defs.h"
#include "dict.h"
#include "stack.h"
#include "system.h"
#include "word.h"

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
	
	b *= a;
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
	Word	*cursor = dict;
	char	 buf[MAX_TOKEN_LENGTH];
	size_t	 buflen = 0;
	
	while (cursor != nullptr) {
		cursor->getname(buf, &buflen);
		sys->interface->wrln(buf, buflen);
		cursor = cursor->next();
	}
	
	return true;
}

void
init_dict()
{
	dict = new Builtin((const char *)"DEFINITIONS", 11, dict, definitions);
	dict = new Builtin((const char *)"+", 1, dict, add);
	dict = new Builtin((const char *)"-", 1, dict, sub);
	dict = new Builtin((const char *)"*", 1, dict, mul);
	dict = new Builtin((const char *)"/", 1, dict, div);
	dict = new Builtin((const char *)"SWAP", 4, dict, swap);
	dict = new Builtin((const char *)"ROT", 3, dict, rot);
}


LOOKUP
lookup(struct Token *token, System *sys)
{
	Word	*cursor = dict;
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