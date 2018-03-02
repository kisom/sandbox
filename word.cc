#include "defs.h"
#include "parser.h"
#include "system.h"
#include "word.h"

#include <string.h>


Builtin::Builtin(const char *name, size_t namelen, Word *head, bool (*target)(System *))
	: prev(head), fun(target)
{
	memcpy(this->name, name, namelen);
	this->namelen = namelen;
}
	
bool
Builtin::eval(System *sys)
{
	return this->fun(sys);
}

Word *
Builtin::next()
{
	return this->prev;
}
	
bool
Builtin::match(struct Token *token)
{
	return match_token(this->name, this->namelen, token->token, token->length);
}

void
Builtin::getname(char *buf, size_t *buflen)
{
	memcpy(buf, this->name, this->namelen);
	*buflen = namelen;
}


Address::Address(const char *name, size_t namelen, Word *head, KF_ADDR addr)
	: prev(head), addr(addr)
{
	memcpy(this->name, name, namelen);
	this->namelen = namelen;
}

bool
Address::eval(System *sys)
{
	KF_INT	a;

	a = static_cast<KF_INT>(this->addr & mask(dshift));
	if (!sys->dstack.push(a)) {
		return false;
	}

	a = static_cast<KF_INT>((this->addr >> dshift) & mask(dshift));
	if (!sys->dstack.push(a)) {
		return false;
	}

	return true;
}

Word *
Address::next(void)
{
	return this->prev;
}

bool
Address::match(struct Token *token)
{
	return match_token(this->name, this->namelen, token->token, token->length);
}

void 
Address::getname(char *buf, size_t *buflen)
{
	memcpy(buf, this->name, this->namelen);
	*buflen = namelen;
}