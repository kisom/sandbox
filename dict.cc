#include "defs.h"
#include "dict.h"
#include "stack.h"
#include "system.h"
#include "word.h"

#include <stdlib.h>
#include <string.h>

constexpr size_t dshift = (sizeof(KF_INT) * 8) - 1;

static bool
pop_long(System *sys, KF_LONG *d)
{
	KF_INT	a = 0;
	KF_INT	b = 0;
	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	if (!sys->dstack.pop(&b)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	*d = static_cast<KF_LONG>(a) << dshift;
	*d += static_cast<KF_LONG>(b);
	sys->status = STATUS_OK;
	return true;
}

static inline KF_INT
mask(size_t bits)
{
	KF_INT m = 0;

	for (size_t i = 0; i < bits; i++) {
		m += 1 << i;
	}
	
	return m;
}

static bool
push_long(System *sys, KF_LONG d)
{
	KF_INT	a = static_cast<KF_INT>((d >> dshift) & mask(dshift));
	KF_INT	b = static_cast<KF_INT>(d & mask(dshift));

	if (!sys->dstack.push(b)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	if (!sys->dstack.push(a)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	sys->status = STATUS_OK;
	return true;
}

static bool
add(System *sys)
{
	KF_INT	a = 0;
	KF_INT	b = 0;
	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	if (!sys->dstack.pop(&b)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	a += b;
	if (!sys->dstack.push(a)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}

	sys->status = STATUS_OK;
	return true;
}

static bool
sub(System *sys)
{
	KF_INT	a = 0;
	KF_INT	b = 0;
	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	if (!sys->dstack.pop(&b)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	b -= a;
	if (!sys->dstack.push(b)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}

	sys->status = STATUS_OK;
	return true;
}

static bool
mul(System *sys)
{
	KF_INT	a = 0;
	KF_INT	b = 0;

	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	if (!sys->dstack.pop(&b)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	b *= a;
	if (!sys->dstack.push(b)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}

	sys->status = STATUS_OK;
	return true;
}

static bool
div(System *sys)
{
	KF_INT	a = 0;
	KF_INT	b = 0;
	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	if (!sys->dstack.pop(&b)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	b /= a;
	if (!sys->dstack.push(b)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}

	sys->status = STATUS_OK;
	return true;
}

static bool
swap(System *sys)
{
	KF_INT	a = 0;
	KF_INT	b = 0;
	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	if (!sys->dstack.pop(&b)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	if (!sys->dstack.push(a)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	if (!sys->dstack.push(b)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}

	sys->status = STATUS_OK;
	return true;
}

static bool
rot(System *sys)
{
	KF_INT	a = 0;
	KF_INT	b = 0;
	KF_INT	c = 0;

	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	if (!sys->dstack.pop(&b)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	if (!sys->dstack.pop(&c)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	if (!sys->dstack.push(b)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	if (!sys->dstack.push(a)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	if (!sys->dstack.push(c)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}

	sys->status = STATUS_OK;
	return true;
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
	sys->status = STATUS_OK;
	return true;
}

static bool
bye(System *sys)
{
	exit(0);
	return true;
}

static bool
dup(System *sys)
{
	KF_INT	a;

	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	if (!sys->dstack.push(a)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	if (!sys->dstack.push(a)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	sys->status = STATUS_OK;
	return true;
}

static bool
drop(System *sys)
{
	KF_INT	a;
	
	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}

	sys->status = STATUS_OK;
	return true;
}

static bool
dotess(System *sys)
{
	write_dstack(sys->interface, sys->dstack);
	sys->interface->newline();
	sys->status = STATUS_OK;
	return true;
}

static bool
dot(System *sys)
{
	KF_INT	a;
	
	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	write_num(sys->interface, a);
	sys->interface->newline();
	sys->status = STATUS_OK;
	return true;
}

static bool
depth(System *sys)
{
	KF_INT	a = static_cast<KF_INT>(sys->dstack.size());
	
	if (!sys->dstack.push(a)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	sys->status = STATUS_OK;
	return true;
}

static bool
times_divide(System *sys)
{
	KF_INT	a, b, c;
	KF_LONG	z;
	
	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	if (!sys->dstack.pop(&b)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	if (!sys->dstack.pop(&c)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	z = static_cast<KF_LONG>(c) * static_cast<KF_LONG>(b);
	z /= static_cast<KF_LONG>(a);
	a = static_cast<KF_INT>(z);
	
	if (!sys->dstack.push(a)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	sys->status = STATUS_OK;
	return true;
}

static bool
times_divide_mod(System *sys)
{
	KF_INT	a, b, c;
	KF_LONG	y, z;
	
	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	if (!sys->dstack.pop(&b)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	if (!sys->dstack.pop(&c)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	z = static_cast<KF_LONG>(c) * static_cast<KF_LONG>(b);
	y = z % static_cast<KF_LONG>(a);
	z /= static_cast<KF_LONG>(a);
	a = static_cast<KF_INT>(z);
	b = static_cast<KF_INT>(y);

	if (!sys->dstack.push(b)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	if (!sys->dstack.push(a)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	sys->status = STATUS_OK;
	return true;
}

static bool
divide_mod(System *sys)
{
	KF_INT	a, b;
	KF_INT	y, z;
	
	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	if (!sys->dstack.pop(&b)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	
	z = b / a;
	y = b % a;

	if (!sys->dstack.push(y)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	if (!sys->dstack.push(z)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	sys->status = STATUS_OK;
	return true;
}


/*
static bool
store(System *sys)
{
	KF_INT	a = 0; // address
	KF_INT	b = 0; // value

	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	if (!sys->dstack.pop(&b)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}

	KF_INT	*p = (KF_INT *)(a);
	*p = b;
	
	sys->status = STATUS_OK;
	return true;
}

static bool
plus_store(System *sys)
{
	KF_INT	a = 0; // address
	KF_INT	b = 0; // value

	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	if (!sys->dstack.pop(&b)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}

	KF_INT	*p = (KF_INT *)(a);
	*p += b;
	
	sys->status = STATUS_OK;
	return true;
}
*/

static bool
zero_less(System *sys)
{
	KF_INT	a;
	bool	ok;

	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	if (a < 0) {
		ok = sys->dstack.push(-1);
	}
	else {
		ok = sys->dstack.push(0);
	}
	
	if (!ok) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	sys->status = STATUS_OK;
	return true;
}

static bool
zero_equals(System *sys)
{
	KF_INT	a;
	bool	ok;

	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	if (a == 0) {
		ok = sys->dstack.push(-1);
	}
	else {
		ok = sys->dstack.push(0);
	}
	
	if (!ok) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	sys->status = STATUS_OK;
	return true;
}

static bool
zero_greater(System *sys)
{
	KF_INT	a;
	bool	ok;

	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	if (a > 0) {
		ok = sys->dstack.push(-1);
	}
	else {
		ok = sys->dstack.push(0);
	}
	
	if (!ok) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	sys->status = STATUS_OK;
	return true;
}

static bool
one_plus(System *sys)
{
	KF_INT	a;

	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	a++;
	if (!sys->dstack.push(a)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	sys->status = STATUS_OK;
	return true;
}

static bool
one_minus(System *sys)
{
	KF_INT	a;

	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	a--;
	if (!sys->dstack.push(a)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	sys->status = STATUS_OK;
	return true;
}

static bool
two_plus(System *sys)
{
	KF_INT	a;

	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	a += 2;
	if (!sys->dstack.push(a)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	sys->status = STATUS_OK;
	return true;
}

static bool
two_minus(System *sys)
{
	KF_INT	a;

	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	a -= 2;
	if (!sys->dstack.push(a)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	sys->status = STATUS_OK;
	return true;
}

static bool
two_divide(System *sys)
{
	KF_INT	a;

	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	a >>= 1;
	if (!sys->dstack.push(a)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	sys->status = STATUS_OK;
	return true;
}

static bool
less_than(System *sys)
{
	KF_INT	a, b;
	bool	ok;
	
	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}

	if (!sys->dstack.pop(&b)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}

	if (b < a) {
		ok = sys->dstack.push(-1);
	}
	else {
		ok = sys->dstack.push(0);
	}
	
	if (!ok) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	sys->status = STATUS_OK;
	return true;
}

static bool
equals(System *sys)
{
	KF_INT	a, b;
	bool	ok;
	
	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}

	if (!sys->dstack.pop(&b)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}

	if (b == a) {
		ok = sys->dstack.push(-1);
	}
	else {
		ok = sys->dstack.push(0);
	}
	
	if (!ok) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	sys->status = STATUS_OK;
	return true;
}

static bool
greater_than(System *sys)
{
	KF_INT	a, b;
	bool	ok;
	
	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}

	if (!sys->dstack.pop(&b)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}

	if (b > a) {
		ok = sys->dstack.push(-1);
	}
	else {
		ok = sys->dstack.push(0);
	}
	
	if (!ok) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	sys->status = STATUS_OK;
	return true;
}

static bool
question_dupe(System *sys)
{
	KF_INT	a;

	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	if (!sys->dstack.push(a)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	if (a != 0) {
		if (!sys->dstack.push(a)) {
			sys->status = STATUS_STACK_OVERFLOW;
			return false;
		}
	}
	
	sys->status = STATUS_OK;
	return true;
}

static bool
absolute(System *sys)
{
	KF_INT	a;
	
	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	if (a < 0) {
		if (!sys->dstack.push(-a)) {
			sys->status = STATUS_STACK_OVERFLOW;
			return false;
		}
	}
	else {
		if (!sys->dstack.push(a)) {
			sys->status = STATUS_STACK_OVERFLOW;
			return false;
		}
	}
	
	sys->status = STATUS_OK;
	return true;
}

static bool
land(System *sys)
{
	KF_INT	a, b;
	
	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}

	if (!sys->dstack.pop(&b)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	a &= b;
	if (!sys->dstack.push(a)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	sys->status = STATUS_OK;
	return true;
}

static bool
lor(System *sys)
{
	KF_INT	a, b;
	
	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}

	if (!sys->dstack.pop(&b)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	a |= b;
	if (!sys->dstack.push(a)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	sys->status = STATUS_OK;
	return true;
}

static bool
roll(System *sys)
{
	KF_INT	a, b;
	size_t	i;
	
	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	i = sys->dstack.size() - static_cast<size_t>(a) - 1;
	if (!sys->dstack.remove(i, &b)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	if (!sys->dstack.push(b)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	sys->status = STATUS_OK;
	return true;
}

static bool
over(System *sys)
{
	KF_INT	a;
	size_t	i = sys->dstack.size() - 2;
	
	if (!sys->dstack.get(i, a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}

	if (!sys->dstack.push(a)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	sys->status = STATUS_OK;
	return true;
}

static bool
dplus(System *sys)
{
	KF_LONG	da, db;
	
	if (!pop_long(sys, &da)) {
		// Status is already set.
		return false;
	}
	
	if (!pop_long(sys, &db)) {
		// Status is already set.
		return false;
	}
	
	da += db;
	
	if (!push_long(sys, da)) {
		// Status is already set.
		return false;
	}
	
	// Status is already set.
	return true;
}

static bool
dlt(System *sys)
{
	KF_LONG	da, db;
	bool	ok;
	
	if (!pop_long(sys, &da)) {
		// Status is already set.
		return false;
	}
	
	if (!pop_long(sys, &db)) {
		// Status is already set.
		return false;
	}
	
	if (db < da) {
		ok = sys->dstack.push(-1);
	}
	else {
		ok = sys->dstack.push(0);
	}
	
	if (!ok) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	sys->status = STATUS_OK;
	return true;
}

static bool
ddot(System *sys)
{
	KF_LONG	da;

	if (!pop_long(sys, &da)) {
		// Status is already set.
		return false;
	}

	write_dnum(sys->interface, da);
	sys->interface->newline();
	sys->status = STATUS_OK;
	return true;
}

static bool
negate(System *sys)
{
	KF_INT	a;
	
	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	
	a = ~a;
	a++;
	
	if (!sys->dstack.push(a)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	sys->status = STATUS_OK;
	return true;
}

static bool
dnegate(System *sys)
{
	KF_LONG	da;

	if (!pop_long(sys, &da)) {
		// Status is already set.
		return false;
	}

	da = ~da;
	da++;

	if (!push_long(sys, da)) {
		// Status is already set.
		return false;
	}

	sys->status = STATUS_OK;
	return true;
}

static bool
pick(System *sys)
{
	KF_INT	a, b;
	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}

	size_t	i = sys->dstack.size() - a - 1;
	if (!sys->dstack.get(i, b)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}

	if (!sys->dstack.push(b)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}

	sys->status = STATUS_OK;
	return true;
}

static bool
min(System *sys)
{
	KF_INT	a, b;
	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	if (!sys->dstack.pop(&b)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}

	if (!sys->dstack.push(a < b ? a : b)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}

	sys->status = STATUS_OK;
	return true;
}

static bool
max(System *sys)
{
	KF_INT	a, b;
	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	if (!sys->dstack.pop(&b)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}

	if (!sys->dstack.push(a > b ? a : b)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}

	sys->status = STATUS_OK;
	return true;
}

static bool
exclusive_or(System *sys)
{
	KF_INT	a, b;
	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	if (!sys->dstack.pop(&b)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}

	if (!sys->dstack.push(a ^ b)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}

	sys->status = STATUS_OK;
	return true;
}

static bool
mod(System *sys)
{
	KF_INT	a, b;
	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	if (!sys->dstack.pop(&b)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}

	if (!sys->dstack.push(b % a)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}

	sys->status = STATUS_OK;
	return true;
}

void
init_dict(System *sys)
{
	sys->dict = nullptr;
	sys->dict = new Builtin((const char *)"SWAP", 4, sys->dict, swap);
	sys->dict = new Builtin((const char *)"XOR", 3, sys->dict, exclusive_or);
	sys->dict = new Builtin((const char *)"ROT", 3, sys->dict, rot);
	sys->dict = new Builtin((const char *)"ROLL", 4, sys->dict, roll);
	sys->dict = new Builtin((const char *)"PICK", 4, sys->dict, pick);
	sys->dict = new Builtin((const char *)"OVER", 4, sys->dict, over);
	sys->dict = new Builtin((const char *)"NEGATE", 6, sys->dict, negate);
	sys->dict = new Builtin((const char *)"OR", 2, sys->dict, lor);
	sys->dict = new Builtin((const char *)"MOD", 3, sys->dict, mod);
	sys->dict = new Builtin((const char *)"MIN", 3, sys->dict, min);
	sys->dict = new Builtin((const char *)"MAX", 3, sys->dict, max);
	sys->dict = new Builtin((const char *)"DUP", 3, sys->dict, dup);
	sys->dict = new Builtin((const char *)"DROP", 4, sys->dict, drop);
	sys->dict = new Builtin((const char *)"DEPTH", 5, sys->dict, depth);
	sys->dict = new Builtin((const char *)"DEFINITIONS", 11, sys->dict, definitions);
	sys->dict = new Builtin((const char *)"DNEGATE", 7, sys->dict, dnegate);
	sys->dict = new Builtin((const char *)"D.", 2, sys->dict, ddot);
	sys->dict = new Builtin((const char *)"D<", 2, sys->dict, dlt);
	sys->dict = new Builtin((const char *)"D+", 2, sys->dict, dplus);
	sys->dict = new Builtin((const char *)"BYE", 3, sys->dict, bye);
	sys->dict = new Builtin((const char *)"ABS", 3, sys->dict, absolute);
	sys->dict = new Builtin((const char *)"AND", 3, sys->dict, land);
	sys->dict = new Builtin((const char *)"?DUP", 4, sys->dict, question_dupe);
	sys->dict = new Builtin((const char *)">", 1, sys->dict, greater_than);
	sys->dict = new Builtin((const char *)"=", 1, sys->dict, equals);
	sys->dict = new Builtin((const char *)"<", 1, sys->dict, less_than);
	sys->dict = new Builtin((const char *)"2/", 2, sys->dict, two_divide);
	sys->dict = new Builtin((const char *)"2-", 2, sys->dict, two_minus);
	sys->dict = new Builtin((const char *)"2+", 2, sys->dict, two_plus);
	sys->dict = new Builtin((const char *)"1-", 2, sys->dict, one_minus);
	sys->dict = new Builtin((const char *)"1+", 2, sys->dict, one_plus);
	sys->dict = new Builtin((const char *)"0>", 2, sys->dict, zero_greater);
	sys->dict = new Builtin((const char *)"0=", 2, sys->dict, zero_equals);
	sys->dict = new Builtin((const char *)"0<", 2, sys->dict, zero_less);
	sys->dict = new Builtin((const char *)"*/MOD", 5, sys->dict, times_divide_mod);
	sys->dict = new Builtin((const char *)"*/", 2, sys->dict, times_divide);
	sys->dict = new Builtin((const char *)"/MOD", 4, sys->dict, divide_mod);
	sys->dict = new Builtin((const char *)"/", 1, sys->dict, div);
	sys->dict = new Builtin((const char *)".S", 2, sys->dict, dotess);
	sys->dict = new Builtin((const char *)".", 1, sys->dict, dot);
	sys->dict = new Builtin((const char *)"-", 1, sys->dict, sub);
	// sys->dict = new Builtin((const char *)"+!", 2, sys->dict, plus_store);
	sys->dict = new Builtin((const char *)"+", 1, sys->dict, add);
	sys->dict = new Builtin((const char *)"*", 1, sys->dict, mul);
	// sys->dict = new Builtin((const char *)"!", 1, sys->dict, store);
}

bool
lookup(struct Token *token, System *sys)
{
	Word	*cursor = sys->dict;
	KF_INT	 n;
	
	if (parse_num(token, &n)) {
		if (sys->dstack.push(n)) {
			sys->status = STATUS_OK;
			return true;
		}
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	
	while (cursor != nullptr) {
		if (cursor->match(token)) {
			if (!cursor->eval(sys)) {
				sys->dstack.clear();
				return false;
			}
			return true;
		}
		cursor = cursor->next();
	}
	
	sys->status = STATUS_UNKNOWN_WORD;
	return false;
}

void
reset_system(System *sys)
{
	sys->status = STATUS_OK;
	sys->dstack.clear();
	
	auto cursor = sys->dict;
	auto next   = cursor;
	while (cursor != nullptr) {
		next = cursor->next();
		delete cursor;
		cursor = next;
	}
}
