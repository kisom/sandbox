#include "defs.h"
#include "dict.h"
#include "stack.h"
#include "system.h"
#include "word.h"

#include <stdlib.h>
#include <string.h>

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
pop_addr(System *sys, KF_ADDR *a)
{
	KF_LONG	b;
	if (!pop_long(sys, &b)) {
		// Status is already set.
		return false;
	}

	*a = static_cast<KF_ADDR>(b);
	sys->status = STATUS_OK;
	return true;
}

static bool
push_addr(System *sys, KF_ADDR a)
{
	KF_LONG	b = static_cast<KF_LONG>(a);
	if (!push_long(sys, b)) {
		// Status is already set.
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

static bool
store(System *sys)
{
	KF_ADDR	a = 0; // address
	KF_INT	b = 0; // value
	KF_LONG	c = 0; // temporary

	if (!pop_long(sys, &c)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	a = static_cast<KF_ADDR>(c);
	
	if (!sys->dstack.pop(&b)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}

	*((KF_INT *)a) = b;
	sys->status = STATUS_OK;
	return true;
}

static bool
plus_store(System *sys)
{
	KF_ADDR	a = 0; // address
	KF_INT	b = 0; // value
	KF_LONG	c = 0; // temporary

	if (!pop_long(sys, &c)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	a = static_cast<KF_ADDR>(c);
	
	if (!sys->dstack.pop(&b)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}

	*((KF_INT *)a) += b;
	sys->status = STATUS_OK;
	return true;
}

static bool
fetch(System *sys)
{
	KF_ADDR	a = 0; // address
	KF_INT	b = 0; // value
	KF_LONG	c = 0; // temporary

	if (!pop_long(sys, &c)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	a = static_cast<KF_ADDR>(c);
	
	b = *((KF_INT *)a);
	if (!sys->dstack.push(b)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}
	sys->status = STATUS_OK;
	return true;	
}

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

static bool
to_r(System *sys)
{
	KF_INT	a;

	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}

	if (!sys->rstack.push(static_cast<KF_ADDR>(a))) {
		sys->status = STATUS_RSTACK_OVERFLOW;
		return false;
	}

	sys->status = STATUS_OK;
	return true;
}

static bool
from_r(System *sys)
{
	KF_ADDR	a;

	if (!sys->rstack.pop(&a)) {
		sys->status = STATUS_RSTACK_UNDERFLOW;
		return false;
	}

	if (!sys->dstack.push(static_cast<KF_INT>(a))) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}

	sys->status = STATUS_OK;
	return true;
}

static bool
r_fetch(System *sys)
{
	KF_ADDR	a;

	if (!sys->rstack.peek(&a)) {
		sys->status = STATUS_RSTACK_UNDERFLOW;
		return false;
	}

	if (!sys->dstack.push(static_cast<KF_INT>(a))) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}

	sys->status = STATUS_OK;
	return true;
}

static bool
c_fetch(System *sys)
{
	KF_ADDR	a;
	uint8_t	b; // the standard explicitly calls for a byte.

	if (!pop_addr(sys, &a)) {
		// Status is already set.
		return false;
	}

	b = *(reinterpret_cast<uint8_t *>(a));
	if (!sys->dstack.push(static_cast<KF_INT>(b))) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}

	sys->status = STATUS_OK;
	return true;
}

static bool
c_store(System *sys)
{
	KF_ADDR	a;
	KF_INT	b;

	if (!pop_addr(sys, &a)) {
		// Status is already set.
		return false;
	}

	if (!sys->dstack.pop(&b)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}

	b &= 0xFF;

	*(reinterpret_cast<uint8_t *>(a)) = b;
	sys->status = STATUS_OK;
	return true;
}


static bool
c_move(System *sys)
{
	KF_UINT	a;
	KF_INT	b;
	KF_ADDR	c, d;

	if (!sys->dstack.pop(&b)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	a = static_cast<KF_UINT>(b);

	if (!pop_addr(sys, &d)) {
		// Status is already set.
		return false;
	}

	if (!pop_addr(sys, &c)) {
		// Status is already set.
		return false;
	}

	for (KF_UINT i = 0; i < a; i++) {
		*reinterpret_cast<uint8_t *>(d + i) = 
		*reinterpret_cast<uint8_t *>(c + i);
	}

	sys->status = STATUS_OK;
	return true;
}

static bool
c_move_up(System *sys)
{
	KF_UINT	a;
	KF_INT	b;
	KF_ADDR	c, d;

	if (!sys->dstack.pop(&b)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	a = static_cast<KF_UINT>(b);

	if (!pop_addr(sys, &d)) {
		// Status is already set.
		return false;
	}

	if (!pop_addr(sys, &c)) {
		// Status is already set.
		return false;
	}

	for (KF_UINT i = 0; i < a; i++) {
		*reinterpret_cast<uint8_t *>(d - i) = 
		*reinterpret_cast<uint8_t *>(c - i);
	}

	sys->status = STATUS_OK;
	return true;
}

static bool
fill(System *sys)
{
	KF_INT	a, c;
	uint8_t	b;
	KF_UINT d;
	KF_ADDR	e;

	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	b = static_cast<uint8_t>(a);

	if (!sys->dstack.pop(&c)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	d = static_cast<KF_UINT>(c);

	if (!pop_addr(sys, &e)) {
		// Status is already set.
		return false;
	}

	for (KF_UINT i = 0; i < d; i++) {
		*reinterpret_cast<uint8_t *>(e + i) = b;
	}

	sys->status = STATUS_OK;
	return true;
}

static bool
count(System *sys)
{
	uint8_t	a;
	KF_ADDR	b;

	if (!pop_addr(sys, &b)) {
		// Status is already set.
		return false;
	}

	a = *reinterpret_cast<uint8_t *>(b);
	b++;

	if (!push_addr(sys, b)) {
		// Status is already set.
		return false;
	}

	if (!sys->dstack.push(static_cast<KF_INT>(a))) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}

	sys->status = STATUS_OK;
	return true;
}

static bool
execute(System *sys)
{
	KF_ADDR	 a;
	Word	*b;

	if (!pop_addr(sys, &a)) {
		// Status is already set.
		return false;
	}

	b = reinterpret_cast<Word *>(a);
	char	buf[MAX_TOKEN_LENGTH];
	size_t	buflen;

	b->getname(buf, &buflen);
	sys->interface->wrbuf((char *)"executing word: ", 16);
	sys->interface->wrbuf(buf, buflen);
	sys->interface->newline();
	return b->eval(sys);
}

static bool
u_dot(System *sys)
{
	KF_INT	a;
	KF_UINT	b;

	if (!sys->dstack.pop(&a)) {
		sys->status = STATUS_STACK_UNDERFLOW;
		return false;
	}
	b = static_cast<KF_UINT>(a);

	write_unum(sys->interface, b);
	sys->interface->newline();
	sys->status = STATUS_OK;
	return true;
}

static bool
ult(System *sys)
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
	
	if (static_cast<KF_UINT>(b) < static_cast<KF_UINT>(a)) {
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
u_times(System *sys)
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
	
	a = static_cast<KF_UINT>(a) * static_cast<KF_UINT>(b);
	if (!sys->dstack.push(a)) {
		sys->status = STATUS_STACK_OVERFLOW;
		return false;
	}

	sys->status = STATUS_OK;
	return true;
}

static bool
udivide_mod(System *sys)
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
	
	
	z = (KF_UINT)b / (KF_UINT)a;
	y = (KF_UINT)b % (KF_UINT)a;

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

void
init_dict(System *sys)
{
	sys->dict = nullptr;
	sys->dict = new Builtin((const char *)"U/MOD", 5, sys->dict, udivide_mod);
	sys->dict = new Builtin((const char *)"UM*", 3, sys->dict, u_times);
	sys->dict = new Builtin((const char *)"U<", 2, sys->dict, ult);
	sys->dict = new Builtin((const char *)"U.", 2, sys->dict, u_dot);
	sys->dict = new Builtin((const char *)"SWAP", 4, sys->dict, swap);
	sys->dict = new Builtin((const char *)"SWAP", 4, sys->dict, swap);
	sys->dict = new Builtin((const char *)"XOR", 3, sys->dict, exclusive_or);
	sys->dict = new Builtin((const char *)"ROT", 3, sys->dict, rot);
	sys->dict = new Builtin((const char *)"ROLL", 4, sys->dict, roll);
	sys->dict = new Builtin((const char *)"R@", 2, sys->dict, r_fetch);
	sys->dict = new Builtin((const char *)"R>", 2, sys->dict, from_r);
	sys->dict = new Builtin((const char *)"PICK", 4, sys->dict, pick);
	sys->dict = new Builtin((const char *)"OVER", 4, sys->dict, over);
	sys->dict = new Builtin((const char *)"NEGATE", 6, sys->dict, negate);
	sys->dict = new Builtin((const char *)"OR", 2, sys->dict, lor);
	sys->dict = new Builtin((const char *)"MOD", 3, sys->dict, mod);
	sys->dict = new Builtin((const char *)"MIN", 3, sys->dict, min);
	sys->dict = new Builtin((const char *)"MAX", 3, sys->dict, max);
	sys->dict = new Builtin((const char *)"FILL", 4, sys->dict, fill);
	sys->dict = new Builtin((const char *)"EXECUTE", 7, sys->dict, execute);
	sys->dict = new Builtin((const char *)"DUP", 3, sys->dict, dup);
	sys->dict = new Builtin((const char *)"DROP", 4, sys->dict, drop);
	sys->dict = new Builtin((const char *)"DEPTH", 5, sys->dict, depth);
	sys->dict = new Builtin((const char *)"DEFINITIONS", 11, sys->dict, definitions);
	sys->dict = new Builtin((const char *)"DNEGATE", 7, sys->dict, dnegate);
	sys->dict = new Builtin((const char *)"D.", 2, sys->dict, ddot);
	sys->dict = new Builtin((const char *)"D<", 2, sys->dict, dlt);
	sys->dict = new Builtin((const char *)"D+", 2, sys->dict, dplus);
	sys->dict = new Builtin((const char *)"COUNT", 5, sys->dict, count);
	sys->dict = new Builtin((const char *)"CMOVE>", 6, sys->dict, c_move_up);
	sys->dict = new Builtin((const char *)"CMOVE", 5, sys->dict, c_move);
	sys->dict = new Builtin((const char *)"C@", 2, sys->dict, c_fetch);
	sys->dict = new Builtin((const char *)"C!", 2, sys->dict, c_store);
	sys->dict = new Builtin((const char *)"BYE", 3, sys->dict, bye);
	sys->dict = new Builtin((const char *)"ABS", 3, sys->dict, absolute);
	sys->dict = new Builtin((const char *)"AND", 3, sys->dict, land);
	sys->dict = new Builtin((const char *)"@", 1, sys->dict, fetch);
	sys->dict = new Builtin((const char *)"?DUP", 4, sys->dict, question_dupe);
	sys->dict = new Builtin((const char *)">R", 2, sys->dict, to_r);
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
	sys->dict = new Builtin((const char *)"+!", 2, sys->dict, plus_store);
	sys->dict = new Builtin((const char *)"+", 1, sys->dict, add);
	sys->dict = new Builtin((const char *)"*", 1, sys->dict, mul);
	sys->dict = new Builtin((const char *)"!", 1, sys->dict, store);
	sys->dict = new Address((const char *)"ARENA", 5, sys->dict, reinterpret_cast<KF_ADDR>(&sys->arena));
	sys->dict = new Address((const char *)"DICT", 5, sys->dict, reinterpret_cast<KF_ADDR>(&sys->dict));
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
