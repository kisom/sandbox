#include "defs.h"
#include "stack.h"

static KF_INT	dstack[DSTACK_SIZE] = {0};
static size_t	dstack_len = 0;

bool
dstack_pop(KF_INT *a)
{
	if (dstack_len == 0) {
		return false;
	}

	*a = dstack[--dstack_len];
	return true;
}

bool
dstack_push(KF_INT a)
{
	if (dstack_len == DSTACK_SIZE) {
		return false;
	}

	dstack[dstack_len++] = a;
	return true;
}

bool
dstack_get(size_t i, KF_INT *a)
{
	if (i >= dstack_len) {
		return false;
	}

	*a = dstack[dstack_len - i - 1];
	return true;
}

size_t
dstack_size()
{
	return dstack_len;
}

void
dstack_clear()
{
	dstack_len = 0;
}

static KF_ADDR	rstack[RSTACK_SIZE] = {0};
static size_t	rstack_len = 0;

bool
rstack_pop(KF_ADDR *a)
{
	if (rstack_len == 0) {
		return false;
	}

	*a = rstack[--rstack_len];
	return true;
}

bool
rstack_push(KF_ADDR a)
{
	if (rstack_len == DSTACK_SIZE) {
		return false;
	}

	rstack[rstack_len++] = a;
	return true;
}

bool
rstack_get(size_t i, KF_ADDR *a)
{
	if (i >= rstack_len) {
		return false;
	}

	*a = rstack[rstack_len - i - 1];
	return true;
}

size_t
rstack_size()
{
	return rstack_len;
}

void
rstack_clear()
{
	rstack_len = 0;
}
