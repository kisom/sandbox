#include "defs.h"
#include "io.h"

#include <string.h>

void
write_num(IO *interface, KF_INT n)
{
	static constexpr size_t	nbuflen = 11;
	char buf[nbuflen];
	uint8_t i = nbuflen - 1;
	memset(buf, 0, nbuflen);

	if (n < 0) {
		interface->wrch('-');
	}
	else if (n == 0) {
		interface->wrch('0');
		return;
	}

	while (n != 0) {
		char x = n % 10;
		x = x < 0 ? -x : x;
		x += '0';
		buf[i--] = x;
		n /= 10;
	}

	interface->wrbuf(buf+i, nbuflen - i);
}

void
write_dnum(IO *interface, KF_LONG n)
{
	static constexpr size_t	dnbuflen = 21;
	char buf[dnbuflen];
	uint8_t i = dnbuflen - 1;
	memset(buf, 0, dnbuflen);

	if (n < 0) {
		interface->wrch('-');
	}
	else if (n == 0) {
		interface->wrch('0');
		return;
	}

	while (n != 0) {
		char x = n % 10;
		x = x < 0 ? -x : x;
		x += '0';
		buf[i--] = x;
		n /= 10;
	}

	interface->wrbuf(buf+i, dnbuflen - i);
}


void
write_dstack(IO *interface, Stack<KF_INT> dstack)
{
	KF_INT	tmp;
	interface->wrch('<');
	for (size_t i = 0; i < dstack.size(); i++) {
		if (i > 0) {
			interface->wrch(' ');
		}

		dstack.get(i, tmp);
		write_num(interface, tmp);
	}
	interface->wrch('>');
}
