#include "defs.h"
#include "io.h"

#include <string.h>

static constexpr size_t	nbuflen = 11;

void
write_num(IO &interface, KF_INT n)
{

	// TODO(kyle): make the size of the buffer depend on the size of
	// KF_INT.
	char buf[nbuflen];
	uint8_t i = nbuflen;
	memset(buf, 0, i);
	bool neg = n < 0;

	if (n < 0) {
		interface.wrch('-');
		n = ~n;
	}

	while (n != 0) {
		char ch = (n % 10) + '0';
		if (neg && (i == nbuflen)) ch++;
		buf[i-1] = ch;
		i--;
		n /= 10;
	}

	uint8_t buflen = nbuflen - i % nbuflen;
	interface.wrbuf(buf+i, buflen);
}