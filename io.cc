#include "defs.h"
#include "io.h"

#include <string.h>

void
write_num(IO &interface, KF_INT n)
{

	// TODO(kyle): make the size of the buffer depend on the size of 
	// KF_INT.
	char buf[10];
	uint8_t i = 10;
	memset(buf, 0, i);
	if (n < 0) {
		interface.wrch('-');
		n *= -1;
	}

	while (n != 0) {
		char ch = (n % 10) + '0';
		buf[i--] = ch;
		n /= 10;
	}

	interface.wrbuf(buf+i, 11-i);
}