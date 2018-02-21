#include <iostream>
#include "io.h"
#include "io-linux.h"

char
Console::rdch()
{
	char ch;
	std::cin >> ch;
	return ch;
}


void
Console::wrch(char c)
{
	std::cout << c;
}

void
Console::rdbuf(char *buf, size_t len, bool stopat, char stopch)
{
	size_t	n = 0;
	char	ch;

	while (n < len) {
		std::cin >> ch;
		if (stopat && stopch == ch) {
			break;
		}

		buf[n] = ch;
	}
}

void
Console::wrbuf(char *buf, size_t len)
{
	for (size_t n = 0; n < len; n++) {
		std::cout << buf[n];
	}
}

// Line I/O
bool
Console::rdln(char *buf, size_t len) {
	size_t	n = 0;
	char	ch;
	bool	line = false;

	while (n < len) {
		std::cin >> ch;
		if (ch == 0xa) {
			line = true;
			break;
		}

		buf[n] = ch;
	}

	return line;
}