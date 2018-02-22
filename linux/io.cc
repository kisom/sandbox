#include <iostream>
#include "../io.h"
#include "io.h"

char
Console::rdch()
{
	std::cout.flush();
	return getchar();
}


void
Console::wrch(char c)
{
	std::cout << c;
}


size_t
Console::rdbuf(char *buf, size_t len, bool stopat, char stopch)
{
	size_t	n = 0;
	char	ch;

	while (n < len) {
		ch = this->rdch();

		if (stopat && stopch == ch) {
			break;
		}

		buf[n++] = ch;
	}
	
	return n;
}


void
Console::wrbuf(char *buf, size_t len)
{
	for (size_t n = 0; n < len; n++) {
		this->wrch(buf[n]);
	}
}


// Line I/O
bool
Console::rdln(char *buf, size_t len, size_t *readlen) {
	size_t	n = 0;
	char	ch;
	bool	line = false;

	while (n < len) {
		ch = this->rdch();

		if (ch == '\n') {
			line = true;
			break;
		}

		buf[n++] = ch;
	}

	if (nullptr != readlen) {
		*readlen = n;
	}
	return line;
}


void
Console::wrln(char *buf, size_t len)
{
	this->wrbuf(buf, len);
	this->wrch(0x0a);
}