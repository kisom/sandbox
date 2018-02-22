#ifndef __KF_IO_H__
#define __KF_IO_H__

#include "defs.h"

class IO {
public:
	// Virtual destructor is required in all ABCs.
	virtual ~IO() {};

	// Building block methods.
	virtual char	rdch(void) = 0;
	virtual void	wrch(char c) = 0;

	// Buffer I/O.
	virtual size_t	rdbuf(char *buf, size_t len, bool stopat, char stopch) = 0;
	virtual void	wrbuf(char *buf, size_t len) = 0;

	// Line I/O
	virtual bool	rdln(char *buf, size_t len, size_t *readlen) = 0;
	virtual void	wrln(char *buf, size_t len) = 0;
};

#endif // __KF_IO_H__