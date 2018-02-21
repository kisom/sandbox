#ifndef __KF_IO_H__
#define __KF_IO_H__

// TODO(kyle): make this selectable by architecture.

#ifdef __linux__
#include <stddef.h>
#endif

class IO {
public:
	// Virtual destructor is required in all ABCs.
	virtual ~IO() {};

	// Building block methods.
	virtual char	rdch(void) = 0;
	virtual void	wrch(char c) = 0;

	// Buffer I/O.
	virtual void	rdbuf(char *buf, size_t len, bool stopat, char stopch) = 0;
	virtual void	wrbuf(char *buf, size_t len) = 0;

	// Line I/O
	virtual bool	rdln(char *buf, size_t len) = 0;
};

#endif // __KF_IO_H__