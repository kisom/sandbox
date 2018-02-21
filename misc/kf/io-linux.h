#ifndef __KF_IO_LINUX_H__
#define __KF_IO_LINUX_H__

#include "io.h"

class Console : public IO {
public:
	~Console() {};
	char	rdch(void);
	void	wrch(char c);

	// Buffer I/O.
	void	rdbuf(char *buf, size_t len, bool stopat, char stopch);
	void	wrbuf(char *buf, size_t len);

	// Line I/O
	bool	rdln(char *buf, size_t len);
private:
};

#endif // __KF_IO_LINUX_H__
