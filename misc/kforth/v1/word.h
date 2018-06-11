#ifndef __KF_WORD_H__
#define __KF_WORD_H__

#include "defs.h"
#include "parser.h"
#include "stack.h"
#include "system.h"

class Word {
public:
	virtual ~Word() {};

	virtual bool  eval(System *) = 0;
	virtual Word *next(void) = 0;
	virtual bool  match(struct Token *) = 0;
	virtual void  getname(char *, size_t *) = 0;
	virtual uintptr_t address(void) = 0;
};

class Builtin : public Word {
public:
	~Builtin() {};
	Builtin(const char *name, size_t namelen, Word *head, bool (*fun)(System *));
	
	bool  eval(System *);
	Word *next(void);
	bool  match(struct Token *);
	void  getname(char *, size_t *);
	uintptr_t address(void) { return reinterpret_cast<uintptr_t>(this); }
private:
	char		 name[MAX_TOKEN_LENGTH];
	size_t		 namelen;
	Word		*prev;
	bool		(*fun)(System *);
};

class Address : public Word {
public:
	~Address() {};
	Address(const char *name, size_t namelen, Word *head, KF_ADDR addr);

	bool eval(System *);
	Word *next(void);
	bool  match(struct Token *);
	void  getname(char *, size_t *);
	uintptr_t address(void) { return reinterpret_cast<uintptr_t>(this); }
private:
	char	 name[MAX_TOKEN_LENGTH];
	size_t	 namelen;
	Word	*prev;
	KF_ADDR	 addr;
};

#endif // __KF_WORD_H__