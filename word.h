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
};

class Builtin : public Word {
public:
	~Builtin() {};
	Builtin(const char *name, size_t namelen, Word *head, bool (*fun)(System *));
	
	bool  eval(System *);
	Word *next(void);
	bool  match(struct Token *);
	void  getname(char *, size_t *);
	
private:
	char		 name[MAX_TOKEN_LENGTH];
	size_t		 namelen;
	Word		*prev;
	bool		(*fun)(System *);
};


#endif // __KF_WORD_H__