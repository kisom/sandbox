#ifndef __KF_WORD_H__
#define __KF_WORD_H__

/*
 * Every word in the dictionary starts with a header:
 * uint8_t	 length;
 * uint8_t	 flags;
 * char		*name;
 * uintptr_t	 next;
 *
 * The body looks like the following:
 * uintptr_t	 codeword;
 * uintptr_t	 body[];
 *
 * The codeword is the interpreter for the body. This is defined in
 * eval.c. Note that a native (or builtin function) has only a single
 * body element.
 *
 * The body of a native word points to a function that's compiled in already.
 */

void	append_native_word(const char *, const uint8_t, void(*)(void));
bool	execute(const char *, const uint8_t);
bool	lookup(const char *, const uint8_t, uintptr_t *);

/*
 * store_native writes a new dictionary entry for a native-compiled
 * function.
 */
void	store_native(uint8_t *, const char *, const uint8_t, void(*)(void));

/*
 * match_word returns true if the current dictionary entry matches the
 * token being searched for.
 */
bool	match_word(uint8_t *, const char *, const uint8_t);

/*
 * word_link returns the offset to the next word.
 */
size_t	word_link(uint8_t *);
	
size_t	word_body(uint8_t *);

#endif /* __KF_WORD_H__ */
