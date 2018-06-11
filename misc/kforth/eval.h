#ifndef __KF_EVAL_H__
#define __KF_EVAL_H__

#include "defs.h"


/*
 * cwexec is the codeword executor. It assumes that the uintptr_t
 * passed into it points to the correct executor (e.g. nexec), 
 * which is called with the next address.
 */
void	cwexec(uintptr_t);


/*
 * nexec is the native executor. 
 *
 * It should take a uintptr_t containing the address of a code block
 * and will execute the function starting there. The function should
 * half the signature void(*target)(void) - a function returning
 * nothing and taking no arguments.
 */
void	nexec(uintptr_t);

static const uintptr_t	nexec_p = (uintptr_t)&nexec;


#endif /* __KF_EVAL_H__ */
