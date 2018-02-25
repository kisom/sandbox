Write You a Forth, 0x05
-----------------------

:date: 2018-02-24 12:23
:tags: wyaf, forth

Today I need to start actually doing things with tokens. This requires two
things:

1. Some idea of what a word is, and
2. A dictionary of words

I started taking some notes on this previously, and I think there are a few
kinds of words that are possible:

1. Numbers (e.g. defining a variable)
2. Built-in functions
3. Lambda functions (that is, user-defined functions).

Stage 1 really only needs to incorporate #2, so that's what I'll focus on for
now. However, to prepare for the future, I'm going to define a ``Word`` base
class and inherit from there. This interface is going to need to be
stack-aware, so what I've done is define a ``System`` struct in ``system.h``::

        #ifndef __KF_CORE_H__
        #define __KF_CORE_H__
        
        #include "defs.h"
        #include "stack.h"
        
        typedef struct _System {
                Stack<KF_INT>        dstack;
        } System;
        
        
        #endif // __KF_CORE_H__

This will let me later add in support for the return stack and other things
that might be useful. Other ideas: adding something like an ``errno``
equivalent to indicate the last error, and storing a dictionary of words. This
will need some restructuring, though. Anyways, this works for now.

The Word interface
^^^^^^^^^^^^^^^^^^

Now I can start defining a Word.h.