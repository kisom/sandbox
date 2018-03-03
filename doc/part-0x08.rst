Write You a Forth, 0x08
-----------------------

:date: 2018-03-01 19:31
:tags: wyaf, forth

After reading some more in Threaded Interpreted Languages (TIL_ from now on),
I've decided to start over.

.. _TIL: http://wiki.c2.com/?ThreadedInterpretiveLanguage

Some design choices that didn't really work out:

+ the system structure
+ not making it easier to test building for different platforms
+ my linked list approach to the dictionary
+ my class-based approach to words

I get the distinct feeling that I could (maybe should) be doing this in C99, so
I think I'll switch to that.

The new design
^^^^^^^^^^^^^^

I'll need to provide a few initial pieces:

1. eval.c
2. stack.c
3. the platform parts

I'll skip the parser at first and hand hack some things, then try to
port over my I/O layer from before.
