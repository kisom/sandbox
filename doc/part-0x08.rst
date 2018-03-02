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

