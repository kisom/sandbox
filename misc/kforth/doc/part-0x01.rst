Write You a Forth, 0x01
-----------------------

:date: 2018-02-21 23:17
:tags: wyaf, forth

Following on from the `last post`_ I've decided to frame this as a Write You an
X-type series where I'll write up my thinking and planning as I go.

.. _last post: https://dl.kyleisom.net/posts/2018/02/21/2018-02-21-revisiting-forth/

I've always wanted to write a Forth_; I've made a few attempts_ at it in the
past. This time, I'm actually going to do it.

.. _Forth: https://en.wikipedia.org/wiki/Forth_(programming_language)
.. _attempts: https://github.com/isrlabs/avr-forth

The basics
^^^^^^^^^^

Let's start with the basics: what are the characteristics of a Forth? First,
it's a stack-based language, so it'll need a stack. Actually, it'll need at
least two stacks --- the data stack and the return stack (where return addresses
are normally stored). Modern Forths also have a floating point stack.

Forth calls functions *words*, and the FORTH-83 standard defines a set of
required words for an implementation. Note that there is an ANS Forth, but I'll
target FORTH-83 first for simplicity. The `required words`_ are:

.. _required words: http://forth.sourceforge.net/standard/fst83/fst83-12.htm)

**Nucleus layer**::

    !  *  */  */MOD  +  +!  -  /  /MOD  0<  0=  0>  1+  1-  2+
    2-  2/  <  =  >  >R  ?DUP  @  ABS  AND  C!  C@  CMOVE
    CMOVE>  COUNT  D+  D<  DEPTH  DNEGATE  DROP  DUP  EXECUTE
    EXIT  FILL  I  J  MAX  MIN  MOD  NEGATE  NOT  OR  OVER  PICK
    R>  R@  ROLL  ROT  SWAP  U<  UM*  UM/MOD  XOR

**Device layer**::

    BLOCK  BUFFER  CR  EMIT  EXPECT  FLUSH  KEY  SAVE-BUFFERS
    SPACE  SPACES  TYPE  UPDATE

**Interpreter layer**::

    #  #>  #S  #TIB  '  (  -TRAILING  .  .(  <#  >BODY  >IN
    ABORT  BASE  BLK  CONVERT  DECIMAL  DEFINITIONS  FIND
    FORGET  FORTH  FORTH-83  HERE  HOLD  LOAD  PAD  QUIT  SIGN
    SPAN  TIB  U.  WORD

**Compiler layer**::

    +LOOP  ,  ."  :  ;  ABORT"  ALLOT  BEGIN  COMPILE  CONSTANT
    CREATE  DO  DOES>  ELSE  IF  IMMEDIATE  LEAVE  LITERAL  LOOP
    REPEAT  STATE  THEN  UNTIL  VARIABLE  VOCABULARY  WHILE
    [']  [COMPILE]  ]

In a lot of cases, Forth is also the operating system for the device. This
won't be a target at first, but something to keep in mind as I progress.

Eventually, I'd like to build a zero-allocation Forth that can run on an
STM-32 or an MSP430, but the first goal is going to get a minimal Forth
working. I'll define the stages tentatively as

Stage 1
~~~~~~~

1. Runs on Linux (that's what my Pixelbook runs, more or less).
2. Implements the nucleus layer.
3. Has a REPL that works in a terminal.
4. Explicit non-goal: performance. I'll build a working minimal Forth to get a
   baseline experience.

Stage 2
~~~~~~~

1. Implement the compiler and interpreter layers.

Stage 3
~~~~~~~~

1. Define a block layer interface.
2. Implement a Linux block layer interface.

Stage 4
~~~~~~~~

1. Build a memory management system.
2. Replace all managed memory with the homebrew memory management system.
3. Switch to a JPL rule #3 (no heap allocation) implementation.

Next steps
^^^^^^^^^^

I've decided to use C++ for two reasons: it's supported by all the targets I
want (amd64, arm/arm64, msp430, avr), and I know it well enough (and
importantly, I know the tooling) to get by. Typically, the TI compilers lag
behind the others in supporting newer C++ standards, so those will be the
limiting factor. Fortunately, just a few days before I started this, the TI
wiki was updated_ to note that the latest compilers now support C++11 and
C++14, so I'll target C++14.

As a reminder to myself: this is not going to be the prettiest or best or most
secure or production ready code. The goal is to have fun writing some software
again and to rekindle some of the joy of computing that I had before. Once I
have something working, I can go back and make an exercise of cleaning it up
and refactoring it. The prose in this series is also not going to be my finest
writing ever --- again, it suffices just to do it. The goal is to have
something to show, not to achieve perfection; it'll mostly going to be hacked
on while I'm on the bus or when I have a bit of downtime here and there.

.. _updated: http://processors.wiki.ti.com/index.php/C%2B%2B_Support_in_TI_Compilers#Status_as_of_February_2018

I don't really know what I'm doing, so in the next section, I'll build out the
basic framework and set up the build.