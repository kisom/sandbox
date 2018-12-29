TinyAdder
========

TinyAdder is a 4-bit adder built on the TinyFPGA BX. It connects four
slide switches (I wanted actual toggle switches, but that's how it goes)
to a pair of 7-segment LCD displays.

Code organisation
-----------------

Currently there are two modules:

+ SegmentLCD: a little awkwardly named due to the naming constraints,
  but this takes a 4-bit input number and outputs a 7-bit output. The
  hookup requirements are described in the module comments.
+ TinyAdder: This is the top-level adder.

Hardware revisions
------------------

+ rev1 (2018-12-22)
+ rev2 (2018-12-22): realised that the LCDs were missing resistors;
  I had been looking at a tutorial that used a shift register; the shift
  register apparently had an internal current limiter so I assumed the
  LCDs didn't need them. This was wrong. Fortunately I was able to cancel
  the rev1 order before it went out.
+ rev3 (2018-12-28): after breadboarding the push button, I realised
  I'd messed up the schematic (and accordingly, the board) by wiring
  both ends of the push buttons to Vcc, which... isn't very useful at
  all. I fixed it, but not before the rev2 was sent to the fab.

TODO
----

+ I'd like to have the two displays show the two numbers before adding
  them. The 'hi' display could show the first addend, and the 'low' could
  show the second addend, with live updating.
