## Chapter 5: Arithmetic in Prolog

Prolog provides basic arithmetic operators.

Ex.

```
  ?-  8  is  6+2.
   yes
   
   ?-  12  is  6*2.
   yes
   
   ?-  -2  is  6-8.
   yes
   
   ?-  3  is  6/2.
   yes
   
   ?-  1  is  mod(7,2).
   yes 

   ?- X is 12/4.
   X = 3.
```

The operators don't actually do arithmetic:

```
?- X = 2 + 3.
X = 2+3.
```

The default is just to do unification; `is` must be used. The arithmetic
expression must be on the RHS. This part of Prolog is a black box that
handles this, and isn't part of the normal KB and unification parts.

### Arithmetic and lists

A recursive list length calculator:

```
len([], 0).
len([_|T], N) :-
	len(T, X),
	N is X+1.
```

A tail-recursive length calculator:

```
alen_([], A, A).
alen_([_|H], A, L) :-
	A2 is A+1,
	alen_(H, A2, L).
alen(X, L) :- alen_(X, 0, L).
```

Standard notes about tail recursion efficiency apply here.

### Comparing integers

* *x < y* &rarr; `X < Y.`
* *x ≤ y* &rarr; `X =< Y.`
* *x = y* &rarr; `X =:= Y.`
* *x ≠ y* &rarr; `X =\= Y.`
* *x ≥ y* &rarr; `X >= Y`
* *x > y* &rarr; `X > Y`

Note the difference between `=` and `=:=`.

Let's write a `max` function:

```