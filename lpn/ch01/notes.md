## Chapter 1: Some basic syntax

```
property(name).
proposition.
/* rules */
head :- tail.
```

Example:

```
happy(yolanda).
listens2Music(mia).
listens2Music(yolanda):- happy(yolanda).
playsAirGuitar(mia):- listens2Music(mia).
playsAirGuitar(yolanda):- listens2Music(yolanda).
```

### Rules

Conditional fact:

```
/* source */
listensToMusic(X) :- happy(X).
playsAirGuitar(X) :- listensToMusic(X).
happy(a).
/* interactive */
playsAirGuitar(a). /* true */
playsAirGuitar(b). /* false */
```

## KB5

```
loves(vincent,mia).
loves(marsellus,mia).
loves(pumpkin,honey_bunny).
loves(honey_bunny,pumpkin).
jealous(X,Y):- loves(X,Z), loves(Y,Z).
```

Note that when you enter queries, sometimes it waits for you to enter something
(so far seems like a semi-colon finishes a query). For example:

```
2 ?- jealous(vincent, W).
W = vincent
```

Output freezes there, waiting on input. Enter the semicolon:

```
2 ?- jealous(vincent, W).
W = vincent ;
W = marsellus.
```

Replacing both atoms with variable:

```
3 ?- jealous(A, B).
A = B, B = vincent ;
A = vincent,
B = marsellus ;
A = marsellus,
B = vincent ;
A = B, B = marsellus ;
A = B, B = pumpkin ;
A = B, B = honey_bunny.
```

This brings up something interesting: *jealous(A, A)* is always true:

```
4 ?- jealous(A, A).
A = vincent ;
A = marsellus ;
A = pumpkin ;
A = honey_bunny.
```

Can we fix that in the definitions?

```
jealous(X,Y):- loves(X,Z), loves(Y,Z), X \= Y.
```

Now:

```
9 ?- jealous(A, A).
false.
```

Re-reading a previous section, seems that ';' means "or".
