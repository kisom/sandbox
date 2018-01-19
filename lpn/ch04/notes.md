## Chapter 4: Lists

Lists are enclosed in square brackets, and are finite sequences of elements.
Elements can be anything: `[mia, robber(yolanda), X, 2, mia]` or `[mia,
[vincent, jules], [butch, girlfriend(butch)]]` --- a list can contain other
lists.

Prolog lists use the standard head/tail vocabulary, and the decomposition operator is `|`:

```
[Head|Tail] = [mia,  [vincent,  jules],  [butch,  girlfriend(butch)]]
```

Note that the empty list behaves as one would think.

```
 ?-  [X|Y]  =  [].
   
   no 
````

Arguments can be chained, such as `[X, Y | Z]`.

### The `member` function

```
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).
```

