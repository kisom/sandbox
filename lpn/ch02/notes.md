## Chapter 2: Unification

Two terms unify if they are
1. The same term
2. They contain variables that can be uniformly instantiated such that the
   resulting terms are equal.

Unification is foundational to Prolog.

`=/2` checks whether its two arguments unify:

```
12 ?- =(a, a).
true.
13 ?- =(a, b).
false.
```

*NB* variable assignment with `=`:

```
24 ?- vincent = X.
X = vincent.
25 ?- jealous(Y, X).
Y = vincent,
X = marsellus ;
Y = marsellus,
X = vincent ;
false.
```

### Lines
```
/* lines.pl */
vertical(line(point(X,Y),point(X,Z))).
horizontal(line(point(X,Y),point(Z,Y))).
```

Check this out:

```
39 ?- vertical(line(point(1, 7), P)).
P = point(1, _402).
```

The answer is structured: `_402` is a placeholder variable that means "any old
value of Y will do." This answer was derived solely through unification, and
didn't require logic (e.g. modus ponens) or any sort of mathematical
relationships. "Moreover, when a program is written that makes heavy use of
unification, it is likely to be extremely efficient."