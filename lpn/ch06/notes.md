## Chapter 6: More lists

### append

append(L1, L2, L3) ⇒ K3 ← L1 + L2

Definition:

```
append([], L, L).
append([H|T], L2, [H|L3]) :- append(T, L2, L3).
```

> [This] illustrates a more general theme: the use of unification to build
> structure. In a nutshell, the recursive calls to append/3 build up this
> nested pattern of variables which code up the required answer. When Prolog
> finally instantiates the innermost variable `_G593` to `[1, 2, 3]`, the
> answer crystallises out, like a snowflake forming around a grain of dust.
> But it is unification, not magic, that produces the result.

The most obvious use is concatenation; but we can build other predicates, too:

```
prefix(P, L) :- append(P, _, L).
suffix(S, L) :- append(_, S, L).
```

We can generate sublists: the text notes that the sublists are the suffixes of
the prefixes of the list. In retrospect, it makes sense. This can be defined as

```
sublists(SubL, L) :- suffix(S, L), prefix(SubL, S).
```

## Reversing a list

`append/3` isn't always what we want and is pretty inefficient. For example, if
we want to reverse a list using the following recursive definition:

1. Reversing the empty list returns the empty list.
2. Otherwise, given [H|T], return [reverse(T)|[H]]


```
reverse([], []).
reverse([H|T], R) :- reverse(T, RevT), append(RevT, [H], R).
```

If a trace is run on a call, it's apparent it's doing a lot of extra work. For
example, given `reverse([a, b, c, d, e], R)`, 12 calls are made to `reverse`
and 30 calls to `append`.

