## Recursion

Recursive definitions require that the recursive function isn't the first
clause, ex:

```
is_digesting(X,Y) :- just_ate(X,Y).
is_digesting(X,Y) :-
just_ate(X,Z),
is_digesting(Z,Y).
```

Recursive definition require a base ("escape") clause in addition to the
recursive clause.

### Rule ordering, goal ordering, and termination

Underlying vision for Prolog (and logic programming in general): the programmer
should be able to describe problems, and the computer derives the solution. The
programmer builds a knowledge base, then asks question of the computer.

Prolog's search order:

1. KB from top to bottom
2. Clauses from left to right
3. Backtracks on bad choices

This is somewhat of a procedural approach to declarative programming which may
affect the answer. Compare `descend1` with `descend2`:

```
child(anne,bridget).
child(bridget,caroline).
child(caroline,donna).
child(donna,emily).

descend1(X,Y) :- child(X,Y).
descend1(X,Y) :- child(X,Z),
                 descend1(Z,Y).

descend2(X,Y) :- descend2(Z,Y),
                 child(X,Z).

descend2(X,Y) :- child(X,Y).
```

Supposedly these give different answers, but `swipl` exploded on the latter...
and continuing reading, that's the point --- a warning to avoid left-recursive
rules.

The takeaway is that you can get the overall idea of how to write the program
by approaching the problem declaratively. Sensible goal orderings ensure
program / query termination: place the recursive definition last.

### Exercises

#### Exercise 3.1

In the text, we discussed the predicate

```
descend(X,Y)  :-  child(X,Y).
descend(X,Y)  :-  child(X,Z),
                  descend(Z,Y).
```

Suppose we reformulated this predicate as follows:

```
descend(X,Y)  :-  child(X,Y).
descend(X,Y)  :-  descend(X,Z),
                  descend(Z,Y).
```

Would this be problematic?

**A**: yes: there's no termination clause. See `descend3` in `descend.pl`.

#### Exercise 3.2

Do you know these wooden Russian dolls (Matryoshka dolls) where the smaller
ones are contained in bigger ones?

Ex katarina(olga(natasha(irina))).

First, write a knowledge base using the predicate `directlyIn/2` which encodes
which doll is directly contained in which other doll. Then, define a recursive
predicate `in/2`, that tells us which doll is (directly or indirectly)
contained in which other dolls. For example, the query `in(katarina,natasha)`
should evaluate to true, while `in(olga, katarina)` should fail.

```
directlyIn(natasha, irina).
directlyIn(olga, natasha).
directlyIn(katarina, olga).

in(X, Y) :- directlyIn(X, Y).
in(X, Y) :- directlyIn(X, Z),
            in(Z, Y).
```

#### Exercise 3.3

We have the following knowledge base:

```
directTrain(saarbruecken,dudweiler).
directTrain(forbach,saarbruecken).
directTrain(freyming,forbach).
directTrain(stAvold,freyming).
directTrain(fahlquemont,stAvold).
directTrain(metz,fahlquemont).
directTrain(nancy,metz).
```

That is, this knowledge base holds facts about towns it is possible to travel
between by taking a direct train. But of course, we can travel further by
chaining together direct train journeys. Write a recursive predicate
`travelFromTo/2` that tells us when we can travel by train between two towns.
For example, when given the query `travelFromTo(nancy,saarbruecken)` it should
reply yes.

```
travelFromTo(X, Y) :- directTrain(X, Y).
travelFromTo(X, Y) :- directTrain(X, Z),
                      travelFromTo(Z, Y).
```

#### Exercise 3.4

Define a predicate `greater_than/2` that takes two numerals in the notation
that we introduced in the text (that is, `0`, `succ(0)`, `succ(succ(0))`, and
so on) as arguments and decides whether the first one is greater than the
second one. For example:

```
?-  greater_than(succ(succ(succ(0))),succ(0)).
yes
?-  greater_than(succ(succ(0)),succ(succ(succ(0)))).
no
```

Answer (in `succ.pl`):

```
greater_than(succ(succ(X)), succ(0)).
greater_than(succ(X), succ(Y)) :-
    greater_than(X, Y).
```

#### Exercise 3.5

Binary trees are trees where all internal nodes have exactly two children. The
smallest binary trees consist of only one leaf node. We will represent leaf
nodes as `leaf(Label)`. For instance, `leaf(3)` and `leaf(7)` are leaf nodes,
and therefore small binary trees. Given two binary trees B1 and B2 we can
combine them into one binary tree using the functor `tree/2` as follows:
`tree(B1,B2)`. So, from the leaves `leaf(1)` and `leaf(2)` we can build the
binary tree `tree(leaf(1),leaf(2))` . And from the binary trees
`tree(leaf(1),leaf(2))` and `leaf(4)` we can build the binary tree
`tree(tree(leaf(1), leaf(2)),leaf(4))`.

Now, define a predicate `swap/2`, which produces the mirror image of the binary
tree that is its first argument. For example:

```
?-  swap(tree(tree(leaf(1),  leaf(2)),  leaf(4)),T).
T  =  tree(leaf(4),  tree(leaf(2),  leaf(1))).
yes
```

This one took a lot of thinking, but the solution is in `tree.pl`.
