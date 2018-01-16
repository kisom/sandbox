Chapter 2: The Meaning of Programs
==================================

> module Meaning where

Programming is about ideas, not about the programs themselves;
they're manifestations (sometimes even physical) of someone's ideas;
there must be some *raison d'être* for program to be written. That
reason can be to scratch an itch, but some motivation needs to drive
the program.

Linguistics has a field called *semantics*, which "is the study of
the connection between words and their meanings;" computer science
has *formal semantics*, which deals with specifying these connections
mathemtically (more or less). Specifying a programming language
requires both a syntax (*how does the program look*?) and semantics
(*what does the program mean*?). Funny enough, many (most?) PL's
don't have some canonical formal specification, relying on
"specification by implementation."


Syntax
------

Remember: syntax = how does the program look?

Programming is currently done largely as text files, which are
really just long strings of characters where newlines give the
appearance of some structure. We fead this string into a parser of
some sort, which knows the language's syntax rules. The parser takes
the string and converts into an *abstract syntax tree*, which is
the computer's representation of the program in memory. **N.B.**:
parsers are covered later.

Syntax provides a mechanism for the meaning of a program to be
expressed, but it doesn't have any deeper meaning. The connection
is entirely arbitrary, though there are common patterns that have
arisen (c.f. the C-style of langauges).


Operational Semantics
---------------------

"The most practical way to think about the meaning of a program is
*what it does*." That is, what happens when it runs? I really liked
how Tom phrased it: "How do different constructs in the programming
language behave at run time, and what effect do they have when
they're plugged together to make larger programs?"

*Operational semantics* defines rules for how programs execute,
typically on an *abstract machine*, allowing for more precise
specifications of a language's behaviour.


Small-Step Semantics
--------------------

How do we go about designing an abstract machine? A first step is
to think about how a program that works by reduction on the syntax.
For example, evaluating (1 × 2) + (3 × 4):

1. First, the leftmost expression is reduced: × is applied to 1
   and 2. This leaves us with 2 + (3 × 4).
2. The rightmost expression is reduced: × is applied to 3
   and 4. This leaves us with 2 + 12.
3. Finally, + is applied to 2 and 12, yielding 14.
4. 14 can't be reduced any further, so this is taken as the result
   of the expression. This is a *value*, which has a standalone
   meaning.

The rules for these reductions need to be codified in their own
*metalanguage*. To explore the semantics of a very simple programming
language called SIMPLE.

[ insert meaning_simple_semantics.png here ]

These are *inference rules* defining a *reduction relation* on
SIMPLE ASTs. "Practically speaking, it's a bunch of weird symbols
that don't say anything intelligible about the meaning of computer
programs." Time to dive into an implementation!

Tom is careful to note that this isn't an attempt at specification
by implementation; it's making the description more approachable
for readers without a mathematical background.


Expressions
-----------

We can start by defining some types for the different elements ofthe AST.

> data Element = Number Int                 |
>                Boolean Bool               |
>                Add Element Element        |
>                Multiply Element Element   |
>                LessThan Element Element   |
>                Equals Element Element     |
>                GreaterThan Element Element

For convenience, we'll override show. In the book, SIMPLE ASTs are
displayed inside «».

> -- strElement returns the string representation of an AST element.
> strElement :: Element -> String
> strElement (Number n) = show n
> strElement (Boolean b) = show b
> strElement (Add a b)  = (strElement a) ++ " + " ++ (strElement b)
> strElement (Multiply a b)  = (strElement a) ++ " × " ++ (strElement b)

> -- showElement implements show for Element.
> showElement :: Element -> String
> showElement e = "«" ++ (strElement e) ++ "»"
> instance Show Element where show = showElement

These elements can be strung together to hand-build an AST:

> anExpression = Add (Multiply (Number 1) (Number 2))
>                    (Multiply (Number 3) (Number 4))

This *should* display as

```
Ok, modules loaded: Meaning.
*Meaning>
*Meaning> anExpression
«1 × 2 + 3 × 4»
```

Note that this doesn't take into account operator precedence, so
`anExpression` is the same as `otherExpression` defined below:

> otherExpression = Multiply (Number 1)
>                            (Multiply (Add (Number 2) (Number 3))
>                                      (Number 4))

With proper precendence, this should be `1 * (2 + 3) * 4`, but note

```
*Meaning> anExpression
«1 × 2 + 3 × 4»
*Meaning> otherExpression
«1 × 2 + 3 × 4»
```

This is a problem, but it's not relevant to the discussion of the
semantics of SIMPLE.

Now we need to implement a reducer. First, let's determine if an
expression *can* be reduced:

> -- reducible returns true if the expression is reducible.
> reducible :: Element -> Bool
> reducible (Number _) = False
> reducible _          = True

> -- reduce reduces the expression.
> reduce :: Element -> Element
> reduce n@(Number _) = n
> reduce (Add (Number a) (Number b)) = Number (a + b)
> reduce (Add a b) = if (reducible a)
>                    then (Add (reduce a) b)
>                    else (Add a (reduce b))
> reduce (Multiply (Number a) (Number b)) = Number (a * b)
> reduce (Multiply a b) = if (reducible a)
>                         then (Multiply (reduce a) b)
>                         else (Multiply a (reduce b))

Using `reduce` right now means repeatedly running `reduce` over
functions:

```
*Meaning> reduce anExpression
«2 + 3 × 4»
*Meaning> reduce (reduce anExpression)
«2 + 12»
*Meaning> reduce (reduce (reduce anExpression))
«14»
```

Note that `reduce` returns different expressions for `anExpression`
and `otherExpression`, which demonstrates that the string display
issue is tangential to our semantics:

```
*Meaning> reduce anExpression
«2 + 3 × 4»
*Meaning> reduce otherExpression
«1 × 5 × 4»
```

Let's abstract this into a machine; note that this won't work quite
like the Ruby examples in the book, as we can't really keep state
in Haskell.

> -- Machine abstracts the SIMPLE VM.
> data Machine = Machine Element
> instance Show Machine where
>     show (Machine e) = "EXP ← " ++ (show e)

We can define two functions on it: `step` and `run`:

> -- step runs a single reduction.
> step :: Machine -> Machine
> step (Machine e) = Machine (reduce e)

> -- run keeps calling step until a non-reducible expression is
> -- found.
> run :: Machine -> IO Machine
> run m@(Machine e) = do
>     putStrLn (show m)
>     if (reducible e)
>     then run (step m)
>     else return (Machine e)

Let's try this with `anExpression`:

> anMachine = Machine anExpression

```
*Meaning> step anMachine
EXP ← «2 + 3 × 4»
*Meaning> run anMachine
EXP ← «1 × 2 + 3 × 4»
EXP ← «2 + 3 × 4»
EXP ← «2 + 12»
EXP ← «14»
EXP ← «14»
```
