
What is computation? what a computer does.

computation environment:

* machine

* language

* program

Fundamentally, programming is about ideas: a program is a snapshot of
an idea, a codification of the mental structures of the programmer(s)
involved.

== The Meaning of "Meaning"

/semantics/ is the connection between words and meanings: while "dog"
is an arrangment of shapes on a page, an actual dog is a very separate
things. Semantics relates concrete signifiers and abstract meanings,
and attempts to study the fundamental nature of the abstract meanings.

_Formal semantics_ is the attempt at formalising the meanings of
programmings, and using this formalisation to reason about
languages. In order to specify a programming language, we need to
define both its /syntax/ (the representation) and its /semantics/ (the
meaning).

Most languages lack a formal specification and opt to use a canonical
reference implementation. An alternative is to write a prose
specification, which is the approach of C++. A third approach is to
mathematically specify the language such that automated mathematical
analysis can be done.

=== Syntax

The language's syntax is what differentiates valid examples of code like

> y x = x + 1

from nonsense like `$%EHI`. In general, a parser reads a string (like
"y x = x + 1") and turns it into an /abstract syntax tree/. Syntax is ultimately only concerned with the surface appearance of the program

=== Operational Semantics

A practical means of thinking about the meaning of a program is /what it does/.
