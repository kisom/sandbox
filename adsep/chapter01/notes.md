# Fundamental Data Structures

+ information to be processed: *abstraction* of some part of the real
  world
+ computer has a selected set of *data* about the real world
+ this data is an abstraction:
  + certain properties/characteristics of the real world are ignored
    because they are considered peripheral to the problem
  + therefore represents a simplification of facts
+ problem solving: defining the set of characteristics relevant to the
  problem
+ the *choice of representation* may involve several levels of detail:
  consider deciding the position of an object.
  + polar v. cartesian coordinates (and selection of origin)
  + floating point v. integer representation of coordinates

## The concept of data type

+ in maths, we have different types: whole, integer, rational, etc...
+ principle: *every constant, variable, expression, or function is of
  a certain data type*
+ adherence to this means the compiler can check constructs for
  legality and compatibility
+ programming languages provide base data types, which can be extended
  by the programmer
+ *cardinality*: the number of distinct values belonging to some type
  *T*
+ *methods of structuring*:
  + the array
  + the record (aka structure)
  + the set
  + the sequence (aka file)
+ operators provide a mechanism for using data
  + comparison and assignment: most important basic operators
    + assignment: `:=`
	+ equality: `=`
  + *transfer operator*: translate between data types
  + *selector* instead of *getter*

## Primitive data types

+ enum: integer used when the data type type represents a choice from
  a small set of choices
  + rule 1.1: **type** *T* = (*c*_1, *c*_2, …, *c*_n)
  + cardinality of T: card(T) ← *n*
  + ordered types define *succ(x)* and *pred(x)*
  + ordering among values of *T* is defined by rule 1.2: (*c*_i < *c*_j) = (*i* < *j*)

## Standard primitive types

+ four types: integer, Boolean, char, real
+ integers
  + subset of whole numbers depending on implementation (e.g. 32-bit v. 64-bit)
  + four basic operations: +, -, *, **div**
  + integer division returns an integer result, e.g. for positive *m* and *n*
  + rule 1.3: *m* - *n* < (*m* **div** *n*) * *n* <= *m*
  + modulus operator is defined by
  + rule 1.4: (*m* **div** *n*) * *n* + (*m* **mod** *n*) = *m*
+ real
  + limitations on precision due to implementation
  + real division: **/**
+ Boolean
  + operators: **and**, **or**, **not**
  + comparisons are operators yielding a Boolean
+ char
  + printable character
  + standard char ←→ integer transfer functions: *ord* and *chr*
