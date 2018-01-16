-- # The Meaning of Programs

-- What is computation? what a computer does.

-- computation environment:
--
-- * machine
-- * language
-- * program
-- 
-- Fundamentally, programming is about ideas: a program is a snapshot of
-- an idea, a codification of the mental structures of the programmer(s)
-- involved.
--  
-- ## The Meaning of "Meaning"
--  
-- *semantics* is the connection between words and meanings: while "dog"
-- is an arrangment of shapes on a page, an actual dog is a very separate
-- things. Semantics relates concrete signifiers and abstract meanings,
-- and attempts to study the fundamental nature of the abstract meanings.
--  
-- *Formal semantics* is the attempt at formalising the meanings of
-- programmings, and using this formalisation to reason about
-- languages. In order to specify a programming language, we need to
-- define both its *syntax* (the representation) and its *semantics* (the
-- meaning).
--  
-- Most languages lack a formal specification and opt to use a canonical
-- reference implementation. An alternative is to write a prose
-- specification, which is the approach of C++. A third approach is to
-- mathematically specify the language such that automated mathematical
-- analysis can be done.
--  
-- ### Syntax
--  
-- The language's syntax is what differentiates valid examples of code like

y x = x + 1

-- from nonsense like `$%EHI`. In general, a parser reads a string
-- (like "y x = x + 1") and turns it into an /abstract syntax
-- tree/. Syntax is ultimately only concerned with the surface
-- appearance of the program

-- ### Operational Semantics

-- A practical means of thinking about the meaning of a program is
-- *what it does*. *operational semantics* defines rules for how
-- programs run on some machine (often an *abstract machine*). 

-- ### Small-Step Semantics
--
-- Let's imagine an abstract machine that evaluates by directly
-- operating on the syntax, reducing it iteratively to bring about the
-- final result.
--
-- For example, to evaluate (1 * 2) + (3 * 4):

-- 0. Compute the left-hand multiplication (1 * 2 -> 2), simplifying
-- the expression to 2 + (3 * 4).

-- 0. Compute the right-hand multiplication (3 * 14 -> 12),
-- simplifying the expression to 2 + 12.

-- 0. Carry out the addition, resulting in 14.

-- We determine that 14 is the result because it cannot be simplified
-- any further. It is a special type of algebraic expression (a
-- *value*) with its own meaning.

-- We can write down formal rules like these describing how to proceed
-- with each small reduction step; these rules are written in a
-- *metalanguage* (which is often mathematical notation).

-- Let's explore the semantics of a toy language called SIMPLE. There
-- is a formal mathematical language to this, but we'll use a
-- programming language to make it more clear.

class AST a where
  reduce :: a -> Number
  reducible :: a -> Bool

data Number = Number { value :: Integer } deriving (Show)

instance AST Number where
  reduce (Number value) = (Number value)
  reducible (Number _) = False

data Add = Add {  leftAdd :: Number
                 , rightAdd :: Number
} deriving (Show)

instance AST Add where
  reduce (Add x y) = Number (value x + value y)
  reducible (Add _ _) = True

data Multiply = Multiply {  leftMult :: Number
                           ,rightMult :: Number
} deriving (Show)

-- We can use these to build an AST by hand:

-- print Add(Multiply (Number 2) Multiply (Number 3))

