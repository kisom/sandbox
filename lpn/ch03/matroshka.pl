directlyIn(natasha, irina).
directlyIn(olga, natasha).
directlyIn(katarina, olga).

in(X, Y) :- directlyIn(X, Y).
in(X, Y) :- directlyIn(X, Z),
            in(Z, Y).
