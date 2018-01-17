numeral(0).
numeral(succ(X)) :- numeral(X).

%% add/3: add(X, Y, Z) -> X + Y = Z
add(0, X, X).
add(succ(X), Y, succ(Z)) :-
  add(X, Y, Z).

