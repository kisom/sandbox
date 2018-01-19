member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

memberr(X, X).
memberr(X, [X|_]).
memberr(X, [[H|T1]|T2]) :-
	memberr(X, H);
	memberr(X, T1);
	memberr(X, T2).
memberr(X, [_|T]) :- member(X, T).

sameLen([], []).
sameLen([_|TA], [_|TB]) :- sameLen(TA, TB).
