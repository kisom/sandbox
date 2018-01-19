len([], 0).
len([_|T], N) :-
	len(T, X),
	N is X+1.

alen_([], A, A).
alen_([_|H], A, L) :-
	A2 is A+1,
	alen_(H, A2, L).
alen(X, L) :- alen_(X, 0, L).

max([], N, N).
max([H|T], N, M) :-
	H > N,
	max(T, H, M).
max([H|T], N, M) :-
	H =< N,
	max(T, N, M).

max([H|T], N) :-
	max(T, H, N).