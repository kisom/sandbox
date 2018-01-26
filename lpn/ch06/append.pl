append([], L, L).
append([H|T], L2, [H|L3]) :- append(T, L2, L3).

prefix(P, L) :- append(P, _, L).
suffix(S, L) :- append(_, S, L).

sublists(SubL, L) :- suffix(S, L), prefix(SubL, S).

%% reverse([], []).
%% reverse([H|T], R) :- reverse(T, RevT), append(RevT, [H], R).

reverse([], A, A).
reverse([H|T], A, R) :- reverse(T, [H|A], R).
