-module(lib_misc).
-export([
  for/3,
  qsort/1,
  pythag/1,
  perms/1,
  kabs/1,
  kfilter/2,
  kclassify/1,
  stash_term/2,
  retrieve_term/1
]).

for(Max, Max, F) ->
    [F(Max)];
for(I, Max, F) ->
    [F(I)|for(I+1, Max, F)].

qsort([]) ->
    [];
qsort([Pivot|T]) ->
    qsort([X || X <- T,
		X < Pivot])
	++
	[Pivot]
	++
	qsort([X || X <- T,
		    X >= Pivot]).


pythag(N) ->
    [{A,B,C} ||
	A <- lists:seq(1,N),
	B <- lists:seq(1,N),
	C <- lists:seq(1,N),
	A+B+C =< N,
	A*A+B*B =:= C*C
    ].

perms([]) ->
    [[]];
perms(L) ->
    [[H|T] || H <- L,
	       T <- perms(L--[H])].

kabs(V) when is_integer(V), V >= 0 ->
    V;
kabs(V) when is_integer(V) -> -V.

kfilter(P, [H|T]) ->
    case P(H) of
	true  -> [H|kfilter(P, T)];
	false -> kfilter(P, T)
    end;
kfilter(_, []) -> [].

kclassify(N) ->
    if
	N >= 10 ->
	    high;
	N >= 3 ->
	    medium;
	true ->
	    low
    end.

stash_term(Path, Term) ->
    file:write_file(Path, erlang:term_to_binary(Term)).

retrieve_term(Path) ->
    case file:read_file(Path) of
	{ok, BinaryTerm} ->
	    erlang:binary_to_term(BinaryTerm);
	{error, Reason} -> throw({read_file, Reason})
    end.
