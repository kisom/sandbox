-module(math_functions).
-export([even/1, odd/1, filter/2, split/1, splitacc/1]).

even(N) ->
    N rem 2 == 0.

odd(N) ->
    N rem 2 /= 0.

filter(F, [H|T]) ->
    case F(H) of
	true  -> [H | filter(F, T)];
	false -> filter(F, T)
    end;
filter(_, []) ->
    [].

split(L) ->
    {filter(fun (N) -> even(N) end, L),
     filter(fun (N) -> odd(N) end, L)}.

splitacc_inner([], Even, Odd) -> {Even, Odd};
splitacc_inner([H|T], Even, Odd) ->
    case even(H) of
	true ->
	    splitacc_inner(T, [H|Even], Odd);
	false ->
	    splitacc_inner(T, Even, [H|Odd])
    end.
    

splitacc(L) -> splitacc_inner(L, [], []).
