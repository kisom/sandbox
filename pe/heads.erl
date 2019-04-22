-module(heads).
-export([len/1]).

alen([], Count) ->
    Count;

alen([_|T], Count) ->
    alen(T, Count+1).

len([_|T]) ->
    alen(T, 1);

len([]) ->
    0.
