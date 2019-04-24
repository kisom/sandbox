-module(rgb).
-export([pack16/1, unpack16/1]).

pack16({Red, Green, Blue}) ->
    <<Red:5, Green:6, Blue:5>>.

unpack16(<<Red:5, Green:6, Blue:5>>) ->
    {Red, Green, Blue}.
