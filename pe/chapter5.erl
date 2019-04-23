%% chapter 5: maps and records
-module(chapter5).
-include("records.hrl").
-export([make_urgent/1, count_characters/1]).

%% Maps are defined with a #{ Key => Value, ... } syntax
%% =>: update existing key or add new key
%% :=: update existing key

make_urgent(M) ->
    M#{status := urgent}.

%% Valid: make_urgent(#{status => reminder, who => kyle}).
%% Invalid: make_urgent(#{who => kyle}).

count_characters(Str) ->
    count_characters(Str, #{}).
count_characters([H|T], #{ H := N }=X) ->
    count_characters(T, X#{H := N + 1});
count_characters([H|T], X) ->
    count_characters(T, X#{H => 1});
count_characters([], X) -> X.

