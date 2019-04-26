-module(chapter8).
-export([unique_funs/0]).

%% The command code:all_loaded() returns a list of {Mod,File} pairs of
%% all modules that have been loaded into the Erlang system. Use the
%% BIF Mod:module_info() to find out about these modules. Write
%% functions to determine which module exports the most functions and
%% which function name is the most common. Write a function to find
%% all unambiguous function names, that is, function names that are
%% used in only one module.
unique_funs() ->
    unique_funs(code:all_loaded(), sets:new()).

unique_funs([], UniqueFuns) ->
    {sets:size(UniqueFuns), sets:to_list(UniqueFuns)};
unique_funs([{Mod, _File}|T], UniqueFuns) ->
    Funs = lists:map(fun ({Fun, _Arity}) -> Fun end, Mod:module_info(exports)),
    unique_funs(T, sets:union(UniqueFuns, sets:from_list(Funs))).
    
    
