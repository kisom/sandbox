-module(fserve).
-export([start/0]).
-include("./msg.hrl").

start () ->
    file_serve(#{}, #{}).

file_serve(Names, DataStore) ->
    throw (not_implemented).
