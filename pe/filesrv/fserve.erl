-module(fserve).
-export([start/0, file_serve/2, put/2, get/1]).
-include("./msg.hrl").

start () ->
    spawn(fserve, file_serve, [#{}, sets:new()]).

file_serve(Names, DataStore) ->
    receive
	#upload{client=Client, data=Data} -> 
	    case upload(DataStore, Data) of
		{ok, UpdatedDataStore, CAS_ID} -> 
		    Client ! CAS_ID,
		    file_serve(Names, UpdatedDataStore);
		{error, Reason} -> throw(Reason)
	    end;
	#stop{client=Client} ->
	    io:format("Shutdown message received from ~w~n", [Client]);
	#dump{client=Client} ->
	    io:format("Dumping store.~n"),
	    Client ! {Names, DataStore},
	    file_serve(Names, DataStore)
    end.

put(DataStore, Data)  ->
    <<HashBinary:256/big-unsigned-integer>> = crypto:hash(sha256, Data),
    Hash = integer_to_list(HashBinary, 16),
    case sets:is_element(Hash, DataStore) of
	false ->
	    Path = "data/" ++ Hash,
	    case file:write_file(Path, Data) of
		ok -> 
		    io:format("Wrote ~s to file ~s~n", [Hash, Path]),
		    {ok, sets:add_element(Hash, DataStore), Hash};
		{error, Reason} -> {error, Reason}
	    end;
	true -> 
	    io:format("Receive request to store previously stored ~s~n", [Hash]),
	    {ok, DataStore, Hash}
    end.
    
get(ID) ->
    Path = "data/" ++ ID,
    file:read_file(Path).

upload(DataStore, Data) ->
    case fserve:put(DataStore, Data) of
	{ok, UpdatedDataStore, ID} ->
	    io:format("Successfully stored file.~n"),
	    {ok, UpdatedDataStore, ID};
	{error, Reason} -> {error, Reason}
    end.
