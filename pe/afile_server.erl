-module(afile_server).
-compile([debug_info]).
-export([start/1, loop/1]).

start(Dir) ->
    spawn(afile_server, loop, [Dir]).

%% Three significant points to observer about this code:
%%
%% + Who to reply to: all the received messages contain
%%   the variable Client - the PID of the sender.
%% + Use of self(): the PID of the server.
%% + Pattern matching to select the message.
loop(Dir) ->
    receive
	{Client, list_dir} ->
	    Client ! {self(), file:list_dir(Dir)};
	{Client, {get_file, File}} ->
	    Full = filename:join(Dir, File),
	    Client ! {self(), file:read_file(Full)};
	{Client, {put_file, File, Contents}} ->
	    Full = filename:join(Dir, File),
	    Client ! {self(), file:write_file(Full, Contents)},
	{Client, ping} ->
	    Client ! {self(), pong};
	{Client, _} ->
	    Client ! {self(), unrecognised_message};
	{_} ->
	    io:format("unhandled message")
    end,
    loop(Dir).

%% Notes:
%% + function names are always lowercased
%% + universal receiver: receive X -> X end.
%% + erlang: use processes to structure the solutions to our problems.
%% + *ATOMS*ARE*LOWER*CASED*
