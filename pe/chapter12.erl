-module(chapter12).
-compile(exportall).
-export([start/2, registrar/0]).

registrar(Client, {Name, Fun}) ->
    case whereis(Name) of
	undefined ->
	    Pid = spawn(Fun),
	    register(Name, Pid),
	    Client ! {ok, Name, Pid};
	Pid -> Client ! {error, exists, Pid}
    end.

registrar() ->
    receive
	{Client, {Name, Fun}} ->
	    registrar(Client, {Name, Fun}),
	    registrar();
	X -> io:format("Spurious message ~w received~n", [X])
    end.

start(AnAtom, Fun) ->
    %% start the registrar if it's not been started.
    case whereis(registrar_proc) of
	undefined ->
	    Pid = spawn(chapter12, registrar, []),
	    register(registrar_proc, Pid);
	_ -> void
    end,
    registrar_proc ! {self(), {AnAtom, Fun}},
    receive X -> X end.
	    
