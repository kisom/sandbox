-module(error_test).
-export([generate_exc/1, demo1/0, demo2/0, demo3/0, catcher/1]).

generate_exc(1) -> a;
generate_exc(2) -> throw(a);
generate_exc(3) -> exit(a);
generate_exc(4) -> {'EXIT', a};
generate_exc(5) -> error(a).

demo1() ->
    [catcher(I) || I <- lists:seq(1, 5)].

demo2() ->
    [{I, (catch generate_exc(I))} || I <- [1,2,3,4,5]].

catcher(N) ->
    try generate_exc(N) of
	Val -> {N, normal, Val}
    catch
	throw:X -> {N, caught, thrown, X};
	exit:X  -> {N, caught, exited, X};
	error:X -> {N, caught, error, X}
    end.


%% NB: catch _ -> ... assumes default tag throw, need to specify
%%     catch _:_ -> ...

demo3() ->
    try generate_exc(5)
    catch
	error:X ->
	    {X, erlang:get_stacktrace()}
    end.

%% In Erlang, when an error is detected internally by the system or is
%% detected by program logic, the correct approach is to crash
%% immediately and generate a meaningful error message. We crash
%% immediately so as not to make matters worse. The error message should
%% be written to a permanent error log and be sufficiently detailed so
%% that we can figure out what went wrong later. 
%%  
%% Second, fail politely means that only the programmer should see the
%% detailed error messages produced when a program crashes. A user of the
%% program should never see these messages.
