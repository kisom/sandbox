%% Write a ring benchmark. Create N processes in a ring. Send a
%% message around the ring N times so that a total of N * M messages
%% get sent. Time how long this takes for different values of N and M.
%%
%% Write a similar program in some other programming language you are
%% familiar with. Compare the results. Write a blog, and publish the
%% results on the internet!
-module(ring).
-compile(export_all). % TODO: remove this
-record(message, {client=undefined, ttl=0, hops=0}).

%% What does a loop need to know? Who to forward to next. 
%%
%% How does the last node know who to forward to? We need to pass the
%% parent in somehow.
%% 
loop(Next) ->
    receive
	#message{client=Client, ttl=TTL, hops=Hops} ->
	    if TTL > 0 ->
		    Next ! #message{client=Client, ttl=TTL-1, hops=Hops};
	       true ->
		    Client ! #message{client=Client, ttl=TTL, hops=Hops}
	    end
    end.


%% gen_process ->
%%   1. is N > 0?
%%      1. Spawn process, use self() as next.
%%         This generates processes in the reverse order, e.g.
%%         this process is the last process in the chain.

gen_process(N) when is_number(N) ->
    Root = self(),
    Child = gen_process(Root, Root, N-1),
    loop(Child).
    
gen_process(Root, _Parent, 0) ->
    loop(Root);

gen_process(Root, Parent, Count) when Count > 0 ->
    Child = gen_process(Root, Parent, Count-1),
    loop(Child).

ring(N) ->
    Root = spawn(fun () -> gen_process(N) end),
    Root ! #message{client=self(), ttl=N, hops=0},
    receive X ->
	    X
    end.
