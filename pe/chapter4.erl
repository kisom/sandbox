-module(chapter4).
-export([t2l/1, timeit/1, date_string/0]).
% 2. The BIF tuple_to_list(T) converts the elements of the tuple T to
% a list. Write a function called my_tuple_to_list(T) that does the
% same thing only not using the BIF that does this.
t2l(T) ->
    [ element(I, T) || I <- lists:seq(1, tuple_size(T)) ].

% 3. Look up the definitions of erlang:now/0, erlang:date/0, and
% erlang:time/0. Write a function called my_time_func(F), which
% evaluates the fun F and times how long it takes. Write a function
% called my_date_string() that neatly formats the current date and
% time of day.

% erlang:time -> Returns the current time as {Hour, Minute, Second}.
% erlang:date -> Returns the current date as {Year, Month, Day}.
% erlang:now -> deprecated, should use erlang:timestamp
% erlang:timestamp -> Returns current Erlang system time on the format
%     {MegaSecs, Secs, MicroSecs}.
% erlang:system_time(Unit) -> Returns current Erlang system time
%     converted into the Unit passed as argument.

timeit(F) ->
    Started = erlang:system_time(microsecond),
    F(),
    {erlang:system_time(microsecond) - Started, microsecond}.

date_string() ->
    {Hour, Minute, Second} = erlang:time(),
    {Year, Month, Day} = erlang:date(),
    io:format("~w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
	      [Year, Month, Day, Hour, Minute, Second]).

% 4. Advanced: Look up the manual pages for the Python datetime
% module. Find out how many of methods in the Python datetime class
% can be implemented using the time-related BIFs in the erlang
% module. Search the erlang manual pages for equivalent
% routines. Implement any glaring omissions.

% 5. Write a module called math_functions.erl, exporting the functions
% even/1 and odd/1. The function even(X) should return true if X is an
% even integer and otherwise false. odd(X) should return true if X is
% an odd integer.

% 6. Add a higher-order function to math_functions.erl called
% filter(F, L), which returns all the elements X in L for which F(X)
% is true.

% 7. Add a function split(L) to math_functions.erl, which returns
% {Even, Odd} where Even is a list of all the even numbers in L and
% Odd is a list of all the odd numbers in L. Write this function in
% two different ways using accumulators and using the function filter
% you wrote in the previous exercise.
