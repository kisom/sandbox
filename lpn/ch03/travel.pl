%% Given facts.
directTrain(saarbruecken,dudweiler).
directTrain(forbach,saarbruecken).
directTrain(freyming,forbach).
directTrain(stAvold,freyming).
directTrain(fahlquemont,stAvold).
directTrain(metz,fahlquemont).
directTrain(nancy,metz).

%% Rules.
travelFromTo(X, Y) :- directTrain(X, Y).
travelFromTo(X, Y) :- directTrain(X, Z),
                      travelFromTo(Z, Y).

%% New version from practical exercises.
%% We are given the following knowledge base of travel information:
byCar(auckland,hamilton).
byCar(hamilton,raglan).
byCar(valmont,saarbruecken).
byCar(valmont,metz).

byTrain(metz,frankfurt).
byTrain(saarbruecken,frankfurt).
byTrain(metz,paris).
byTrain(saarbruecken,paris).

byPlane(frankfurt,bangkok).
byPlane(frankfurt,singapore).
byPlane(paris,losAngeles).
byPlane(bangkok,auckland).
byPlane(singapore,auckland).
byPlane(losAngeles,auckland).

%% Write a predicate travel/2 which determines whether it is possible
%% to travel from one place to another by chaining together car,
%% train, and plane journeys. For example, your program should answer
%% yes to the query travel(valmont,raglan).

%% The base case is a direct route via car, train or plane.
travelDirect(X, Y) :- byCar(X, Y).
travelDirect(X, Y) :- byTrain(X, Y).
travelDirect(X, Y) :- byPlane(X, Y).
travel(X, Y)       :- travelDirect(X, Y).

%% The recursive case chains these together.
travel(X, Y) :- travel(X, Z),
		travel(Z, Y).

%% So, by using travel/2 to query the above database, you can find out
%% that it is possible to go from Valmont to Raglan. If you are
%% planning such a voyage, that’s already something useful to know,
%% but you would probably prefer to have the precise route from
%% Valmont to Raglan. Write a predicate travel/3 which tells you which
%% route to take when travelling from one place to another. For
%% example, the program should respond
%% X  =  go(valmont,metz,
%%  	      go(metz,paris,
%%  		    go(paris,losAngeles)))
%% to the query travel(valmont,losAngeles,X).

%% First, we need set up the terminal scenario:
%% travel(X, Y, go(travelDirect(X, Y))) :- travelDirect(X, Y).

%% Next, we recurse.
%% travel(X, Y, go(travelDirect(X, Z), W)) :-
%%  	   travelDirect(X, Z),
%%  	   travel(Z, Y, W).

%% The next exercise says we should modify this to include the
%% mechanism of transport. This is more tedious, but doable.
travel(X, Y, go(byCar(X, Y))) :- byCar(X, Y).
travel(X, Y, go(byCar(X, Z), W)) :-
    byCar(X, Z),
    travel(Z, Y, W).
travel(X, Y, go(byPlane(X, Y))) :- byPlane(X, Y).
travel(X, Y, go(byPlane(X, Z), W)) :-
    byPlane(X, Z),
    travel(Z, Y, W).
travel(X, Y, go(byTrain(X, Y))) :- byTrain(X, Y).
travel(X, Y, go(byTrain(X, Z), W)) :-
    byTrain(X, Z),
    travel(Z, Y, W).
