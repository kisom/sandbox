%% facts
child(anne,bridget).
child(bridget,caroline).
child(caroline,donna).
child(donna,emily).

%% rules
descend(X,Y) :- child(X,Y).

descend(X,Y) :- child(X,Z),
  descend(Z,Y).

descend1(X,Y) :- child(X,Y).
descend1(X,Y) :- child(X,Z),
  descend1(Z,Y).

descend2(X,Y) :- child(X,Y).
descend2(X,Y) :- descend2(Z,Y),
  child(X,Z).

%% WARNING: non-terminating.
descend3(X,Y)  :-  child(X,Y).
descend3(X,Y)  :-  descend3(X,Z),
                   descend3(Z,Y).