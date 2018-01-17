%% Exercise 2.4: Here are six Italian words:
%% 
%%     - astante
%%     - astoria
%%     - baratto
%%     - cobalto
%%     - pistola
%%     - statale
%% 
%% They are to be arranged, crossword puzzle fashion, in the following grid: 
%% 
%%      V1  V2  V3
%%      .   .   .
%% H1 . 1 . 4 . 7 .
%%      .   .   .
%% H2 . 2 . 5 . 8 .
%%      .   .   .
%% H3 . 3 . 6 . 9 .
%%      .   .   .
%% 
%% The following knowledge base represents a lexicon containing these words: 

word(astante, a,s,t,a,n,t,e).
word(astoria, a,s,t,o,r,i,a).
word(baratto, b,a,r,a,t,t,o).
word(cobalto, c,o,b,a,l,t,o).
word(pistola, p,i,s,t,o,l,a).
word(statale, s,t,a,t,a,l,e).

%% Write a predicate crossword/6 that tells us how to fill in the
%% grid. The first three arguments should be the vertical words from
%% left to right, and the last three arguments the horizontal words
%% from top to bottom.

crossword(V1, V2, V3, H1, H2, H3) :-
    word(V1, _, _1, _, _2, _, _3, _),
    word(V2, _, _4, _, _5, _, _6, _),
    word(V3, _, _7, _, _8, _, _9, _),
    word(H1, _, _1, _, _4, _, _7, _),
    word(H2, _, _2, _, _5, _, _8, _),
    word(H3, _, _3, _, _6, _, _9, _),
    V1 \= V2, V1 \= V3, V1 \= H1, V1 \= H2, V1 \= H3,
    V2 \= V3, V2 \= H1, V2 \= H2, V2 \= H3,
    V3 \= H1, V3 \= H2, V3 \= H3,
    H1 \= H2, H1 \= H3, H2 \= H3.

%% NB: execute the following query to find out the correct arrangement.
%% crossword(A, B, C, D, E, F).

%% $ swipl -q                                                                                             
%% 1 ?- [italiano].                 
%% true.                            
%% 
%% 2 ?- crossword(A, B, C, D, E, F). 
%% A = astante,
%% B = cobalto,
%% C = pistola,
%% D = astoria,
%% E = baratto,
%% F = statale ;
%% A = astoria,
%% B = baratto,
%% C = statale,
%% D = astante,
%% E = cobalto,
%% F = pistola ;
%% false.

%% Note that there are two solutions; these are mirrors of each other
%% with the vertical words substituted for the horizontal words.
