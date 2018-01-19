%% 1. Write a 3-place predicate combine1 which takes three lists as arguments
%% and combines the elements of the first two lists into the third as follows:
%%
%%    ?-  combine1([a,b,c],[1,2,3],X).
%%    
%%    X  =  [a,1,b,2,c,3]
%%    
%%    ?-  combine1([f,b,yip,yup],[glu,gla,gli,glo],Result).
%%    
%%    Result  =  [f,glu,b,gla,yip,gli,yup,glo]
combine1([], [], []).
combine1([X|TX], [Y|TY], [X,Y|TXY]) :- combine1(TX, TY, TXY).

%% Now write a 3-place predicate combine2 which takes three lists as arguments
%% and combines the elements of the first two lists into the third as follows:
%%
%%   ?-  combine2([a,b,c],[1,2,3],X).
%%   
%%   X  =  [[a,1],[b,2],[c,3]]
%%   
%%   ?-  combine2([f,b,yip,yup],[glu,gla,gli,glo],Result).
%%   
%%   Result  =  [[f,glu],[b,gla],[yip,gli],[yup,glo]]
combine2([], [], []).
combine2([X|TX], [Y|TY], [[X, Y]|TXY]) :- combine2(TX, TY, TXY).

%% Finally, write a 3-place predicate combine3 which takes three lists as
%% arguments and combines the elements of the first two lists into the third
%% as follows:
%%
%%   ?-  combine3([a,b,c],[1,2,3],X).
%%   
%%   X  =  [j(a,1),j(b,2),j(c,3)]
%%   
%%   ?-  combine3([f,b,yip,yup],[glu,gla,gli,glo],R).
%%   
%%   R  =  [j(f,glu),j(b,gla),j(yip,gli),j(yup,glo)] 
combine3([], [], []).
combine3([X|TX], [Y|TY], [j(X,Y)|TXY]) :- combine3(TX, TY, TXY).