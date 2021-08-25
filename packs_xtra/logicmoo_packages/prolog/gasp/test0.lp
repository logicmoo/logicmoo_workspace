%%% Test 0. A definite program
%%% 

q(1..512). %%% enlarge upper bound freely!
%%% if you use >645, please modify the GASP interpreter
%%% using a higher limit
p(X,Y) :- q(X),q(Y),Y = X + 1.
h(X,Y) :- p(X,Y).
h(X,Y) :- p(X,Z),h(Z,Y),q(Y).

%%%% 
% hide.
 

