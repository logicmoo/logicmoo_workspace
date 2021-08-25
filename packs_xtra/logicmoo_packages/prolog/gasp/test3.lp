%%% Test 3 Marriage problem:

man(0..800).
woman(0..800).

domain(X) :- man(X).
range(Y) :- woman(Y).

%%%% lparse
1{assignment(X,Y):range(Y)}1 :- domain(X).

% gasp with functions
%assignment(X,Y).

%%% constraint on assignments
:-domain(X),range(Y),assignment(X,Y), X < Y.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

