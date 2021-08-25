%%% Test 6 MARRIAGE2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

man(0..800).
woman(0..800).

domain(X) :- man(X).
range(Y) :- woman(Y).

:- man(X),woman(Y),hate(X,Y),assignment(X,Y).

% gasp
%assignment(X,Y).
%hate(X,Y) :- man(X), woman(Y), ((X + 3) mod 10 ) neq Y.

% lparse
1{assignment(X,Y):range(Y)}1 :- domain(X).
hate(X,Y) :- man(X), woman(Y), ((X + 3) mod 10 ) != Y.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

