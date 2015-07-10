% Simple illustration of the use multiple-recall to guess
% 	constants
% To run do the following:
%	a. Load Aleph
%	b. read_all(ineq).
%	c. sat(1).
%	d. reduce.


:- modeh(1,p(+number)).
:- modeb(*,gteq(+number,#number)).

:- determination(p/1,gteq/2).


% definition to use during normal evaluation
gteq(X,Value):-
	number(X), number(Value), !,
        X >= Value.
% definition to use during construction of bottom clause
gteq(X,Y):-
	number(X), var(Y), !,
	guess(X,Y).

guess(X,Y):- 
	Y is X.
guess(X,Y):- 
	Y is 2*X.
guess(X,Y):- 
	Y is 3*X.


