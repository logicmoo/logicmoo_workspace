% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% fxamples.pl [Chapter  4] Another set of test examples
%
% predicate 'test' must be defined for parser
%
test1 :-
	test([kim,dies]).
test2 :-
	test([we,see,a,duck]).
test3 :-
	test([i,am,a,man]).
test4 :-
	test([the,women,who,were,pregnant,are,numerous]).
test5 :-
	test([this,duck,eats,fish,that,die]).
test6 :-
	test([we,sees,a,duck]).
test7 :-
	test([i,am,these,man]).
test8 :-
	test([the,man,who,was,pregnant,is,numerous]).
test9 :-
	test([this,duck,eats,scissors,who,die]).
test10 :-
	test([he,saw,her,duck]).
%
