% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% examples.pl [Chapter --] A set of test examples
%
% - predicate 'test' must be defined for parser


% A set of test examples - predicate 'test' must be defined for parser
%
test1 :-
	test([kim,died]).
test2 :-
	test([sandy,saw,a,duck]).
test3 :-
	test([kim,knew,sandy,knew,lee,died]).
test4 :-
	test([the,woman,gave,a,duck,to,her,man]).
test5 :-
	test([lee,handed,a,duck,that,died,to,the,woman]).
%
teste1 :-
  test(['Maria',lacks,a,house]).
teste2 :-
  test(['Mayumi',sees,a,table]).
teste3 :-
  test(['Mayumi',sees,that,'Maria',sees,that,'Mayumi',lacks,a,house]).
teste4 :-
  test([the,woman,who,sees,'Maria',sings]).
teste5 :-
  test(['Maria',hits,a,house,that,lacks,a,table]).
%
testf1 :-
  test([kim,was,happy]).

testf2 :-
  test([lee,is,a,consumer,and,often,very,very,stupid]).

testf3 :-
  test([sandy,was,sometimes,a,stupid,man,and,kim,is,always,the,happy,woman]).
