% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% tdrecg.pl [Chapter  5] A simple top-down recognizer
%
% treats strings as difference lists
%
?- reconsult('examples.pl').
?- reconsult('library.pl').
?- reconsult('psgrules.pl').
?- consult('lexicon.pl').
%
use_rule.		% use 'rule' predicate to access lexicon
%
recognize(Category,S1,S2) :-
	rule(Category,Daughters),
	matches(Daughters,S1,S2),
	dbgwrite(recognize(Category,S1,S2)).
%
matches([],S,S).
matches([Word],[Word|S],S).
matches([Category|Categories],S1,S3) :-
	recognize(Category,S1,S2),
	matches(Categories,S2,S3).
%
test(String) :-
	recognize(s,String,[]).
