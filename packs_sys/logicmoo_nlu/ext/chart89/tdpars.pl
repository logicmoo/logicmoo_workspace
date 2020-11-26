% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% tdpars.pl [Chapter  5] A simple top-down parser
%
% it can be run as a generator also and uses difference lists to
% represent strings
%
?- reconsult('examples.pl').
?- reconsult('library.pl').
?- reconsult('psgrules.pl').
?- consult('lexicon.pl').
%
use_rule.		% use 'rule' predicate to access lexicon
%
parse(Category,[Category|Parses],X,Y) :-
	rule(Category,Daughters),
	matches(Daughters,Parses,X,Y),
	dbgwrite(parse(Category,[Category|Parses],X,Y)).
%
matches([],[],X,X).
matches([Word],[Word],[Word|X],X).
matches([Daughter|Daughters],[Parse|Parses],X,Z) :-
	parse(Daughter,Parse,X,Y),
	matches(Daughters,Parses,Y,Z).
%
test(String) :-
	parse(s,Parse,String,[]),
	write(Parse),
	nl.
