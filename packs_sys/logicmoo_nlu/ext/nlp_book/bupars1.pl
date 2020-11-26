% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% bupars1.pl [Chapter  5] A bottom-up shift-reduce parser
% (descended from Stabler's AAAI-83 paper)
%
?- reconsult('library.pl').
?- reconsult('examples.pl').
?- reconsult('psgrules.pl').
?- consult('lexicon.pl').
%
%use_rule:- fail.

test([Word|Words]) :-
	parse(Words,[Word],[s|S]),
	write([s|S]),
	nl.
%
parse([Word|Words],Stack,Parse) :-
	reduce1(Stack,Newstack),
	parse(Words,[Word|Newstack],Parse).
parse([],Stack,Parse) :-
	reduce1(Stack,[Parse]).

reduce1([X0|Y0],[X2|Y2]) :-
	reduce2([X0|Y0],[X1|Y1]),
	reduce1([X1|Y1],[X2|Y2]).
reduce1(X,X).
%
reduce2([Word|Pairs],[[Category|Word]|Pairs]) :-
	word(Category,Word).
reduce2(Stack,[Head|Tail]) :-
	rule(Category,Categories),
	heads(Categories,DPlist),
	rappend(DPlist,Tail,Stack),
	append([Category],DPlist,Head).
%
heads([],[]).
heads([Head|Heads],[[Head|Tail]|Rest]) :-
	 heads(Heads,Rest).
%
% rappend - reverses the first list and appends second list
%
rappend([],L,L).
rappend([X|L1],L2,L3) :-
	rappend(L1,[X|L2],L3).
