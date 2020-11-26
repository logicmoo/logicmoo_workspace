% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% library.pl [Chapter --] A collection of utility predicates
%
% '--->' an arrow for rules that distinguishes them from DCG ('-->') rules
%
?- op(255,xfx,--->).
%
% definitions to provide a uniform interface to DCG-style rule format
% the 'word' predicate is used by the RTNs and other parsers
% the 'rule' clause that subsumes words is used by the chart parsers
%
word(Category,Word) :-
	l_clause((Category ---> [Word])).
%
rule(Category,[Word]) :-
	% use_rule,
	l_clause((Category ---> [Word])).
%
% in order for the clause above to be useful,
% use_rule.          needs to be in the file.
%
rule(Mother,List_of_daughters) :-
	l_clause((Mother ---> Daughters)),
	not(islist(Daughters)),
	conjtolist(Daughters,List_of_daughters).

l_clause(G):- clause(G,B),call(B).
%
% conjtolist - convert a conjunction of terms to a list of terms
%
conjtolist((Term,Terms), [Term|List_of_terms]) :- !,
	conjtolist(Terms,List_of_terms).
conjtolist(Term,[Term]).
%
% islist(X) - if X is a list, C&M 3rd ed. p52-53
%
islist([]) :- !.
islist([_|_]).
%
% append(X,Y,Z) - append Y to X to form Z, C&M 3rd ed. p59
%
append([],List,List).
append([Head|Tail1],List,[Head|Tail2]) :-
	append(Tail1,List,Tail2).
%
% retractall(X) - remove all clauses that match X, C&M 3rd ed. p172
%
% retractall(Clause):-
%   retract(Clause),
%    fail.
% retractall(_).
%
% read_in(X) - convert keyboard input to list X, C&M 3rd ed. p101-103
%
read_in([Word|Words]) :-
	get0(Character1),
	readword(Character1,Word,Character2),
	restsent(Word,Character2,Words).
%
restsent(Word,Character,[]) :-
	lastword(Word),!.
restsent(Word1,Character1,[Word2|Words]) :-
	readword(Character1,Word2,Character2),
	restsent(Word2,Character2,Words).
%
readword(Character1,Word,Character2) :-
	single_character(Character1), !,
	name(Word,[Character1]),
	get0(Character2).
readword(Character1,Word,Character2) :-
	in_word(Character1,Character3),!,
	get0(Character4),
	restword(Character4,Characters,Character2),
	name(Word,[Character3|Characters]).
readword(Character1,Word,Character2) :-
	get0(Character3),
	readword(Character3,Word,Character2).
%
restword(Character1,[Character2|Characters],Character3) :-
	in_word(Character1,Character2),!,
	get0(Character4),
	restword(Character4,Characters,Character3).
restword(Character,[],Character).
%
single_character(33). % !
single_character(44). % ,
single_character(46). % .
single_character(58). % :
single_character(59). % ;
single_character(63). % ?
%
in_word(Character,Character) :-
	Character > 96,
	Character < 123.  % a-z
in_word(Character,Character) :-
	Character > 47,
	Character < 58.  % 1-9
in_word(Character1,Character2) :-
	Character1 > 64,
	Character1 < 91,
	Character2 is Character1 + 32.  % A-Z
in_word(39,39). % '
in_word(45,45). % -
%
lastword('.').
lastword('!').
lastword('?').
%
% testi - get user's input and pass it to test predicate, then repeat
%
testi :-
	write('End with period and <CR>'),
	read_in(Words),
	append(String,[Period],Words),
	nl,
	test(String),
	nl,
	testi.
%
% dbgwrite - a switchable tracing predicate
%
dbgwrite(Term) :-
	dbgon,
	write(Term),
	nl, !.
dbgwrite(Term).
%
dbgwrite(Term,Var) :-
	dbgon,
	integer(Var),
	tab(3 * (Var - 1)),
	write(Term),
	nl, !.
dbgwrite(Term,Var) :-
	dbgon,
	write(Term), write(" "), write(Var),
	nl, !.
dbgwrite(Term,Var).
%
dbgon.  % retract this to switch dbg tracing off
