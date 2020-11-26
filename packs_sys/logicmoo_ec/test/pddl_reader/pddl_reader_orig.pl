
%:-module(pddl_reader,[read_pddl/2]).

%%:-include(readFile).
%:-include(parseProblem).
%:-include(parseDomain).

:-[parseProblem, parseDomain].

read_pddl(F,L):-
	(
		parseDomain(F, L, _) ; parseProblem(F, L, _)
	), !.
read_pddl(F,[]):-
	write('Parsing file failed. '), write('('), write(F), write(')'), nl.

 pd:-parseDomain('FairyTaleCastle.PDDL',X,L),writeq(X:L),nl,nl,nl.
