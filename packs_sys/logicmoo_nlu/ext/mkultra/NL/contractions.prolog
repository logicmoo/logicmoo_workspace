%% contracted_form(?UncontractedList, ?ContractedList) is det
%  True when UncontractedList is the expansion of the contractions
%  in ContractedList. Also replaces "a" with "an" where appropriate
contracted_form([ ], [ ]).
contracted_form([a], [an]) :-
   !.
contracted_form([a, Word | UncontractedTail], [an, Word | ContractedTail]) :-
   starts_with_one_of("aeiou", Word),
   !,
   contracted_form(UncontractedTail, ContractedTail).
contracted_form([X, Y | UncontractedTail], [Head, '\'', Clitic | ContractedTail]) :-
	contraction(X, Y, Head, Clitic), !,
	contracted_form(UncontractedTail, ContractedTail).
contracted_form([A | UncontractedTail], [A | ContractedTail]) :-
	contracted_form(UncontractedTail, ContractedTail).
	
%% contraction(?First, ?Second, ?Contraction)
%  Contraction is the single-word contraction of [First, Second].
contraction(will, not, won, t).
contraction(can, not, can, t).
contraction(should, not, 'shouldn','t').
contraction(must, not, 'mustn','t').

contraction(do, not, 'don','t').
contraction(does, not, 'doesn','t').

contraction(have, not, 'haven','t').
contraction(has, not, 'hasn','t').
contraction(had, not, 'hadn','t').

contraction(is, not, 'isn','t').
contraction(are, not, 'aren','t').

contraction('I', am, 'I','m').
contraction(you, are, 'you','re').
contraction(he, is, 'he','s').
contraction(she, is, 'she','s').
contraction(we, are, 'we','re').
contraction(they, are, 'they','re').

contraction(what, is, what, s).
contraction(what, are, what, re).
contraction(who, is, who, s).
contraction(who, are, who, re).

contraction('I', will, 'I','ll').
contraction(let, us, 'let','s').

:- register_all_lexical_items([A, B, C, D], contraction(A,B,C,D)).
