
t1:-	theorem( max(X,max(Y,Z)) , max(max(X,Y),Z) ).

theorem( C ):-
		internalClauseRep(C,IC),
		nl, nl, write('Proving the following theorem: '),nl,nl,
		print('$clause'(IC)),nl,
		nf_set(IC,L),
		prove_message(L),!.

prove_message([]):-
		nl, write('  Theorem proved. '),nl,!.
prove_message(L):-
		nl, write('  Theorem disproved. Set of normal forms follows:'),nl,
		user:map(lambda([X,'$clause'(X)],true),L,L1),
		user:writeList(L1,'
'), !.

internalClauseRep([],[]) :- !.
internalClauseRep([L1|Cl],[IL1|ICl]) :-
	internalLiteralRep(L1,IL1),
	internalClauseRep(Cl,ICl).


internalLiteralRep(T1 = T2,IT1 = IT2) :-
	user:internalEqRep((T1 = T2),([],[IT1 = IT2])), !.
internalLiteralRep(T1,IT1) :-
	user:internalEqRep(T1 = true,([],[IT1 = _])), !.

