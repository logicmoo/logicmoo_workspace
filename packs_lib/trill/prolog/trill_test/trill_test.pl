

:-module(trill_test,
  [close_to/2,close_to/3,run/1]).

:- meta_predicate run(:).

run(M:H):-
	copy_term(H,NH),
	numbervars(NH),
%	NH=(_Query,close_to('P',_Prob)),
	format("~p.~n",[NH]),
	(H=(G,R)),
	time(call(M:G)),!,
	format("\t~p.~n~n",[G]),
	call(R).

epsilon(0.09).

close_to(V,T):-
	epsilon(E),
	TLow is T-E,
	THigh is T+E,
	TLow<V,
	V<THigh.

close_to(V,T,E):-
	TLow is T-E,
	THigh is T+E,
	TLow<V,
	V<THigh.

same_expl(Expl, CorrExpl):-
	length(Expl,NE),
	length(CorrExpl,NE),
	same_expl_int(Expl, CorrExpl).

same_expl_int([],_CorrExpls).

same_expl_int([Expl|Expls],CorrExpls):-
  sort(Expl,ExplSort),
  member(X,CorrExpls),
  sort(X,ExplSort),!,
  same_expl_int(Expls,CorrExpls).

one_of(Expl,CorrExpls):-
  sort(Expl,ExplSort),
  member(X,CorrExpls),
  sort(X,ExplSort),!.

test_formula(F1,F2):-
  \+ trill:test(_,F1,F2),
  \+ trill:test(_,F2,F1).

