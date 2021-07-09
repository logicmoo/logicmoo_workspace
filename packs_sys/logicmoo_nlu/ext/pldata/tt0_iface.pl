:- module(tt_iface,[prim_acts/1]).



% :- re export(ac_xnl_iface).
% :- re export(clex_iface).

prim_acts(Z):- ttholds(_,Y,primitive_action_tt),(ttholds(_,Z,Y)*->true; Z=Y).
%prim_acts(Z):- ttholds(_,Y,primitive_action_tt),(ttholds(_,Z,Y)*->true; Z=Y).

:- export(prim_acts/1).


:- nl_iface:rexport_qlf(tt_iface,tt0_00022_cycl).

:-
  forall( 
  (clause(ttholds(Pos,Word,String),true,R),Pos\==time_rangeOf,string(String)),
  must_or_rtrace(((assert(ttholds_nl(Pos,Word,String)),erase(R))))).


:-
  forall( 
  (between(2,13,N), N\==3, current_predicate(ttholds/N), functor(P,ttholds,N),
   clause(P,true,R), arg(_,P,String), string(String)),
  ignore(must_or_rtrace(((P=..[_|ARGS],C=..[ttholds_nl|ARGS],assert(C),ignore(erase(R))))))).

