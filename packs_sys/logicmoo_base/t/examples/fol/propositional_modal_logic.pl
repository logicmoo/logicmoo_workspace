#!/usr/bin/env swipl
:- if(set_prolog_flag(runtime_testing_module,user)).
:- if(set_prolog_flag(test_module,user)).
:- include(test_header).
:- endif.
:- endif.

writex(X):- is_list(X), !, maplist(writex,X).
writex(kb_why_flags_assert(_,_,_,X)):-!,writex(X).
writex(X):- writeq(X),nl.

show_xamples:- show_xample(writex).

kif_to_boxlogx(X):-
  format('



=======================================================
?- kif_to_boxlog( ~q ).\n\n',[(X)]),ttyflush,
  ignore((
   with_no_output(kif_to_boxlog(X,O)),
   sort(O,OO),ttyflush,!,writex(OO))).

dkif_to_boxlog:- show_xample(kif_to_boxlogx),ah.

f_n_r(F,N,U):- atomic_list_concat([F,'_',N],R), %upcase_atom
  =(R,U).

xamplen(F,N,R):- N=<1,!,f_n_r(F,N,R).
xamplen(F,N,O+R):- Nm1 is  N -1,f_n_r(F,N,R),xamplen(F,Nm1,O),!.

xample(F,O,L,U):- between(L,U,N),xamplen(F,N,R),(N=1->O=R;(subst(R,+,&,O);subst(R,+,v,O))).

xampl(F):- xample(fact,F,1,1).
xampl(F => R):- xample(result,R,1,1),xample(fact,F,1,2).
%xampl(F):- xample(fact,F,2,3).
%xampl(F => R):- xample(result,R,2,2),xample(fact,F,1,2).
%xampl(F & R):- xample(left,F),xample(right,R).
%xampl(F v R):- xample(left,F),xample(right,R).

do_xampl(P1,E):- 

   call(P1,(E)),
   call(P1,nesc(E)),
   call(P1, ~E),
   call(P1,poss(E)),
   call(P1,poss(~E)),
   !.

show_xample(P1):- update_changed_files,cls,!, forall(xampl(X), do_xampl(P1,X)).

:-   show_xamples.
ah:- add_history(update_changed_files),add_history(show_xamples),add_history(dkif_to_boxlog).

end_of_file.

% the output of this is...
root /opt/logicmoo_workspace/bin/cls /dev/tty




=======================================================
?- kif_to_boxlog( fact_1 ).

fact_1




=======================================================
?- kif_to_boxlog( nesc(fact_1) ).

nesc(fact_1)




=======================================================
?- kif_to_boxlog( ~fact_1 ).

~fact_1




=======================================================
?- kif_to_boxlog( poss(fact_1) ).

poss(fact_1)




=======================================================
?- kif_to_boxlog( poss(~fact_1) ).

poss(~fact_1)




=======================================================
?- kif_to_boxlog( fact_1=>result_1 ).

nesc(fact_1)==>nesc(result_1)
nesc(~result_1)==>nesc(~fact_1)




=======================================================
?- kif_to_boxlog( nesc((fact_1=>result_1)) ).

nesc(fact_1)==>nesc(result_1)
poss(~result_1)==>poss(~fact_1)




=======================================================
?- kif_to_boxlog( ~ (fact_1=>result_1) ).

nesc(result_1)==>nesc(~fact_1)
nesc(~fact_1)==>nesc(result_1)
poss(fact_1)==>nesc(~result_1)
poss(~result_1)==>nesc(fact_1)




=======================================================
?- kif_to_boxlog( poss((fact_1=>result_1)) ).

nesc(fact_1)==>poss(result_1)
nesc(~result_1)==>poss(~fact_1)




=======================================================
?- kif_to_boxlog( poss(~ (fact_1=>result_1)) ).

nesc(~fact_1 v result_1)&(poss(~result_1)v poss(fact_1)&(~fact_1 v poss(fact_1)))==>poss(result_1)
nesc(~fact_1 v result_1)&(poss(~result_1)v poss(fact_1)&(~fact_1 v poss(fact_1)))==>poss(~result_1)
poss(~result_1)v result_1&(poss(~result_1)v poss(fact_1)&(~fact_1 v poss(fact_1)))==>poss(fact_1)
poss(~result_1)v result_1&(poss(~result_1)v poss(fact_1)&(~fact_1 v poss(fact_1)))==>poss(~result_1)
poss(~result_1)v poss(fact_1)&(poss(~result_1)v result_1&nesc(~fact_1 v result_1))==>poss(fact_1)
poss(~result_1)v poss(fact_1)&(poss(~result_1)v result_1&nesc(~fact_1 v result_1))==>poss(~fact_1)
~fact_1 v poss(fact_1)&(poss(~result_1)v result_1&nesc(~fact_1 v result_1))==>poss(result_1)
~fact_1 v poss(fact_1)&(poss(~result_1)v result_1&nesc(~fact_1 v result_1))==>poss(~fact_1)




=======================================================
?- kif_to_boxlog( fact_1&fact_2=>result_1 ).

nesc(fact_1)&nesc(fact_2)==>nesc(result_1)
nesc(~result_1)&nesc(fact_1)==>nesc(~fact_2)
nesc(~result_1)&nesc(fact_2)==>nesc(~fact_1)




=======================================================
?- kif_to_boxlog( nesc((fact_1&fact_2=>result_1)) ).

nesc(fact_1)&nesc(fact_2)==>nesc(result_1)
poss(~result_1)&nesc(fact_1)==>poss(~fact_2)
poss(~result_1)&nesc(fact_2)==>poss(~fact_1)




=======================================================
?- kif_to_boxlog( ~ (fact_1&fact_2=>result_1) ).

~result_1
nesc(~fact_1)==>nesc(~fact_2)
nesc(~fact_2)==>nesc(~fact_1)
poss(fact_1)==>nesc(fact_2)
poss(fact_2)==>nesc(fact_1)




=======================================================
?- kif_to_boxlog( poss((fact_1&fact_2=>result_1)) ).

nesc(fact_1)&nesc(fact_2)==>poss(result_1)
nesc(~result_1)&nesc(fact_1)==>poss(~fact_2)
nesc(~result_1)&nesc(fact_2)==>poss(~fact_1)




=======================================================
?- kif_to_boxlog( poss(~ (fact_1&fact_2=>result_1)) ).

nesc(result_1 v (~fact_1 v ~fact_2))&(result_1 v (poss(fact_2)v poss(fact_1))&(result_1 v (~fact_1 v poss(fact_1))))==>poss(fact_2)
nesc(result_1 v (~fact_1 v ~fact_2))&(result_1 v (poss(fact_2)v poss(fact_1))&(result_1 v (~fact_1 v poss(fact_1))))==>poss(~fact_2)
nesc(result_1 v (~fact_1 v ~fact_2))&(result_1 v (poss(fact_2)v poss(fact_1))&(result_1 v (~fact_1 v poss(fact_1))))==>poss(~result_1)
result_1 v (poss(fact_2)v poss(fact_1))&(result_1 v (poss(fact_2)v~fact_2)&nesc(result_1 v (~fact_1 v ~fact_2)))==>poss(fact_1)
result_1 v (poss(fact_2)v poss(fact_1))&(result_1 v (poss(fact_2)v~fact_2)&nesc(result_1 v (~fact_1 v ~fact_2)))==>poss(~fact_1)
result_1 v (poss(fact_2)v poss(fact_1))&(result_1 v (poss(fact_2)v~fact_2)&nesc(result_1 v (~fact_1 v ~fact_2)))==>poss(~result_1)
result_1 v (poss(fact_2)v~fact_2)&(result_1 v (poss(fact_2)v poss(fact_1))&(result_1 v (~fact_1 v poss(fact_1))))==>poss(fact_1)
result_1 v (poss(fact_2)v~fact_2)&(result_1 v (poss(fact_2)v poss(fact_1))&(result_1 v (~fact_1 v poss(fact_1))))==>poss(fact_2)
result_1 v (poss(fact_2)v~fact_2)&(result_1 v (poss(fact_2)v poss(fact_1))&(result_1 v (~fact_1 v poss(fact_1))))==>poss(~result_1)
result_1 v (~fact_1 v poss(fact_1))&(result_1 v (poss(fact_2)v~fact_2)&nesc(result_1 v (~fact_1 v ~fact_2)))==>poss(~fact_1)
result_1 v (~fact_1 v poss(fact_1))&(result_1 v (poss(fact_2)v~fact_2)&nesc(result_1 v (~fact_1 v ~fact_2)))==>poss(~fact_2)
result_1 v (~fact_1 v poss(fact_1))&(result_1 v (poss(fact_2)v~fact_2)&nesc(result_1 v (~fact_1 v ~fact_2)))==>poss(~result_1)




=======================================================
?- kif_to_boxlog( fact_1 v fact_2=>result_1 ).

nesc(~result_1)&nesc(fact_1)==>nesc(fact_2)
nesc(~result_1)&nesc(fact_2)==>nesc(fact_1)
nesc(~result_1)&poss(~fact_1)==>nesc(~fact_2)
nesc(~result_1)&poss(~fact_2)==>nesc(~fact_1)
poss(~fact_1)&nesc(fact_2)==>nesc(result_1)
poss(~fact_2)&nesc(fact_1)==>nesc(result_1)




=======================================================
?- kif_to_boxlog( nesc((fact_1 v fact_2=>result_1)) ).

poss(~fact_1)&nesc(fact_2)==>nesc(result_1)
poss(~fact_2)&nesc(fact_1)==>nesc(result_1)
poss(~result_1)&nesc(fact_1)==>nesc(fact_2)
poss(~result_1)&nesc(fact_2)==>nesc(fact_1)
poss(~result_1)&poss(~fact_1)==>poss(~fact_2)
poss(~result_1)&poss(~fact_2)==>poss(~fact_1)




=======================================================
?- kif_to_boxlog( ~ (fact_1 v fact_2=>result_1) ).

~result_1
nesc(~fact_1)==>nesc(fact_2)
nesc(~fact_2)==>nesc(fact_1)




=======================================================
?- kif_to_boxlog( poss((fact_1 v fact_2=>result_1)) ).

nesc(~result_1)&(nesc(fact_1 v fact_2)&(poss(~fact_2)v poss(~fact_1)&(fact_1 v poss(~fact_1))))==>poss(fact_2)
nesc(~result_1)&(nesc(fact_1 v fact_2)&(poss(~fact_2)v poss(~fact_1)&(fact_1 v poss(~fact_1))))==>poss(~fact_2)
nesc(~result_1)&(fact_1 v poss(~fact_1)&(poss(~fact_2)v fact_2&nesc(fact_1 v fact_2)))==>poss(fact_1)
nesc(~result_1)&(fact_1 v poss(~fact_1)&(poss(~fact_2)v fact_2&nesc(fact_1 v fact_2)))==>poss(fact_2)
nesc(~result_1)&(poss(~fact_2)v fact_2&(poss(~fact_2)v poss(~fact_1)&(fact_1 v poss(~fact_1))))==>poss(~fact_1)
nesc(~result_1)&(poss(~fact_2)v fact_2&(poss(~fact_2)v poss(~fact_1)&(fact_1 v poss(~fact_1))))==>poss(~fact_2)
nesc(~result_1)&(poss(~fact_2)v poss(~fact_1)&(poss(~fact_2)v fact_2&nesc(fact_1 v fact_2)))==>poss(fact_1)
nesc(~result_1)&(poss(~fact_2)v poss(~fact_1)&(poss(~fact_2)v fact_2&nesc(fact_1 v fact_2)))==>poss(~fact_1)
poss(~fact_2)v poss(~fact_1)&(fact_1 v poss(~fact_1))&(poss(~fact_2)v fact_2&nesc(fact_1 v fact_2))==>poss(result_1)




=======================================================
?- kif_to_boxlog( poss(~ (fact_1 v fact_2=>result_1)) ).

nesc(result_1)==>poss(fact_1)
nesc(result_1)==>poss(fact_2)
nesc(result_1)==>poss(~result_1)
nesc(~fact_1)==>poss(fact_2)
nesc(~fact_1)==>poss(~result_1)
nesc(~fact_2)==>poss(fact_1)
nesc(~fact_2)==>poss(~result_1)
true.

134 ?-


