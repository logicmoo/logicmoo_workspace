#!/usr/bin/env swipl
:- if(set_prolog_flag(runtime_testing_module,user)).
:- if(set_prolog_flag(test_module,user)).
:- include(test_header).
:- endif.
:- endif.

show_xamples:- show_xample(write_each).

kif_to_boxlogx(X):-
  format('~n~n~n~n~n=======================================================~n'),
  write_eng(rule(X)),
  format('~n~n?- kif_to_boxlog( ~q ).\n\n',[(X)]),ttyflush,
  ignore((   
   with_no_output(kif_to_boxlog(X,O)),
   length(O,L),
   format('~n~n% Results in the following ~w entailment(s): ~n~n',[L]),
   sort(O,OO),ttyflush,!,maplist(write_each,OO))).

dkif_to_boxlog:- show_xample(kif_to_boxlogx),ah.

write_each(X):- is_list(X), !, maplist(write_each,X).
write_each(kb_why_flags_assert(_,_,_,X)):-!,write_each(X).
write_each(X):- ttyflush,nl,write_eng(rule(X)),nl,write(' \n'),write((X)),nl,nl,ttyflush.

write_eng_s(X):- is_list(X), !, write('['), maplist(write_eng,X), write(']').
write_eng_s(X):- write_eng(X).

write_eng(X):- is_list(X), !, maplist(write_eng,X).
write_eng(X):- var(X),!,write_eng(var(X)).
write_eng(A=>B):- !, write_eng(['if ',A,'then it is implied that ',B]).
write_eng(A==>B):- !, write_eng(['Whenever: ~n%   ',A,'~n% It\'s Proof that:~n%  ',B]).
write_eng(A & B):- !, write_eng([A,'and ',B]).
write_eng(A v B):- write_eng([A,'or ~n%   ',B]).
write_eng(rule(~B)):- write_eng(['% it is false that ',B]).
write_eng(rule(B)):- write_eng(['% ',B]).
write_eng(poss(~B)):- write_eng(['it is possibly false that ',B]).
write_eng(poss(B)):- write_eng(['it is possible that ',B]).
write_eng(~nesc(B)):- write_eng(['it is not nessicarily true that ',B]).
write_eng(nesc(~B)):- write_eng(['it is nessicarily false that ',B]).
write_eng(nesc(B)):- write_eng(['it is nessicarily true that ',B]).

write_eng(fact(X)):- X=.._L, writeq(X),write(' ').
write_eng(A):- is_fact(A),!, write_eng([fact(A),'is true ']).
write_eng(~A):- is_fact(A),!, write_eng([fact(A),'is false ']).
write_eng(~A):- !, write_eng([A,'is false ']).
write_eng(X):- atom(X), atom_contains(X,' '), format(X),!.
write_eng(X):- atom(X), format(X), write(' '),!.
write_eng(X):- writeq(X), write(' ').

is_fact(A):- atom(A),atom_contains(A,'_').
is_fact(A):- compound(A),arg(1,A,N),number(N).

%f_n_r(F,N,R):- R=..[F,N],!.

f_n_r(F,N,R):- atomic_list_concat([F,'_',N],R).

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
% fact_1 is true

?- kif_to_boxlog( fact_1 ).



% Results in the following 1 entailment(s):


% fact_1 is true

fact_1






=======================================================
% it is nessicarily true that fact_1 is true

?- kif_to_boxlog( nesc(fact_1) ).



% Results in the following 1 entailment(s):


% it is nessicarily true that fact_1 is true

nesc(fact_1)






=======================================================
% it is false that fact_1 is true

?- kif_to_boxlog( ~fact_1 ).



% Results in the following 1 entailment(s):


% it is false that fact_1 is true

~fact_1






=======================================================
% it is possible that fact_1 is true

?- kif_to_boxlog( poss(fact_1) ).



% Results in the following 1 entailment(s):


% it is possible that fact_1 is true

poss(fact_1)






=======================================================
% it is possibly false that fact_1 is true

?- kif_to_boxlog( poss(~fact_1) ).



% Results in the following 1 entailment(s):


% it is possibly false that fact_1 is true

poss(~fact_1)






=======================================================
% if fact_1 is true then it is implied that result_1 is true

?- kif_to_boxlog( fact_1=>result_1 ).



% Results in the following 2 entailment(s):


% Whenever:
%   it is nessicarily true that fact_1 is true
% It's Proof that:
%  it is nessicarily true that result_1 is true

nesc(fact_1)==>nesc(result_1)


% Whenever:
%   it is nessicarily false that result_1 is true
% It's Proof that:
%  it is nessicarily false that fact_1 is true

nesc(~result_1)==>nesc(~fact_1)






=======================================================
% it is nessicarily true that if fact_1 is true then it is implied that result_1 is true

?- kif_to_boxlog( nesc((fact_1=>result_1)) ).



% Results in the following 2 entailment(s):


% Whenever:
%   it is nessicarily true that fact_1 is true
% It's Proof that:
%  it is nessicarily true that result_1 is true

nesc(fact_1)==>nesc(result_1)


% Whenever:
%   it is possibly false that result_1 is true
% It's Proof that:
%  it is possibly false that fact_1 is true

poss(~result_1)==>poss(~fact_1)






=======================================================
% it is false that if fact_1 is true then it is implied that result_1 is true

?- kif_to_boxlog( ~ (fact_1=>result_1) ).



% Results in the following 4 entailment(s):


% Whenever:
%   it is nessicarily true that result_1 is true
% It's Proof that:
%  it is nessicarily false that fact_1 is true

nesc(result_1)==>nesc(~fact_1)


% Whenever:
%   it is nessicarily false that fact_1 is true
% It's Proof that:
%  it is nessicarily true that result_1 is true

nesc(~fact_1)==>nesc(result_1)


% Whenever:
%   it is possible that fact_1 is true
% It's Proof that:
%  it is nessicarily false that result_1 is true

poss(fact_1)==>nesc(~result_1)


% Whenever:
%   it is possibly false that result_1 is true
% It's Proof that:
%  it is nessicarily true that fact_1 is true

poss(~result_1)==>nesc(fact_1)






=======================================================
% it is possible that if fact_1 is true then it is implied that result_1 is true

?- kif_to_boxlog( poss((fact_1=>result_1)) ).



% Results in the following 2 entailment(s):


% Whenever:
%   it is nessicarily true that fact_1 is true
% It's Proof that:
%  it is possible that result_1 is true

nesc(fact_1)==>poss(result_1)


% Whenever:
%   it is nessicarily false that result_1 is true
% It's Proof that:
%  it is possibly false that fact_1 is true

nesc(~result_1)==>poss(~fact_1)






=======================================================
% it is possibly false that if fact_1 is true then it is implied that result_1 is true

?- kif_to_boxlog( poss(~ (fact_1=>result_1)) ).



% Results in the following 8 entailment(s):


% Whenever:
%   it is nessicarily true that fact_1 is false or
%   result_1 is true and it is possibly false that result_1 is true or
%   it is possible that fact_1 is true and fact_1 is false or
%   it is possible that fact_1 is true
% It's Proof that:
%  it is possible that result_1 is true

nesc(~fact_1 v result_1)&(poss(~result_1)v poss(fact_1)&(~fact_1 v poss(fact_1)))==>poss(result_1)


% Whenever:
%   it is nessicarily true that fact_1 is false or
%   result_1 is true and it is possibly false that result_1 is true or
%   it is possible that fact_1 is true and fact_1 is false or
%   it is possible that fact_1 is true
% It's Proof that:
%  it is possibly false that result_1 is true

nesc(~fact_1 v result_1)&(poss(~result_1)v poss(fact_1)&(~fact_1 v poss(fact_1)))==>poss(~result_1)


% Whenever:
%   it is possibly false that result_1 is true or
%   result_1 is true and it is possibly false that result_1 is true or
%   it is possible that fact_1 is true and fact_1 is false or
%   it is possible that fact_1 is true
% It's Proof that:
%  it is possible that fact_1 is true

poss(~result_1)v result_1&(poss(~result_1)v poss(fact_1)&(~fact_1 v poss(fact_1)))==>poss(fact_1)


% Whenever:
%   it is possibly false that result_1 is true or
%   result_1 is true and it is possibly false that result_1 is true or
%   it is possible that fact_1 is true and fact_1 is false or
%   it is possible that fact_1 is true
% It's Proof that:
%  it is possibly false that result_1 is true

poss(~result_1)v result_1&(poss(~result_1)v poss(fact_1)&(~fact_1 v poss(fact_1)))==>poss(~result_1)


% Whenever:
%   it is possibly false that result_1 is true or
%   it is possible that fact_1 is true and it is possibly false that result_1 is true or
%   result_1 is true and it is nessicarily true that fact_1 is false or
%   result_1 is true
% It's Proof that:
%  it is possible that fact_1 is true

poss(~result_1)v poss(fact_1)&(poss(~result_1)v result_1&nesc(~fact_1 v result_1))==>poss(fact_1)


% Whenever:
%   it is possibly false that result_1 is true or
%   it is possible that fact_1 is true and it is possibly false that result_1 is true or
%   result_1 is true and it is nessicarily true that fact_1 is false or
%   result_1 is true
% It's Proof that:
%  it is possibly false that fact_1 is true

poss(~result_1)v poss(fact_1)&(poss(~result_1)v result_1&nesc(~fact_1 v result_1))==>poss(~fact_1)


% Whenever:
%   fact_1 is false or
%   it is possible that fact_1 is true and it is possibly false that result_1 is true or
%   result_1 is true and it is nessicarily true that fact_1 is false or
%   result_1 is true
% It's Proof that:
%  it is possible that result_1 is true

~fact_1 v poss(fact_1)&(poss(~result_1)v result_1&nesc(~fact_1 v result_1))==>poss(result_1)


% Whenever:
%   fact_1 is false or
%   it is possible that fact_1 is true and it is possibly false that result_1 is true or
%   result_1 is true and it is nessicarily true that fact_1 is false or
%   result_1 is true
% It's Proof that:
%  it is possibly false that fact_1 is true

~fact_1 v poss(fact_1)&(poss(~result_1)v result_1&nesc(~fact_1 v result_1))==>poss(~fact_1)






=======================================================
% if fact_1 is true and fact_2 is true then it is implied that result_1 is true

?- kif_to_boxlog( fact_1&fact_2=>result_1 ).



% Results in the following 3 entailment(s):


% Whenever:
%   it is nessicarily true that fact_1 is true and it is nessicarily true that fact_2 is true
% It's Proof that:
%  it is nessicarily true that result_1 is true

nesc(fact_1)&nesc(fact_2)==>nesc(result_1)


% Whenever:
%   it is nessicarily false that result_1 is true and it is nessicarily true that fact_1 is true
% It's Proof that:
%  it is nessicarily false that fact_2 is true

nesc(~result_1)&nesc(fact_1)==>nesc(~fact_2)


% Whenever:
%   it is nessicarily false that result_1 is true and it is nessicarily true that fact_2 is true
% It's Proof that:
%  it is nessicarily false that fact_1 is true

nesc(~result_1)&nesc(fact_2)==>nesc(~fact_1)






=======================================================
% it is nessicarily true that if fact_1 is true and fact_2 is true then it is implied that result_1 is true

?- kif_to_boxlog( nesc((fact_1&fact_2=>result_1)) ).



% Results in the following 3 entailment(s):


% Whenever:
%   it is nessicarily true that fact_1 is true and it is nessicarily true that fact_2 is true
% It's Proof that:
%  it is nessicarily true that result_1 is true

nesc(fact_1)&nesc(fact_2)==>nesc(result_1)


% Whenever:
%   it is possibly false that result_1 is true and it is nessicarily true that fact_1 is true
% It's Proof that:
%  it is possibly false that fact_2 is true

poss(~result_1)&nesc(fact_1)==>poss(~fact_2)


% Whenever:
%   it is possibly false that result_1 is true and it is nessicarily true that fact_2 is true
% It's Proof that:
%  it is possibly false that fact_1 is true

poss(~result_1)&nesc(fact_2)==>poss(~fact_1)






=======================================================
% it is false that if fact_1 is true and fact_2 is true then it is implied that result_1 is true

?- kif_to_boxlog( ~ (fact_1&fact_2=>result_1) ).



% Results in the following 5 entailment(s):


% it is false that result_1 is true

~result_1


% Whenever:
%   it is nessicarily false that fact_1 is true
% It's Proof that:
%  it is nessicarily false that fact_2 is true

nesc(~fact_1)==>nesc(~fact_2)


% Whenever:
%   it is nessicarily false that fact_2 is true
% It's Proof that:
%  it is nessicarily false that fact_1 is true

nesc(~fact_2)==>nesc(~fact_1)


% Whenever:
%   it is possible that fact_1 is true
% It's Proof that:
%  it is nessicarily true that fact_2 is true

poss(fact_1)==>nesc(fact_2)


% Whenever:
%   it is possible that fact_2 is true
% It's Proof that:
%  it is nessicarily true that fact_1 is true

poss(fact_2)==>nesc(fact_1)






=======================================================
% it is possible that if fact_1 is true and fact_2 is true then it is implied that result_1 is true

?- kif_to_boxlog( poss((fact_1&fact_2=>result_1)) ).



% Results in the following 3 entailment(s):


% Whenever:
%   it is nessicarily true that fact_1 is true and it is nessicarily true that fact_2 is true
% It's Proof that:
%  it is possible that result_1 is true

nesc(fact_1)&nesc(fact_2)==>poss(result_1)


% Whenever:
%   it is nessicarily false that result_1 is true and it is nessicarily true that fact_1 is true
% It's Proof that:
%  it is possibly false that fact_2 is true

nesc(~result_1)&nesc(fact_1)==>poss(~fact_2)


% Whenever:
%   it is nessicarily false that result_1 is true and it is nessicarily true that fact_2 is true
% It's Proof that:
%  it is possibly false that fact_1 is true

nesc(~result_1)&nesc(fact_2)==>poss(~fact_1)






=======================================================
% it is possibly false that if fact_1 is true and fact_2 is true then it is implied that result_1 is true

?- kif_to_boxlog( poss(~ (fact_1&fact_2=>result_1)) ).



% Results in the following 12 entailment(s):


% Whenever:
%   it is nessicarily true that result_1 is true or
%   fact_1 is false or
%   fact_2 is false and result_1 is true or
%   it is possible that fact_2 is true or
%   it is possible that fact_1 is true and result_1 is true or
%   fact_1 is false or
%   it is possible that fact_1 is true
% It's Proof that:
%  it is possible that fact_2 is true

nesc(result_1 v (~fact_1 v ~fact_2))&(result_1 v (poss(fact_2)v poss(fact_1))&(result_1 v (~fact_1 v poss(fact_1))))==>poss(fact_2)


% Whenever:
%   it is nessicarily true that result_1 is true or
%   fact_1 is false or
%   fact_2 is false and result_1 is true or
%   it is possible that fact_2 is true or
%   it is possible that fact_1 is true and result_1 is true or
%   fact_1 is false or
%   it is possible that fact_1 is true
% It's Proof that:
%  it is possibly false that fact_2 is true

nesc(result_1 v (~fact_1 v ~fact_2))&(result_1 v (poss(fact_2)v poss(fact_1))&(result_1 v (~fact_1 v poss(fact_1))))==>poss(~fact_2)


% Whenever:
%   it is nessicarily true that result_1 is true or
%   fact_1 is false or
%   fact_2 is false and result_1 is true or
%   it is possible that fact_2 is true or
%   it is possible that fact_1 is true and result_1 is true or
%   fact_1 is false or
%   it is possible that fact_1 is true
% It's Proof that:
%  it is possibly false that result_1 is true

nesc(result_1 v (~fact_1 v ~fact_2))&(result_1 v (poss(fact_2)v poss(fact_1))&(result_1 v (~fact_1 v poss(fact_1))))==>poss(~result_1)


% Whenever:
%   result_1 is true or
%   it is possible that fact_2 is true or
%   it is possible that fact_1 is true and result_1 is true or
%   it is possible that fact_2 is true or
%   fact_2 is false and it is nessicarily true that result_1 is true or
%   fact_1 is false or
%   fact_2 is false
% It's Proof that:
%  it is possible that fact_1 is true

result_1 v (poss(fact_2)v poss(fact_1))&(result_1 v (poss(fact_2)v~fact_2)&nesc(result_1 v (~fact_1 v ~fact_2)))==>poss(fact_1)


% Whenever:
%   result_1 is true or
%   it is possible that fact_2 is true or
%   it is possible that fact_1 is true and result_1 is true or
%   it is possible that fact_2 is true or
%   fact_2 is false and it is nessicarily true that result_1 is true or
%   fact_1 is false or
%   fact_2 is false
% It's Proof that:
%  it is possibly false that fact_1 is true

result_1 v (poss(fact_2)v poss(fact_1))&(result_1 v (poss(fact_2)v~fact_2)&nesc(result_1 v (~fact_1 v ~fact_2)))==>poss(~fact_1)


% Whenever:
%   result_1 is true or
%   it is possible that fact_2 is true or
%   it is possible that fact_1 is true and result_1 is true or
%   it is possible that fact_2 is true or
%   fact_2 is false and it is nessicarily true that result_1 is true or
%   fact_1 is false or
%   fact_2 is false
% It's Proof that:
%  it is possibly false that result_1 is true

result_1 v (poss(fact_2)v poss(fact_1))&(result_1 v (poss(fact_2)v~fact_2)&nesc(result_1 v (~fact_1 v ~fact_2)))==>poss(~result_1)


% Whenever:
%   result_1 is true or
%   it is possible that fact_2 is true or
%   fact_2 is false and result_1 is true or
%   it is possible that fact_2 is true or
%   it is possible that fact_1 is true and result_1 is true or
%   fact_1 is false or
%   it is possible that fact_1 is true
% It's Proof that:
%  it is possible that fact_1 is true

result_1 v (poss(fact_2)v~fact_2)&(result_1 v (poss(fact_2)v poss(fact_1))&(result_1 v (~fact_1 v poss(fact_1))))==>poss(fact_1)


% Whenever:
%   result_1 is true or
%   it is possible that fact_2 is true or
%   fact_2 is false and result_1 is true or
%   it is possible that fact_2 is true or
%   it is possible that fact_1 is true and result_1 is true or
%   fact_1 is false or
%   it is possible that fact_1 is true
% It's Proof that:
%  it is possible that fact_2 is true

result_1 v (poss(fact_2)v~fact_2)&(result_1 v (poss(fact_2)v poss(fact_1))&(result_1 v (~fact_1 v poss(fact_1))))==>poss(fact_2)


% Whenever:
%   result_1 is true or
%   it is possible that fact_2 is true or
%   fact_2 is false and result_1 is true or
%   it is possible that fact_2 is true or
%   it is possible that fact_1 is true and result_1 is true or
%   fact_1 is false or
%   it is possible that fact_1 is true
% It's Proof that:
%  it is possibly false that result_1 is true

result_1 v (poss(fact_2)v~fact_2)&(result_1 v (poss(fact_2)v poss(fact_1))&(result_1 v (~fact_1 v poss(fact_1))))==>poss(~result_1)


% Whenever:
%   result_1 is true or
%   fact_1 is false or
%   it is possible that fact_1 is true and result_1 is true or
%   it is possible that fact_2 is true or
%   fact_2 is false and it is nessicarily true that result_1 is true or
%   fact_1 is false or
%   fact_2 is false
% It's Proof that:
%  it is possibly false that fact_1 is true

result_1 v (~fact_1 v poss(fact_1))&(result_1 v (poss(fact_2)v~fact_2)&nesc(result_1 v (~fact_1 v ~fact_2)))==>poss(~fact_1)


% Whenever:
%   result_1 is true or
%   fact_1 is false or
%   it is possible that fact_1 is true and result_1 is true or
%   it is possible that fact_2 is true or
%   fact_2 is false and it is nessicarily true that result_1 is true or
%   fact_1 is false or
%   fact_2 is false
% It's Proof that:
%  it is possibly false that fact_2 is true

result_1 v (~fact_1 v poss(fact_1))&(result_1 v (poss(fact_2)v~fact_2)&nesc(result_1 v (~fact_1 v ~fact_2)))==>poss(~fact_2)


% Whenever:
%   result_1 is true or
%   fact_1 is false or
%   it is possible that fact_1 is true and result_1 is true or
%   it is possible that fact_2 is true or
%   fact_2 is false and it is nessicarily true that result_1 is true or
%   fact_1 is false or
%   fact_2 is false
% It's Proof that:
%  it is possibly false that result_1 is true

result_1 v (~fact_1 v poss(fact_1))&(result_1 v (poss(fact_2)v~fact_2)&nesc(result_1 v (~fact_1 v ~fact_2)))==>poss(~result_1)






=======================================================
% if fact_1 is true or
%   fact_2 is true then it is implied that result_1 is true

?- kif_to_boxlog( fact_1 v fact_2=>result_1 ).



% Results in the following 6 entailment(s):


% Whenever:
%   it is nessicarily false that result_1 is true and it is nessicarily true that fact_1 is true
% It's Proof that:
%  it is nessicarily true that fact_2 is true

nesc(~result_1)&nesc(fact_1)==>nesc(fact_2)


% Whenever:
%   it is nessicarily false that result_1 is true and it is nessicarily true that fact_2 is true
% It's Proof that:
%  it is nessicarily true that fact_1 is true

nesc(~result_1)&nesc(fact_2)==>nesc(fact_1)


% Whenever:
%   it is nessicarily false that result_1 is true and it is possibly false that fact_1 is true
% It's Proof that:
%  it is nessicarily false that fact_2 is true

nesc(~result_1)&poss(~fact_1)==>nesc(~fact_2)


% Whenever:
%   it is nessicarily false that result_1 is true and it is possibly false that fact_2 is true
% It's Proof that:
%  it is nessicarily false that fact_1 is true

nesc(~result_1)&poss(~fact_2)==>nesc(~fact_1)


% Whenever:
%   it is possibly false that fact_1 is true and it is nessicarily true that fact_2 is true
% It's Proof that:
%  it is nessicarily true that result_1 is true

poss(~fact_1)&nesc(fact_2)==>nesc(result_1)


% Whenever:
%   it is possibly false that fact_2 is true and it is nessicarily true that fact_1 is true
% It's Proof that:
%  it is nessicarily true that result_1 is true

poss(~fact_2)&nesc(fact_1)==>nesc(result_1)






=======================================================
% it is nessicarily true that if fact_1 is true or
%   fact_2 is true then it is implied that result_1 is true

?- kif_to_boxlog( nesc((fact_1 v fact_2=>result_1)) ).



% Results in the following 6 entailment(s):


% Whenever:
%   it is possibly false that fact_1 is true and it is nessicarily true that fact_2 is true
% It's Proof that:
%  it is nessicarily true that result_1 is true

poss(~fact_1)&nesc(fact_2)==>nesc(result_1)


% Whenever:
%   it is possibly false that fact_2 is true and it is nessicarily true that fact_1 is true
% It's Proof that:
%  it is nessicarily true that result_1 is true

poss(~fact_2)&nesc(fact_1)==>nesc(result_1)


% Whenever:
%   it is possibly false that result_1 is true and it is nessicarily true that fact_1 is true
% It's Proof that:
%  it is nessicarily true that fact_2 is true

poss(~result_1)&nesc(fact_1)==>nesc(fact_2)


% Whenever:
%   it is possibly false that result_1 is true and it is nessicarily true that fact_2 is true
% It's Proof that:
%  it is nessicarily true that fact_1 is true

poss(~result_1)&nesc(fact_2)==>nesc(fact_1)


% Whenever:
%   it is possibly false that result_1 is true and it is possibly false that fact_1 is true
% It's Proof that:
%  it is possibly false that fact_2 is true

poss(~result_1)&poss(~fact_1)==>poss(~fact_2)


% Whenever:
%   it is possibly false that result_1 is true and it is possibly false that fact_2 is true
% It's Proof that:
%  it is possibly false that fact_1 is true

poss(~result_1)&poss(~fact_2)==>poss(~fact_1)






=======================================================
% it is false that if fact_1 is true or
%   fact_2 is true then it is implied that result_1 is true

?- kif_to_boxlog( ~ (fact_1 v fact_2=>result_1) ).



% Results in the following 3 entailment(s):


% it is false that result_1 is true

~result_1


% Whenever:
%   it is nessicarily false that fact_1 is true
% It's Proof that:
%  it is nessicarily true that fact_2 is true

nesc(~fact_1)==>nesc(fact_2)


% Whenever:
%   it is nessicarily false that fact_2 is true
% It's Proof that:
%  it is nessicarily true that fact_1 is true

nesc(~fact_2)==>nesc(fact_1)






=======================================================
% it is possible that if fact_1 is true or
%   fact_2 is true then it is implied that result_1 is true

?- kif_to_boxlog( poss((fact_1 v fact_2=>result_1)) ).



% Results in the following 9 entailment(s):


% Whenever:
%   it is nessicarily false that result_1 is true and it is nessicarily true that fact_1 is true or
%   fact_2 is true and it is possibly false that fact_2 is true or
%   it is possibly false that fact_1 is true and fact_1 is true or
%   it is possibly false that fact_1 is true
% It's Proof that:
%  it is possible that fact_2 is true

nesc(~result_1)&(nesc(fact_1 v fact_2)&(poss(~fact_2)v poss(~fact_1)&(fact_1 v poss(~fact_1))))==>poss(fact_2)


% Whenever:
%   it is nessicarily false that result_1 is true and it is nessicarily true that fact_1 is true or
%   fact_2 is true and it is possibly false that fact_2 is true or
%   it is possibly false that fact_1 is true and fact_1 is true or
%   it is possibly false that fact_1 is true
% It's Proof that:
%  it is possibly false that fact_2 is true

nesc(~result_1)&(nesc(fact_1 v fact_2)&(poss(~fact_2)v poss(~fact_1)&(fact_1 v poss(~fact_1))))==>poss(~fact_2)


% Whenever:
%   it is nessicarily false that result_1 is true and fact_1 is true or
%   it is possibly false that fact_1 is true and it is possibly false that fact_2 is true or
%   fact_2 is true and it is nessicarily true that fact_1 is true or
%   fact_2 is true
% It's Proof that:
%  it is possible that fact_1 is true

nesc(~result_1)&(fact_1 v poss(~fact_1)&(poss(~fact_2)v fact_2&nesc(fact_1 v fact_2)))==>poss(fact_1)


% Whenever:
%   it is nessicarily false that result_1 is true and fact_1 is true or
%   it is possibly false that fact_1 is true and it is possibly false that fact_2 is true or
%   fact_2 is true and it is nessicarily true that fact_1 is true or
%   fact_2 is true
% It's Proof that:
%  it is possible that fact_2 is true

nesc(~result_1)&(fact_1 v poss(~fact_1)&(poss(~fact_2)v fact_2&nesc(fact_1 v fact_2)))==>poss(fact_2)


% Whenever:
%   it is nessicarily false that result_1 is true and it is possibly false that fact_2 is true or
%   fact_2 is true and it is possibly false that fact_2 is true or
%   it is possibly false that fact_1 is true and fact_1 is true or
%   it is possibly false that fact_1 is true
% It's Proof that:
%  it is possibly false that fact_1 is true

nesc(~result_1)&(poss(~fact_2)v fact_2&(poss(~fact_2)v poss(~fact_1)&(fact_1 v poss(~fact_1))))==>poss(~fact_1)


% Whenever:
%   it is nessicarily false that result_1 is true and it is possibly false that fact_2 is true or
%   fact_2 is true and it is possibly false that fact_2 is true or
%   it is possibly false that fact_1 is true and fact_1 is true or
%   it is possibly false that fact_1 is true
% It's Proof that:
%  it is possibly false that fact_2 is true

nesc(~result_1)&(poss(~fact_2)v fact_2&(poss(~fact_2)v poss(~fact_1)&(fact_1 v poss(~fact_1))))==>poss(~fact_2)


% Whenever:
%   it is nessicarily false that result_1 is true and it is possibly false that fact_2 is true or
%   it is possibly false that fact_1 is true and it is possibly false that fact_2 is true or
%   fact_2 is true and it is nessicarily true that fact_1 is true or
%   fact_2 is true
% It's Proof that:
%  it is possible that fact_1 is true

nesc(~result_1)&(poss(~fact_2)v poss(~fact_1)&(poss(~fact_2)v fact_2&nesc(fact_1 v fact_2)))==>poss(fact_1)


% Whenever:
%   it is nessicarily false that result_1 is true and it is possibly false that fact_2 is true or
%   it is possibly false that fact_1 is true and it is possibly false that fact_2 is true or
%   fact_2 is true and it is nessicarily true that fact_1 is true or
%   fact_2 is true
% It's Proof that:
%  it is possibly false that fact_1 is true

nesc(~result_1)&(poss(~fact_2)v poss(~fact_1)&(poss(~fact_2)v fact_2&nesc(fact_1 v fact_2)))==>poss(~fact_1)


% Whenever:
%   it is possibly false that fact_2 is true or
%   it is possibly false that fact_1 is true and fact_1 is true or
%   it is possibly false that fact_1 is true and it is possibly false that fact_2 is true or
%   fact_2 is true and it is nessicarily true that fact_1 is true or
%   fact_2 is true
% It's Proof that:
%  it is possible that result_1 is true

poss(~fact_2)v poss(~fact_1)&(fact_1 v poss(~fact_1))&(poss(~fact_2)v fact_2&nesc(fact_1 v fact_2))==>poss(result_1)






=======================================================
% it is possibly false that if fact_1 is true or
%   fact_2 is true then it is implied that result_1 is true

?- kif_to_boxlog( poss(~ (fact_1 v fact_2=>result_1)) ).



% Results in the following 8 entailment(s):


% Whenever:
%   it is nessicarily true that result_1 is true
% It's Proof that:
%  it is possible that fact_1 is true

nesc(result_1)==>poss(fact_1)


% Whenever:
%   it is nessicarily true that result_1 is true
% It's Proof that:
%  it is possible that fact_2 is true

nesc(result_1)==>poss(fact_2)


% Whenever:
%   it is nessicarily true that result_1 is true
% It's Proof that:
%  it is possibly false that result_1 is true

nesc(result_1)==>poss(~result_1)


% Whenever:
%   it is nessicarily false that fact_1 is true
% It's Proof that:
%  it is possible that fact_2 is true

nesc(~fact_1)==>poss(fact_2)


% Whenever:
%   it is nessicarily false that fact_1 is true
% It's Proof that:
%  it is possibly false that result_1 is true

nesc(~fact_1)==>poss(~result_1)


% Whenever:
%   it is nessicarily false that fact_2 is true
% It's Proof that:
%  it is possible that fact_1 is true

nesc(~fact_2)==>poss(fact_1)


% Whenever:
%   it is nessicarily false that fact_2 is true
% It's Proof that:
%  it is possibly false that result_1 is true

nesc(~fact_2)==>poss(~result_1)

true.

112 ?-

