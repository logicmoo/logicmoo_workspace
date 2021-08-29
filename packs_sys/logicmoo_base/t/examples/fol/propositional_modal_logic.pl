#!/usr/bin/env swipl
:- if(set_prolog_flag(runtime_testing_module,user)).
:- if(set_prolog_flag(test_module,user)).
:- include(test_header).
:- endif.
:- endif.

show_xamples:- show_xample(write_each).

local_pretty_numbervars_ground(P,X):-
   pretty_numbervars_ground(P,X),
  guess_pretty(X).

kif_to_boxlogx(P):-
  local_pretty_numbervars_ground(P,X),
  format('~n~n~n~n~n=======================================================~n'),
  write_eng(rule(X)),
  % format('~n~n'),display(X),
  format('~n~n?- kif_to_boxlog( ~q ).\n\n',[(X)]),ttyflush,
  ignore((   
   %with_no_output
   (kif_to_boxlog(X,O)),
   length(O,L),
   format('~n~n% Results in the following ~w entailment(s): ~n~n',[L]),
   sort(O,OO),ttyflush,!,maplist(write_each,OO))).

dkif_to_boxlog:- cls,show_xample(kif_to_boxlogx),ah.

write_each(X):- is_list(X), !, maplist(write_each,X).
write_each(kb_why_flags_assert(_,_,_,X)):-!,write_each(X).
write_each(X):- ttyflush,nl,in_block((write_eng(rule(X)),nl,show_boxlog((X)))),nl,nl,ttyflush.

in_block(G):-
  writeln("----------------------------------------"),ttyflush,
  ignore(G),ttyflush,
  format("~N----------------------------------------"),ttyflush,!.

write_eng_s(X):- is_list(X), !, write('['), maplist(write_eng,X), write(']').
write_eng_s(X):- write_eng(X).

write_eng(X):- is_list(X), !, maplist(write_eng,X).
write_eng(X):- var(X),!,writeq(var(X)), write(' ').
write_eng(X):- atom(X), atom_contains(X,' '), format(X),!.
write_eng(A=>B):- !, write_eng(['If: ~n%   ',A,'then it is~n% Implied that:~n%   ',B]).
write_eng(A==>B):- !, write_eng(['Whenever: ~n%   ',A,'~n% It\'s Proof that:~n%   ',B]).
write_eng(A & B):- write_eng([paren(A),'and ~n%   ',paren(B)]).
write_eng(A v B):- write_eng([paren(A),'or ',paren(B)]).
write_eng(rule(~P)):- write_eng(['% it is false that ',P]).
write_eng(rule(poss(P))):-  write_eng(['% it is possible that ',P]).
write_eng(rule(poss(~P))):-  write_eng(['% it is possiblly FALSE that ',P]).
write_eng(rule(nesc(P))):-  write_eng(['% it is necessarily true that ',P]).
write_eng(rule(nesc(~P))):-  write_eng(['% it is necessarily FALSE that ',P]).
write_eng(rule(P)):- write_eng(['% ',P]).

write_eng(~poss(P)):-  write_eng(['it is NOT possible that ',paren(P)]).
write_eng(~nesc(P)):-  write_eng(['it is not necessarily true that ',paren(P)]).
write_eng(poss(~P)):-   write_eng([paren(P),'is possibly false ']).
write_eng(nesc(~P)):-   write_eng([paren(P),'is necessarily false ']).
write_eng(poss(P)):-   write_eng([paren(P),'is possible ']).
write_eng(nesc(P)):-   write_eng([paren(P),'is necessarily true ']).

write_eng('$VAR'(P)):- !,write('?'),write(P), write(' '),!.
write_eng(fact(P)):- !, write_fact(P).
write_eng(paren(P)):- is_lit_fact(P),!, write_qfact(P).
write_eng(paren(P)):- no_connectives(P),!, write_eng(P).
write_eng(paren(P)):- !, write_eng(['( ',P,') ']).
write_eng(~P):- write_fact([paren(P),' is false']).
write_eng(P):- write_qfact(P).

is_lit_fact(P):- contains_modal(P),!,fail.
is_lit_fact(P):- is_lit_atom(P).

no_connectives(P):- is_lit_fact(P),!.
no_connectives(P):- \+ compound(P).
no_connectives(P):- P=..[_,A],!,no_connectives(A).

write_qfact(P):- write_fact(['"',P,'"']).

write_fact(X):- \+ compound(X),!, write(X),write(' ').
write_fact('$VAR'(X)):- !,write('?'),write(X), write(' '),!.
write_fact(X):- is_list(X), !, maplist(write_fact,X). 
write_fact(X):- X=..[F,A,B],!,write_fact([A,'is',F,B]).
write_fact(X):- X=..[F,A],!,write_fact([A,'is a',F]).
write_fact(X):- writeq(X),write(' ').

%f_n_r(F,N,R):- R=..[F,N],!.

f_n_r(F,N,R):- atomic_list_concat([F,'_',N],R).

xamplen(F,N,R):- N=<1,!,f_n_r(F,N,R).
xamplen(F,N,O+R):- Nm1 is  N -1,f_n_r(F,N,R),xamplen(F,Nm1,O),!.

xample(F,O,L,U):- between(L,U,N),xamplen(F,N,R),(N=1->O=R;(subst(R,+,&,O);subst(R,+,v,O))).

xampl((leftof(H1, H2) => house(H1) & house(H2))).
xampl(F):- xample(fact,F,1,1).
xampl(F => R):- xample(result,R,1,1),xample(fact,F,1,2).
%xampl(F):- xample(fact,F,2,3).
%xampl(F => R):- xample(result,R,2,2),xample(fact,F,1,2).
%xampl(F & R):- xample(left,F),xample(right,R).
%xampl(F v R):- xample(left,F),xample(right,R).

do_xampl(P1,F):- 
   local_pretty_numbervars_ground(F,E),
   call(P1,(E)),
   call(P1,nesc(E)),
   call(P1, ~E),
   call(P1,poss(E)),
   call(P1,poss(~E)),
   !.

show_xample(P1):- update_changed_files,!, forall(xampl(X), do_xampl(P1,X)).

:-   show_xamples.
ah:- add_history(update_changed_files),add_history(show_xamples),add_history(dkif_to_boxlog).

end_of_file.

% the output of this is...


=======================================================
% If:
%   " ?House1 is leftof ?House2 " then it is
% Implied that:
%   " ?House1 is a house " and
%   " ?House2 is a house "

?- kif_to_boxlog( leftof(House1,House2)=>house(House1)&house(House2) ).

% kifm = leftof(House1,House2)=>house(House1)&house(House2).
% kif_to_boxlog_attvars2 = =>(leftof('$VAR'('House1'),'$VAR'('House2')),and(house('$VAR'('House1')),house('$VAR'('House2'))))


% Results in the following 6 entailment(s):


----------------------------------------
% Whenever:
%   " ?House1 is leftof ?House2 " is necessarily true and
%   " ?House1 is a house " is necessarily false
% It's Proof that:
%   " ?House2 is a house " is necessarily false

( nesc(leftof(House1,House2))&nesc(~house(House1)) ==>
  nesc( ~( house(House2))))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?House1 is leftof ?House2 " is necessarily true and
%   " ?House2 is a house " is necessarily false
% It's Proof that:
%   " ?House1 is a house " is necessarily false

( nesc(leftof(House1,House2))&nesc(~house(House2)) ==>
  nesc( ~( house(House1))))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?House1 is leftof ?House2 " is necessarily true and
%   " ?House1 is a house " is possible
% It's Proof that:
%   " ?House2 is a house " is necessarily true

nesc(leftof(House1,House2))&poss(house(House1))==>nesc(house(House2))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?House1 is leftof ?House2 " is necessarily true and
%   " ?House2 is a house " is possible
% It's Proof that:
%   " ?House1 is a house " is necessarily true

nesc(leftof(House1,House2))&poss(house(House2))==>nesc(house(House1))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?House1 is a house " is possible and
%   " ?House2 is a house " is necessarily false
% It's Proof that:
%   " ?House1 is leftof ?House2 " is necessarily false

( poss(house(House1))&nesc(~house(House2)) ==>
  nesc( ~( leftof(House1,House2))))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?House2 is a house " is possible and
%   " ?House1 is a house " is necessarily false
% It's Proof that:
%   " ?House1 is leftof ?House2 " is necessarily false

( poss(house(House2))&nesc(~house(House1)) ==>
  nesc( ~( leftof(House1,House2))))

----------------------------------------






=======================================================
% ( If:
%   " ?House1 is leftof ?House2 " then it is
% Implied that:
%   " ?House1 is a house " and
%   " ?House2 is a house " ) is necessarily true

?- kif_to_boxlog( nesc((leftof(House1,House2)=>house(House1)&house(House2))) ).

% kifm = nesc( leftof(House1,House2)=>house(House1)&house(House2)).
% kif_to_boxlog_attvars2 = necessary(=>(leftof('$VAR'('House1'),'$VAR'('House2')),and(house('$VAR'('House1')),house('$VAR'('House2')))))


% Results in the following 6 entailment(s):


----------------------------------------
% Whenever:
%   " ?House1 is a house " is necessarily true and
%   " ?House2 is a house " is possibly false
% It's Proof that:
%   " ?House1 is leftof ?House2 " is possibly false

( nesc(house(House1))&poss(~house(House2)) ==>
  poss( ~( leftof(House1,House2))))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?House2 is a house " is necessarily true and
%   " ?House1 is a house " is possibly false
% It's Proof that:
%   " ?House1 is leftof ?House2 " is possibly false

( nesc(house(House2))&poss(~house(House1)) ==>
  poss( ~( leftof(House1,House2))))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?House1 is leftof ?House2 " is necessarily true and
%   " ?House1 is a house " is necessarily true
% It's Proof that:
%   " ?House2 is a house " is necessarily true

nesc(leftof(House1,House2))&nesc(house(House1))==>nesc(house(House2))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?House1 is leftof ?House2 " is necessarily true and
%   " ?House2 is a house " is necessarily true
% It's Proof that:
%   " ?House1 is a house " is necessarily true

nesc(leftof(House1,House2))&nesc(house(House2))==>nesc(house(House1))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?House1 is leftof ?House2 " is necessarily true and
%   " ?House1 is a house " is possibly false
% It's Proof that:
%   " ?House2 is a house " is possibly false

( nesc(leftof(House1,House2))&poss(~house(House1)) ==>
  poss( ~( house(House2))))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?House1 is leftof ?House2 " is necessarily true and
%   " ?House2 is a house " is possibly false
% It's Proof that:
%   " ?House1 is a house " is possibly false

( nesc(leftof(House1,House2))&poss(~house(House2)) ==>
  poss( ~( house(House1))))

----------------------------------------






=======================================================
% it is false that If:
%   " ?House1 is leftof ?House2 " then it is
% Implied that:
%   " ?House1 is a house " and
%   " ?House2 is a house "

?- kif_to_boxlog( ~ (leftof(House1,House2)=>house(House1)&house(House2)) ).

% kifm = ~( leftof(House1,House2)=>house(House1)&house(House2)).
% kif_to_boxlog_attvars2 = not(=>(leftof('$VAR'('House1'),'$VAR'('House2')),and(house('$VAR'('House1')),house('$VAR'('House2')))))


% Results in the following 7 entailment(s):


----------------------------------------
% Whenever:
%   " ?House1 is leftof ?House2 " is necessarily false
% It's Proof that:
%   " ?House1 is a house " is necessarily false or " ?House2 is a house "

nesc(~leftof(House1,House2))==>nesc(~house(House1))v house(House2)

----------------------------------------


----------------------------------------
% Whenever:
%   " ?House1 is leftof ?House2 " is necessarily false
% It's Proof that:
%   " ?House2 is a house " is necessarily false or " ?House1 is a house "

nesc(~leftof(House1,House2))==>nesc(~house(House2))v house(House1)

----------------------------------------


----------------------------------------
% Whenever:
%   ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible
% It's Proof that:
%   " ?House1 is leftof ?House2 " is necessarily true

poss(poss(house(House1))& ~house(House2))==>nesc(leftof(House1,House2))

----------------------------------------


----------------------------------------
% Whenever:
%   ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible
% It's Proof that:
%   " ?House1 is leftof ?House2 " is necessarily true

poss(poss(house(House2))& ~house(House1))==>nesc(leftof(House1,House2))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?House1 is a house " is necessarily true and
%   " ?House2 is a house " is necessarily true
% It's Proof that:
%   " ?House1 is leftof ?House2 " is necessarily false

nesc(house(House1))&nesc(house(House2))==>nesc(~leftof(House1,House2))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?House1 is leftof ?House2 " is possible and
%   " ?House1 is a house " is necessarily true
% It's Proof that:
%   " ?House2 is a house " is necessarily false

poss(leftof(House1,House2))&nesc(house(House1))==>nesc(~house(House2))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?House1 is leftof ?House2 " is possible and
%   " ?House2 is a house " is necessarily true
% It's Proof that:
%   " ?House1 is a house " is necessarily false

poss(leftof(House1,House2))&nesc(house(House2))==>nesc(~house(House1))

----------------------------------------






=======================================================
% it is possible that If:
%   " ?House1 is leftof ?House2 " then it is
% Implied that:
%   " ?House1 is a house " and
%   " ?House2 is a house "

?- kif_to_boxlog( poss((leftof(House1,House2)=>house(House1)&house(House2))) ).

% kifm = poss( leftof(House1,House2)=>house(House1)&house(House2)).
% kif_to_boxlog_attvars2 = possible(=>(leftof('$VAR'('House1'),'$VAR'('House2')),and(house('$VAR'('House1')),house('$VAR'('House2')))))


% Results in the following 5 entailment(s):


----------------------------------------
% Whenever:
%   " ?House1 is leftof ?House2 " is necessarily true and
%   ( ( ( " ?House2 is a house " is possible or " ?House1 is a house " is possible ) and
%   ( ?House1 is a house is false or " ?House1 is a house " is possible ) ) and
%   " ?House1 is a house is , ?House2 is a house " is necessarily false )
% It's Proof that:
%   " ?House2 is a house " is possibly false

( (   nesc( leftof(House1,House2))  &
    poss(house(House2))v poss(house(House1)) &
    ~house(House1)v poss(house(House1)) &
    nesc( ~( house(House1),house(House2)))) ==>
  poss( ~( house(House2))))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?House1 is leftof ?House2 " is necessarily true and
%   ( ( ( " ?House2 is a house " is possible or " ?House1 is a house " is possible ) and
%   ( ?House1 is a house is false or " ?House1 is a house " is possible ) ) and
%   " ?House1 is a house is , ?House2 is a house " is necessarily false )
% It's Proof that:
%   " ?House2 is a house " is possible

( (   nesc( leftof(House1,House2))  &
    poss(house(House2))v poss(house(House1)) &
    ~house(House1)v poss(house(House1)) &
    nesc( ~( house(House1),house(House2)))) ==>
  poss( house(House2)))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?House1 is leftof ?House2 " is necessarily true and
%   ( ( " ?House2 is a house " is possible or " ?House1 is a house " is possible ) and
%   ( ( " ?House2 is a house " is possible or ?House2 is a house is false ) and
%   " ?House1 is a house is , ?House2 is a house " is necessarily false ) )
% It's Proof that:
%   " ?House1 is a house " is possible

( (   nesc( leftof(House1,House2))  &
    poss(house(House2))v poss(house(House1)) &
    poss(house(House2))v~house(House2) &
    nesc( ~( house(House1),house(House2)))) ==>
  poss( house(House1)))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?House1 is leftof ?House2 " is necessarily true and
%   ( ( " ?House2 is a house " is possible or " ?House1 is a house " is possible ) and
%   ( ( " ?House2 is a house " is possible or ?House2 is a house is false ) and
%   " ?House1 is a house is , ?House2 is a house " is necessarily false ) )
% It's Proof that:
%   " ?House1 is a house " is possibly false

( (   nesc( leftof(House1,House2))  &
    poss(house(House2))v poss(house(House1)) &
    poss(house(House2))v~house(House2) &
    nesc( ~( house(House1),house(House2)))) ==>
  poss( ~( house(House1))))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?House1 is leftof ?House2 " is necessarily true and
%   ( ( ?House1 is a house is false or " ?House1 is a house " is possible ) and
%   ( ( " ?House2 is a house " is possible or ?House2 is a house is false ) and
%   " ?House1 is a house is , ?House2 is a house " is necessarily false ) )
% It's Proof that:
%   " ?House2 is a house " is possibly false

( (   nesc( leftof(House1,House2))  &
    ~house(House1)v poss(house(House1)) &
    poss(house(House2))v~house(House2) &
    nesc( ~( house(House1),house(House2)))) ==>
  poss( ~( house(House2))))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?House1 is leftof ?House2 " is necessarily true and
%   ( ( ?House1 is a house is false or " ?House1 is a house " is possible ) and
%   ( ( " ?House2 is a house " is possible or ?House2 is a house is false ) and
%   " ?House1 is a house is , ?House2 is a house " is necessarily false ) )
% It's Proof that:
%   " ?House1 is a house " is possibly false

( (   nesc( leftof(House1,House2))  &
    ~house(House1)v poss(house(House1)) &
    poss(house(House2))v~house(House2) &
    nesc( ~( house(House1),house(House2)))) ==>
  poss( ~( house(House1))))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " ?House2 is a house " is possible or " ?House1 is a house " is possible ) and
%   ( ?House1 is a house is false or " ?House1 is a house " is possible ) ) and
%   ( ( " ?House2 is a house " is possible or ?House2 is a house is false ) and
%   " ?House1 is a house is , ?House2 is a house " is necessarily false )
% It's Proof that:
%   " ?House1 is leftof ?House2 " is possibly false

( (   poss(house(House2))v poss(house(House1))  &
    ~house(House1)v poss(house(House1)) &
    poss(house(House2))v~house(House2) &
    nesc( ~( house(House1),house(House2)))) ==>
  poss( ~( leftof(House1,House2))))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?House1 is leftof ?House2 " is necessarily true and
%   ( ( ( " ?House2 is a house " is possible or " ?House1 is a house " is possible ) and
%   ( ?House1 is a house is false or " ?House1 is a house " is possible ) ) and
%   ( " ?House2 is a house " is possible or ?House2 is a house is false ) )
% It's Proof that:
%   " ?House1 is a house " is possible

( (   nesc( leftof(House1,House2))  &
    poss(house(House2))v poss(house(House1)) &
    ~house(House1)v poss(house(House1)) &
    poss(house(House2))v~house(House2)) ==>
  poss( house(House1)))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?House1 is leftof ?House2 " is necessarily true and
%   ( ( ( " ?House2 is a house " is possible or " ?House1 is a house " is possible ) and
%   ( ?House1 is a house is false or " ?House1 is a house " is possible ) ) and
%   ( " ?House2 is a house " is possible or ?House2 is a house is false ) )
% It's Proof that:
%   " ?House2 is a house " is possible

( (   nesc( leftof(House1,House2))  &
    poss(house(House2))v poss(house(House1)) &
    ~house(House1)v poss(house(House1)) &
    poss(house(House2))v~house(House2)) ==>
  poss( house(House2)))

----------------------------------------






=======================================================
% it is possible that ?House1 is leftof ?House2 is => ?House1 is a house is & ?House2 is a house is false

?- kif_to_boxlog( poss(~ (leftof(House1,House2)=>house(House1)&house(House2))) ).

% kifm = poss( ~( leftof(House1,House2)=>house(House1)&house(House2))).
% kif_to_boxlog_attvars2 = possible(not(=>(leftof('$VAR'('House1'),'$VAR'('House2')),and(house('$VAR'('House1')),house('$VAR'('House2'))))))


% Results in the following 6 entailment(s):


----------------------------------------
% Whenever:
%   ( ( ( " ?House1 is a house " or ?House1 is leftof ?House2 is false ) is necessarily true ) and
%   ( ( " ?House2 is a house " or ?House1 is leftof ?House2 is false ) is necessarily true ) ) and
%   ( ( " ?House1 is leftof ?House2 " is possible or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) and
%   ( ( " ?House1 is a house " or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) and
%   ( " ?House2 is a house " or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) ) )
% It's Proof that:
%   " ?House1 is leftof ?House2 " is possibly false

( (   nesc( house(House1)v~leftof(House1,House2))  &
    nesc( house(House2)v~leftof(House1,House2)) &
    (   poss( leftof(House1,House2))  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2))) &
    (   house(House1)  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2))) &
    (   house(House2)  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2)))) ==>
  poss( ~( leftof(House1,House2))))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( ( " ?House1 is a house " or ?House1 is leftof ?House2 is false ) is necessarily true ) and
%   ( ( " ?House2 is a house " or ?House1 is leftof ?House2 is false ) is necessarily true ) ) and
%   ( ( " ?House1 is leftof ?House2 " is possible or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) and
%   ( ( " ?House1 is a house " or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) and
%   ( " ?House2 is a house " or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) ) )
% It's Proof that:
%   " ?House1 is leftof ?House2 " is possible

( (   nesc( house(House1)v~leftof(House1,House2))  &
    nesc( house(House2)v~leftof(House1,House2)) &
    (   poss( leftof(House1,House2))  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2))) &
    (   house(House1)  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2))) &
    (   house(House2)  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2)))) ==>
  poss( leftof(House1,House2)))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " ?House1 is leftof ?House2 " is possible or ?House1 is leftof ?House2 is false ) and
%   ( ( " ?House1 is a house " or ?House1 is leftof ?House2 is false ) is necessarily true ) ) and
%   ( ( " ?House1 is leftof ?House2 " is possible or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) and
%   ( ( " ?House1 is a house " or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) and
%   ( " ?House2 is a house " or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) ) )
% It's Proof that:
%   " ?House2 is a house " is possibly false

( (   poss(leftof(House1,House2))v~leftof(House1,House2)  &
    nesc( house(House1)v~leftof(House1,House2)) &
    (   poss( leftof(House1,House2))  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2))) &
    (   house(House1)  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2))) &
    (   house(House2)  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2)))) ==>
  poss( ~( house(House2))))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " ?House1 is leftof ?House2 " is possible or ?House1 is leftof ?House2 is false ) and
%   ( ( " ?House1 is a house " or ?House1 is leftof ?House2 is false ) is necessarily true ) ) and
%   ( ( " ?House1 is leftof ?House2 " is possible or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) and
%   ( ( " ?House1 is a house " or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) and
%   ( " ?House2 is a house " or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) ) )
% It's Proof that:
%   " ?House1 is leftof ?House2 " is possible

( (   poss(leftof(House1,House2))v~leftof(House1,House2)  &
    nesc( house(House1)v~leftof(House1,House2)) &
    (   poss( leftof(House1,House2))  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2))) &
    (   house(House1)  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2))) &
    (   house(House2)  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2)))) ==>
  poss( leftof(House1,House2)))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " ?House1 is leftof ?House2 " is possible or ?House1 is leftof ?House2 is false ) and
%   ( ( " ?House2 is a house " or ?House1 is leftof ?House2 is false ) is necessarily true ) ) and
%   ( ( " ?House1 is leftof ?House2 " is possible or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) and
%   ( ( " ?House1 is a house " or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) and
%   ( " ?House2 is a house " or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) ) )
% It's Proof that:
%   " ?House1 is a house " is possibly false

( (   poss(leftof(House1,House2))v~leftof(House1,House2)  &
    nesc( house(House2)v~leftof(House1,House2)) &
    (   poss( leftof(House1,House2))  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2))) &
    (   house(House1)  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2))) &
    (   house(House2)  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2)))) ==>
  poss( ~( house(House1))))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " ?House1 is leftof ?House2 " is possible or ?House1 is leftof ?House2 is false ) and
%   ( ( " ?House2 is a house " or ?House1 is leftof ?House2 is false ) is necessarily true ) ) and
%   ( ( " ?House1 is leftof ?House2 " is possible or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) and
%   ( ( " ?House1 is a house " or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) and
%   ( " ?House2 is a house " or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) ) )
% It's Proof that:
%   " ?House1 is leftof ?House2 " is possible

( (   poss(leftof(House1,House2))v~leftof(House1,House2)  &
    nesc( house(House2)v~leftof(House1,House2)) &
    (   poss( leftof(House1,House2))  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2))) &
    (   house(House1)  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2))) &
    (   house(House2)  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2)))) ==>
  poss( leftof(House1,House2)))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " ?House1 is leftof ?House2 " is possible or ?House1 is leftof ?House2 is false ) and
%   ( ( ( " ?House1 is a house " or ?House1 is leftof ?House2 is false ) is necessarily true ) and
%   ( ( " ?House2 is a house " or ?House1 is leftof ?House2 is false ) is necessarily true ) ) ) and
%   ( ( " ?House1 is a house " or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) and
%   ( " ?House2 is a house " or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) )
% It's Proof that:
%   " ?House1 is leftof ?House2 " is possibly false

( (   poss(leftof(House1,House2))v~leftof(House1,House2)  &
    nesc( house(House1)v~leftof(House1,House2)) &
    nesc( house(House2)v~leftof(House1,House2)) &
    (   house(House1)  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2))) &
    (   house(House2)  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2)))) ==>
  poss( ~( leftof(House1,House2))))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " ?House1 is leftof ?House2 " is possible or ?House1 is leftof ?House2 is false ) and
%   ( ( ( " ?House1 is a house " or ?House1 is leftof ?House2 is false ) is necessarily true ) and
%   ( ( " ?House2 is a house " or ?House1 is leftof ?House2 is false ) is necessarily true ) ) ) and
%   ( ( " ?House1 is a house " or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) and
%   ( " ?House2 is a house " or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) )
% It's Proof that:
%   " ?House2 is a house " is possibly false or " ?House1 is a house " is possible

( (   poss(leftof(House1,House2))v~leftof(House1,House2)  &
    nesc( house(House1)v~leftof(House1,House2)) &
    nesc( house(House2)v~leftof(House1,House2)) &
    (   house(House1)  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2))) &
    (   house(House2)  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2)))) ==>
  poss(~house(House2))v poss(house(House1)))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " ?House1 is leftof ?House2 " is possible or ?House1 is leftof ?House2 is false ) and
%   ( ( ( " ?House1 is a house " or ?House1 is leftof ?House2 is false ) is necessarily true ) and
%   ( ( " ?House2 is a house " or ?House1 is leftof ?House2 is false ) is necessarily true ) ) ) and
%   ( ( " ?House1 is a house " or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) and
%   ( " ?House2 is a house " or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) )
% It's Proof that:
%   " ?House1 is a house " is possibly false or " ?House2 is a house " is possible

( (   poss(leftof(House1,House2))v~leftof(House1,House2)  &
    nesc( house(House1)v~leftof(House1,House2)) &
    nesc( house(House2)v~leftof(House1,House2)) &
    (   house(House1)  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2))) &
    (   house(House2)  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2)))) ==>
  poss(~house(House1))v poss(house(House2)))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " ?House1 is leftof ?House2 " is possible or ?House1 is leftof ?House2 is false ) and
%   ( ( ( " ?House1 is a house " or ?House1 is leftof ?House2 is false ) is necessarily true ) and
%   ( ( " ?House2 is a house " or ?House1 is leftof ?House2 is false ) is necessarily true ) ) ) and
%   ( ( " ?House1 is leftof ?House2 " is possible or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) and
%   ( " ?House1 is a house " or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) )
% It's Proof that:
%   " ?House2 is a house " is possibly false

( (   poss(leftof(House1,House2))v~leftof(House1,House2)  &
    nesc( house(House1)v~leftof(House1,House2)) &
    nesc( house(House2)v~leftof(House1,House2)) &
    (   poss( leftof(House1,House2))  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2))) &
    (   house(House1)  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2)))) ==>
  poss( ~( house(House2))))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " ?House1 is leftof ?House2 " is possible or ?House1 is leftof ?House2 is false ) and
%   ( ( ( " ?House1 is a house " or ?House1 is leftof ?House2 is false ) is necessarily true ) and
%   ( ( " ?House2 is a house " or ?House1 is leftof ?House2 is false ) is necessarily true ) ) ) and
%   ( ( " ?House1 is leftof ?House2 " is possible or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) and
%   ( " ?House1 is a house " or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) )
% It's Proof that:
%   " ?House2 is a house " is possibly false or " ?House1 is a house " is possible

( (   poss(leftof(House1,House2))v~leftof(House1,House2)  &
    nesc( house(House1)v~leftof(House1,House2)) &
    nesc( house(House2)v~leftof(House1,House2)) &
    (   poss( leftof(House1,House2))  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2))) &
    (   house(House1)  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2)))) ==>
  poss(~house(House2))v poss(house(House1)))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " ?House1 is leftof ?House2 " is possible or ?House1 is leftof ?House2 is false ) and
%   ( ( ( " ?House1 is a house " or ?House1 is leftof ?House2 is false ) is necessarily true ) and
%   ( ( " ?House2 is a house " or ?House1 is leftof ?House2 is false ) is necessarily true ) ) ) and
%   ( ( " ?House1 is leftof ?House2 " is possible or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) and
%   ( " ?House1 is a house " or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) )
% It's Proof that:
%   " ?House1 is a house " is possibly false or " ?House2 is a house " is possible

( (   poss(leftof(House1,House2))v~leftof(House1,House2)  &
    nesc( house(House1)v~leftof(House1,House2)) &
    nesc( house(House2)v~leftof(House1,House2)) &
    (   poss( leftof(House1,House2))  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2))) &
    (   house(House1)  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2)))) ==>
  poss(~house(House1))v poss(house(House2)))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " ?House1 is leftof ?House2 " is possible or ?House1 is leftof ?House2 is false ) and
%   ( ( ( " ?House1 is a house " or ?House1 is leftof ?House2 is false ) is necessarily true ) and
%   ( ( " ?House2 is a house " or ?House1 is leftof ?House2 is false ) is necessarily true ) ) ) and
%   ( ( " ?House1 is leftof ?House2 " is possible or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) and
%   ( " ?House2 is a house " or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) )
% It's Proof that:
%   " ?House1 is a house " is possibly false

( (   poss(leftof(House1,House2))v~leftof(House1,House2)  &
    nesc( house(House1)v~leftof(House1,House2)) &
    nesc( house(House2)v~leftof(House1,House2)) &
    (   poss( leftof(House1,House2))  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2))) &
    (   house(House2)  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2)))) ==>
  poss( ~( house(House1))))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " ?House1 is leftof ?House2 " is possible or ?House1 is leftof ?House2 is false ) and
%   ( ( ( " ?House1 is a house " or ?House1 is leftof ?House2 is false ) is necessarily true ) and
%   ( ( " ?House2 is a house " or ?House1 is leftof ?House2 is false ) is necessarily true ) ) ) and
%   ( ( " ?House1 is leftof ?House2 " is possible or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) and
%   ( " ?House2 is a house " or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) )
% It's Proof that:
%   " ?House2 is a house " is possibly false or " ?House1 is a house " is possible

( (   poss(leftof(House1,House2))v~leftof(House1,House2)  &
    nesc( house(House1)v~leftof(House1,House2)) &
    nesc( house(House2)v~leftof(House1,House2)) &
    (   poss( leftof(House1,House2))  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2))) &
    (   house(House2)  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2)))) ==>
  poss(~house(House2))v poss(house(House1)))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " ?House1 is leftof ?House2 " is possible or ?House1 is leftof ?House2 is false ) and
%   ( ( ( " ?House1 is a house " or ?House1 is leftof ?House2 is false ) is necessarily true ) and
%   ( ( " ?House2 is a house " or ?House1 is leftof ?House2 is false ) is necessarily true ) ) ) and
%   ( ( " ?House1 is leftof ?House2 " is possible or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) and
%   ( " ?House2 is a house " or ( ( ( " ?House2 is a house " is possible and
%   ?House1 is a house is false ) is possible ) or ( ( " ?House1 is a house " is possible and
%   ?House2 is a house is false ) is possible ) ) ) )
% It's Proof that:
%   " ?House1 is a house " is possibly false or " ?House2 is a house " is possible

( (   poss(leftof(House1,House2))v~leftof(House1,House2)  &
    nesc( house(House1)v~leftof(House1,House2)) &
    nesc( house(House2)v~leftof(House1,House2)) &
    (   poss( leftof(House1,House2))  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2))) &
    (   house(House2)  v
      poss( poss(house(House2))& ~house(House1)) v
      poss( poss(house(House1))& ~house(House2)))) ==>
  poss(~house(House1))v poss(house(House2)))

----------------------------------------






=======================================================
% " fact_1 "

?- kif_to_boxlog( fact_1 ).

% kifm=fact_1.
% kif_to_boxlog_attvars2 = fact_1


% Results in the following 1 entailment(s):


----------------------------------------
% " fact_1 "

fact_1

----------------------------------------






=======================================================
% " fact_1 " is necessarily true

?- kif_to_boxlog( nesc(fact_1) ).

% kifm = nesc(fact_1).
% kif_to_boxlog_attvars2 = necessary(fact_1)


% Results in the following 1 entailment(s):


----------------------------------------
% " fact_1 " is necessarily true

nesc(fact_1)

----------------------------------------






=======================================================
% it is false that " fact_1 "

?- kif_to_boxlog( ~fact_1 ).

% kifm = ~fact_1.
% kif_to_boxlog_attvars2 = not(fact_1)


% Results in the following 1 entailment(s):


----------------------------------------
% it is false that " fact_1 "

~fact_1

----------------------------------------






=======================================================
% it is possible that " fact_1 "

?- kif_to_boxlog( poss(fact_1) ).

% kifm = poss(fact_1).
% kif_to_boxlog_attvars2 = possible(fact_1)


% Results in the following 1 entailment(s):


----------------------------------------
% it is possible that " fact_1 "

poss(fact_1)

----------------------------------------






=======================================================
% it is possible that fact_1 is false

?- kif_to_boxlog( poss(~fact_1) ).

% kifm = poss( ~fact_1).
% kif_to_boxlog_attvars2 = possible(not(fact_1))


% Results in the following 1 entailment(s):


----------------------------------------
% it is possible that fact_1 is false

poss( ~fact_1)

----------------------------------------






=======================================================
% If:
%   " fact_1 " then it is
% Implied that:
%   " result_1 "

?- kif_to_boxlog( fact_1=>result_1 ).

% kifm = fact_1=>result_1.
% kif_to_boxlog_attvars2 = =>(fact_1,result_1)


% Results in the following 2 entailment(s):


----------------------------------------
% Whenever:
%   " fact_1 " is necessarily true
% It's Proof that:
%   " result_1 " is necessarily true

nesc(fact_1)==>nesc(result_1)

----------------------------------------


----------------------------------------
% Whenever:
%   " result_1 " is necessarily false
% It's Proof that:
%   " fact_1 " is necessarily false

nesc(~result_1)==>nesc(~fact_1)

----------------------------------------






=======================================================
% ( If:
%   " fact_1 " then it is
% Implied that:
%   " result_1 " ) is necessarily true

?- kif_to_boxlog( nesc((fact_1=>result_1)) ).

% kifm = nesc( fact_1=>result_1).
% kif_to_boxlog_attvars2 = necessary(=>(fact_1,result_1))


% Results in the following 2 entailment(s):


----------------------------------------
% Whenever:
%   " fact_1 " is necessarily true
% It's Proof that:
%   " result_1 " is necessarily true

nesc(fact_1)==>nesc(result_1)

----------------------------------------


----------------------------------------
% Whenever:
%   " result_1 " is possibly false
% It's Proof that:
%   " fact_1 " is possibly false

poss(~result_1)==>poss(~fact_1)

----------------------------------------






=======================================================
% it is false that If:
%   " fact_1 " then it is
% Implied that:
%   " result_1 "

?- kif_to_boxlog( ~ (fact_1=>result_1) ).

% kifm = ~( fact_1=>result_1).
% kif_to_boxlog_attvars2 = not(=>(fact_1,result_1))


% Results in the following 4 entailment(s):


----------------------------------------
% Whenever:
%   " result_1 " is necessarily true
% It's Proof that:
%   " fact_1 " is necessarily false

nesc(result_1)==>nesc(~fact_1)

----------------------------------------


----------------------------------------
% Whenever:
%   " fact_1 " is necessarily false
% It's Proof that:
%   " result_1 " is necessarily true

nesc(~fact_1)==>nesc(result_1)

----------------------------------------


----------------------------------------
% Whenever:
%   " fact_1 " is possible
% It's Proof that:
%   " result_1 " is necessarily false

poss(fact_1)==>nesc(~result_1)

----------------------------------------


----------------------------------------
% Whenever:
%   " result_1 " is possibly false
% It's Proof that:
%   " fact_1 " is necessarily true

poss(~result_1)==>nesc(fact_1)

----------------------------------------






=======================================================
% it is possible that If:
%   " fact_1 " then it is
% Implied that:
%   " result_1 "

?- kif_to_boxlog( poss((fact_1=>result_1)) ).

% kifm = poss( fact_1=>result_1).
% kif_to_boxlog_attvars2 = possible(=>(fact_1,result_1))


% Results in the following 2 entailment(s):


----------------------------------------
% Whenever:
%   " fact_1 " is necessarily true
% It's Proof that:
%   " result_1 " is possible

nesc(fact_1)==>poss(result_1)

----------------------------------------


----------------------------------------
% Whenever:
%   " result_1 " is necessarily false
% It's Proof that:
%   " fact_1 " is possibly false

nesc(~result_1)==>poss(~fact_1)

----------------------------------------






=======================================================
% it is possible that fact_1 is => result_1 is false

?- kif_to_boxlog( poss(~ (fact_1=>result_1)) ).

% kifm = poss( ~( fact_1=>result_1)).
% kif_to_boxlog_attvars2 = possible(not(=>(fact_1,result_1)))


% Results in the following 4 entailment(s):


----------------------------------------
% Whenever:
%   ( ( fact_1 is false or " result_1 " ) is necessarily true ) and
%   ( ( " result_1 " is possibly false or " fact_1 " is possible ) and
%   ( fact_1 is false or " fact_1 " is possible ) )
% It's Proof that:
%   " result_1 " is possible

nesc(~fact_1 v result_1)&(poss(~result_1)v poss(fact_1)&(~fact_1 v poss(fact_1)))==>poss(result_1)

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( fact_1 is false or " result_1 " ) is necessarily true ) and
%   ( ( " result_1 " is possibly false or " fact_1 " is possible ) and
%   ( fact_1 is false or " fact_1 " is possible ) )
% It's Proof that:
%   " result_1 " is possibly false

nesc(~fact_1 v result_1)&(poss(~result_1)v poss(fact_1)&(~fact_1 v poss(fact_1)))==>poss(~result_1)

----------------------------------------


----------------------------------------
% Whenever:
%   ( " result_1 " is possibly false or " result_1 " ) and
%   ( ( " result_1 " is possibly false or " fact_1 " is possible ) and
%   ( fact_1 is false or " fact_1 " is possible ) )
% It's Proof that:
%   " fact_1 " is possible

poss(~result_1)v result_1&(poss(~result_1)v poss(fact_1)&(~fact_1 v poss(fact_1)))==>poss(fact_1)

----------------------------------------


----------------------------------------
% Whenever:
%   ( " result_1 " is possibly false or " result_1 " ) and
%   ( ( " result_1 " is possibly false or " fact_1 " is possible ) and
%   ( fact_1 is false or " fact_1 " is possible ) )
% It's Proof that:
%   " result_1 " is possibly false

poss(~result_1)v result_1&(poss(~result_1)v poss(fact_1)&(~fact_1 v poss(fact_1)))==>poss(~result_1)

----------------------------------------


----------------------------------------
% Whenever:
%   ( " result_1 " is possibly false or " fact_1 " is possible ) and
%   ( ( " result_1 " is possibly false or " result_1 " ) and
%   ( ( fact_1 is false or " result_1 " ) is necessarily true ) )
% It's Proof that:
%   " fact_1 " is possible

poss(~result_1)v poss(fact_1)&(poss(~result_1)v result_1&nesc(~fact_1 v result_1))==>poss(fact_1)

----------------------------------------


----------------------------------------
% Whenever:
%   ( " result_1 " is possibly false or " fact_1 " is possible ) and
%   ( ( " result_1 " is possibly false or " result_1 " ) and
%   ( ( fact_1 is false or " result_1 " ) is necessarily true ) )
% It's Proof that:
%   " fact_1 " is possibly false

poss(~result_1)v poss(fact_1)&(poss(~result_1)v result_1&nesc(~fact_1 v result_1))==>poss(~fact_1)

----------------------------------------


----------------------------------------
% Whenever:
%   ( fact_1 is false or " fact_1 " is possible ) and
%   ( ( " result_1 " is possibly false or " result_1 " ) and
%   ( ( fact_1 is false or " result_1 " ) is necessarily true ) )
% It's Proof that:
%   " result_1 " is possible

~fact_1 v poss(fact_1)&(poss(~result_1)v result_1&nesc(~fact_1 v result_1))==>poss(result_1)

----------------------------------------


----------------------------------------
% Whenever:
%   ( fact_1 is false or " fact_1 " is possible ) and
%   ( ( " result_1 " is possibly false or " result_1 " ) and
%   ( ( fact_1 is false or " result_1 " ) is necessarily true ) )
% It's Proof that:
%   " fact_1 " is possibly false

~fact_1 v poss(fact_1)&(poss(~result_1)v result_1&nesc(~fact_1 v result_1))==>poss(~fact_1)

----------------------------------------






=======================================================
% If:
%   " fact_1 " and
%   " fact_2 " then it is
% Implied that:
%   " result_1 "

?- kif_to_boxlog( fact_1&fact_2=>result_1 ).

% kifm = fact_1&fact_2=>result_1.
% kif_to_boxlog_attvars2 = =>(and(fact_1,fact_2),result_1)


% Results in the following 3 entailment(s):


----------------------------------------
% Whenever:
%   " fact_1 " is necessarily true and
%   " fact_2 " is necessarily true
% It's Proof that:
%   " result_1 " is necessarily true

nesc(fact_1)&nesc(fact_2)==>nesc(result_1)

----------------------------------------


----------------------------------------
% Whenever:
%   " result_1 " is necessarily false and
%   " fact_1 " is necessarily true
% It's Proof that:
%   " fact_2 " is necessarily false

nesc(~result_1)&nesc(fact_1)==>nesc(~fact_2)

----------------------------------------


----------------------------------------
% Whenever:
%   " result_1 " is necessarily false and
%   " fact_2 " is necessarily true
% It's Proof that:
%   " fact_1 " is necessarily false

nesc(~result_1)&nesc(fact_2)==>nesc(~fact_1)

----------------------------------------






=======================================================
% ( If:
%   " fact_1 " and
%   " fact_2 " then it is
% Implied that:
%   " result_1 " ) is necessarily true

?- kif_to_boxlog( nesc((fact_1&fact_2=>result_1)) ).

% kifm = nesc( fact_1&fact_2=>result_1).
% kif_to_boxlog_attvars2 = necessary(=>(and(fact_1,fact_2),result_1))


% Results in the following 3 entailment(s):


----------------------------------------
% Whenever:
%   " fact_1 " is necessarily true and
%   " fact_2 " is necessarily true
% It's Proof that:
%   " result_1 " is necessarily true

nesc(fact_1)&nesc(fact_2)==>nesc(result_1)

----------------------------------------


----------------------------------------
% Whenever:
%   " result_1 " is possibly false and
%   " fact_1 " is necessarily true
% It's Proof that:
%   " fact_2 " is possibly false

poss(~result_1)&nesc(fact_1)==>poss(~fact_2)

----------------------------------------


----------------------------------------
% Whenever:
%   " result_1 " is possibly false and
%   " fact_2 " is necessarily true
% It's Proof that:
%   " fact_1 " is possibly false

poss(~result_1)&nesc(fact_2)==>poss(~fact_1)

----------------------------------------






=======================================================
% it is false that If:
%   " fact_1 " and
%   " fact_2 " then it is
% Implied that:
%   " result_1 "

?- kif_to_boxlog( ~ (fact_1&fact_2=>result_1) ).

% kifm = ~( fact_1&fact_2=>result_1).
% kif_to_boxlog_attvars2 = not(=>(and(fact_1,fact_2),result_1))


% Results in the following 5 entailment(s):


----------------------------------------
% Whenever:
%   " fact_1 " is necessarily false
% It's Proof that:
%   " fact_2 " is necessarily false

nesc(~fact_1)==>nesc(~fact_2)

----------------------------------------


----------------------------------------
% Whenever:
%   " fact_2 " is necessarily false
% It's Proof that:
%   " fact_1 " is necessarily false

nesc(~fact_2)==>nesc(~fact_1)

----------------------------------------


----------------------------------------
% Whenever:
%   " fact_1 " is possible
% It's Proof that:
%   " fact_2 " is necessarily true

poss(fact_1)==>nesc(fact_2)

----------------------------------------


----------------------------------------
% Whenever:
%   " fact_2 " is possible
% It's Proof that:
%   " fact_1 " is necessarily true

poss(fact_2)==>nesc(fact_1)

----------------------------------------


----------------------------------------
% it is false that " result_1 "

~result_1

----------------------------------------






=======================================================
% it is possible that If:
%   " fact_1 " and
%   " fact_2 " then it is
% Implied that:
%   " result_1 "

?- kif_to_boxlog( poss((fact_1&fact_2=>result_1)) ).

% kifm = poss( fact_1&fact_2=>result_1).
% kif_to_boxlog_attvars2 = possible(=>(and(fact_1,fact_2),result_1))


% Results in the following 3 entailment(s):


----------------------------------------
% Whenever:
%   " fact_1 " is necessarily true and
%   " fact_2 " is necessarily true
% It's Proof that:
%   " result_1 " is possible

nesc(fact_1)&nesc(fact_2)==>poss(result_1)

----------------------------------------


----------------------------------------
% Whenever:
%   " result_1 " is necessarily false and
%   " fact_1 " is necessarily true
% It's Proof that:
%   " fact_2 " is possibly false

nesc(~result_1)&nesc(fact_1)==>poss(~fact_2)

----------------------------------------


----------------------------------------
% Whenever:
%   " result_1 " is necessarily false and
%   " fact_2 " is necessarily true
% It's Proof that:
%   " fact_1 " is possibly false

nesc(~result_1)&nesc(fact_2)==>poss(~fact_1)

----------------------------------------






=======================================================
% it is possible that fact_1 is & fact_2 is => result_1 is false

?- kif_to_boxlog( poss(~ (fact_1&fact_2=>result_1)) ).

% kifm = poss( ~( fact_1&fact_2=>result_1)).
% kif_to_boxlog_attvars2 = possible(not(=>(and(fact_1,fact_2),result_1)))


% Results in the following 4 entailment(s):


----------------------------------------
% Whenever:
%   ( ( " result_1 " or ( fact_1 is false or fact_2 is false ) ) is necessarily true ) and
%   ( ( " result_1 " or ( " fact_2 " is possible or " fact_1 " is possible ) ) and
%   ( " result_1 " or ( fact_1 is false or " fact_1 " is possible ) ) )
% It's Proof that:
%   " result_1 " is possibly false

( (   nesc( (   result_1  v
              ~fact_1 v
              ~fact_2))  &
    (   result_1  v
      poss(fact_2) v
      poss(fact_1)) &
    (   result_1  v
      ~fact_1 v
      poss(fact_1))) ==>
  poss( ~result_1))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " result_1 " or ( fact_1 is false or fact_2 is false ) ) is necessarily true ) and
%   ( ( " result_1 " or ( " fact_2 " is possible or " fact_1 " is possible ) ) and
%   ( " result_1 " or ( fact_1 is false or " fact_1 " is possible ) ) )
% It's Proof that:
%   " fact_2 " is possibly false

( (   nesc( (   result_1  v
              ~fact_1 v
              ~fact_2))  &
    (   result_1  v
      poss(fact_2) v
      poss(fact_1)) &
    (   result_1  v
      ~fact_1 v
      poss(fact_1))) ==>
  poss( ~fact_2))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " result_1 " or ( fact_1 is false or fact_2 is false ) ) is necessarily true ) and
%   ( ( " result_1 " or ( " fact_2 " is possible or " fact_1 " is possible ) ) and
%   ( " result_1 " or ( fact_1 is false or " fact_1 " is possible ) ) )
% It's Proof that:
%   " fact_2 " is possible

( (   nesc( (   result_1  v
              ~fact_1 v
              ~fact_2))  &
    (   result_1  v
      poss(fact_2) v
      poss(fact_1)) &
    (   result_1  v
      ~fact_1 v
      poss(fact_1))) ==>
  poss(fact_2))

----------------------------------------


----------------------------------------
% Whenever:
%   ( " result_1 " or ( " fact_2 " is possible or " fact_1 " is possible ) ) and
%   ( ( " result_1 " or ( " fact_2 " is possible or fact_2 is false ) ) and
%   ( ( " result_1 " or ( fact_1 is false or fact_2 is false ) ) is necessarily true ) )
% It's Proof that:
%   " result_1 " is possibly false

( (   (   result_1  v
        poss(fact_2) v
        poss(fact_1))  &
    (   result_1  v
      poss(fact_2) v
      ~fact_2) &
    nesc( (   result_1  v
            ~fact_1 v
            ~fact_2))) ==>
  poss( ~result_1))

----------------------------------------


----------------------------------------
% Whenever:
%   ( " result_1 " or ( " fact_2 " is possible or " fact_1 " is possible ) ) and
%   ( ( " result_1 " or ( " fact_2 " is possible or fact_2 is false ) ) and
%   ( ( " result_1 " or ( fact_1 is false or fact_2 is false ) ) is necessarily true ) )
% It's Proof that:
%   " fact_1 " is possible

( (   (   result_1  v
        poss(fact_2) v
        poss(fact_1))  &
    (   result_1  v
      poss(fact_2) v
      ~fact_2) &
    nesc( (   result_1  v
            ~fact_1 v
            ~fact_2))) ==>
  poss(fact_1))

----------------------------------------


----------------------------------------
% Whenever:
%   ( " result_1 " or ( " fact_2 " is possible or " fact_1 " is possible ) ) and
%   ( ( " result_1 " or ( " fact_2 " is possible or fact_2 is false ) ) and
%   ( ( " result_1 " or ( fact_1 is false or fact_2 is false ) ) is necessarily true ) )
% It's Proof that:
%   " fact_1 " is possibly false

( (   (   result_1  v
        poss(fact_2) v
        poss(fact_1))  &
    (   result_1  v
      poss(fact_2) v
      ~fact_2) &
    nesc( (   result_1  v
            ~fact_1 v
            ~fact_2))) ==>
  poss( ~fact_1))

----------------------------------------


----------------------------------------
% Whenever:
%   ( " result_1 " or ( " fact_2 " is possible or fact_2 is false ) ) and
%   ( ( " result_1 " or ( " fact_2 " is possible or " fact_1 " is possible ) ) and
%   ( " result_1 " or ( fact_1 is false or " fact_1 " is possible ) ) )
% It's Proof that:
%   " result_1 " is possibly false

( (   (   result_1  v
        poss(fact_2) v
        ~fact_2)  &
    (   result_1  v
      poss(fact_2) v
      poss(fact_1)) &
    (   result_1  v
      ~fact_1 v
      poss(fact_1))) ==>
  poss( ~result_1))

----------------------------------------


----------------------------------------
% Whenever:
%   ( " result_1 " or ( " fact_2 " is possible or fact_2 is false ) ) and
%   ( ( " result_1 " or ( " fact_2 " is possible or " fact_1 " is possible ) ) and
%   ( " result_1 " or ( fact_1 is false or " fact_1 " is possible ) ) )
% It's Proof that:
%   " fact_1 " is possible

( (   (   result_1  v
        poss(fact_2) v
        ~fact_2)  &
    (   result_1  v
      poss(fact_2) v
      poss(fact_1)) &
    (   result_1  v
      ~fact_1 v
      poss(fact_1))) ==>
  poss(fact_1))

----------------------------------------


----------------------------------------
% Whenever:
%   ( " result_1 " or ( " fact_2 " is possible or fact_2 is false ) ) and
%   ( ( " result_1 " or ( " fact_2 " is possible or " fact_1 " is possible ) ) and
%   ( " result_1 " or ( fact_1 is false or " fact_1 " is possible ) ) )
% It's Proof that:
%   " fact_2 " is possible

( (   (   result_1  v
        poss(fact_2) v
        ~fact_2)  &
    (   result_1  v
      poss(fact_2) v
      poss(fact_1)) &
    (   result_1  v
      ~fact_1 v
      poss(fact_1))) ==>
  poss(fact_2))

----------------------------------------


----------------------------------------
% Whenever:
%   ( " result_1 " or ( fact_1 is false or " fact_1 " is possible ) ) and
%   ( ( " result_1 " or ( " fact_2 " is possible or fact_2 is false ) ) and
%   ( ( " result_1 " or ( fact_1 is false or fact_2 is false ) ) is necessarily true ) )
% It's Proof that:
%   " result_1 " is possibly false

( (   (   result_1  v
        ~fact_1 v
        poss(fact_1))  &
    (   result_1  v
      poss(fact_2) v
      ~fact_2) &
    nesc( (   result_1  v
            ~fact_1 v
            ~fact_2))) ==>
  poss( ~result_1))

----------------------------------------


----------------------------------------
% Whenever:
%   ( " result_1 " or ( fact_1 is false or " fact_1 " is possible ) ) and
%   ( ( " result_1 " or ( " fact_2 " is possible or fact_2 is false ) ) and
%   ( ( " result_1 " or ( fact_1 is false or fact_2 is false ) ) is necessarily true ) )
% It's Proof that:
%   " fact_2 " is possibly false

( (   (   result_1  v
        ~fact_1 v
        poss(fact_1))  &
    (   result_1  v
      poss(fact_2) v
      ~fact_2) &
    nesc( (   result_1  v
            ~fact_1 v
            ~fact_2))) ==>
  poss( ~fact_2))

----------------------------------------


----------------------------------------
% Whenever:
%   ( " result_1 " or ( fact_1 is false or " fact_1 " is possible ) ) and
%   ( ( " result_1 " or ( " fact_2 " is possible or fact_2 is false ) ) and
%   ( ( " result_1 " or ( fact_1 is false or fact_2 is false ) ) is necessarily true ) )
% It's Proof that:
%   " fact_1 " is possibly false

( (   (   result_1  v
        ~fact_1 v
        poss(fact_1))  &
    (   result_1  v
      poss(fact_2) v
      ~fact_2) &
    nesc( (   result_1  v
            ~fact_1 v
            ~fact_2))) ==>
  poss( ~fact_1))

----------------------------------------






=======================================================
% If:
%   " fact_1 " or " fact_2 " then it is
% Implied that:
%   " result_1 "

?- kif_to_boxlog( fact_1 v fact_2=>result_1 ).

% kifm = fact_1 v fact_2=>result_1.
% kif_to_boxlog_attvars2 = =>(or(fact_1,fact_2),result_1)


% Results in the following 6 entailment(s):


----------------------------------------
% Whenever:
%   " result_1 " is necessarily false and
%   " fact_1 " is necessarily true
% It's Proof that:
%   " fact_2 " is necessarily true

nesc(~result_1)&nesc(fact_1)==>nesc(fact_2)

----------------------------------------


----------------------------------------
% Whenever:
%   " result_1 " is necessarily false and
%   " fact_2 " is necessarily true
% It's Proof that:
%   " fact_1 " is necessarily true

nesc(~result_1)&nesc(fact_2)==>nesc(fact_1)

----------------------------------------


----------------------------------------
% Whenever:
%   " result_1 " is necessarily false and
%   " fact_1 " is possibly false
% It's Proof that:
%   " fact_2 " is necessarily false

nesc(~result_1)&poss(~fact_1)==>nesc(~fact_2)

----------------------------------------


----------------------------------------
% Whenever:
%   " result_1 " is necessarily false and
%   " fact_2 " is possibly false
% It's Proof that:
%   " fact_1 " is necessarily false

nesc(~result_1)&poss(~fact_2)==>nesc(~fact_1)

----------------------------------------


----------------------------------------
% Whenever:
%   " fact_1 " is possibly false and
%   " fact_2 " is necessarily true
% It's Proof that:
%   " result_1 " is necessarily true

poss(~fact_1)&nesc(fact_2)==>nesc(result_1)

----------------------------------------


----------------------------------------
% Whenever:
%   " fact_2 " is possibly false and
%   " fact_1 " is necessarily true
% It's Proof that:
%   " result_1 " is necessarily true

poss(~fact_2)&nesc(fact_1)==>nesc(result_1)

----------------------------------------






=======================================================
% ( If:
%   " fact_1 " or " fact_2 " then it is
% Implied that:
%   " result_1 " ) is necessarily true

?- kif_to_boxlog( nesc((fact_1 v fact_2=>result_1)) ).

% kifm = nesc( fact_1 v fact_2=>result_1).
% kif_to_boxlog_attvars2 = necessary(=>(or(fact_1,fact_2),result_1))


% Results in the following 6 entailment(s):


----------------------------------------
% Whenever:
%   " fact_1 " is possibly false and
%   " fact_2 " is necessarily true
% It's Proof that:
%   " result_1 " is necessarily true

poss(~fact_1)&nesc(fact_2)==>nesc(result_1)

----------------------------------------


----------------------------------------
% Whenever:
%   " fact_2 " is possibly false and
%   " fact_1 " is necessarily true
% It's Proof that:
%   " result_1 " is necessarily true

poss(~fact_2)&nesc(fact_1)==>nesc(result_1)

----------------------------------------


----------------------------------------
% Whenever:
%   " result_1 " is possibly false and
%   " fact_1 " is necessarily true
% It's Proof that:
%   " fact_2 " is necessarily true

poss(~result_1)&nesc(fact_1)==>nesc(fact_2)

----------------------------------------


----------------------------------------
% Whenever:
%   " result_1 " is possibly false and
%   " fact_2 " is necessarily true
% It's Proof that:
%   " fact_1 " is necessarily true

poss(~result_1)&nesc(fact_2)==>nesc(fact_1)

----------------------------------------


----------------------------------------
% Whenever:
%   " result_1 " is possibly false and
%   " fact_1 " is possibly false
% It's Proof that:
%   " fact_2 " is possibly false

poss(~result_1)&poss(~fact_1)==>poss(~fact_2)

----------------------------------------


----------------------------------------
% Whenever:
%   " result_1 " is possibly false and
%   " fact_2 " is possibly false
% It's Proof that:
%   " fact_1 " is possibly false

poss(~result_1)&poss(~fact_2)==>poss(~fact_1)

----------------------------------------






=======================================================
% it is false that If:
%   " fact_1 " or " fact_2 " then it is
% Implied that:
%   " result_1 "

?- kif_to_boxlog( ~ (fact_1 v fact_2=>result_1) ).

% kifm = ~( fact_1 v fact_2=>result_1).
% kif_to_boxlog_attvars2 = not(=>(or(fact_1,fact_2),result_1))


% Results in the following 3 entailment(s):


----------------------------------------
% Whenever:
%   " fact_1 " is necessarily false
% It's Proof that:
%   " fact_2 " is necessarily true

nesc(~fact_1)==>nesc(fact_2)

----------------------------------------


----------------------------------------
% Whenever:
%   " fact_2 " is necessarily false
% It's Proof that:
%   " fact_1 " is necessarily true

nesc(~fact_2)==>nesc(fact_1)

----------------------------------------


----------------------------------------
% it is false that " result_1 "

~result_1

----------------------------------------






=======================================================
% it is possible that If:
%   " fact_1 " or " fact_2 " then it is
% Implied that:
%   " result_1 "

?- kif_to_boxlog( poss((fact_1 v fact_2=>result_1)) ).

% kifm = poss( fact_1 v fact_2=>result_1).
% kif_to_boxlog_attvars2 = possible(=>(or(fact_1,fact_2),result_1))


% Results in the following 5 entailment(s):


----------------------------------------
% Whenever:
%   " result_1 " is necessarily false and
%   ( ( ( " fact_1 " or " fact_2 " ) is necessarily true ) and
%   ( ( " fact_2 " is possibly false or " fact_1 " is possibly false ) and
%   ( " fact_1 " or " fact_1 " is possibly false ) ) )
% It's Proof that:
%   " fact_2 " is possible

( (   nesc( ~result_1)  &
    nesc( fact_1 v fact_2) &
    poss(~fact_2)v poss(~fact_1) &
    fact_1 v poss(~fact_1)) ==>
  poss(fact_2))

----------------------------------------


----------------------------------------
% Whenever:
%   " result_1 " is necessarily false and
%   ( ( ( " fact_1 " or " fact_2 " ) is necessarily true ) and
%   ( ( " fact_2 " is possibly false or " fact_1 " is possibly false ) and
%   ( " fact_1 " or " fact_1 " is possibly false ) ) )
% It's Proof that:
%   " fact_2 " is possibly false

( (   nesc( ~result_1)  &
    nesc( fact_1 v fact_2) &
    poss(~fact_2)v poss(~fact_1) &
    fact_1 v poss(~fact_1)) ==>
  poss( ~fact_2))

----------------------------------------


----------------------------------------
% Whenever:
%   " result_1 " is necessarily false and
%   ( ( " fact_1 " or " fact_1 " is possibly false ) and
%   ( ( " fact_2 " is possibly false or " fact_2 " ) and
%   ( ( " fact_1 " or " fact_2 " ) is necessarily true ) ) )
% It's Proof that:
%   " fact_2 " is possible

nesc(~result_1)&(fact_1 v poss(~fact_1)&(poss(~fact_2)v fact_2&nesc(fact_1 v fact_2)))==>poss(fact_2)

----------------------------------------


----------------------------------------
% Whenever:
%   " result_1 " is necessarily false and
%   ( ( " fact_1 " or " fact_1 " is possibly false ) and
%   ( ( " fact_2 " is possibly false or " fact_2 " ) and
%   ( ( " fact_1 " or " fact_2 " ) is necessarily true ) ) )
% It's Proof that:
%   " fact_1 " is possible

nesc(~result_1)&(fact_1 v poss(~fact_1)&(poss(~fact_2)v fact_2&nesc(fact_1 v fact_2)))==>poss(fact_1)

----------------------------------------


----------------------------------------
% Whenever:
%   " result_1 " is necessarily false and
%   ( ( " fact_2 " is possibly false or " fact_2 " ) and
%   ( ( " fact_2 " is possibly false or " fact_1 " is possibly false ) and
%   ( " fact_1 " or " fact_1 " is possibly false ) ) )
% It's Proof that:
%   " fact_1 " is possibly false

( (   nesc( ~result_1)  &
    poss(~fact_2)v fact_2 &
    poss(~fact_2)v poss(~fact_1) &
    fact_1 v poss(~fact_1)) ==>
  poss( ~fact_1))

----------------------------------------


----------------------------------------
% Whenever:
%   " result_1 " is necessarily false and
%   ( ( " fact_2 " is possibly false or " fact_2 " ) and
%   ( ( " fact_2 " is possibly false or " fact_1 " is possibly false ) and
%   ( " fact_1 " or " fact_1 " is possibly false ) ) )
% It's Proof that:
%   " fact_2 " is possibly false

( (   nesc( ~result_1)  &
    poss(~fact_2)v fact_2 &
    poss(~fact_2)v poss(~fact_1) &
    fact_1 v poss(~fact_1)) ==>
  poss( ~fact_2))

----------------------------------------


----------------------------------------
% Whenever:
%   " result_1 " is necessarily false and
%   ( ( " fact_2 " is possibly false or " fact_1 " is possibly false ) and
%   ( ( " fact_2 " is possibly false or " fact_2 " ) and
%   ( ( " fact_1 " or " fact_2 " ) is necessarily true ) ) )
% It's Proof that:
%   " fact_1 " is possibly false

( (   nesc( ~result_1)  &
    poss(~fact_2)v poss(~fact_1) &
    poss(~fact_2)v fact_2 &
    nesc( fact_1 v fact_2)) ==>
  poss( ~fact_1))

----------------------------------------


----------------------------------------
% Whenever:
%   " result_1 " is necessarily false and
%   ( ( " fact_2 " is possibly false or " fact_1 " is possibly false ) and
%   ( ( " fact_2 " is possibly false or " fact_2 " ) and
%   ( ( " fact_1 " or " fact_2 " ) is necessarily true ) ) )
% It's Proof that:
%   " fact_1 " is possible

( (   nesc( ~result_1)  &
    poss(~fact_2)v poss(~fact_1) &
    poss(~fact_2)v fact_2 &
    nesc( fact_1 v fact_2)) ==>
  poss(fact_1))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " fact_2 " is possibly false or " fact_1 " is possibly false ) and
%   ( " fact_1 " or " fact_1 " is possibly false ) ) and
%   ( ( " fact_2 " is possibly false or " fact_2 " ) and
%   ( ( " fact_1 " or " fact_2 " ) is necessarily true ) )
% It's Proof that:
%   " result_1 " is possible

( (   poss(~fact_2)v poss(~fact_1)  &
    fact_1 v poss(~fact_1) &
    poss(~fact_2)v fact_2 &
    nesc( fact_1 v fact_2)) ==>
  poss(result_1))

----------------------------------------






=======================================================
% it is possible that fact_1 is v fact_2 is => result_1 is false

?- kif_to_boxlog( poss(~ (fact_1 v fact_2=>result_1)) ).

% kifm = poss( ~( fact_1 v fact_2=>result_1)).
% kif_to_boxlog_attvars2 = possible(not(=>(or(fact_1,fact_2),result_1)))


% Results in the following 2 entailment(s):


----------------------------------------
% Whenever:
%   " result_1 " is necessarily true
% It's Proof that:
%   " result_1 " is possibly false

nesc(result_1)==>poss(~result_1)

----------------------------------------


----------------------------------------
% Whenever:
%   " fact_1 " is necessarily false
% It's Proof that:
%   " result_1 " is possibly false

nesc(~fact_1)==>poss(~result_1)

----------------------------------------


----------------------------------------
% Whenever:
%   " result_1 " is necessarily true
% It's Proof that:
%   " fact_2 " is possible

nesc(result_1)==>poss(fact_2)

----------------------------------------


----------------------------------------
% Whenever:
%   " fact_1 " is necessarily false
% It's Proof that:
%   " fact_2 " is possible

nesc(~fact_1)==>poss(fact_2)

----------------------------------------


----------------------------------------
% Whenever:
%   " result_1 " is necessarily true
% It's Proof that:
%   " result_1 " is possibly false

nesc(result_1)==>poss(~result_1)

----------------------------------------


----------------------------------------
% Whenever:
%   " fact_2 " is necessarily false
% It's Proof that:
%   " result_1 " is possibly false

nesc(~fact_2)==>poss(~result_1)

----------------------------------------


----------------------------------------
% Whenever:
%   " result_1 " is necessarily true
% It's Proof that:
%   " fact_1 " is possible

nesc(result_1)==>poss(fact_1)

----------------------------------------


----------------------------------------
% Whenever:
%   " fact_2 " is necessarily false
% It's Proof that:
%   " fact_1 " is possible

nesc(~fact_2)==>poss(fact_1)

----------------------------------------

true.

105 ?-

