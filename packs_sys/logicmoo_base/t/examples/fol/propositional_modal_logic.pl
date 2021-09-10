#!/usr/bin/env lmoo-junit
:- if(set_prolog_flag(runtime_testing_module,user)).
:- if(set_prolog_flag(test_module,user)).
:- include(test_header).
:- endif.
:- endif.

f_n_r(F,N,R):- number(N), atomic_list_concat([F,'_',N],R),!.
f_n_r(F,N,R):- R=..[F,N],!.

xamplen(F,N,R):- N=<1,!,f_n_r(F,N,R).
xamplen(F,N,O+R):- Nm1 is  N -1,f_n_r(F,N,R),xamplen(F,Nm1,O),!.

xample(F,O,L,U):- between(L,U,N),xamplen(F,N,R),(N=1->O=R;(subst(R,+,&,O);subst(R,+,v,O))).

do_xampl(P1,F):- 
   local_pretty_numbervars_ground(F,E),
   call(P1,(E)),      
   call(P1,poss(E)),
   call(P1, ~E),
   call(P1,nesc(E)),
   %call(P1,poss(~E)),
   !.

show_xample(P1):- update_changed_files,!, forall(xampl(X), do_xampl(P1,X)),ah.

xampl((leftof('$VAR'('H1'), '$VAR'('H2')) => house('$VAR'('H1')) & house('$VAR'('H2')))).
xampl(F):- xample(fact,F,1,1).
xampl(F => R):- xample(result,R,1,1),xample(fact,F,1,2).
xampl((all('$VAR'('P1'), exists('$VAR'('H2'), person('$VAR'('P1')) => has('$VAR'('P1'),'$VAR'('H2')))))).
xampl(all(P1,exists([ [H2,heart]],(person(P1)=>has(P1,H2))))).
xampl((~poss(a) v nesc(a))).

%xampl(F):- xample(fact,F,2,3).
%xampl(F => R):- xample(result,R,2,2),xample(fact,F,1,2).
%xampl(F & R):- xample(left,F),xample(right,R).
%xampl(F v R):- xample(left,F),xample(right,R).


show_xamples:- show_xample(show_boxlog).

kif_to_boxlogd:- cls,update_changed_files,show_xample(kif_to_boxlog).

ah:- add_history(update_changed_files),add_history(show_xamples),add_history(kif_to_boxlogd).

:-   show_xamples.

end_of_file.

% the output of this is...


=======================================================
% If:
%   " ?H1 is leftof ?H2 " then it is
% Implied that:
%   " ?H1 is a house " and
%   " ?H2 is a house "

?- kif_to_boxlog( leftof(H1,H2)=>house(H1)&house(H2) ).

% kifm = leftof(H1,H2)=>house(H1)&house(H2).
% kif_to_boxlog_attvars2 = =>(leftof('$VAR'('H1'),'$VAR'('H2')),and(house('$VAR'('H1')),house('$VAR'('H2'))))


% Results in the following 6 entailment(s):


----------------------------------------
% Whenever:
%   " ?H1 is leftof ?H2 " is necessarily true and
%   " ?H1 is a house " is necessarily false
% It's Proof that:
%   " ?H2 is a house " is necessarily false

( nesc(leftof(H1,H2))&nesc(~house(H1)) ==>
  nesc( ~( house(H2))))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?H1 is leftof ?H2 " is necessarily true and
%   " ?H2 is a house " is necessarily false
% It's Proof that:
%   " ?H1 is a house " is necessarily false

( nesc(leftof(H1,H2))&nesc(~house(H2)) ==>
  nesc( ~( house(H1))))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?H1 is leftof ?H2 " is necessarily true and
%   " ?H1 is a house " is possible
% It's Proof that:
%   " ?H2 is a house " is necessarily true

nesc(leftof(H1,H2))&poss(house(H1))==>nesc(house(H2))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?H1 is leftof ?H2 " is necessarily true and
%   " ?H2 is a house " is possible
% It's Proof that:
%   " ?H1 is a house " is necessarily true

nesc(leftof(H1,H2))&poss(house(H2))==>nesc(house(H1))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?H1 is a house " is possible and
%   " ?H2 is a house " is necessarily false
% It's Proof that:
%   " ?H1 is leftof ?H2 " is necessarily false

( poss(house(H1))&nesc(~house(H2)) ==>
  nesc( ~( leftof(H1,H2))))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?H2 is a house " is possible and
%   " ?H1 is a house " is necessarily false
% It's Proof that:
%   " ?H1 is leftof ?H2 " is necessarily false

( poss(house(H2))&nesc(~house(H1)) ==>
  nesc( ~( leftof(H1,H2))))

----------------------------------------






=======================================================
% ( If:
%   " ?H1 is leftof ?H2 " then it is
% Implied that:
%   " ?H1 is a house " and
%   " ?H2 is a house " ) is necessarily true

?- kif_to_boxlog( nesc((leftof(H1,H2)=>house(H1)&house(H2))) ).

% kifm = nesc( leftof(H1,H2)=>house(H1)&house(H2)).
% kif_to_boxlog_attvars2 = necessary(=>(leftof('$VAR'('H1'),'$VAR'('H2')),and(house('$VAR'('H1')),house('$VAR'('H2')))))


% Results in the following 6 entailment(s):


----------------------------------------
% Whenever:
%   " ?H1 is a house " is necessarily true and
%   " ?H2 is a house " is possibly false
% It's Proof that:
%   " ?H1 is leftof ?H2 " is possibly false

( nesc(house(H1))&poss(~house(H2)) ==>
  poss( ~( leftof(H1,H2))))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?H2 is a house " is necessarily true and
%   " ?H1 is a house " is possibly false
% It's Proof that:
%   " ?H1 is leftof ?H2 " is possibly false

( nesc(house(H2))&poss(~house(H1)) ==>
  poss( ~( leftof(H1,H2))))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?H1 is leftof ?H2 " is necessarily true and
%   " ?H1 is a house " is necessarily true
% It's Proof that:
%   " ?H2 is a house " is necessarily true

nesc(leftof(H1,H2))&nesc(house(H1))==>nesc(house(H2))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?H1 is leftof ?H2 " is necessarily true and
%   " ?H2 is a house " is necessarily true
% It's Proof that:
%   " ?H1 is a house " is necessarily true

nesc(leftof(H1,H2))&nesc(house(H2))==>nesc(house(H1))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?H1 is leftof ?H2 " is necessarily true and
%   " ?H1 is a house " is possibly false
% It's Proof that:
%   " ?H2 is a house " is possibly false

( nesc(leftof(H1,H2))&poss(~house(H1)) ==>
  poss( ~( house(H2))))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?H1 is leftof ?H2 " is necessarily true and
%   " ?H2 is a house " is possibly false
% It's Proof that:
%   " ?H1 is a house " is possibly false

( nesc(leftof(H1,H2))&poss(~house(H2)) ==>
  poss( ~( house(H1))))

----------------------------------------






=======================================================
% it is false that If:
%   " ?H1 is leftof ?H2 " then it is
% Implied that:
%   " ?H1 is a house " and
%   " ?H2 is a house "

?- kif_to_boxlog( ~ (leftof(H1,H2)=>house(H1)&house(H2)) ).

% kifm = ~( leftof(H1,H2)=>house(H1)&house(H2)).
% kif_to_boxlog_attvars2 = not(=>(leftof('$VAR'('H1'),'$VAR'('H2')),and(house('$VAR'('H1')),house('$VAR'('H2')))))


% Results in the following 7 entailment(s):


----------------------------------------
% Whenever:
%   " ?H1 is leftof ?H2 " is necessarily false
% It's Proof that:
%   " ?H1 is a house " is necessarily false or " ?H2 is a house "

nesc(~leftof(H1,H2))==>nesc(~house(H1))v house(H2)

----------------------------------------


----------------------------------------
% Whenever:
%   " ?H1 is leftof ?H2 " is necessarily false
% It's Proof that:
%   " ?H2 is a house " is necessarily false or " ?H1 is a house "

nesc(~leftof(H1,H2))==>nesc(~house(H2))v house(H1)

----------------------------------------


----------------------------------------
% Whenever:
%   ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible
% It's Proof that:
%   " ?H1 is leftof ?H2 " is necessarily true

poss(poss(house(H1))& ~house(H2))==>nesc(leftof(H1,H2))

----------------------------------------


----------------------------------------
% Whenever:
%   ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible
% It's Proof that:
%   " ?H1 is leftof ?H2 " is necessarily true

poss(poss(house(H2))& ~house(H1))==>nesc(leftof(H1,H2))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?H1 is a house " is necessarily true and
%   " ?H2 is a house " is necessarily true
% It's Proof that:
%   " ?H1 is leftof ?H2 " is necessarily false

nesc(house(H1))&nesc(house(H2))==>nesc(~leftof(H1,H2))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?H1 is leftof ?H2 " is possible and
%   " ?H1 is a house " is necessarily true
% It's Proof that:
%   " ?H2 is a house " is necessarily false

poss(leftof(H1,H2))&nesc(house(H1))==>nesc(~house(H2))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?H1 is leftof ?H2 " is possible and
%   " ?H2 is a house " is necessarily true
% It's Proof that:
%   " ?H1 is a house " is necessarily false

poss(leftof(H1,H2))&nesc(house(H2))==>nesc(~house(H1))

----------------------------------------






=======================================================
% it is possible that If:
%   " ?H1 is leftof ?H2 " then it is
% Implied that:
%   " ?H1 is a house " and
%   " ?H2 is a house "

?- kif_to_boxlog( poss((leftof(H1,H2)=>house(H1)&house(H2))) ).

% kifm = poss( leftof(H1,H2)=>house(H1)&house(H2)).
% kif_to_boxlog_attvars2 = possible(=>(leftof('$VAR'('H1'),'$VAR'('H2')),and(house('$VAR'('H1')),house('$VAR'('H2')))))


% Results in the following 5 entailment(s):


----------------------------------------
% Whenever:
%   " ?H1 is leftof ?H2 " is necessarily true and
%   ( ( ( " ?H2 is a house " is possible or " ?H1 is a house " is possible ) and
%   ( ?H1 is a house is false or " ?H1 is a house " is possible ) ) and
%   " ?H1 is a house is , ?H2 is a house " is necessarily false )
% It's Proof that:
%   " ?H2 is a house " is possibly false

( (   nesc( leftof(H1,H2))  &
    poss(house(H2))v poss(house(H1)) &
    ~house(H1)v poss(house(H1)) &
    nesc( ~( house(H1),house(H2)))) ==>
  poss( ~( house(H2))))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?H1 is leftof ?H2 " is necessarily true and
%   ( ( ( " ?H2 is a house " is possible or " ?H1 is a house " is possible ) and
%   ( ?H1 is a house is false or " ?H1 is a house " is possible ) ) and
%   " ?H1 is a house is , ?H2 is a house " is necessarily false )
% It's Proof that:
%   " ?H2 is a house " is possible

( (   nesc( leftof(H1,H2))  &
    poss(house(H2))v poss(house(H1)) &
    ~house(H1)v poss(house(H1)) &
    nesc( ~( house(H1),house(H2)))) ==>
  poss( house(H2)))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?H1 is leftof ?H2 " is necessarily true and
%   ( ( " ?H2 is a house " is possible or " ?H1 is a house " is possible ) and
%   ( ( " ?H2 is a house " is possible or ?H2 is a house is false ) and
%   " ?H1 is a house is , ?H2 is a house " is necessarily false ) )
% It's Proof that:
%   " ?H1 is a house " is possible

( (   nesc( leftof(H1,H2))  &
    poss(house(H2))v poss(house(H1)) &
    poss(house(H2))v~house(H2) &
    nesc( ~( house(H1),house(H2)))) ==>
  poss( house(H1)))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?H1 is leftof ?H2 " is necessarily true and
%   ( ( " ?H2 is a house " is possible or " ?H1 is a house " is possible ) and
%   ( ( " ?H2 is a house " is possible or ?H2 is a house is false ) and
%   " ?H1 is a house is , ?H2 is a house " is necessarily false ) )
% It's Proof that:
%   " ?H1 is a house " is possibly false

( (   nesc( leftof(H1,H2))  &
    poss(house(H2))v poss(house(H1)) &
    poss(house(H2))v~house(H2) &
    nesc( ~( house(H1),house(H2)))) ==>
  poss( ~( house(H1))))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?H1 is leftof ?H2 " is necessarily true and
%   ( ( ?H1 is a house is false or " ?H1 is a house " is possible ) and
%   ( ( " ?H2 is a house " is possible or ?H2 is a house is false ) and
%   " ?H1 is a house is , ?H2 is a house " is necessarily false ) )
% It's Proof that:
%   " ?H2 is a house " is possibly false

( (   nesc( leftof(H1,H2))  &
    ~house(H1)v poss(house(H1)) &
    poss(house(H2))v~house(H2) &
    nesc( ~( house(H1),house(H2)))) ==>
  poss( ~( house(H2))))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?H1 is leftof ?H2 " is necessarily true and
%   ( ( ?H1 is a house is false or " ?H1 is a house " is possible ) and
%   ( ( " ?H2 is a house " is possible or ?H2 is a house is false ) and
%   " ?H1 is a house is , ?H2 is a house " is necessarily false ) )
% It's Proof that:
%   " ?H1 is a house " is possibly false

( (   nesc( leftof(H1,H2))  &
    ~house(H1)v poss(house(H1)) &
    poss(house(H2))v~house(H2) &
    nesc( ~( house(H1),house(H2)))) ==>
  poss( ~( house(H1))))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " ?H2 is a house " is possible or " ?H1 is a house " is possible ) and
%   ( ?H1 is a house is false or " ?H1 is a house " is possible ) ) and
%   ( ( " ?H2 is a house " is possible or ?H2 is a house is false ) and
%   " ?H1 is a house is , ?H2 is a house " is necessarily false )
% It's Proof that:
%   " ?H1 is leftof ?H2 " is possibly false

( (   poss(house(H2))v poss(house(H1))  &
    ~house(H1)v poss(house(H1)) &
    poss(house(H2))v~house(H2) &
    nesc( ~( house(H1),house(H2)))) ==>
  poss( ~( leftof(H1,H2))))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?H1 is leftof ?H2 " is necessarily true and
%   ( ( ( " ?H2 is a house " is possible or " ?H1 is a house " is possible ) and
%   ( ?H1 is a house is false or " ?H1 is a house " is possible ) ) and
%   ( " ?H2 is a house " is possible or ?H2 is a house is false ) )
% It's Proof that:
%   " ?H1 is a house " is possible

( (   nesc( leftof(H1,H2))  &
    poss(house(H2))v poss(house(H1)) &
    ~house(H1)v poss(house(H1)) &
    poss(house(H2))v~house(H2)) ==>
  poss( house(H1)))

----------------------------------------


----------------------------------------
% Whenever:
%   " ?H1 is leftof ?H2 " is necessarily true and
%   ( ( ( " ?H2 is a house " is possible or " ?H1 is a house " is possible ) and
%   ( ?H1 is a house is false or " ?H1 is a house " is possible ) ) and
%   ( " ?H2 is a house " is possible or ?H2 is a house is false ) )
% It's Proof that:
%   " ?H2 is a house " is possible

( (   nesc( leftof(H1,H2))  &
    poss(house(H2))v poss(house(H1)) &
    ~house(H1)v poss(house(H1)) &
    poss(house(H2))v~house(H2)) ==>
  poss( house(H2)))

----------------------------------------






=======================================================
% it is possible that ?H1 is leftof ?H2 is => ?H1 is a house is & ?H2 is a house is false

?- kif_to_boxlog( poss(~ (leftof(H1,H2)=>house(H1)&house(H2))) ).

% kifm = poss( ~( leftof(H1,H2)=>house(H1)&house(H2))).
% kif_to_boxlog_attvars2 = possible(not(=>(leftof('$VAR'('H1'),'$VAR'('H2')),and(house('$VAR'('H1')),house('$VAR'('H2'))))))


% Results in the following 6 entailment(s):


----------------------------------------
% Whenever:
%   ( ( ( " ?H1 is a house " or ?H1 is leftof ?H2 is false ) is necessarily true ) and
%   ( ( " ?H2 is a house " or ?H1 is leftof ?H2 is false ) is necessarily true ) ) and
%   ( ( " ?H1 is leftof ?H2 " is possible or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) and
%   ( ( " ?H1 is a house " or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) and
%   ( " ?H2 is a house " or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) ) )
% It's Proof that:
%   " ?H1 is leftof ?H2 " is possibly false

( (   nesc( house(H1)v~leftof(H1,H2))  &
    nesc( house(H2)v~leftof(H1,H2)) &
    (   poss( leftof(H1,H2))  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2))) &
    (   house(H1)  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2))) &
    (   house(H2)  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2)))) ==>
  poss( ~( leftof(H1,H2))))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( ( " ?H1 is a house " or ?H1 is leftof ?H2 is false ) is necessarily true ) and
%   ( ( " ?H2 is a house " or ?H1 is leftof ?H2 is false ) is necessarily true ) ) and
%   ( ( " ?H1 is leftof ?H2 " is possible or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) and
%   ( ( " ?H1 is a house " or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) and
%   ( " ?H2 is a house " or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) ) )
% It's Proof that:
%   " ?H1 is leftof ?H2 " is possible

( (   nesc( house(H1)v~leftof(H1,H2))  &
    nesc( house(H2)v~leftof(H1,H2)) &
    (   poss( leftof(H1,H2))  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2))) &
    (   house(H1)  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2))) &
    (   house(H2)  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2)))) ==>
  poss( leftof(H1,H2)))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " ?H1 is leftof ?H2 " is possible or ?H1 is leftof ?H2 is false ) and
%   ( ( " ?H1 is a house " or ?H1 is leftof ?H2 is false ) is necessarily true ) ) and
%   ( ( " ?H1 is leftof ?H2 " is possible or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) and
%   ( ( " ?H1 is a house " or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) and
%   ( " ?H2 is a house " or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) ) )
% It's Proof that:
%   " ?H2 is a house " is possibly false

( (   poss(leftof(H1,H2))v~leftof(H1,H2)  &
    nesc( house(H1)v~leftof(H1,H2)) &
    (   poss( leftof(H1,H2))  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2))) &
    (   house(H1)  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2))) &
    (   house(H2)  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2)))) ==>
  poss( ~( house(H2))))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " ?H1 is leftof ?H2 " is possible or ?H1 is leftof ?H2 is false ) and
%   ( ( " ?H1 is a house " or ?H1 is leftof ?H2 is false ) is necessarily true ) ) and
%   ( ( " ?H1 is leftof ?H2 " is possible or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) and
%   ( ( " ?H1 is a house " or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) and
%   ( " ?H2 is a house " or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) ) )
% It's Proof that:
%   " ?H1 is leftof ?H2 " is possible

( (   poss(leftof(H1,H2))v~leftof(H1,H2)  &
    nesc( house(H1)v~leftof(H1,H2)) &
    (   poss( leftof(H1,H2))  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2))) &
    (   house(H1)  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2))) &
    (   house(H2)  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2)))) ==>
  poss( leftof(H1,H2)))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " ?H1 is leftof ?H2 " is possible or ?H1 is leftof ?H2 is false ) and
%   ( ( " ?H2 is a house " or ?H1 is leftof ?H2 is false ) is necessarily true ) ) and
%   ( ( " ?H1 is leftof ?H2 " is possible or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) and
%   ( ( " ?H1 is a house " or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) and
%   ( " ?H2 is a house " or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) ) )
% It's Proof that:
%   " ?H1 is a house " is possibly false

( (   poss(leftof(H1,H2))v~leftof(H1,H2)  &
    nesc( house(H2)v~leftof(H1,H2)) &
    (   poss( leftof(H1,H2))  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2))) &
    (   house(H1)  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2))) &
    (   house(H2)  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2)))) ==>
  poss( ~( house(H1))))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " ?H1 is leftof ?H2 " is possible or ?H1 is leftof ?H2 is false ) and
%   ( ( " ?H2 is a house " or ?H1 is leftof ?H2 is false ) is necessarily true ) ) and
%   ( ( " ?H1 is leftof ?H2 " is possible or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) and
%   ( ( " ?H1 is a house " or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) and
%   ( " ?H2 is a house " or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) ) )
% It's Proof that:
%   " ?H1 is leftof ?H2 " is possible

( (   poss(leftof(H1,H2))v~leftof(H1,H2)  &
    nesc( house(H2)v~leftof(H1,H2)) &
    (   poss( leftof(H1,H2))  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2))) &
    (   house(H1)  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2))) &
    (   house(H2)  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2)))) ==>
  poss( leftof(H1,H2)))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " ?H1 is leftof ?H2 " is possible or ?H1 is leftof ?H2 is false ) and
%   ( ( ( " ?H1 is a house " or ?H1 is leftof ?H2 is false ) is necessarily true ) and
%   ( ( " ?H2 is a house " or ?H1 is leftof ?H2 is false ) is necessarily true ) ) ) and
%   ( ( " ?H1 is a house " or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) and
%   ( " ?H2 is a house " or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) )
% It's Proof that:
%   " ?H1 is leftof ?H2 " is possibly false

( (   poss(leftof(H1,H2))v~leftof(H1,H2)  &
    nesc( house(H1)v~leftof(H1,H2)) &
    nesc( house(H2)v~leftof(H1,H2)) &
    (   house(H1)  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2))) &
    (   house(H2)  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2)))) ==>
  poss( ~( leftof(H1,H2))))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " ?H1 is leftof ?H2 " is possible or ?H1 is leftof ?H2 is false ) and
%   ( ( ( " ?H1 is a house " or ?H1 is leftof ?H2 is false ) is necessarily true ) and
%   ( ( " ?H2 is a house " or ?H1 is leftof ?H2 is false ) is necessarily true ) ) ) and
%   ( ( " ?H1 is a house " or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) and
%   ( " ?H2 is a house " or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) )
% It's Proof that:
%   " ?H2 is a house " is possibly false or " ?H1 is a house " is possible

( (   poss(leftof(H1,H2))v~leftof(H1,H2)  &
    nesc( house(H1)v~leftof(H1,H2)) &
    nesc( house(H2)v~leftof(H1,H2)) &
    (   house(H1)  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2))) &
    (   house(H2)  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2)))) ==>
  poss(~house(H2))v poss(house(H1)))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " ?H1 is leftof ?H2 " is possible or ?H1 is leftof ?H2 is false ) and
%   ( ( ( " ?H1 is a house " or ?H1 is leftof ?H2 is false ) is necessarily true ) and
%   ( ( " ?H2 is a house " or ?H1 is leftof ?H2 is false ) is necessarily true ) ) ) and
%   ( ( " ?H1 is a house " or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) and
%   ( " ?H2 is a house " or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) )
% It's Proof that:
%   " ?H1 is a house " is possibly false or " ?H2 is a house " is possible

( (   poss(leftof(H1,H2))v~leftof(H1,H2)  &
    nesc( house(H1)v~leftof(H1,H2)) &
    nesc( house(H2)v~leftof(H1,H2)) &
    (   house(H1)  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2))) &
    (   house(H2)  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2)))) ==>
  poss(~house(H1))v poss(house(H2)))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " ?H1 is leftof ?H2 " is possible or ?H1 is leftof ?H2 is false ) and
%   ( ( ( " ?H1 is a house " or ?H1 is leftof ?H2 is false ) is necessarily true ) and
%   ( ( " ?H2 is a house " or ?H1 is leftof ?H2 is false ) is necessarily true ) ) ) and
%   ( ( " ?H1 is leftof ?H2 " is possible or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) and
%   ( " ?H1 is a house " or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) )
% It's Proof that:
%   " ?H2 is a house " is possibly false

( (   poss(leftof(H1,H2))v~leftof(H1,H2)  &
    nesc( house(H1)v~leftof(H1,H2)) &
    nesc( house(H2)v~leftof(H1,H2)) &
    (   poss( leftof(H1,H2))  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2))) &
    (   house(H1)  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2)))) ==>
  poss( ~( house(H2))))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " ?H1 is leftof ?H2 " is possible or ?H1 is leftof ?H2 is false ) and
%   ( ( ( " ?H1 is a house " or ?H1 is leftof ?H2 is false ) is necessarily true ) and
%   ( ( " ?H2 is a house " or ?H1 is leftof ?H2 is false ) is necessarily true ) ) ) and
%   ( ( " ?H1 is leftof ?H2 " is possible or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) and
%   ( " ?H1 is a house " or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) )
% It's Proof that:
%   " ?H2 is a house " is possibly false or " ?H1 is a house " is possible

( (   poss(leftof(H1,H2))v~leftof(H1,H2)  &
    nesc( house(H1)v~leftof(H1,H2)) &
    nesc( house(H2)v~leftof(H1,H2)) &
    (   poss( leftof(H1,H2))  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2))) &
    (   house(H1)  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2)))) ==>
  poss(~house(H2))v poss(house(H1)))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " ?H1 is leftof ?H2 " is possible or ?H1 is leftof ?H2 is false ) and
%   ( ( ( " ?H1 is a house " or ?H1 is leftof ?H2 is false ) is necessarily true ) and
%   ( ( " ?H2 is a house " or ?H1 is leftof ?H2 is false ) is necessarily true ) ) ) and
%   ( ( " ?H1 is leftof ?H2 " is possible or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) and
%   ( " ?H1 is a house " or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) )
% It's Proof that:
%   " ?H1 is a house " is possibly false or " ?H2 is a house " is possible

( (   poss(leftof(H1,H2))v~leftof(H1,H2)  &
    nesc( house(H1)v~leftof(H1,H2)) &
    nesc( house(H2)v~leftof(H1,H2)) &
    (   poss( leftof(H1,H2))  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2))) &
    (   house(H1)  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2)))) ==>
  poss(~house(H1))v poss(house(H2)))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " ?H1 is leftof ?H2 " is possible or ?H1 is leftof ?H2 is false ) and
%   ( ( ( " ?H1 is a house " or ?H1 is leftof ?H2 is false ) is necessarily true ) and
%   ( ( " ?H2 is a house " or ?H1 is leftof ?H2 is false ) is necessarily true ) ) ) and
%   ( ( " ?H1 is leftof ?H2 " is possible or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) and
%   ( " ?H2 is a house " or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) )
% It's Proof that:
%   " ?H1 is a house " is possibly false

( (   poss(leftof(H1,H2))v~leftof(H1,H2)  &
    nesc( house(H1)v~leftof(H1,H2)) &
    nesc( house(H2)v~leftof(H1,H2)) &
    (   poss( leftof(H1,H2))  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2))) &
    (   house(H2)  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2)))) ==>
  poss( ~( house(H1))))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " ?H1 is leftof ?H2 " is possible or ?H1 is leftof ?H2 is false ) and
%   ( ( ( " ?H1 is a house " or ?H1 is leftof ?H2 is false ) is necessarily true ) and
%   ( ( " ?H2 is a house " or ?H1 is leftof ?H2 is false ) is necessarily true ) ) ) and
%   ( ( " ?H1 is leftof ?H2 " is possible or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) and
%   ( " ?H2 is a house " or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) )
% It's Proof that:
%   " ?H2 is a house " is possibly false or " ?H1 is a house " is possible

( (   poss(leftof(H1,H2))v~leftof(H1,H2)  &
    nesc( house(H1)v~leftof(H1,H2)) &
    nesc( house(H2)v~leftof(H1,H2)) &
    (   poss( leftof(H1,H2))  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2))) &
    (   house(H2)  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2)))) ==>
  poss(~house(H2))v poss(house(H1)))

----------------------------------------


----------------------------------------
% Whenever:
%   ( ( " ?H1 is leftof ?H2 " is possible or ?H1 is leftof ?H2 is false ) and
%   ( ( ( " ?H1 is a house " or ?H1 is leftof ?H2 is false ) is necessarily true ) and
%   ( ( " ?H2 is a house " or ?H1 is leftof ?H2 is false ) is necessarily true ) ) ) and
%   ( ( " ?H1 is leftof ?H2 " is possible or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) and
%   ( " ?H2 is a house " or ( ( ( " ?H2 is a house " is possible and
%   ?H1 is a house is false ) is possible ) or ( ( " ?H1 is a house " is possible and
%   ?H2 is a house is false ) is possible ) ) ) )
% It's Proof that:
%   " ?H1 is a house " is possibly false or " ?H2 is a house " is possible

( (   poss(leftof(H1,H2))v~leftof(H1,H2)  &
    nesc( house(H1)v~leftof(H1,H2)) &
    nesc( house(H2)v~leftof(H1,H2)) &
    (   poss( leftof(H1,H2))  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2))) &
    (   house(H2)  v
      poss( poss(house(H2))& ~house(H1)) v
      poss( poss(house(H1))& ~house(H2)))) ==>
  poss(~house(H1))v poss(house(H2)))

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

