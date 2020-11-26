/* Sex Discrimination in College Admission
From Section 4.5.3 of Pearl, Judea. Causality. Cambridge university press, 2009.

*/

:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(graphviz).
:- endif.

:- pita.

:- begin_lpad.
female:0.5.

qualified:0.5.


admitted:-  deptB,female,qualified.
admitted:- \+ deptB,\+ female.

deptB:- female.
deptB:0.5:- \+ female.

:-end_lpad.

adjusted_effect_of_female(P):-
  prob(admitted,(female,deptB),PAFB),
  prob(deptB,PB),
  P is PAFB*PB.

adjusted_effect_of_male(P):-
  prob(admitted,(\+ female, \+ deptB),PAFA),
  prob(admitted,(\+ female,deptB),PAFB),
  prob(deptB,PB),
  P is PAFA*(1-PB)+PAFB*PB.


/** <examples>
?- Network = dot(digraph(['Gender X1'->'Qualification Q',
   'Gender X1'->'Department X2','Gender X1'->'Admission Y',
   'Qualification Q'->'Admission Y','Qualification Q'->'Department X2',
   'Department X2'->'Admission Y'])).

?- prob(admitted,female,P).
%P = 0.5.

?- prob(admitted,\+female,P).
%P = 0.5.

?- adjusted_effect_of_female(P).
%P = 0.375.

?- adjusted_effect_of_male(P).
%P = 0.25.

*/
