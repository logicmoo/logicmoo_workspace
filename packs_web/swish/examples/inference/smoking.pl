/* Smoking and the Genotype Theory

From Section 3.3.3 of Pearl, Judea. Causality. Cambridge university press, 2009.

*/

:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(graphviz).
:- endif.

:- pita.

:- begin_lpad.

:- action smoker/0.
smoker:0.5.

tar:0.05:- \+ smoker.
tar:0.95:- smoker.

cancer:0.10:- \+ smoker,\+ tar.
cancer:0.90:- smoker,\+ tar.
cancer:0.05:- \+ smoker, tar.
cancer:0.85:- smoker, tar.

:-end_lpad.

smoking_effect_on_cancer(P):-
  prob(tar,smoker,P_z_x),
  prob(cancer,(smoker,tar),P_y_xz),
  prob(cancer,(smoker,\+ tar), P_y_xnz),
  prob(cancer,(\+ smoker,tar),P_y_nxz),
  prob(cancer,(\+ smoker,\+ tar), P_y_nxnz),
  prob(smoker,P_x),
  P_nx is 1-P_x,
  P_nz_x is 1-P_z_x,
  P is P_z_x*(P_y_xz*P_x+P_y_nxz*P_nx)+P_nz_x*(P_y_xnz*P_x+P_y_nxnz*P_nx).

no_smoking_effect_on_cancer(P):-
  prob(tar,\+ smoker,P_z_nx),
  prob(cancer,(smoker,tar),P_y_xz),
  prob(cancer,(smoker,\+ tar), P_y_xnz),
  prob(cancer,(\+ smoker,tar),P_y_nxz),
  prob(cancer,(\+ smoker,\+ tar), P_y_nxnz),
  prob(smoker,P_x),
  P_nx is 1-P_x,
  P_nz_nx is 1-P_z_nx,
  P is P_z_nx*(P_y_xz*P_x+P_y_nxz*P_nx)+P_nz_nx*(P_y_xnz*P_x+P_y_nxnz*P_nx).


/** <examples>
?- Network = dot(digraph(['Tar Z'->'Cancer Y','Smoking X'->'Tar Z','Smoking X'->'Cancer Y'])).
?- MutilatedNetwork = dot(digraph(['Drug C'->'Recovery E','Gender F'->'Recovery E'])).
?- smoking_effect_on_cancer(P).
?- no_smoking_effect_on_cancer(P).
?- prob(cancer,smoker,P).
?- prob(cancer,\+ smoker,P).
?- prob(cancer,tar,P).
?- prob(cancer,\+ tar,P).
*/
