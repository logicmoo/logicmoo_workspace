
:- module(muarc,[do_forgotten_exports/0,do_forgotten_exports/1]).

skip_export_pred(goal_expansion,4). 
skip_export_pred('$pldoc',4). % skip_export_pred(is_color,1).
skip_export_pred(is_point,1). skip_export_pred(is_cpoint,1).
skip_export_pred(F,A):- lmconfig:never_export_named(m,F,A).

:- module_transparent(do_forgotten_exports/1).
do_forgotten_exports(M):-  
  MP= (M:P),
  forall((
    predicate_property(MP,defined), 
 \+ predicate_property(MP,imported_from(_)),
 \+ predicate_property(MP,exported),
    functor(P,F,A),
 \+ skip_export_pred(F,A)),
    (export(M:F/A), '@'(import(M:F/A),system), true)).

:- module_transparent(do_forgotten_exports/0).
do_forgotten_exports:- 
   strip_module(_,M,_), 
   u_dmsg(do_forgotten_exports(M)), 
   do_forgotten_exports(M).

%:- use_module(library(pfc_lib)).

% :- system:ensure_loaded(kaggle_arc).
:- ensure_loaded(kaggle_arc).
:- system:ensure_loaded(kaggle_arc).

:- do_forgotten_exports.


:- include(kaggle_arc_footer).

