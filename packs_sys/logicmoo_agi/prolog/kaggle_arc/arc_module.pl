
:- module(muarc,[do_forgotten_exports/0,do_forgotten_exports/1]).

do_forgotten_exports(M):-  
  MP= (M:P),
  forall((
    predicate_property(MP,defined), 
 \+ predicate_property(MP,imported_from(_)),
 \+ predicate_property(MP,exported),
    functor(P,F,A)),
    export(M:F/A)).

do_forgotten_exports:- 
   strip_module(_,M,_), 
   wdmsg(do_forgotten_exports(M)), 
   do_forgotten_exports(M).

%:- use_module(library(pfc_lib)).

:- system:ensure_loaded(kaggle_arc).
%:- muarc:ensure_loaded(kaggle_arc).

:- do_forgotten_exports.


:- fixup_exports.

