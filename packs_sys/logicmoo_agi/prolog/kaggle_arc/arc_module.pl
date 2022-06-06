
:- module(muarc,[]).

:- ensure_loaded(kaggle_arc).

:- fixup_exports.

do_forgotten_exports(M):-  
  MP= (M:P),
  forall((
    predicate_property(MP,defined), 
 \+ predicate_property(MP,imported_from(_)),
 \+ predicate_property(MP,exported),
    functor(P,F,A)),
    export(M:F/A)).

:- prolog_load_context(module,M), 
   wdmsg(do_forgotten_exports(M)), 
   do_forgotten_exports(M).
