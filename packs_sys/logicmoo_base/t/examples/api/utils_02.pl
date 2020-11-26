
% TODO
% :- guitracer.


:- ensure_loaded(library(logicmoo_user)).
% :- autoload.



no_profile_meta_preds:- 
 ignore(((current_module(M),current_predicate(_,M:P), \+ predicate_property(M:P,imported_from(_)),
   predicate_property(M:P,meta_predicate(P)), 
   ((arg(_,P,E),number(E)) ->(functor(P,F,A),noprofile(M:F/A)) ; true)),fail)).

% :- set_prolog_flag(bad_idea,true).

:- profiler(_Old, cputime).
:- no_profile_meta_preds.
:- reset_profiler.
:- profile(ensure_mpred_file_loaded('logicmoo/pfc/autoexec.pfc'),[]).


