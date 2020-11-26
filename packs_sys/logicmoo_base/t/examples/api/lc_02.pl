/* <module>
%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
% Dec 13, 2035
% Douglas Miles
*/
:- module(sanity_neg,[]).

:- ensure_loaded(system:library(logicmoo_utils)).

on_xf_cont(G):-catch(G,_,true).

:- dynamic(isRegisterCycPredFA/3).

:-
 forall((current_module(M),module_property(M,class(user)),atom_concat('logicmoo_util',_,M),
  predicate_property(M:P,defined), \+ predicate_property(M:P,imported_from(_)),
   functor(P,F,A)), 
     assert(isRegisterCycPredFA(M,F,A))).

:-
 forall((current_module(M),module_property(M,class(library)),
   predicate_property(M:P,defined), 
 \+ predicate_property(M:P,imported_from(_)),
 functor(P,F,A),on_xf_cont(baseKB:import(M:F/A))),
     assert(isRegisterCycPredFA(M,F,A))).


:- printAll(isRegisterCycPredFA(_,_,_)).




% vs

:- baseKB:mpred_ops.

:- dynamic(isRegisterCycPredFW/3).

:-
 baseKB:ain(({current_module(M),module_property(M,class(user)),atom_concat('logicmoo_util',_,M),
  predicate_property(M:P,defined), \+ predicate_property(M:P,imported_from(_)),
   functor(P,F,A)})
     ==>(isRegisterCycPredFW(M,F,A))).

:- baseKB:ain((
({current_module(M),module_property(M,class(library)),
   predicate_property(M:P,defined), 
 \+ predicate_property(M:P,imported_from(_)),
 functor(P,F,A),on_xf_cont(baseKB:import(M:F/A))}
    ==>isRegisterCycPredFW(M,F,A)))).

:- printAll(isRegisterCycPredFW(_,_,_)).

