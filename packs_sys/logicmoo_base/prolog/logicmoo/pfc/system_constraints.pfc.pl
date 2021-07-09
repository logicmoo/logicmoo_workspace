%:- module(system_constraints,[]).
%:- set_module(class(development)).
:- '$set_source_module'(baseKB).

/** <module> system_constraints
% =============================================
% File 'system_constraints.pfc'
% Purpose: Agent Reactivity for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface' 1.0.0
% Revision: $Revision: 1.9 $
% Revised At: $Date: 2002/06/27 14:13:20 $
% =============================================
%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
%
% props(Obj,[height(ObjHt)]) == t(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == t(height(Obj,ObjHt)).
% pain(Obj,[height(ObjHt)]) == prop_set(height,Obj,ObjHt,...) == ain(height(Obj,ObjHt))
% [pdel/pclr](Obj,[height(ObjHt)]) == [del/clr](height,Obj,ObjHt) == [del/clr]svo(Obj,height,ObjHt) == [del/clr](height(Obj,ObjHt))
% keraseall(AnyTerm).
%
%                      ANTECEEDANT                                   CONSEQUENT
%
%         P =         test nesc true                         assert(P),retract(~P) , enable(P).
%       ~ P =         test nesc false                        assert(~P),retract(P), disable(P)
%
%   ~ ~(P) =         test possible (via not impossible)      retract( ~(P)), enable(P).
%  \+ ~(P) =         test impossiblity is unknown            retract( ~(P))
%   ~ \+(P) =        same as P                               same as P
%     \+(P) =        test naf(P)                             retract(P)
%
% Dec 13, 2035
% Douglas Miles
*/

:- expects_dialect(pfc).

:- file_begin(pfc).

:- kb_shared(predicate_relaxed/1).

% if we can assert clauses with attvars
:- if(current_prolog_flag(assert_attvars,true)).

:- use_module(library(clause_attvars)).

predicate_relaxed(MSpec),{ strip_module(MSpec,M,Spec),mpred_functor(Spec,F,A),functor(P,F,A)} ==>  
   macroExpandExact(P, relax_goal(P,Q),M:Q).
       
predicate_relaxed(MSpec),
 { strip_module(MSpec,M,Spec), mpred_functor(Spec,F,A),functor(LOOP,F,A),kb_shared(M:F/A),LOOP=..[F|ARGS]} 
 ==>  
 (((M:LOOP:- awc, \+ maplist(is_iz_or_iza,ARGS), LOOPY=LOOP,!,M:relax(LOOPY),!,M:call(LOOPY))),
 prologOrdered(F)).  % Prolog Ordered is secondary insurance new assertions use assertz

:- else.

predicate_relaxed(MSpec),
 { strip_module(MSpec,M,Spec), mpred_functor(Spec,F,A),functor(LOOP,F,A),kb_shared(M:F/A),LOOP=..[F|ARGS]} 
 ==>  
 (((M:LOOP:- awc, \+ maplist(is_iz_or_iza,ARGS), LOOPY=LOOP,!,M:relax(LOOPY),!,M:call(LOOPY))),
 prologOrdered(F)).  % Prolog Ordered is secondary insurance new assertions use assertz

/*
% @TODO THIS IS DISABLED but good to figure out
predicate_relaxed(Spec),{ fail, mpred_functor(Spec,F,A),functor(LOOP,F,A)} ==>  
 (((LOOP:- awc,LOOPY=LOOP,relaxing(LOOPY),!,call(LOOPY))),
  prologOrdered(F)).  % Prolog Ordered is secondary insurance new assertions use assertz

% @TODO THIS IS DISABLED but good to figure out
predicate_relaxed(Spec),{ fail, mpred_functor(Spec,F,A),functor(LOOP,F,A)} ==>  
 (((LOOP:-
    awc,    % awc means this rule is always first
      \+ is_loop_checked(LOOP),!,
      loop_check_term((LOOPY = LOOP, relax(LOOPY), LOOPY),LOOP,fail))),
  prologOrdered(F)).  % Prolog Ordered is secondary insurance new assertions use assertz
*/

% make sure current bug is caught
prologOrdered(F),predSingleValued(F) ==> {trace_or_throw(unsupported(prologOrdered(F),predSingleValued(F)))}.

:- endif.

% ?- G=(loves(X,Y),~knows(Y,tHuman(X))),relax_goal(G,Out),writeq(Out).





:- if((current_prolog_flag(runtime_safety,D),D>2)).

predicate_relaxed(weak_test/2).

weak_test("Weak1","Weak2").
weak_test("Weak0","weAk2").

:- export(weak_test/2).
:- public(weak_test/2).
:- if((current_prolog_flag(runtime_debug,D),D>2)).
:- listing(weak_test/2).
:- endif.

%:- listing(weak_test/2).

:- mpred_trace_exec.
:- if(\+ current_predicate(mpred_test/1)).
:- use_module(library(pfc_test)).
:- endif.
:- mpred_notrace_exec.

:- if((current_prolog_flag(runtime_safety,D),D>2)).
:- mpred_test((weak_test(weak1,"WeAK2")))->true;(writeln(mpred_test(weak_test(weak1,"WeAK2"))),break).
:- if((current_prolog_flag(runtime_debug,D),D>1)).
:- dmsg(call(mpred_test(weak_test("Weak1","Weak2")))).
:- dmsg(call(mpred_test(weak_test("Weak1","wEak2")))).
:- endif.
:- endif.


% =======================================================
% =======================================================
weac_test("Weac1","Weac2").
weac_test("Weac0","weAc2").

:- export(weac_test/2).
:- public(weac_test/2).

%:- listing(weac_test/2).

predicate_relaxed(weac_test/2).

%:- listing(weac_test/2).
:- (dmsg(call(
   mpred_test(weac_test("Weac1","Weac2")),
   mpred_test(weac_test("Weac1","wEac2")),
   mpred_test((weac_test(weac1,"WeAC2")))))->true;(writeln(mpred_test(weac_test(weac1,"WeAC2"))),break)).

% =======================================================
% =======================================================


:- endif.


