%:- module(system_base_lite,[]).
%:- set_module(class(development)).
%:- mpred_unload_file.
:- '$set_source_module'(baseKB).
%:- ensure_abox(baseKB).
:- baseKB:export(baseKB:never_assert_u/1).
:- baseKB:export(baseKB:never_assert_u/2).
:- rdf_rewrite:import(baseKB:never_assert_u/1).
:- rdf_rewrite:import(baseKB:never_assert_u/2).
%:- use_module(library(rtrace)).
:- dynamic(prologHybrid/1).
:- pfc_lib:use_module(library(pfc_lib)).
:- use_module(library(ctypes)).

%:- set_fileAssertMt(baseKB).
%:- add_import_module(baseKB,pfc_lib,end).
:- fix_baseKB_imports.
%:- use_module(library(no_repeats)).
%:- use_module(library(dictoo)).
% ensure this file does not get unloaded with mpred_reset



/** <module> system_base
% =============================================
% File 'system_base_lite.pfc'
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

%:- use_module(library(pfc_test)).

:- set_prolog_flag(runtime_debug, 1). % 2 = important but dont sacrifice other features for it

:- if((current_prolog_flag(runtime_debug,D),D>1)).
:- endif.
%:- '$def_modules'([clause_expansion/2],O),dmsg_pretty('$def_modules'([clause_expansion/2],O)),nl.

:- expects_dialect(pfc).
:- sanity(prolog_load_context(dialect,pfc)).

:- sanity(is_pfc_file).

:- dynamic(pfcSanityA/0).
:- dynamic(pfcSanityB/0).

%:- trace.
pfcSanityA ==> pfcSanityB.
%:- \+ clause(pfcSanityB,true).
pfcSanityA.
%:- listing(pfcSanityA).
%:- listing(pfcSanityB).
%:- rtrace.
:- clause(pfcSanityB,true).
%:- nortrace.
% :- kb_shared( ('~') /1).
:- kb_shared(mtExact/1).
% :- kb_shared(arity/2).
:- kb_shared(col_as_unary/1).  % never used for arg2 of isa/2
:- kb_shared(comment/2).
:- kb_shared(feature_setting/2).
:- kb_shared(hybrid_support/2).
:- kb_shared(never_assert_u/1).
:- kb_shared(never_assert_u/2).
:- kb_shared(never_retract_u/1).
:- kb_shared(never_retract_u/2).
:- kb_shared(predicateConventionMt/2).
:- kb_shared(startup_option/2).
:- kb_shared(tooSlow/0).
:- kb_shared(ttRelationType/1).
:- kb_shared(singleValuedInArg/2).
%:- kb_shared(singleValuedInArgAX/3).
:- kb_shared(functorIsMacro/1).
:- kb_shared(support_hilog/2).
:- kb_shared(mpred_undo_sys/3).
:- kb_shared(genlPreds/2).



:- kb_shared(alwaysGaf/1).

:- kb_shared(rtReformulatorDirectivePredicate/1).
:- kb_global(rtArgsVerbatum/1).
:- kb_shared(rtAvoidForwardChain/1).

:- kb_shared(pfcControlled/1).
:- kb_shared(prologHybrid/1).
:- kb_shared(prologOnly/1).
:- kb_shared(quasiQuote/1).


% :- forall(between(1,11,A),kb_shared(t/A)).

/*
:- meta_predicate t(*,?).
:- meta_predicate t(*,?,?).
:- meta_predicate t(*,?,?,?).
:- meta_predicate t(*,?,?,?,?).
:- meta_predicate t(*,?,?,?,?,?).
:- meta_predicate t(*,?,?,?,?,?,?).
:- meta_predicate t(*,?,?,?,?,?,?,?).
*/

% ===================================================================
%  Microtheory System
% ===================================================================

:- kb_global(baseKB:mtHybrid/1).
:- kb_global(baseKB:mtProlog/1).
:- kb_global(baseKB:genlMt/2).

:- kb_global(baseKB:mtNotInherits/1).
:- kb_global(baseKB:mtInherits/1).


%((ttTypeType(TT),abox:isa(T,TT))==>tSet(T)).
%tSet(T)==>functorDeclares(T).
:- kb_shared(functorDeclares/1).
:- kb_shared(mtNonAssertable/1).
:- kb_shared(prologBuiltin/1).
:- kb_shared(predicateConventionMt/2).
% :- kb_shared(genlMt/2).
:- kb_shared(do_import_modules/0).

:- baseKB:export(baseKB:ttRelationType/1).
:- rtrace:import(baseKB:ttRelationType/1).
:- baseKB:export(baseKB:prologOrdered/1).
:- rtrace:import(baseKB:prologOrdered/1).
((mtHybrid(C)/(C\=baseKB)) ==> genlMt(C,baseKB),{ensure_abox(C),(C==user->dmsg_pretty(warn(mtHybrid(C)));true)}).

%:- wdmsg(loading_system_base()).

predicateTriggerType(kb_local).
predicateTriggerType(kb_shared).
predicateTriggerType(kb_global).
predicateTriggerType(kbi_define).

% TODO make these undoable
:- if((current_predicate(predicate_m_f_a_decl/4))).
(genlMt(C,M)/(C\=baseKB)) ==> {doall(((predicate_m_f_a_decl(M,F,A,Type)),
  ain(baseKB:mpred_prop(M,F,A,Type))))}.

predicateTriggerType(Type) ==>
(( mpred_prop(M,F,A,Type),genlMt(C,M)/(C\=baseKB)) ==> {
 ( nop(dmsg_pretty(C:call(Type,C:F/A))),
   show_failure(on_x_fail(C:call(Type,C:F/A))))}).


:- else.

%:- break.
(genlMt(C,P)/(C\=baseKB)) ==> {doall(((pred_decl_kb_mfa_type(P,F,A,Type)),C:call(Type,C:F/A)))}.
:- endif.

(genlMt(C,P)/(is_ftNonvar(C),is_ftNonvar(P),P\==baseKB,(mtProlog(P))) ==> {P\==user,catch((add_import_module(C,P,end)),error(_,_),dmsg_pretty(error(add_import_module(C,P,end))))}).

%(do_import_modules,genlMt(C,P),mtHybrid(C),mtProlog(P)) ==>  {catch(add_import_module(C,P,end),error(_,_),dmsg_pretty(error(add_import_module(C,P,end))))}.
%(do_import_modules,genlMt(C,P),mtProlog(C),mtHybrid(P)) ==>  {catch(add_import_module(C,P,end),error(_,_),dmsg_pretty(error(add_import_module(C,P,end))))}.
%((mtHybrid(C),{is_ftNonvar(C)},{ensure_abox_hybrid(C)}, \+ mtProlog(C)) <==> (genlMt(C,baseKB),{is_ftNonvar(C)}, \+ mtProlog(C))).

%
% mtProlog(C) ==> {decl_assertable_module(C)}. % , \+ mtHybrid(C). 

mtHybrid(C) ==> {decl_assertable_module(C)}. % , \+ mtProlog(C). 
%(predicateConventionMt(F,MT),arity(F,A))==>{(MT==baseKB;mtProlog(MT))->kb_shared(MT:F/A);kb_shared(MT:F/A)}.
% :- break.
% predicateConventionMt(predicateConventionMt,baseKB).
predicateConventionMt(genlMt,baseKB).
predicateConventionMt(mtHybrid,baseKB).
predicateConventionMt(mtProlog,baseKB).

% predicateConventionMt(mtNonAssertable,baseKB).
(predicateConventionMt(F,MT),arity(F,A))==>{kb_global(MT:F/A)}.



ttTypeType(ttTypeType).
ttTypeType(ttRelationType).
ttTypeType(TT)==>functorDeclares(TT).

==>ttTypeType(ttModuleType,mudToCyc('MicrotheoryType')).
typeGenls(ttModuleType,tMicrotheory).
% :- break.
==>ttModuleType(tSourceCode,mudToCyc('tComputerCode'),comment("Source code files containing callable features")).
==>ttModuleType(tSourceData,mudToCyc('iboPropositionalInformationThing'),comment("Source data files containing world state information")).

ttRelationType(RT)==> { decl_rt(RT) },functorDeclares(RT).
% ttRelationType(RT)==>predicateConventionMt(RT,baseKB).

functorDeclares(RT)==> % {kb_shared(RT/1)},
   arityMax(RT,1), % prologHybrid(RT),
   functorIsMacro(RT).

% ttRelationType(RT) ==> ( ~genlPreds(RT,tFunction) <==> genlPreds(RT,tPred)).

:- kb_shared(baseKB:compilerDirective/1).
functorDeclares(compilerDirective).
%compilerDirective(F)==>{kb_global(F/0)}.
compilerDirective(F)==>{kb_shared(F/0)}.

==> compilerDirective(hardCodedExpansion,comment("Is Already Implemented From Code")).
==> compilerDirective(codeTooSlow,comment("A faster more incomplete version is filling in for it")).
==> compilerDirective(pfc_checking,comment("Checks for common Pfc Errors")).
==> compilerDirective(pass2,comment("Probably not needed at first")).
==> compilerDirective(tooSlow,comment("Slow and Probably not needed at first")).
==> compilerDirective(redundantMaybe,comment("Probably redundant")).
==> compilerDirective(isRedundant,comment("Redundant")).
==> compilerDirective(isRuntime,comment("Only use rule/fact at runtime")).


:- forall(member(PredType,[
                  prologBuiltin,
                  prologDynamic,
                  prologHybrid,
                  singleValuedHybrid,

                  prologKIF,
                  prologPTTP,
                  pfcMustFC, 

                  prologListValued,
                  prologMultiValued,
                  prologSingleValued,
                  prologOrdered,

                  prologEquality,

                  rtArgsVerbatum,
                  prologSideEffects,
                  rtNotForUnboundPredicates,
                  rtAvoidForwardChain,
                  rtSymmetricBinaryPredicate,
                  predCanHaveSingletons,
                  pfcControlled,  % pfc decides when to forward and backchain this pred
/*
                  
                  pfcWatches,   % pfc needs to know about new assertions
                  pfcCreates,   % pfc asserts 

                  pfcCallCode,  % called as prolog

                  pfcNegTrigger,
                  pfcPosTrigger,
                  pfcBcTrigger,
                  pfcRHS,
                  pfcLHS,
*/
                  prologNegByFailure,
                  prologIsFlag,
                  tFunction
                  ]),must_or_rtrace(ain(ttRelationType(PredType)))).



%:- listing(ttRelationType/1).

:- kb_shared(do_and_undo/2).
:- kb_shared(tFunction/1).

do_and_undo(A,U):-cwc,atom(A),atom_concat('assert',Suffix,A),!,atom_concat('delete',Suffix,U),current_predicate(U/_).
do_and_undo(A,U):-cwc,atom(A),atom_concat('def',_,A),atom_concat('un',A,U),current_predicate(U/_).
do_and_undo(A,U):-cwc,strip_module(A,M,P),compound(P),P=..[F|ARGS],lookup_u(do_and_undo(F,UF)),UA=..[UF|ARGS], U = (M:UA).
ll:- cwc,call(listing,[isa/2,mtHybrid/1,col_as_unary/1, tRRP2/1,tRR/1,tRRP/1]). % ttTypeType,

:- is_pfc_file.

:- ain(arity(arity,2)).
:- ain(arity(do_and_undo,2)).
%:- rtrace.
%:- trace.
arity(functorIsMacro,1).
%:- break.
functorIsMacro(functorIsMacro).

((prologHybrid(F),arity(F,A))==>{kb_shared(F/A)}).

:- sanity(ttRelationType(prologMultiValued)).

:- scan_missed_source.



% :- ((ain((hybrid_support(F,A)/(F\==arity,F\==genlMt))==> {must(kb_shared(F/A))}))).



pfcControlled(P),arity(P,A)==>hybrid_support(P,A).


rtArgsVerbatum(mpred_prop).
rtArgsVerbatum(listing).

rtNotForUnboundPredicates(~).
rtNotForUnboundPredicates(t).
rtNotForUnboundPredicates(call).



% ==> pfc_checking.



/*
% catching of misinterpreations
*/
pfc_checking ==> (mpred_prop(M,F,A,pfcPosTrigger)==>{M:warn_if_static(F,A)}).
pfc_checking ==> (mpred_prop(M,F,A,pfcNegTrigger)==>{M:warn_if_static(F,A)}).
pfc_checking ==> (mpred_prop(M,F,A,pfcBcTrigger)==>{M:warn_if_static(F,A)}).
mpred_prop(M,F,A,What)/(\+ ground(F/A))==>{trace_or_throw_ex(mpred_prop(M,F,A,What))}.


mpred_prop(M,F,A,pfcCreates)==> 
 % {functor(P,F,A),quietly(make_dynamic(P)),kb_shared(F/A),create_predicate_inheritance(abox,F,A)},
  {kb_shared(M:F/A)},
  {M:warn_if_static(F,A)}.
mpred_prop(M,F,A,pfcControlled)==> {kb_shared(M:F/A)}.
mpred_prop(M,F,A,pfcWatches)==> {kb_shared(M:F/A)}.
                                                                                     

mpred_prop(M,F,A,pfcPosTrigger)==>mpred_prop(M,F,A,pfcWatches).
mpred_prop(M,F,A,pfcNegTrigger)==>mpred_prop(M,F,A,pfcWatches).
mpred_prop(M,F,A,pfcBcTrigger)==>mpred_prop(M,F,A,pfcCreates).
mpred_prop(M,F,A,pfcLHS)==> arity(F,A),functorIsMacro(F),mpred_prop(M,F,A,pfcWatches).
mpred_prop(M,F,A,pfcRHS)==> mpred_prop(M,F,A,pfcCreates).



mpred_prop(M,F,A,pfcCallCode)/predicate_is_undefined_fa(F,A)
    ==> mpred_prop(M,F,A,needsDefined).
/*
mpred_prop(M,F,A,pfcCallCode)/predicate_is_undefined_fa(F,A)
    ==> mpred_prop(M,F,A,pfcWatches).
*/
genlPreds(pfcRHS,pfcControlled).

genlPreds(prologSideEffects,rtNotForUnboundPredicates).

:- kb_shared(nondet/0).
:- kb_shared(typeCheckDecl/2).

==> nondet.


:- kb_shared(warningsAbout/2).

==>prologHybrid(warningsAbout/2,rtArgsVerbatum).
warningsAbout(Msg,Why)==>{wdmsg_pfc(error(warningsAbout(Msg,Why))),break}.

%% t( ?CALL) is semidet.
%
% True Structure.
%
%:- kb_shared(t/1).
%t([P|LIST]):- cwc, !,mpred_plist_t(P,LIST).
%t(naf(CALL)):- cwc, !,not(t(CALL)).
%t(not(CALL)):- cwc, !,mpred_f(CALL).
t(CALL):- cwc, call(into_plist_arities(3,10,CALL,[P|LIST])),mpred_plist_t(P,LIST).


%% t( ?VALUE1, ?VALUE2) is semidet.
%
% True Structure.
%
% t(C,I):- cwc,  trace_or_throw_ex(t(C,I)),t(C,I). % ,fail,loop_check_term(isa_backchaing(I,C),t(C,I),fail).
% t(P,A1):- vwc, isa(A1,P).
t(A,B):- cwc, atom(A),!,ABC=..[A,B],call_u(ABC).
%t(A,B):- (atom(A)->true;(no_repeats(arity(A,1)),atom(A))),ABC=..[A,B],loop_check(call_u(ABC)).
%t(A,B):- call_u(call(A,B)).
t(P,A1):- cwc, mpred_fa_call(P,1,call(P,A1)).


%% t( ?P, ?A1, ?A2) is semidet.
%
% True Structure.
%
t(P,A1,A2):- cwc,  mpred_fa_call(P,2,call(P,A1,A2)).
%t(P,A1,A2):- cwc,  call_u(t(P,A1,A2)).



%% t( ?P, ?A1, ?A2, ?A3) is semidet.
%
% True Structure.
%
t(P,A1,A2,A3):- cwc,  mpred_fa_call(P,3,call(P,A1,A2,A3)).
%t(P,A1,A2,A3):- vwc,  t(P,A1,A2,A3).


%% t( ?P, ?A1, ?A2, ?A3, ?A4) is semidet.
%
% True Structure.
%
t(P,A1,A2,A3,A4):- cwc,  mpred_fa_call(P,4,call(P,A1,A2,A3,A4)).
%t(P,A1,A2,A3,A4):- cwc,  call_u(t(P,A1,A2,A3,A4)).



%% t( :PRED5P, ?A1, ?A2, ?A3, ?A4, ?A5) is semidet.
%
% True Structure.
%
t(P,A1,A2,A3,A4,A5):- cwc,  mpred_fa_call(P,5,call(P,A1,A2,A3,A4,A5)).
%t(P,A1,A2,A3,A4,A5):- cwc,  call_u(t(P,A1,A2,A3,A4,A5)).



%% t( :PRED6P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6) is semidet.
%
% True Structure.
%
t(P,A1,A2,A3,A4,A5,A6):- cwc,  mpred_fa_call(P,6,call(P,A1,A2,A3,A4,A5,A6)).
%t(P,A1,A2,A3,A4,A5,A6):- cwc,  call_u(t(P,A1,A2,A3,A4,A5,A6)).



%% t( :PRED7P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7) is semidet.
%
% True Structure.
%
t(P,A1,A2,A3,A4,A5,A6,A7):- cwc,  mpred_fa_call(P,7,call(P,A1,A2,A3,A4,A5,A6,A7)).
%t(P,A1,A2,A3,A4,A5,A6,A7):- cwc,  call_u(t(P,A1,A2,A3,A4,A5,A6,A7)).

%prologHybrid(C)==>{must(callable(C))}.
%pfcControlled(C)==>{must(callable(C))}.
:- multifile(typeCheckDecl/2).
typeCheckDecl(prologHybrid(C),callable(C)).
typeCheckDecl(pfcControlled(C),callable(C)).


arity(comment,2).



arity(alwaysGaf,1).
alwaysGaf(alwaysGaf).
alwaysGaf(pfcRHS).
alwaysGaf(pfcLHS).

%arity('$VAR',_).
%arity(is_never_type,1).
%arity(prologSingleValued,1).
%arity(Prop,1):- cwc, clause_b(ttRelationType(Prop)).
% :- rtrace.
arity(F,A):- cwc,((is_ftNameArity(F,A), current_predicate(F/A),A>1)).
arity(F,1):- cwc,((call_u(ttRelationType(F)))). % current_predicate(F/1)).  % is_ftNameArity(F,1), , (col_as_unary(F);ttTypeType(F)), \+((call((dif:dif(Z,1))), arity(F,Z))).


arity(rtArgsVerbatum,1).
arity(quasiQuote,1).
rtArgsVerbatum('$spft').


% this mean to leave terms at EL:  foo('xQuoteFn'([cant,touch,me])).

quasiQuote('xQuoteFn').

rtArgsVerbatum('with_current_why').
rtArgsVerbatum('loop_check_term').
rtArgsVerbatum('loop_check_term_key').
rtArgsVerbatum('xQuoteFn').
rtArgsVerbatum('$VAR').
rtArgsVerbatum('NART').
rtArgsVerbatum(X):- cwc, atom(X),atom_concat(_,'Fn',X).
rtArgsVerbatum(ain).
rtArgsVerbatum(ruleRewrite).
rtArgsVerbatum(mpred_action).
rtArgsVerbatum(mpred_prop).
rtArgsVerbatum(ain).
rtArgsVerbatum(mpred_rem).
rtArgsVerbatum(added).
rtArgsVerbatum(call).
rtArgsVerbatum(call_u).
rtArgsVerbatum(clause_asserted_i).
rtArgsVerbatum(member).
rtArgsVerbatum( <- ).
rtArgsVerbatum(=..).
% rtArgsVerbatum({}). % Needs mpred_expansion to visit
rtArgsVerbatum(second_order).

% rtArgsVerbatum((':-')).




:- kb_shared(support_hilog/2).



% genlPreds(support_hilog,arity).


%prologBuiltin(resolveConflict/1).

% :- kb_shared('$bt'/2).
('$bt'(P,_)/(nonvar(P),must(get_bc_clause(P,Post)))) ==> ({ignore(kb_shared(P))},Post).

%redundantMaybe ==> ((prologHybrid(F),arity(F,A))==>mpred_prop(M,F,A,pfcVisible)).
%redundantMaybe ==> (mpred_prop(M,F,A,pfcVisible)==>prologHybrid(F),arity(F,A)).

% ((mpred_prop(M,F,A,pfcRHS)/(A\=0)) ==> {kb_shared(F/A)}).
% ((mpred_prop(M,F,A,_)/(A\=0)) ==> {kb_shared(F/A)}).

% pfcMustFC(F) ==> pfcControlled(F).
genlPreds(pfcMustFC, pfcControlled).

% pfcControlled(C)==>prologHybrid(C).
genlPreds(pfcControlled, prologHybrid).

((mpred_prop(M,F,A,R1),genlPreds(R1,R2))==>mpred_prop(M,F,A,R2)).

do_and_undo(mpred_post_exactly,mpred_remove_exactly).







:- meta_predicate(without_depth_limit(0)).
without_depth_limit(G):- cwc, call_with_depth_limit(G,72057594037927935,Result),sanity(Result\==depth_limit_exceeded).
:- scan_missed_source.



:- dynamic(mpred_undo_sys/3).
:- (mpred_ain(==>pfcControlled(mpred_undo_sys(ftAssertion, ftCallable, ftCallable)))).
:- (ain(==>pfcControlled(mpred_undo_sys(ftAssertion, ftCallable, ftCallable)))).
mpred_undo_sys(P, WhenAdded, WhenRemoved) ==> (P ==> {WhenAdded}), mpred_do_and_undo_method(WhenAdded,WhenRemoved).

% DONT mpred_undo_sys(added(P),ain(P),mpred_retract(P)).
% mpred_undo_sys(asserted(P),assert_eq_quitely(PE),retract_eq_quitely(PE)):-expand_goal(P,PE).

/*
without_depth_limit(G):-
   ('$depth_limit'(72057594037927935,Was,_), 
    (Was == -1 -> call(G);  % Not inside cwdl
    (Was > 72000000000000000 -> call(G);  % We left Depth limit slightly messed
      call_cleanup(G,'$depth_limit'(Was,_,_))))).
*/

~ singleValuedInArg(arity,_).
~ singleValuedInArg(support_hilog,_).


%:- rtrace,dtrace.
% ==>(prologBuiltin(pfcSelect/1)).
% :- nortrace,quietly.

:- kb_shared(conflict/1).
% a conflict triggers a Prolog action to resolve it.
conflict(C) ==> {must(with_mpred_trace_exec((resolveConflict(C),\+conflict(C))))}.


% meta rules to schedule inferencing.
% resolve conflicts asap
pfcSelect(conflict(X)) :- cwc, que(conflict(X),_Why).


%tPred(t,prologDynamic).
% tPred(member/2,prologBuiltin).
rtNotForUnboundPredicates(member/2).


% ===================================================================
%  Never Assert / Retraction checks
% ===================================================================

:- kb_shared(never_assert_u/2).
never_assert_u(~(X),is_ftVar(~X)):- cwc, is_ftVar(X).

never_assert_u(mpred_unload_option(never,X),is_ftVar(mpred_unload_option(never,X))):- cwc, is_ftVar(X).
never_assert_u(X,is_ftVar(X)):- cwc, is_ftVar(X).
never_assert_u(prologSingleValued(BAD),var_prologSingleValued(BAD)):-cwc, is_ftVar(BAD).
never_assert_u(baseKB:mtProlog(baseKB),must(mtHybrid(baseKB))).
never_assert_u(A,never_assert_u(A)):- cwc, loop_check(never_assert_u(A)).
% P/never_assert_u(P,Why) ==> conflict(never_assert_u(P,Why))
:- kb_shared(never_assert_u/1).
% never_assert_u(X):- cwc, loop_check(never_assert_u(X,_)).


:- kb_shared(never_retract_u/2).
never_retract_u(~(X),is_ftVar(~X)):- cwc, is_ftVar(X).
never_retract_u(X,is_ftVar(X)):- cwc, is_ftVar(X).
never_retract_u(X,never_retract_u(X)):- cwc, never_retract_u(X).

:- kb_shared(never_retract_u/1).
never_retract_u(X):- cwc, loop_check(never_retract_u(X,_)).

%:- dynamic(mpred_unload_option/2).
%:- listing(mpred_unload_option/2).
%:- mpred_trace_exec.
%:- rtrace,trace.
:- prolog_load_context(file,F), ain(mpred_unload_option(F,never)).
%:- nortrace.
%:- mpred_notrace_exec.
%:- listing(mpred_unload_option/2).

%:- mpred_trace_exec.
%(P/mpred_positive_fact(P),~P) ==> \+ ~P.



% ~P ==>  ({retractall(P)}).
% P ==>  ({retractall(~P)}).
 
% ~P ==>  \+ P.
 %(P ,{mpred_positive_fact(P)})==>  \+ ~ P.
%:- break.

% :- rtrace.

P/mpred_positive_fact(P) ==> \+ ~P.
(~P)/mpred_positive_fact(P) ==> (\+ P, nesc(~P)).
(nesc(~P)/mpred_positive_fact(P)) ==> (~P, (P ==> \+ P)).
%:- rtrace.
(nesc(P) /mpred_positive_fact(P) ==>  ( P, (~P ==> \+ ~P))).
%:- break.

 nesc(P)==>P.

% % preventedWhen(P,{Cond})==> (((P:- awc,Cond,!,fail))).
preventedWhen(P,Cond)==> (((P/mpred_positive_fact(P),Cond)==> nesc(~P))).
% preventedWhen(P,Cond)==> ((((~P) <- Cond))).
preventedWhen(P,{Cond})/mpred_positive_fact(P)==> ((~P) :- cwc, Cond).

%:- mpred_trace_exec.
%  can this ever happen?
% (( \+ P, P) ==> {dumpST,dmsg_pretty(warn(weak_conflict(P)))}).
% TAKEN CARE OF ( (~ P/mpred_positive_fact(P)), P) ==> ({dmsg_pretty(warn(conflict(P)))}).
% (\+ P, P) => conflict(P).

%% ~( ?VALUE1) is semidet.
%
%

:- call(assertz_if_new,(((~(G):-  (cwc, neg_in_code(G)))))).

% :- pfcNoWatch.


%:- rtrace.
% prologHybrid(arity/2).
prologDynamic(term_expansion/2).
prologBuiltin(var/1).


