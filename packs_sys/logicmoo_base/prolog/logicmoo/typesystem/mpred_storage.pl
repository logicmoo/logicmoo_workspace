/* 
% ===================================================================
% File 'mpred_db_preds.pl'
% Purpose: Emulation of OpenCyc for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface.pl' 1.0.0
% Revision:  $Revision: 1.9 $
% Revised At:   $Date: 2002/06/27 14:13:20 $
% ===================================================================
% File used as storage place for all predicates which change as
% the world is run.
%
%
% Dec 13, 2035
% Douglas Miles
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_storage.pl
:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
:- module(mpred_storage,
          [ % ain/1,
          %  add_0/1,
            call_props/2,
            clr/1,
            clr0/1,
            compound_functor/2,
            confirm_hook/1,
            database_modify_0/2,
            database_modify_assert/2,
            db_must_asserta_confirmed_sv/3,
            deduceEachArgType/1,
            deduceEachArgType/4,
            admit_str_type/3,
            del/1,
            del0/1,
            ensure_dynamic/1,
            equals_call/2,
            fact_checked/2,
            fact_loop_checked/2,
            forall_setof/2,
            get_body_functor/3,
            get_pifunctor/2,
            get_pifunctor3/3,
            get_pifunctor/4,
            idel/1,
            if_main/1,
            implied_skipped/1,
            infSecondOrderCheck/0,
            infThirdOrderCheck/0,
            iprops/2,
            ireq/1,
            is_source_proof/1,
            is_static_pred/1,
            is_asserted/1,
            is_asserted_eq/1,
            not_asserted/1,
            make_body_clause/3,
            may_storage_op/2,
            mdel/1,
            mpred_modify/2,
            baseKB:mpred_provide_storage_op/4,
            must_storage_op/2,
            nonground_throw_else_fail/1,
            not_variant/2,
            padd/2,
            padd/3,
            preq/2,
            prolog_modify/2,
            prolog_mpred_provide_storage_op/2,
            prolog_op/2,
            prop/3,
            prop_or/4,
            %call_u/1,
            requires_storage/2,
            requires_storage/3,
            rescan_argIsa/3,
            rescan_meta_argtypes/1,
            retract_all/1,
            side_effect_prone/0,
            singletons_throw_else_fail/1,
            skip_is_asserted_expansion/1,
            special_head/3,
            special_head0/2,
            special_wrapper_body/2,
            special_wrapper_functor/2,
            test_expand_units/1,
            upprop/1,
            upprop/2,
            use_if_modify_new/0,
            with_assert_op_override/2,
            with_fail_is_asserted/2,
            with_fallbacks/1,
            with_fallbacksg/1,
            with_kb_assertions/2,
            with_logical_functor/3,
            with_no_db_hooks/1,
            with_no_fallbacks/1,
            with_no_fallbacksg/1,
            with_no_modifications/1,
            world_clear/1,
            mpred_storage_file/0
          ]).

%:- include('mpred_header.pi').

:- endif.

:- meta_predicate  with_fail_is_asserted(*,0).
/*


:- meta_predicate 
         % ain(-),
        clr(-),
        del(-),
        fact_checked(?, 0),
        fact_loop_checked(+, 0),
        /*
        aina(+),
        ainz(+),
        del(+),
        clr(+),
	    */
   with_fail_is_asserted(*,0),
        with_fallbacks(0),
        with_fallbacksg(0),
        with_no_db_hooks(0),
        with_no_fallbacks(0),
        with_no_fallbacksg(0),
        with_no_modifications(0),
   % mpred_storage
   clr0(0),
   % mpred_storage
   db_must_asserta_confirmed_sv(0,*,*),
   % mpred_storage
   del0(0),
   % mpred_storage
   forall_setof(0,0),
   % mpred_storage
   if_main(0),
   % mpred_storage
   singletons_throw_else_fail(0),
   % mpred_storage
   with_kb_assertions(*,0),
   % mpred_storage
   with_logical_functor(*,*,1).
*/

% ========================================
% Shared Preds
% ========================================



:- was_export((  clr/1,ireq/1,del/1,  
  padd/2, padd/3, prop/3, prop_or/4, call_props/2, iprops/2, upprop/2, % ain/1, 
    ireq/1, % call_u/1, 
    %(ain)/1,
    upprop/1, % call_u/1, 
  % use_term_listing/2,  
  world_clear/1,  
   with_kb_assertions/2)).

:- meta_predicate is_asserted(?).
:- meta_predicate is_asserted_eq(?).
:- meta_predicate not_asserted(?).

:- meta_predicate forall_setof(*,*).
:- meta_predicate clr0(*).
:- meta_predicate whenAnd(*,*).
:- meta_predicate with_logical_functor(*,*,*).
:- meta_predicate with_logical_functor(*,*,1).
:- meta_predicate with_kb_assertions(*,*).
:- meta_predicate if_main(*).


:- set_how_virtualize_file(bodies).


%% not_asserted( ?X) is semidet.
%
% Not Asserted.
%
not_asserted(X):- \+ is_asserted(X).

%% is_asserted_eq( ?HB) is semidet.
%
% If Is A Asserted Using (==/2) (or =@=/2) ).
%
is_asserted_eq(HB):- copy_term(HB,HBC), is_asserted(HBC), HBC=@=HB,!.


%% is_asserted( ?X) is semidet.
%
% If Is An Assertion
%
% TODO Convert loop checking to a "fresh" loop_check
is_asserted(X):- fully_expand(change(assert,is_asserted),X,Y),
  no_repeats(loop_check(lookup_u(Y))).



%= 	 	 

%% compound_functor( ?Compound, ?F) is semidet.
%
% Compound Functor.
%
compound_functor(Compound,F):-compound(Compound),nonvar(Compound),get_functor(Compound,F).


%= 	 	 

%% not_variant( ?G, ?GG) is semidet.
%
% Not Variant.
%
not_variant(G,GG):-
 not(not((
  %numbervars(G,0,_),
  %numbervars(GG,0,_),
  G\=@=GG))).

% ========================================
% Shared Preds
% ========================================


% TODO: canonicalize clauses first!

%= 	 	 

%% with_kb_assertions( ?With, :Goal) is semidet.
%
% Using Knowledge Base Assertions.
%
with_kb_assertions([],Call):- !,Call.
with_kb_assertions([With|MORE],Call):-!,with_kb_assertions(With,with_kb_assertions(MORE,Call)).
with_kb_assertions(With,Call):-
   setup_call_cleanup(asserta(With,Ref),Call,erase_safe(With,Ref)).



%= 	 	 

%% world_clear( ?Named) is semidet.
%
% World Clear.
%
world_clear(Named):-fmt('Clearing world database: ~q.~n',[Named]).




%= 	 	 

%% get_pifunctor( ?Head, ?PHead) is semidet.
%
% Get Pifunctor.
%
get_pifunctor(t,PHead):-!,between(1,9,A),functor(PHead,t,A).
get_pifunctor(isa,isa(_,_)).
get_pifunctor(baseKB:isa/2,baseKB:isa(_,_)).
get_pifunctor(Head,PHead):- get_pifunctor(Head,PHead,_,_),!.

%= 	 	 

%% get_pifunctor3( ?Head, ?PHead, ?F) is semidet.
%
% Get Pifunctor.
%
get_pifunctor3(Head,PHead,F):- get_pifunctor(Head,PHead,F,_).


%= 	 	 

%% get_pifunctor( ?Head, ?PHead, ?F, ?A) is semidet.
%
% Get Pifunctor.
%
get_pifunctor(M:Head,M:PHead,F,A):-atom(M),!,get_pifunctor(Head,PHead,F,A).
get_pifunctor((F/A),PHead,F,A):- integer(A),!,must(atom(F)),functor(PHead,F,A).
get_pifunctor(Head,PHead,F,A):-atom(Head),F=Head,!,arity_no_bc(F,A),must(integer(A)),functor(PHead,F,A).
%get_pifunctor(Head,PHead,F,A):-atom(Head),ensure_arity(Head,A),!,get_pifunctor(Head/A,PHead,F,A).
get_pifunctor(Head,PHead,F,A):-var(Head),!,(atom(F)),must(ensure_arity(F,A)),functor(PHead,F,A),ignore(PHead=Head).
get_pifunctor(Head,PHead,F,A):-get_functor(Head,F,A),functor(PHead,F,A),ignore(PHead=Head),!.


%= 	 	 

%% rescan_meta_argtypes( ?MT) is semidet.
%
% Rescan Meta Argument Types.
%
rescan_meta_argtypes(MT):- functor(MT,F,A),functor(M,F,A),MT=..[F|ARGST],M=..[F|ARGS],forall(clause_u(M,_),
  maplist(admit_str_type(t),ARGS,ARGST)),!.

%= 	 	 

%% rescan_argIsa( ?VALUE1, ?VALUE2, ?VALUE3) is semidet.
%
% rescan Argument  (isa/2).
%
rescan_argIsa(F,N,Type):- ignore(( arity_no_bc(F,A), functor(M,F,A),forall((clause_u(M,_),arg(N,M,E)),admit_str_type(t,E,Type)))),!.


%= 	 	 

%% deduceEachArgType( ?Var) is semidet.
%
% Deduce Each Argument Type.
%
deduceEachArgType(P):- doArgType(t,deduceEachArgType,P).

deduceEachArgType(Str,F,N,A):- ignore((clause_u(argIsa(F,N,Type)),\+ \+ admit_str_type(Str,A,Type), fail)).

%% admit_type(Str, ?M, ?VALUE2) is semidet.
%
% Deduce Each Argument With Type.
%
admit_str_type(Str0,M,MT):- str_to_str(Str0,Str), admit_type(Str,M,MT,Result),!,(atom(Result)->true;ain_expanded(t(Str,Result))).

str_to_str(~(~(Was)),Was):-!.
str_to_str(Str0,Str):- functor(Str0,Str,_).

admit_type(_Str,M,_,exists):- (is_ftVar(M);number(M)),!.
admit_type(_Str,_,MT,exists):- (is_ftVar(MT);MT=ftTerm;call_u(ttExpressionType(MT))),!.
admit_type(_Str,M,M,exists):-!.
admit_type(Str,M,MT,exists):- compound(M),!, (compound(MT)->(( M =..ARGS,MT =..ARGST,
  maplist(admit_str_type(Str),ARGS,ARGST))); doArgType(Str,deduceEachArgType,M)).
admit_type(_Str,M,tSpatialThing,exists):-a(tSpatialThing,M),!.
admit_type(_Str,M,tTemporalhing,exists):-a(tTemporalThing,M),!.
admit_type(_Str,M,MT,exists):- call_u(isa(M,MT)),!.
admit_type(_Str,M,MT,isa(M,MT)).




:- meta_predicate doArgType(+,4,*).
:- meta_predicate doArgType(+,4,?,*,*).
:- meta_predicate doEachArg(+,4,?,?,*).



doArgType(_Str,_DAT,Var):- \+ compound(Var),!.
doArgType(_Str,deduceEachArgType,meta_argtypes(MT)):- !, rescan_meta_argtypes(MT).
doArgType(_Str,deduceEachArgType,tRelation(M)):-compound(M),functor(M,F,A),ain_expanded(meta_argtypes(M)),ain(tRelation(F)),ain(arity(F,A)).
doArgType(Str,DAT,M):-functor(M,F,A),M=..[F|ARGS],doArgType(Str,DAT,F,A,ARGS).

%= 	 	 

%% doArgType(Str,:DAT, ?F, ?VALUE2, ?VALUE3) is semidet.
%
% Deduce Each Argument Type.
%

doArgType(_Str,_,Isa,_,_):- skipped_doArgType(Isa),!.
doArgType(~t,DAT,poss,_,[E]):- !,doArgType(poss,DAT,E),!.
doArgType(Str,DAT,poss,_,[E]):- !,doArgType(poss(Str),DAT,E),!.
doArgType(Str,DAT, ~, _,[E]):- !,doArgType(~(Str),DAT,E),!.
doArgType(Str,DAT,_,_,[E]):- !,doArgType(Str,DAT,E),!.
doArgType(_Str,_DAT,HOLDS,_,[F|_]):- is_holds_functor(HOLDS), var(F),!.
doArgType(Str,DAT,HOLDS,A,[F|ARGS]):- is_holds_functor(HOLDS), A2 is A-1, doArgType(Str,DAT,F,A2,ARGS).
doArgType(Str,DAT,F,_,ARGS):- upcase_atom(F,F),!,maplist(doArgType(Str,DAT),ARGS).

% doArgType(Str,DAT,argQuotedIsa,3,[_F,_N,_Type]):-!.
% doArgType(Str,DAT,argIsa,3,[F,N,Type]):- ttExpressionType(Type),ain(argQuotedIsa(F,N,Type)),!.
% doArgType(Str,DAT,argIsa,3,[F,N,Type]):- rescan_argIsa(F,N,Type),fail.
doArgType(Str,DAT,F,A,ARGS):- DAT==deduceEachArgType,
  functor(MT,F,A),meta_argtypes(MT),MT =..[_|ARGST],interargIsa_match(ARGS,ARGST),!,
  nop(if_main(dmsg(doArgType(Str,DAT,F,ARGS,ARGST)))),
  maplist(admit_str_type(Str),ARGS,ARGST).
doArgType(Str,DAT,F,_,ARGS):-doEachArg(Str,DAT,F,1,ARGS),!.

%% doEachArg(Str,DAT, ?F, ?N, :TermA) is semidet.
%
% deduce each Argument With Argument  (isa/2).
%
doEachArg(_Str,_ ,_,_,[]).
doEachArg(Str,DAT,F,N,[A|RGS]):-
 (compound(A)-> doArgType(Str,DAT,A) ; true),
 ignore(call(DAT,Str,F,N,A)),
 N2 is N+1,doEachArg(Str,DAT,F,N2,RGS),!.
   


skipped_doArgType(Var):- is_ftVar(Var),!.
skipped_doArgType(F):- clause_b(rtArgsVerbatum(F)).
skipped_doArgType(tRelation).
skipped_doArgType(meta_argtypes).
skipped_doArgType(isa).
skipped_doArgType(call).
skipped_doArgType({}).
skipped_doArgType(==>).
skipped_doArgType(comment).
skipped_doArgType(argQuotedIsa).

interargIsa_match([],[]):-!,fail.
interargIsa_match([A|_],[T|_]):- \+ is_ftVar(A),\+ is_ftVar(T),\+ number(A), atom(A), isa(A,T),!.
interargIsa_match([_|ARGS],[_|ARGST]):- interargIsa_match(ARGS,ARGST).

%= 	 	 

%% if_main( :GoalG) is semidet.
%
% If Main.
%
if_main(G):-(thread_self(M),lmcache:thread_main(_,M))->G ; true.




addAdmittedArguments(P):- doArgType(t,add_admitted_argument,P).



dont_aquire_admitted_argument(admittedArgument).
dont_aquire_admitted_argument(isa).
dont_aquire_admitted_argument({}).
dont_aquire_admitted_argument( '/' ).
dont_aquire_admitted_argument(mudIsa).
dont_aquire_admitted_argument(safe_wrap).
dont_aquire_admitted_argument(arity).
dont_aquire_admitted_argument(mpred_prop).


% add_admitted_argument(Str,_,_,_).
add_admitted_argument(_,_,_,M):- \+ atom(M),!.
add_admitted_argument(_,_,1,_):-!.
add_admitted_argument(_,F,_,_):- dont_aquire_admitted_argument(F),!.
add_admitted_argument(_,F,N,M):- (var(M); number(M); \+ number(N) ; var(F)),!.
add_admitted_argument(Str,F,N,M):- must(add_admitted_argument0(Str,F,N,M)).

add_admitted_argument0(t,F,N,M):- !,add_admitted_argument1(t,F,N,M).
add_admitted_argument0(~t,F,N,M):- !,add_admitted_argument1(~t,F,N,M).
add_admitted_argument0(poss(t),F,N,M):-!,add_admitted_argument1(poss,F,N,M).
add_admitted_argument0(t(poss(t)),F,N,M):-!,add_admitted_argument1(poss,F,N,M).
add_admitted_argument0(~(poss(t)),F,N,M):-!,add_admitted_argument1(~t,F,N,M).
add_admitted_argument0(~t(poss(t)),F,N,M):-!,add_admitted_argument1(~t,F,N,M).
add_admitted_argument0(~(poss(~t)),F,N,M):-!,add_admitted_argument1(t,F,N,M).
add_admitted_argument0(Str,F,N,M):- show_call(add_admitted_argument1(Str,F,N,M)).

add_admitted_argument1(~t,F,N,M):- !,show_failure(baseKB:mpred_retract_supported_relations(admittedArgument(F,N,M))). 
add_admitted_argument1(t,F,N,M):- !,show_failure(baseKB:ain_fast(admittedArgument(F,N,M))).
add_admitted_argument1(poss,F,N,M):- \+ clause_b(feature_setting(admitted_arguments_modal,true)),!,add_admitted_argument1(t,F,N,M).
add_admitted_argument1(Str,F,N,M):- show_call(baseKB:ain_expanded(t(Str,admittedArgument(F,N,M)))).                                                      
 	 

%% side_effect_prone is semidet.
%
% Side Effect Prone.
%
side_effect_prone:- \+ t_l:noDBaseMODs(_), t_l:side_effect_ok.



:- meta_predicate(with_no_modifications(0)).

%= 	 	 

%% with_no_modifications( :GoalCALL) is semidet.
%
% Using No Modifications.
%
with_no_modifications(CALL):-!,CALL.
with_no_modifications(CALL):-locally_tl(noDBaseMODs(_),CALL).

:- meta_predicate(with_no_db_hooks(0)).

%= 	 	 

%% with_no_db_hooks( :GoalCALL) is semidet.
%
% Using No Database Hooks.
%
with_no_db_hooks(CALL):-!,CALL.
with_no_db_hooks(CALL):-locally_tl(noDBaseHOOKS(_),CALL).

:- meta_predicate(with_fallbacks(0)).

%= 	 	 

%% with_fallbacks( :GoalCALL) is semidet.
%
% Using Fallbacks.
%
with_fallbacks(CALL):-locally_hide(t_l:infAssertedOnly(_),CALL).

:- meta_predicate(with_fallbacksg(0)).

%= 	 	 

%% with_fallbacksg( :GoalCALL) is semidet.
%
% Using Fallbacksg.
%
with_fallbacksg(CALL):-locally_hide(t_l:noRandomValues(_),CALL).

:- meta_predicate(with_no_fallbacksg(0)).

%= 	 	 

%% with_no_fallbacksg( :GoalCALL) is semidet.
%
% Using No Fallbacksg.
%
with_no_fallbacksg(CALL):-locally_tl(noRandomValues(_),CALL).

:- meta_predicate(with_no_fallbacks(0)).

%= 	 	 

%% with_no_fallbacks( :GoalCALL) is semidet.
%
% Using No Fallbacks.
%
with_no_fallbacks(CALL):-locally_tl(infAssertedOnly(_),CALL).


%= 	 	 

%% infSecondOrderCheck is semidet.
%
% Inf Second Order Check.
%
infSecondOrderCheck :- \+ t_l:infInstanceOnly(_), t_l:infSecondOrder.


%= 	 	 

%% infThirdOrderCheck is semidet.
%
% Inf Third Order Check.
%
infThirdOrderCheck:- t_l:infThirdOrder,!.
infThirdOrderCheck :- fail, infSecondOrderCheck, not(t_l:noRandomValues(_)).


:- thread_local t_l:fail_is_asserted/1.


%= 	 	 

%% with_fail_is_asserted( ?Temp, ?Goal) is semidet.
%
% Using Fail If Is A Asserted.
%
with_fail_is_asserted(Temp,Goal):-ground(Temp),!,Goal.
with_fail_is_asserted(Temp,Goal):-locally_tl(fail_is_asserted(Temp),Goal).



%= 	 	 

%% skip_is_asserted_expansion( ?VALUE1) is semidet.
%
% Skip If Is A Asserted Expansion.
%
skip_is_asserted_expansion(_).


%= 	 	 

%% is_source_proof( ?VALUE1) is semidet.
%
% If Is A Source Proof.
%
is_source_proof(_).


% ================================================
% fact_checked/2, fact_loop_checked/2
% ================================================
:- meta_predicate(fact_checked(?,0)).


%= 	 	 

%% fact_checked( ?Fact, :Goal) is semidet.
%
% Fact Checked.
%
fact_checked(Fact,Call):- not(ground(Fact)),!,no_loop_check(lc_tcall(Call),clause_u(Fact)).
fact_checked(Fact,_):- is_known_false0(Fact),!,fail.
fact_checked(Fact,_):- is_known_trew(Fact),!.
fact_checked(Fact,Call):- no_loop_check(lc_tcall(Call),clause_u(Fact)).

:- meta_predicate(fact_loop_checked(+,0)).

%= 	 	 

%% fact_loop_checked( +Fact, :Goal) is semidet.
%
% Fact Loop Checked.
%
fact_loop_checked(Fact,Call):- no_repeats(fact_checked(Fact,Call)).



% ================================================
% hooked_assert/1 hooked_retract/1
% ================================================
/*
ensure_predicate_reachable(_,_):- fast_mud,!.
%ensure_predicate_reachable(M,C):-functor(C,F,A),ensure_predicate_reachable(M,C,F,A),fail.
ensure_predicate_reachable(_,_):- is_release,!.
ensure_predicate_reachable(M,C):-once((predicate_property(C,imported_from(Other)),M\=Other,
                                       source_context_module(CM),
                                       dmsg(wrong_import_module(M,Other:C,from(CM))),
                                       ignore(delete_import_module(CM,Other)),
                                       '@'((M:dynamic(C),M:export(C)),M),baseKB:zz2import(M:C))),fail.
ensure_predicate_reachable(_,_).
*/


%= 	 	 

%% singletons_throw_else_fail( :GoalC) is semidet.
%
% Singletons Throw Else Fail.
%
singletons_throw_else_fail(C):- fail,not_is_release,contains_singletons(C),!,(test_tl(t_l:already_in_file_term_expansion) -> (dmsg(contains_singletons(C))); dmsg(trace_or_throw(contains_singletons(C)))),fail.

%= 	 	 

%% nonground_throw_else_fail( ?C) is semidet.
%
% Nonground Throw Else Fail.
%
nonground_throw_else_fail(C):- not_is_release,not(ground(C)),!,( (test_tl(t_l:already_in_file_term_expansion) ->dmsg(not_ground(C)); trace_or_throw(not_ground(C)))),fail.

% ================================================
% mpred_modify/2
% ================================================


%= 	 	 

%% with_logical_functor( ?UPARAM1, ?G, :PRED1Call) is semidet.
%
% Using Logical Functor.
%
with_logical_functor(not,[G],Call):- !, not(call(Call,G)).
with_logical_functor(_And,[G],Call):- !, call(Call,G).
with_logical_functor(And,[G|T],Call):-
   DO =..[And,call(Call,G),with_logical_functor(And,T,Call)],
   call(DO).



%= 	 	 

%% requires_storage( ?C, ?Why) is semidet.
%
% Requires Storage.
%
requires_storage((Head :- Body),Why):- nonvar(Head),!, requires_storage(Head,Body,Why).
requires_storage(C,Why):- requires_storage(C,true,Why).


%= 	 	 

%% requires_storage( ?G, ?VALUE2, ?Why) is semidet.
%
% Requires Storage.
%
requires_storage(G,_,Why):-get_functor(G,F),!,special_head(G,F,Why),!.
requires_storage(_,_,t_l:consulting_sources):- t_l:consulting_sources,mpred_may_expand,!.
% requires_storage(_,_,t_l:consulting_sources):- t_l:consulting_sources,in_file_expansion.


%= 	 	 

%% special_wrapper_functor( ?VALUE1, ?VALUE2) is semidet.
%
% Special Wrapper Functor.
%
special_wrapper_functor(call_mpred_body,direct_to_prolog).
special_wrapper_functor(body_req,direct_to_prolog).
special_wrapper_functor(baseKB:mpred_provide_setup,direct_to_prolog).
special_wrapper_functor(call_provided_mpred_storage_op,direct_to_prolog).
special_wrapper_functor(loop_check,meta).
special_wrapper_functor(loop_check_term,meta).
%special_wrapper_functor(pttp_req).



%= 	 	 

%% make_body_clause( ?Head, ?Body, ?Body) is semidet.
%
% Make Body Clause.
%
make_body_clause(_Head,Body,Body):-atomic(Body),!.
make_body_clause(_Head,Body,Body):-special_wrapper_body(Body,_Why),!.
make_body_clause(Head,Body,call_mpred_body(Head,Body)).


%= 	 	 

%% special_head( ?VALUE1, ?VALUE2, ?VALUE3) is semidet.
%
% Special Head.
%
special_head(_,F,Why):-special_head0(F,Why),!,show_failure(why,\+(a(prologDynamic,F))).

%= 	 	 

%% special_head0( ?F, ?VALUE2) is semidet.
%
% Special Head Primary Helper.
%
special_head0(F,ttRelationType):-a(ttRelationType,F),!.
special_head0(F,functorDeclares):-a(functorDeclares,F),!.
special_head0(F,functorIsMacro):-a(functorIsMacro,F),!.
special_head0(F,pfcControlled):-a(pfcControlled,F).
special_head0(isa,isa).
special_head0(F,tCol):-a(tCol,F),!.
special_head0(F,prologHybrid):-a(prologHybrid,F).





%= 	 	 

%% special_wrapper_body( ?W, ?Why) is semidet.
%
% Special Wrapper Body.
%
special_wrapper_body(W,Why):-get_body_functor(W,F,_),!,special_wrapper_functor(F,Why).


%= 	 	 

%% get_body_functor( :TermBDY, ?BF, ?A) is semidet.
%
% Get Body Functor.
%
get_body_functor(Var,_,call):-var(Var),!.
get_body_functor((M:BDY),BF,A):-atom(M),!,get_body_functor(BDY,BF,A).
get_body_functor((!,BDY),BF,A):-!,get_body_functor(BDY,BF,A).
get_body_functor(call(BDY),BF,A):-!,get_body_functor(BDY,BF,A).
get_body_functor(once(BDY),BF,A):-!,get_body_functor(BDY,BF,A).
get_body_functor((BDY1;BDY2),BF,A):-!, (get_body_functor(BDY1,BF,A);get_body_functor(BDY2,BF,A)).
get_body_functor((BDY1,BDY2),BF,A):-!, (get_body_functor(BDY1,BF,A);get_body_functor(BDY2,BF,A)).
get_body_functor(BDY,BF,A):-get_functor(BDY,BF,A).


% ================================================
% CHECKED 
% ================================================


% -  del(RetractOne) 

%= 	 	 

%% del( -C) is semidet.
%
% Remove/erase.
%
del(C):- fully_expand(change(retract,a),C,C0),map_first_arg(del0,C0).

%= 	 	 

%% del0( :GoalC0) is semidet.
%
% Remove/erase Primary Helper.
%
del0(C0):- call_u(C0),!,clr(C0),!.
del0(C0):- ireq(C0),!,idel(C0),!.
del0(C0):- call_u(C0),!,mdel(C0),!.


%= 	 	 

%% idel( ?C0) is semidet.
%
% Idel.
%
idel(C0):- dmsg(idel(C0)),mpred_modify(change( retract,a),C0), sanity(ireq(C0)->(dmsg(warn(incomplete_I_DEL(C0))),fail);true),!.
idel(C0):- dmsg(warn(failed(idel(C0)))),!,fail.


%= 	 	 

%% mdel( ?C0) is semidet.
%
% Mdel.
%
mdel(C0):- dmsg(mdel(C0)),mpred_modify(change( retract,one),C0), sanity(call_u(C0)->(dmsg(warn(incomplete_M_DEL(C0))),fail);true),!.
mdel(C0):- dmsg(warn(failed(mdel(C0)))),!,fail.

% -  clr(Retractall)
% clr(C0):- dmsg(clr(C0)),fail,mpred_modify(change(retract,all),/*to_exp*/(C0)),sanity(ireq(C0)->(dmsg(warn(incomplete_CLR(C0))));true).

%= 	 	 

%% clr( -P) is semidet.
%
% Remove/erase.
%
clr(P):- agenda_do_prequery,
  fully_expand(change(retract,all),P,PL),map_first_arg(clr0,PL).


%= 	 	 

%% clr0( :GoalP) is semidet.
%
% Remove/erase Primary Helper.
%
clr0(P):- 
  forall(on_x_debug(P), ((forall( mpred_remove(P), true)),nop((sanity((not(mpred_tms_supported(local,P)),must(\+(P)))))))).


% -  preq(Query) = query with P note

%= 	 	 

%% preq( ?P, ?C0) is semidet.
%
% Preq.
%
preq(P,C0In):- fully_expand(query(preq),C0In,C0), agenda_do_prequery,!,no_repeats(C0,mpred_op(query(t,P),C0)).

% -  call_u(Query) = Normal query

%= 	 	 

%% req_old2( ?C0) is semidet.
%
% Req Old Extended Helper.
%
req_old2(C0):- nop(dmsg(call_u(C0))), !,preq(call_u,/*to_exp*/(C0)).

% -  call_u(Query) = Forced Full query

%= 	 	 

%% mreq_old2( ?C0) is semidet.
%
% Mreq Old Extended Helper.
%
mreq_old2(C0):- nop(dmsg(call_u(C0))), agenda_rescan_for_module_ready,
   no_loop_check(locally([ - t_l:infInstanceOnly(_), - t_l:infAssertedOnly(_), - t_l:noRandomValues(_)],
     preq(must,/*to_exp*/(C0)))).

% -  ireq(Query) = Normal query (May not use second order logic) (must be asserted on isntance) (used mainly by 2nd order logic to avoid looping)

%= 	 	 

%% ireq( ?C0) is semidet.
%
% Ireq.
%
ireq(C0):- nop(dmsg(ireq(C0))), 
  agenda_rescan_for_module_ready,
   no_loop_check(locally_tl(infInstanceOnly(_), locally_tl(infAssertedOnly(_),locally_tl(noRandomValues(_),preq(ireq,/*to_exp*/(C0)))))).

% -  call_props(Obj,QueryPropSpecs)

%= 	 	 

%% call_props( ?Obj, ?PropSpecs) is semidet.
%
% Call Props.
%
call_props(Obj,PropSpecs):- ireq(props(Obj,PropSpecs)).

%= 	 	 

%% iprops( ?Obj, ?PropSpecs) is semidet.
%
% Iprops.
%
iprops(Obj,PropSpecs):- ireq(props(Obj,PropSpecs)).

:- asserta((baseKB:props(Obj,PropSpecs):-  ireq(props(Obj,PropSpecs)))).



:- was_export(forall_setof/2).

%= 	 	 

%% forall_setof( :GoalForEach, :Goal) is semidet.
%
% Forall Setof.
%
forall_setof(ForEach,Call):-
   findall(ForEach,ForEach,ForEachAll),
   list_to_set(ForEachAll,Set),!,
   ignore(forall(member(ForEach,Set),Call)).


:- thread_local add_thread_override/1.
% t_l:add_thread_override(A):-add_from_macropred(A),!.

/*
:- was_export(((ain)/1)).
% :- mpred_trace_nochilds((ain)/1).
ain(A):- throw(depricated), var(A),!,trace_or_throw(var_add(A)).
ain(end_of_file):-!.
ain(grid_key(KW=COL)):- !, ain(typeHasGlyph(COL,KW)).
% ain(Term):- unnumbervars(Term,TermE), Term \=@= TermE,!,ain(TermE).
% ain(Term):-  forall(do_expand_args(isEach,Term,O),add_0(O)),!.
ain(TermIn):- fully_expand(change(assert,ain),TermIn,Term),add_0(Term).

add_0(A):-  throw(depricated),  is_ftVar(A),!,trace_or_throw(var_add(A)).
add_0(((H1,H2):-B)):-!,add_0((H1:-B)),add_0((H2:-B)).
add_0(((H1,H2))):-!,add_0((H1)),add_0((H2)).
add_0(dynamic(Term)):- !,must(get_arity(Term,F,A)), must(dynamic(F/A)).
add_0(A):- A =(:-(Term)), !, call_u(Term).
% add_0(C0):-check_override(ain(C0)),!.
% add_0(Skipped):- ground(Skipped),implied_skipped(Skipped),!. % ,dmsg(implied_skipped(Skipped)).
%add_0(C0):- ignore((ground(C0),asserta(baseKB:already_added_this_round(C0)))),!,must(ain_fast(C0)),!.
add_0(C0):- must(ain_fast(C0)),!.
add_0(A):-trace_or_throw(fmt('ain/1 is failing ~q.',[A])).
*/


%= 	 	 

%% implied_skipped( ?C0) is semidet.
%
% Implied Skipped.
%
implied_skipped(genls(C0,C1)):-C0==C1.
implied_skipped(props(_,[])).
%implied_skipped(Skipped):-compound(Skipped), not(functor(Skipped,_,1)),fail, (t(Skipped);out_of_mpred_t(Skipped)).
implied_skipped(Skipped):-baseKB:already_added_this_round(Skipped),(clause_u(Skipped)).


:- was_export(ain/1).
% -  ain(Assertion)
% ain_fast(C0):- must_det((ain_fast(C0), xtreme_debug(once(ireq(C0);(with_all_dmsg((debug(blackboard),show_failure(why,ain_fast(C0)),rtrace(ain_fast(C0)),dtrace(ireq(C0))))))))),!.

%= 	 	 

%% ain( ?Term) is semidet.
%
% Add Fast.
%
% ain(Term):-mpred_numbervars_with_names(Term),ain(Term),!. % ,xtreme_debug(ireq(C0)->true;dmsg(warn(failed_ireq(C0)))))),!.

% -  upprop(Obj,PropSpecs) update the properties

%= 	 	 

%% upprop( ?Obj, ?PropSpecs) is semidet.
%
% Upprop.
%
upprop( _O,PropSpecs):- PropSpecs==[],!.
upprop(Obj,PropSpecs):- upprop(props(Obj,PropSpecs)).

%= 	 	 

%% upprop( ?C0) is semidet.
%
% Upprop.
%
upprop(C0):- ain_expanded(C0).
% -  padd(Obj,Prop,Value)

%= 	 	 

%% padd( ?Obj, ?PropSpecs) is semidet.
%
% Padd.
%
padd( _O,PropSpecs):- PropSpecs==[],!.
padd(Obj,PropSpecs):- ain_expanded((props(Obj,PropSpecs))).
% -  padd(Obj,Prop,Value)

%= 	 	 

%% padd( ?Obj, ?Prop, ?Value) is semidet.
%
% Padd.
%
padd(Obj,Prop,Value):- ain_expanded((t(Prop,Obj,Value))).
% -  props(Obj,Prop,Value)

%= 	 	 

%% prop( ?Obj, ?Prop, ?Value) is semidet.
%
% Prop.
%
prop(Obj,Prop,Value):- call_u(t(Prop,Obj,Value)).
% -  prop_or(Obj,Prop,Value,OrElse)

%= 	 	 

%% prop_or( ?Obj, ?Prop, ?Value, ?OrElse) is semidet.
%
% Prop Or.
%
prop_or(Obj,Prop,Value,OrElse):- one_must(ireq(t(Prop,Obj,Value)),Value=OrElse).



% ================================================
% db_assert_sv/3
% ================================================

/*
% update_single_valued_arg(P,N):- arg(N,P,UPDATE),replace_arg(P,N,OLD,Q),
  (is_relative(UPDATE)->
     must_det_l((Q,update_value(OLD,UPDATE,NEW),\+ is_relative(NEW), replace_arg(Q,N,NEW,R),enqueue(\+Q),enqueue(R)));
     forall((Q,UPDATE\=OLD),mpred_enqueue(\+Q))),!.
*/

/*

% assert_with to change(CA1,CB2) singlevalue pred
:- was_export((db_assert_sv/4)).
%db_assert_sv(_Must,C,F,A):- throw_if_true_else_fail(contains_singletons(C),db_assert_sv(C,F,A)).

db_assert_sv(C):- get_functor(C,F,A), db_assert_sv(must,C,F,A),!.

db_assert_sv(Must,C,F,A):-  ignore(( loop_check(db_assert_sv_ilc(Must,C,F,A),true))).

:- was_export((db_assert_sv_ilc/4)).
db_assert_sv_ilc(Must,C,F,A):- arg(A,C,UPDATE),is_relative(UPDATE),db_assert_sv_now(Must,C,F,A,UPDATE),!.

:- was_export(db_assert_sv_now/5).

db_assert_sv_now(Must,C,F,A,    +UPDATE):-!,  db_assert_sv_update(Must,C,F,A,+UPDATE).
db_assert_sv_now(Must,C,F,A,    -UPDATE):-!,  db_assert_sv_update(Must,C,F,A,-UPDATE).
db_assert_sv_now(Must,C,F,A,    V):- is_relative(V),!,  db_assert_sv_update(Must,C,F,A,+V).
db_assert_sv_now(Must,C,F,A, UPDATE):- has_free_args(db_assert_sv_now(Must,C,F,A, UPDATE)),!,trace_or_throw(var_db_assert_sv_now(Must,C,F,A, UPDATE)).
db_assert_sv_now(Must,C,F,A, NEGREPLACE):- number(NEGREPLACE),NEGREPLACE<0, !,db_assert_sv_replace(Must,C,F,A,NEGREPLACE).
db_assert_sv_now(Must,C,F,A, -(+(UPDATE))):-!,db_assert_sv_update(Must,C,F,A,-UPDATE).
db_assert_sv_now(Must,C,F,A, +(-(UPDATE))):-  db_assert_sv_update(Must,C,F,A,-UPDATE).
db_assert_sv_now(Must,C,F,A, REPLACE):- db_assert_sv_replace(Must,C,F,A, REPLACE).

:- was_export(db_assert_sv_update/5).
db_assert_sv_update(Must,C,F,A,UPDATE):-
   replace_arg(C,A,OLD,COLD),
   % prefer updated values to come from instances but will settle with anything legal
   quietly(must((once(ireq(COLD);call_u(COLD)),ground(COLD)))),
   update_value(OLD,UPDATE,NEW),!,
   db_assert_sv_replace(Must,C,F,A,NEW),!.

:- was_export(db_assert_sv_replace/5).

:- style_check(-singleton).
% db_assert_sv_replace_noisey_so_disabled
db_assert_sv_replace(_Must,C,_,A,NEW):- fail,
   replace_arg(C,A,_,CBLANK),
   clr(CBLANK),
   replace_arg(C,A,NEW,CNEW),
   db_must_asserta_confirmed_sv(CNEW,A,NEW),!.

db_assert_sv_replace(Must,C,F,A,NEW):- 
   replace_arg(C,A,OLD,COLD),
   replace_arg(C,A,NEW,CNEW),
   quietly(ignore(ireq(COLD))),
   must_det(db_assert_sv_replace_with(Must,C,F,A,COLD,CNEW,OLD,NEW)),!.

db_assert_sv_replace_with(Must,C,F,A,COLD,CNEW,OLD,NEW):- var(OLD),!,   
   dmsg(db_assert_sv(COLD,'__add__',CNEW)),
   % replace_arg(C,A,_,CBLANK),clr(CBLANK),
   db_must_asserta_confirmed_sv(CNEW,A,NEW),!.

db_assert_sv_replace_with(Must,C,F,A,COLD,CNEW,OLD,NEW):- OLD =@= NEW,!.
db_assert_sv_replace_with(Must,C,F,A,COLD,CNEW,OLD,NEW):- unify_with_occurs_check(OLD,NEW),!,dmsg(db_assert_sv_same(COLD,'__unify_with_occurs_check__',CNEW)).
db_assert_sv_replace_with(Must,C,F,A,COLD,CNEW,OLD,NEW):- equals_call(OLD,NEW),!,dmsg(db_assert_sv_same(COLD,'__same__',CNEW)),trace_or_throw(dtrace).
db_assert_sv_replace_with(Must,C,F,A,COLD,CNEW,OLD,NEW):-
   dmsg(db_assert_sv(COLD,'__replace__',CNEW)),
   quietly((ignore(show_failure(why,(clr(COLD), not(ireq(COLD))))))),
   %replace_arg(C,A,_,CBLANK),must_det(clr(CBLANK)),clr(CBLANK),   
   db_must_asserta_confirmed_sv(CNEW,A,NEW),!.

*/

:- style_check(+singleton).



%= 	 	 

%% equals_call( ?X, ?Y) is semidet.
%
% Equals Call.
%
equals_call(X,Y):-unify_with_occurs_check(X,Y),!.
equals_call(X,Y):-once((any_to_string(X,XX),any_to_string(Y,YY))),unify_with_occurs_check(XX,YY),!.
equals_call(X,Y):-once((to_word_list(X,XX),to_word_list(Y,YY))),unify_with_occurs_check(XX,YY),!.
equals_call(X,Y):-compound(X),compound(Y),once((correctArgsIsa(X,XX),correctArgsIsa(Y,YY))),unify_with_occurs_check(XX,YY),!.



%= 	 	 

%% confirm_hook( :TermCNEW) is semidet.
%
% Confirm Hook.
%
confirm_hook(CNEW:NEW=@=CNOW:NOW):-
   sanity(var(NOW)),               
   quietly((once(ireq(CNOW)))),
   CNEW:NEW=@=CNOW:NOW,!.

confirm_hook(CNEW:NEW=@=CNOW:NOW):-
   dmsg(warn(failed_i_a_req(CNOW,expected(CNEW)))),   
   dtrace((sanity((call_u(CNOW),(CNEW:NEW=@=CNOW:NOW))))),!.



% Expect CNEW to be what is found

%= 	 	 

%% db_must_asserta_confirmed_sv( :GoalCNEW, ?A, ?NEW) is semidet.
%
% Database Must Be Successfull Asserta Confirmed Sv.
%
db_must_asserta_confirmed_sv(CNEW,A,NEW):- 
   replace_arg(CNEW,A,NOW,CNOW),
   sanity(not(singletons_throw_else_fail(CNEW))),
   mpred_modify(change(assert,sv),CNEW),!,
   ain_expanded(CNEW),
   sanity(confirm_hook(CNEW:NEW=@=CNOW:NOW)),!.

db_must_asserta_confirmed_sv(CNEW,A,NEW):-dmsg(unconfirmed(db_must_asserta_confirmed_sv(CNEW,A,NEW))).



%= 	 	 

%% test_expand_units( ?IN) is semidet.
%
% Test Expand Units.
%
test_expand_units(IN):-fully_expand(query(t,must),IN,OUT),dmsg(test_expand_units((IN->OUT))).






%% second_order( ?VALUE1, ?VALUE2) is semidet.
%
% Second Order.
%
second_order(_,_):-fail.

:- meta_predicate(deducedSimply(*)).
:- was_export(deducedSimply/1).



%% deducedSimply( :Goal) is semidet.
%
% Deduced Simply.
%
deducedSimply(Call):- clause(deduce_facts(Fact,Call),Body),\+ clause_u((Call)),nonvar(Fact),Body,dmsg((deducedSimply2(Call):-Fact)),!,show_call(why,(clause_u(Fact),ground(Call))).

% deducedSimply(Call):- clause(deduce_facts(Fact,Call),Body),nonvar(Fact),Body,ground(Call),dmsg((deducedSimply1(Call):-Fact)),show_call(why,(clause_u(Fact),ground(Call))).

:- meta_predicate(mpred_op(?,+)).



%% mpred_op( ?Op, ?H) is semidet.
%
% Managed Predicate Oper..
%
mpred_op(Op,     H ):- (var(Op);var(H)),!,trace_or_throw(var_database_call(Op,  H )).
mpred_op(clause_u,H):-!,clause_u(H).
mpred_op(Op,     H ):- once(fully_expand(Op,H,HH)),H\=@=HH,!,mpred_op(Op, HH).
mpred_op(~(_,Op),  H ):- !, show_call(why, \+ (mpred_op(Op,  H ))).
mpred_op(change(assert,Op),H):-!,must(mpred_modify(change(assert,Op),H)),!.
mpred_op(change(retract,Op),H):-!,must(mpred_modify(change(retract,Op),H)),!.
mpred_op(query(t,Ireq),  H ):-!, mpred_op(Ireq,H).
mpred_op(query(Dbase_t,_Ireq),  H ):-!, mpred_op(Dbase_t,H).
mpred_op(call(Op),H):-!,mpred_op(Op,H).
mpred_op(Op,((include(A)))):- locally_hide(t_l:already_in_file_term_expansion,mpred_op(Op,((load_data_file(A))))),!.
mpred_op(Op, call(H)):- nonvar(H),!, mpred_op(Op,H).
mpred_op(Op,  not(H)):- nonvar(H),!, mpred_op(~(not,Op),H).
mpred_op(Op,'\\+'(H)):- nonvar(H),!, mpred_op(~(('\\+'),Op),H).
mpred_op(Op,    ~(H)):- nonvar(H),!, mpred_op(~(~,Op),H).
mpred_op(Op,     {H}):- nonvar(H),!, mpred_op(Op,H).

mpred_op(must,Call):- !,must(mpred_op(call_u,Call)).
mpred_op(once,Call):- !,once(mpred_op(call_u,Call)).

mpred_op(assertedOnly,Call):- !,locally_tl(infInstanceOnly(Call),mpred_op(call_u,Call)).
mpred_op(_ , clause(H,B) ):- !, clause_u(H,B).
mpred_op(_ , clause(H,B,Ref) ):- !,  clause_u(H,B,Ref).
mpred_op(_ , (H :- B) ):- !, clause_u(H,B).
mpred_op(clauses(Op),  H):-!,mpred_op((Op),  H).
mpred_op(_,C):- call_u(C).

:- was_export(whenAnd/2).
:- module_transparent(whenAnd/2).



%% whenAnd( :GoalA, :GoalB) is semidet.
%
% When And.
%
whenAnd(A,B):-A,ground(B),once(B).


% =======================================
% Transforming DBASE OPs
% ========================================




%% reduce_mpred_op( ?Op, ?Op2) is semidet.
%
% Reduce Managed Predicate Oper..
%
reduce_mpred_op(Op,Op2):-must(quietly(transitive(how_to_op,Op,Op2))),!.
reduce_mpred_op(A,A).




%% how_to_op( ?HowOP, ?HowOP) is semidet.
%
% How Converted To Oper..
%
how_to_op(assert(a),asserta_new).
how_to_op(assert(z),assertz_if_new).
how_to_op(retract(one),retract).
how_to_op(retract(all),retract_all).
how_to_op(retract(all),retractall).
how_to_op(change(assert,z),assertz_if_new).
how_to_op(change(assert,_),asserta_if_new).
how_to_op(change(retract,one),retract).
how_to_op(change(retract,_),retract_all).
how_to_op(asserta,asserta_new).
how_to_op(assertz,assertz_if_new).
how_to_op(call(W),W).
how_to_op(clauses(W),W).
how_to_op(assert,assert_if_new).
how_to_op(assert(_),asserta_new).
how_to_op(retract(_),retract_all).
how_to_op(conjecture,call).
how_to_op(query(t, call_u),call_u).
how_to_op(query(t, Req),Req).
how_to_op(change(Op,HOW),O):- !, O=..[Op,HOW].
how_to_op(HowOP,HowOP).





%% lookup_inverted_op( ?VALUE1, ?VALUE2, +OUT3) is semidet.
%
% Lookup Inverted Oper..
%
lookup_inverted_op(retract,assert,-).
lookup_inverted_op(retractall,assert,-).
lookup_inverted_op(assert_if_new,retract,+).
lookup_inverted_op(assert_new,retract,+).
lookup_inverted_op(assertz_if_new,retract,+).
lookup_inverted_op(assertz_new,retract,+).
lookup_inverted_op(asserta_if_new,retract,+).
lookup_inverted_op(asserta_new,retract,+).


%= 	 	 

%% mpred_modify( ?Op, ?G) is semidet.
%
% Managed Predicate Modify.
%
mpred_modify(Op,                 G):- (var(Op);var(G)),!,trace_or_throw(var_database_modify_op(Op,  G )).
mpred_modify(Op,                 G):- \+ skip_is_asserted_expansion(G),G\=meta_argtypes(_),full_transform_warn_if_changed(Op,G,GG),not_variant(G,GG),!,mpred_modify(Op, GG ),!.
mpred_modify(_,  (:-include(FILE))):- !,must(ensure_mpred_file_loaded(FILE)).
mpred_modify(Op,  (:-(G))         ):- !,must(with_assert_op_override(Op,on_x_debug(G))).
mpred_modify(P,                  G):- t_l:noDBaseMODs(_),!,dmsg(noDBaseMODs(P,G)).
%mpred_modify(Op,                 G):- mpred_head_expansion(clause,G,GG),not_variant(G,GG),database_modify_0(Op, GG ),!.
mpred_modify(Op,                 G):- database_modify_0(Op,G ),!.
mpred_modify(Op,                 G):- trace_or_throw(unknown_database_modify(Op,G)).



%= 	 	 

%% database_modify_0( ?Op, ?M) is semidet.
%
% database modify  Primary Helper.
%
database_modify_0(Op,                       M:G):- atom(M),!, database_modify_0(Op,G).
database_modify_0(Op,                   (C1,C2)):- !, must(database_modify_0(Op,C1)), must(database_modify_0(Op,C2)).
database_modify_0(change(Assert,AorZ),(G:-TRUE)):- is_src_true(TRUE),!,database_modify_0(change(Assert,AorZ),G).
database_modify_0(change(retract,a),          G):- !, del(G).
database_modify_0(change(retract,one),        G):- !, del(G),!.
database_modify_0(change(retract,all),          G):- !, clr(G).
database_modify_0(change(retract,_  ),          G):- !, clr(G).
database_modify_0(change(assert,AZ),          G):- singletons_throw_else_fail(assert(AZ,G)).
database_modify_0(change(assert,AZ),          G):- database_modify_assert(change(assert,AZ),G).


% database_modify_assert(change(assert,_),        G):- ( \+ \+ clause_u(G)),must(variant(G,GG)),!.
% database_modify_assert(change(assert,AZ),       G):- expire_pre_change(AZ,GG),fail.


% ========================================
% only place ever should actual game database be changed from
% ========================================

%% database_modify_assert( :TermAorZ, ?G) is semidet.
%
% Database Modify Assert.
%
database_modify_assert(change(assert,_AorZ),       G):- !,ain_expanded(G).
database_modify_assert(change(assert,AorZ),       G):- 
 get_functor(G,F,_),!,
   (AorZ == a -> aina(G);
    AorZ == z ->  ainz(G);
    a(prologOrdered,F) -> database_modify_assert(change(assert,z),G);
    a(prologSingleValued,F) -> database_modify_assert(change(assert,a),G);
      aina(G)).


/*	 	 

%% hooked_asserta( +G) is semidet.
%
% Hooked Asserta.
%
hooked_asserta(G):- loop_check(mpred_modify(change(assert,a),G),aina(G)).

%% hooked_assertz( +G) is semidet.
%
% Hooked Assertz.
%
hooked_assertz(G):- loop_check(mpred_modify(change(assert,z),G),ainz(G)).

 	 
%% hooked_retract( +G) is semidet.
%
% Hooked Retract.
%
hooked_retract(G):-  Op = change(retract,a),
                   ignore(slow_sanity(ignore(show_failure(why,(mpred_op(clause_u,G)))))),
                   slow_sanity(not(singletons_throw_else_fail(hooked_retract(G)))),
                   slow_sanity(ignore(((ground(G), once(show_failure(why,(clause_u(G)))))))),
                   must_storage_op(Op,G),expire_post_change( Op,G),
                   sanity(ignore(show_failure(why,\+ clause_u((G))))),
                   loop_check(run_database_hooks_depth_1(change(retract,a),G),true).

 	 	 

%% hooked_retractall( +G) is semidet.
%
% Hooked Retractall.
%
hooked_retractall(G):- Op = change(retract,all),
                   slow_sanity(ignore(((ground(G), once(show_failure(why,(clause_u(G)))))))),
                   must_storage_op(Op,G),expire_post_change( Op,G),
                   sanity(ignore(show_failure(why,\+ clause_u((G))))),
                   loop_check(run_database_hooks_depth_1(change(retract,all),G),true).



*/ 	 	 

%% mpred_provide_storage_op( :TermOp, ?G) is semidet.
%
% Hook To [isa_lmconf:mpred_provide_storage_op/2] For Module Mpred_storage.
% Managed Predicate Provide Storage Oper..
%
baseKB:mpred_provide_storage_op(Op,G):- get_functor(G,F,A),baseKB:mpred_provide_storage_op(Op,G,F,A).


%= 	 	 

%% mpred_provide_storage_op( ?Op, ?G, ?F, ?A) is semidet.
%
% Hook To [baseKB:mpred_provide_storage_op/4] For Module Mpred_storage.
% Managed Predicate Provide Storage Oper..
%
baseKB:mpred_provide_storage_op(Op,G, F,_A):- a(pfcControlled,F),!,loop_check(prolog_mpred_provide_storage_op(Op,G)).
baseKB:mpred_provide_storage_op(Op,G, F,_A):- a(prologDynamic,F),!,loop_check(baseKB:mpred_provide_storage_op(Op,G)).
baseKB:mpred_provide_storage_op(Op,G,_F,_A):- loop_check(prolog_mpred_provide_storage_op(Op,G)).

%baseKB:mpred_provide_storage_op(Op,G):- (loop_check(isa_lmconf:mpred_provide_storage_op(Op,G))).
%baseKB:mpred_provide_storage_op(Op,G):- Op\=change(_,_), (call_no_cuts(baseKB:mpred_provide_storage_clauses(G,true,_Proof))).


%= 	 	 

%% must_storage_op( ?Op, ?G) is semidet.
%
% Must Be Successfull Storage Oper..
%
must_storage_op(Op,G):- doall(must(may_storage_op(Op,G))).


%= 	 	 

%% may_storage_op( ?Op, ?G) is semidet.
%
% May Storage Oper..
%
may_storage_op(Op,G):-call_no_cuts(baseKB:mpred_provide_storage_op(Op,G)).


% :- meta_predicate aina(+), ainz(+), del(+), clr(+).

:- meta_predicate del(-),clr(-). % ,ain(-). % ,call_u(-).

% Found new meta-predicates in iteration 1 (0.281 sec)
%:- meta_predicate mpred_modify(?,?,?,0).





%retract_all((G:-B)) :-!, forall(clause(G,B,Ref),erase(Ref)).

%= 	 	 

%% retract_all( ?HB) is semidet.
%
% Retract All.
%
retract_all(HB) :- ignore((retract(HB),fail)).



%= 	 	 

%% is_static_pred( ?Head) is semidet.
%
% If Is A Static Predicate.
%
is_static_pred(Head:-_):-!,predicate_property(Head,_),not(predicate_property(Head,dynamic)).
is_static_pred(Head):-  predicate_property(Head,static),!.
is_static_pred(Head):- predicate_property(Head,_), !, \+ (predicate_property(Head,dynamic)).
is_static_pred(Head):-  predicate_property(Head,meta_predicate),!.


%= 	 	 

%% prolog_mpred_provide_storage_op( ?VALUE1, ?VALUE2) is semidet.
%
% Prolog Managed Predicate Provide Storage Oper..
%
prolog_mpred_provide_storage_op(Op,G):- G\=isa(_,_), get_functor(G,F,A),call(call_u,mpred_prop(M,F,A,prologDynamic)),!, prolog_op(Op,M:G).
prolog_mpred_provide_storage_op(Op,G):- G\=isa(_,_), get_functor(G,F,A),\+ call(call_u,mpred_prop(_,F,A,prologHybrid)),!,current_predicate(_,M:G), prolog_op(Op,M:G).

%= 	 	 

%% use_if_modify_new is semidet.
%
% Use If Modify New.
%
use_if_modify_new:- current_predicate(assert_if_new/1).

%= 	 	 

%% prolog_op( ?Op, ?G) is semidet.
%
% Prolog Oper..
%
prolog_op(change(AR,Op), G):-ensure_dynamic(G),!,prolog_modify(change(AR,Op), G).

prolog_op(_,clause(G,B)):-!,clause_u(G,B).
prolog_op(_,clause(G,B,Ref)):-!,clause(G,B,Ref).

prolog_op(query(_,Op),G):-!,prolog_op(Op,G).
prolog_op(call(Op),G):-!, prolog_op(Op,G).
prolog_op(clauses(Op),G):-!, prolog_op(Op,G).
prolog_op(clause_u,(G:-B)):-!,clause_u(G,B).
prolog_op(clause_u,(G)):-!,clause_u(G,true).

prolog_op(conjecture,G):-!, call_u(G).
prolog_op(call,G):-!, call_u(G).
prolog_op(Op,G):- reduce_mpred_op(Op,Op2), on_x_debug(call(Op2,G)).




%= 	 	 

%% prolog_modify( ?Op, ?G) is semidet.
%
% Prolog Modify.
%
prolog_modify(_Op,(:-(G))):-!, call_u(G).
prolog_modify(change(assert,z),G):- use_if_modify_new,!,assertz_if_new(G).
prolog_modify(change(assert,a),G):- use_if_modify_new,!,asserta_if_new(G).
prolog_modify(change(assert,_),G):- use_if_modify_new,!,assert_if_new(G).
prolog_modify(change(assert,z),G):-!,assertz(G).
prolog_modify(change(assert,a),G):-!,asserta(G).
prolog_modify(change(assert,_),G):-!,assert(G).
prolog_modify(change( retract,all),G):-!,retractall(G).
prolog_modify(change(retract,one),(G-B)):-!,retract((G-B)).

prolog_modify(change(retract,_),G):-!,retract(G).
prolog_modify(Op,G):- reduce_mpred_op(Op,Op2), mud_call_store_op(Op2,G).


%= 	 	 

%% ensure_dynamic( :TermVar) is semidet.
%
% Ensure Dynamic.
%
ensure_dynamic(Var):-var(Var),!,trace_or_throw(var_ensure_dynamic(Var)).
ensure_dynamic(M:H1):-atom(M),!,ensure_dynamic(H1).
ensure_dynamic((H1,H2)):-!,ensure_dynamic(H1),ensure_dynamic(H2).
ensure_dynamic((H1;H2)):-!,ensure_dynamic(H1),ensure_dynamic(H2).
ensure_dynamic((H1:-_)):-!,ensure_dynamic(H1).
ensure_dynamic(':-'(_)):-!.
ensure_dynamic(Head):- Head\=isa(_,_),
   get_functor(Head,F,A),
   functor(PF,F,A),
   (\+ predicate_property(PF,_)->show_failure(why,(dynamic(F/A),multifile(F/A),export(F/A)));
   (is_static_pred(PF)-> 
     ((listing(F/A),dmsg(want_to_assert(ensure_dynamic(Head),decl_mpred_prolog(F,A,Head))),nop(dtrace))); true)).

:- fixup_exports.

mpred_storage_file.

