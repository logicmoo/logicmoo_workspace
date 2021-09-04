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
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl
%:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
:- module(mpred_hooks,[
/*
lmcache:agent_session/2,
lmcache:session_agent/2,
lmcache:session_io/4,
lmcache:agent_session/2,
lmcache:session_agent/2,
lmcache:session_io/4,
*/
differentTerms/2,
relax_term/6,
fix_sentence/2,
holds_f/1,
holds_f/2,
holds_f/3,
holds_f/4,
holds_f/5,
holds_f/6,
holds_f/7,
holds_f/8,
holds_t/1,
holds_t/2,
holds_t/3,
holds_t/4,
holds_t/5,
holds_t/6,
holds_t/7,
holds_t/8,

holds_plist_t/2,
holds_relaxed_0_t/4,
holds_relaxed_t/3,
holds_f_p2/2,
holds_relaxed_0_f/4,
holds_relaxed_f/3,
assertion_f/1,
assertion_t/1,
call_f/3,
call_f/4,
call_f/5,
call_f/6,
call_f/7,
call_f/8,
call_f/9,
call_mt_f/4,
call_mt_f/5,
call_mt_f/6,
call_mt_f/7,
call_mt_f/8,
call_mt_f/9,
call_mt_f/10,
call_mt_f/11,
call_mt_t/4,
call_mt_t/5,
call_mt_t/6,
call_mt_t/7,
call_mt_t/8,
call_mt_t/9,
call_mt_t/10,
call_mt_t/11,
call_which_t/3,
call_which_t/4,
call_which_t/5,
call_which_t/6,
call_which_t/7,
call_which_t/8,
call_which_t/9,
call_whichlist_t/3,
callable_tf/2,

xcall_f/1,
xcall_f/2,
xcall_f/3,
xcall_f/4,
xcall_f/5,
xcall_f/6,
xcall_f/7,
xcall_f/8,
xcall_f/9,
xcall_f/10,
xcall_t/1,
xcall_t/2,
xcall_t/3,
xcall_t/4,
xcall_t/5,
xcall_t/6,
xcall_t/7,
xcall_t/8,
xcall_t/9,
xcall_t/10,

mpred_f/1,
which_f/1,
(which_t)/1,

add_arg_parts_of_speech/4,
verb_after_arg/3,

/*
local_qh_mpred_isa/2,
fact_always_true/1,
create_random_fact/1,

deduce_facts/2,
*/
print_sentence/1,
argIsa_call_or_undressed/4,
compute_value/2,
compute_value_no_dice/2,
%fact_always_true/1,
%fact_is_false/2,
%fact_maybe_deduced/1,
flatten_append/3,
%fskel/7,
%hooked_random_instance/3,
%if_result/2,
insert_into/4,
into_plist/2,
into_plist_arities/4,
inverse_args/2,
isCycPredArity_ignoreable/2,
list_update_op/3,
%local_term_anglify/2,

mpred_fact_arity/2,
%mpred_module_ready/0,
%mpred_fa_call/3,
%mpred_plist_t/2,
never_mpred_tcall/1,
prologHybridFact/1,
replace_arg/4,
replace_nth_arglist/4,
replace_nth_ref/5,
same_vars/2,
%tf_result/2,
%never_assert_u/2,
update_value/3
]).


%:- include('mpred_header.pi').

%:- endif.
:- system:import(replace_arg/4).
/*
add_arg_parts_of_speech/4,
agent_action_queue/3,
baseKB:agent_text_command/4,

telnet_fmt_shown/3,
term_anglify_last/2,
term_anglify_np/3,
term_anglify_np_last/3,
verb_after_arg/3

*/

:- meta_predicate 
        holds_f(*,?,?,?,?,?),
        holds_f(*,?,?,?,?,?,?),
        holds_t(*,?,?,?,?,?),
        holds_t(*,?,?,?,?,?,?).
        % if_result(0, 0),
        % mpred_fa_call(?, ?, 0).
        


:- meta_predicate 
      % common_logic_kb_hooks
      call_f(*,*,?),
      % common_logic_kb_hooks
      call_f(*,*,?,?,?,?),
      % common_logic_kb_hooks
      call_f(*,*,?,?,?,?,?,?),
      % common_logic_kb_hooks
      call_mt_f(*,*,?,?,?,?),
      % common_logic_kb_hooks
      call_mt_f(*,*,?,?,?,?,?,?),
      % common_logic_kb_hooks
      call_mt_t(*,?,?,?),
      % common_logic_kb_hooks
      call_mt_t(*,*,?,?,?),
      % common_logic_kb_hooks
      call_mt_t(*,*,?,?,?,?),
      % common_logic_kb_hooks
      call_mt_t(*,*,?,?,?,?,?),
      % common_logic_kb_hooks
      call_mt_t(*,*,?,?,?,?,?,?),
      % common_logic_kb_hooks
      xcall_f(?,?,?),
      % common_logic_kb_hooks
      xcall_f(*,?,?,?),
      % common_logic_kb_hooks
      xcall_f(*,?,?,?,?,?).

% XXXXXXXXXXXXXXXXXXXXXXXXXx
% XXXXXXXXXXXXXXXXXXXXXXXXXx
% XXXXXXXXXXXXXXXXXXXXXXXXXx
% XXXXXXXXXXXXXXXXXXXXXXXXXx
                                                                        
:- meta_predicate 

        call_f(?,*,?),
        call_f(?,?,?,?),
        call_f(?,*,?,?,?),
        call_f(?,*,?,?,?,?),
        call_f(?,*,?,?,?,?,?),
        call_f(?,*,?,?,?,?,?,?),
        call_mt_f(?,?,?,?),
        call_mt_f(?,*,?,?,?),
        call_mt_f(?,*,?,?,?,?),
        call_mt_f(?,*,?,?,?,?,?),
        call_mt_f(?,*,?,?,?,?,?,?),
        call_mt_t(?,?,?,?),
        call_mt_t(?,*,?,?,?),
        call_mt_t(?,*,?,?,?,?),
        call_mt_t(?,*,?,?,?,?,?),
        call_mt_t(?,*,?,?,?,?,?,?),
        call_which_t(?,*,?),
        call_which_t(?,?,?,?),
        call_which_t(?,*,?,?,?),
        call_which_t(?,*,?,?,?,?),
        call_which_t(?,*,?,?,?,?,?),
        call_which_t(?,*,?,?,?,?,?,?),
        xcall_f(0),
        xcall_f(*,?),
        xcall_f(?,?,?),
        xcall_f(*,?,?,?),
        xcall_f(*,?,?,?,?),
        xcall_f(*,?,?,?,?,?),
        xcall_f(*,?,?,?,?,?,?),
        xcall_t(0),
        xcall_t(*,?),
        xcall_t(?,?,?),
        xcall_t(*,?,?,?),
        xcall_t(*,?,?,?,?),
        xcall_t(*,?,?,?,?,?),
        xcall_t(*,?,?,?,?,?,?).

:- meta_predicate xcall_f(0).
:- meta_predicate xcall_f(*,?).
:- meta_predicate xcall_f(?,?,?).
:- meta_predicate xcall_f(*,?,?,?).
:- meta_predicate xcall_f(*,?,?,?,?).
:- meta_predicate xcall_f(*,?,?,?,?,?).
:- meta_predicate xcall_f(*,?,?,?,?,?,?).
:- meta_predicate xcall_t(0).
:- meta_predicate xcall_t(*,?).
:- meta_predicate xcall_t(?,?,?).
:- meta_predicate xcall_t(*,?,?,?).
:- meta_predicate xcall_t(*,?,?,?,?).
:- meta_predicate xcall_t(*,?,?,?,?,?).
:- meta_predicate xcall_t(*,?,?,?,?,?,?).
:- meta_predicate call_f(?,*,?).
:- meta_predicate call_f(?,?,?,?).
:- meta_predicate call_f(?,*,?,?,?).
:- meta_predicate call_f(?,*,?,?,?,?).
:- meta_predicate call_f(?,*,?,?,?,?,?).
:- meta_predicate call_f(?,*,?,?,?,?,?,?).
:- meta_predicate call_mt_f(?,?,?,?).
:- meta_predicate call_mt_f(?,*,?,?,?).
:- meta_predicate call_mt_f(?,*,?,?,?,?).
:- meta_predicate call_mt_f(?,*,?,?,?,?,?).
:- meta_predicate call_mt_f(?,*,?,?,?,?,?,?).
:- meta_predicate call_mt_t(?,?,?,?).
:- meta_predicate call_mt_t(?,*,?,?,?).
:- meta_predicate call_mt_t(?,*,?,?,?,?).
:- meta_predicate call_mt_t(?,*,?,?,?,?,?).
:- meta_predicate call_mt_t(?,*,?,?,?,?,?,?).
:- meta_predicate call_which_t(?,*,?).
:- meta_predicate call_which_t(?,?,?,?).
:- meta_predicate call_which_t(?,*,?,?,?).
:- meta_predicate call_which_t(?,*,?,?,?,?).
:- meta_predicate call_which_t(?,*,?,?,?,?,?).
:- meta_predicate call_which_t(?,*,?,?,?,?,?,?).

:- meta_predicate holds_f(*,?,?,?,?,?).
:- meta_predicate holds_f(*,?,?,?,?,?,?).
:- meta_predicate holds_t(*,?,?,?,?,?).
:- meta_predicate holds_t(*,?,?,?,?,?,?).


:- meta_predicate call_f(*,?,?,?).
:- meta_predicate call_f(*,*,?,?,?).
:- meta_predicate call_f(*,*,?,?,?,?,?).
:- meta_predicate call_mt_f(*,?,?,?).
:- meta_predicate call_mt_f(*,*,?,?,?).
:- meta_predicate call_mt_f(*,*,?,?,?,?,?).
:- meta_predicate call_which_t(*,*,?,?,?,?,?).
:- meta_predicate call_which_t(*,*,?,?,?,?,?,?).
:- meta_predicate holds_f(*,?).
:- meta_predicate holds_f(*,?,?,?,?,?).
:- meta_predicate holds_f(*,?,?,?,?,?,?).
:- meta_predicate holds_relaxed_0_f(*,*,?,?).
:- meta_predicate xcall_f(0).
:- meta_predicate xcall_f(*,?).
:- meta_predicate xcall_f(*,?,?,?,?).
:- meta_predicate xcall_f(*,?,?,?,?,?,?).
:- meta_predicate xcall_t(0).
:- meta_predicate xcall_t(*,?).
:- meta_predicate xcall_t(?,?,?).
:- meta_predicate xcall_t(*,?,?,?).
:- meta_predicate xcall_t(*,?,?,?,?).
:- meta_predicate xcall_t(*,?,?,?,?,?).
:- meta_predicate xcall_t(*,?,?,?,?,?,?).

:- meta_predicate call_whichlist_t(?,0,?).

:- dynamic((
holds_f/1,
holds_f/2,
holds_f/3,
holds_f/4,
holds_f/5,
holds_f/6,
holds_f/7,
holds_f/8,
holds_t/1,
holds_t/2,
holds_t/3,
holds_t/4,
holds_t/5,
holds_t/6,
holds_t/7,
holds_t/8,
(which_t)/1
/*
fact_always_true/1
add_arg_parts_of_speech/4,
agent_action_queue/3,
baseKB:agent_text_command/4,
argIsa_call_or_undressed/4,
create_random_fact/1,
deduce_facts/2,
fact_is_false/2,
fact_maybe_deduced/1,
fskel/7,
hooked_random_instance/3,
isCycPredArity_ignoreable/2,
% lmcache:loaded_external_kbs/1,
%local_term_anglify/2,
mpred_fact_arity/2,
%mpred_module_ready/0,
never_mpred_tcall/1,
telnet_fmt_shown/3,
term_anglify_last/2,
term_anglify_np/3,
term_anglify_np_last/3,
never_assert_u/2,
% use_cyc_database/0,
verb_after_arg/3
*/




/*
if_result/2,
insert_into/4,
into_plist/2,
into_plist_arities/4,
inverse_args/2,
mpred_fa_call/3,
mpred_plist_t/2,
list_update_op/3,
compute_value/2,
compute_value_no_dice/2,
*/
/*
prologHybridFact/1,
flatten_append/3,
replace_arg/4,
replace_nth_arglist/4,
replace_nth_ref/5,
same_vars/2,
update_value/3,
*/
)).
% :- registerCycPredMtWhy([genlPreds/4,genlInverse/4,localityOfObject/4]).

/* <module> mpred_mpred_t
% Provides a prolog dabase in these predicates...
%
%  t/N
%  hybridRule/2
%  
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

:- set_how_virtualize_file(bodies).


% %%% :- kb_shared(create_random_fact/1).

% % %%% :- kb_shared baseKB:decl_database_hook/2.
% %%% :- kb_shared deduce_facts/2.
% %%% :- kb_shared default_type_props/3.
% %%% :- kb_shared fact_always_true/1.
% %%% :- kb_shared fact_maybe_deduced/1.
% %%% :- kb_shared never_assert_u/2.
% %%% :- kb_shared fskel/7.
% %%% :- kb_shared hooked_random_instance/3.

% %%% :- kb_shared now_unused/1.


% %%% :- kb_shared(baseKB:startup_option/2).
% %%% :- kb_shared(is_edited_clause/3).

% %%% :- kb_shared(lmcache:loaded_external_kbs/1).


% %%% :- kb_shared fact_is_false/2.




% %%% :- kb_shared mudKeyword/2.
% %%% :- kb_shared baseKB:only_if_pttp/0.
% %%% :- kb_shared relationMostInstance/3.


% %%% :- kb_shared tFarthestReachableItem/1.
% %%% :- kb_shared tNearestReachableItem/1.


:- multifile(baseKB:use_cyc_database/0).
:- thread_local(baseKB:use_cyc_database/0).
% % %%% :- kb_shared decl_database_hook/2.


% %%% :- kb_shared(mpred_module_ready).

% % %%% :- kb_shared loading_module/1.
% %%% :- kb_shared local_term_anglify/2.


% %%% :- kb_shared term_anglify_last/2.
% %%% :- kb_shared term_anglify_np/3.
% %%% :- kb_shared term_anglify_np_last/3.

% ================================================================================
% begin holds_t
% ================================================================================

%= 	 	 

%% isCycPredArity_ignoreable( ?VALUE1, ?VALUE2) is semidet.
%
% If Is A Cyc Predicate Arity Ignoreable.
%
isCycPredArity_ignoreable(F,A):- ignore(local_qh_mpred_prop(_,F,A,cycPred(A))),ignore(arity(F,A)).


%= 	 	 

%% which_t( ?VALUE1) is semidet.
%
% Which True Stucture.
%
which_t(dac(d,a_notnow,c,no_fallback)).


%= 	 	 

%% holds_t( ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7) is semidet.
%
% Holds True Stucture.
%
holds_t(P,A1,A2,A3,A4,A5,A6,A7):- isCycPredArity_ignoreable(P,7),which_t(DBS),(call_which_t(DBS,P,A1,A2,A3,A4,A5,A6,A7);call_mt_t(DBS,P,A1,A2,A3,A4,A5,A6,A7,_,_);assertion_t([P,A1,A2,A3,A4,A5,A6,A7])).

%= 	 	 

%% holds_t( :PRED6P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6) is semidet.
%
% Holds True Stucture.
%
holds_t(P,A1,A2,A3,A4,A5,A6):- isCycPredArity_ignoreable(P,6),which_t(DBS),(call_which_t(DBS,P,A1,A2,A3,A4,A5,A6);call_mt_t(DBS,P,A1,A2,A3,A4,A5,A6,_,_)).


%= 	 	 

%% holds_t( :PRED5P, ?A1, ?A2, ?A3, ?A4, ?A5) is semidet.
%
% Holds True Stucture.
%
holds_t(P,A1,A2,A3,A4,A5):- isCycPredArity_ignoreable(P,5),which_t(DBS),(call_which_t(DBS,P,A1,A2,A3,A4,A5);call_mt_t(DBS,P,A1,A2,A3,A4,A5,_,_)).


%= 	 	 

%% holds_t( ?P, ?A1, ?A2, ?A3, ?A4) is semidet.
%
% Holds True Stucture.
%
holds_t(P,A1,A2,A3,A4):- isCycPredArity_ignoreable(P,4),which_t(DBS),(call_which_t(DBS,P,A1,A2,A3,A4);call_mt_t(DBS,P,A1,A2,A3,A4,_,_)).

%= 	 	 

%% holds_t( ?P, ?A1, ?A2, ?A3) is semidet.
%
% Holds True Stucture.
%
holds_t(P,A1,A2,A3):- isCycPredArity_ignoreable(P,3),which_t(DBS),(call_which_t(DBS,P,A1,A2,A3);call_mt_t(DBS,P,A1,A2,A3,_,_)).
%holds_t(P,A1,A2):- quietly(holds_relaxed_t(P,A1,A2)).

%= 	 	 

%% holds_t( ?P, ?A1, ?A2) is semidet.
%
% Holds True Stucture.
%
holds_t(P,A1,A2):- isCycPredArity_ignoreable(P,2),which_t(DBS),(call_which_t(DBS,P,A1,A2);call_mt_t(DBS,P,A1,A2,_,_)).

%= 	 	 

%% holds_t( ?P, ?A1) is semidet.
%
% Holds True Stucture.
%
holds_t(P,A1):- isCycPredArity_ignoreable(P,1),which_t(DBS),(call_which_t(DBS,P,A1);call_mt_t(DBS,P,A1,_,_)).


% holds_relaxed_t(P,A1,A2):-var(A1),var(A2),!,t(P,A1,A2).

%= 	 	 

%% holds_relaxed_t( ?P, ?A1, ?A2) is semidet.
%
% Holds Relaxed True Stucture.
%
holds_relaxed_t(P,A1,A2):-
  isCycPredArity_ignoreable(P,2),which_t(DBS),
      relax_term(P,PR,A1,R1,A2,R2),
         holds_relaxed_0_t(DBS,PR,R1,R2).


%= 	 	 

%% holds_relaxed_0_t( ?DBS, ?P, ?A1, ?A2) is semidet.
%
% holds relaxed  Primary Helper True Stucture.
%
holds_relaxed_0_t(DBS,P,A1,A2):- call_which_t(DBS,P,A1,A2).
holds_relaxed_0_t(DBS,P,A1,A2):- call_mt_t(DBS,P,A1,A2,_,_).

/*
holds_relaxed_0_t(dac(_,a,_,_),P,A1,A2):- assertion_t([P,A1,A2]).
holds_relaxed_0_t(dac(d,_,_,_),P,A1,A2):- t(P,A1,A2).
holds_relaxed_0_t(dac(_,_,_,h),P,A1,A2):- call_which_t(DBS,P,A1,A2).
holds_relaxed_0_t(DBS,P,A1,A2):- call_mt_t(DBS,P,A1,A2,_,_).
holds_relaxed_0_t(_DBS,P,A1,A2):- ground((P,A1)), TEMPL=..[P,T1,_],t(relationMostInstance,TEMPL,T1,A2),call_u(isa(A1,T1)),!.
*/


%= 	 	 

%% holds_t( :TermCALL) is semidet.
%
% Holds True Stucture.
%
holds_t([AH,P|LIST]):- is_holds_true(AH),!,holds_plist_t(P,LIST).
holds_t([AH,P|LIST]):- is_holds_false(AH),!,holds_f_p2(P,LIST).
holds_t([P|LIST]):- !,holds_plist_t(P,LIST).
holds_t(not(CALL)):- !, holds_f(CALL).
holds_t(CALL):- '=..'(CALL,PLIST),holds_t(PLIST).


%= 	 	 

%% holds_plist_t( ?P, ?LIST) is semidet.
%
% Holds Plist True Stucture.
%
holds_plist_t(P,LIST):- apply(holds_t,[P|LIST]).








                             

%% add_arg_parts_of_speech( ?F, ?N, :TermARG3, :TermARG4) is semidet.
%
% Add Argument Parts Of Speech.
%
add_arg_parts_of_speech(_F,_N,[],[]).
add_arg_parts_of_speech(F,N,[A|ARGS0],[ARG|ARGS]):-argIsa_call_or_undressed(F,N,A,ARG),N1 is N+1, add_arg_parts_of_speech(F,N1,ARGS0,ARGS).




%% argIsa_call_or_undressed( ?F, ?N, ?Obj, ?Obj) is semidet.
%
% Argument  (isa/2) call or undressed.
%
argIsa_call_or_undressed(F,N,Obj,fN(Obj,Type)):- call_u(argIsa(F,N,Type)),!.
argIsa_call_or_undressed(_F,_N,Obj,Obj).




%% verb_after_arg( ?VALUE1, ?VALUE2, :PRED1VALUE3) is semidet.
%
% Verb After Argument.
%
verb_after_arg(_,_,1).



%= 	 	 

%% print_sentence( ?Proof) is semidet.
%
% Print Sentence.
%
print_sentence(Proof):- fix_sentence(Proof,New),!,ignore((Proof\=New,!,must_det(retract(Proof)),assert(assert_next_queue(New)))),!.



%= 	 	 

%% fix_sentence( ?X, ?X) is semidet.
%
% Fix Sentence.
%
fix_sentence(X,X).


%= 	 	 

%% relax_term( ?P, ?P, ?Aic, ?Aic, ?Bic, ?Bic) is semidet.
%
% Relax Term.
%
relax_term(P,P,Aic,Aic,Bic,Bic):- !.
/*
relax_term(P,P,A,A,Bi,Bc):- arg(_,v(genls,isa),P),!,fail.
relax_term(P,P,Ai,Ac,Bic,Bic):- when_met(nonvar(Ac), same_arg(same_or(isa),Ac,Ai)),!.
relax_term(P,P,Ai,Ac,Bi,Bc):- is_type(Ai),!,when_met(pred(nonvar,Ac), (same_arg(same_or(genls),Ac,Ai),same_arg(same_or(equals),Bc,Bi))),!.
relax_term(P,P,Ai,Ac,Bi,Bc):- when_met(pred(nonvar,Ac),when_met(pred(nonvar,Bc), (same_arg(same_or(genls),Ac,Ai),same_arg(same_or(equals),Bc,Bi)))).
*/

% ?- member(R,[a,b,c]),when_met(nonvar(Re), dbase:same_arg(same_or(termOfUnit),n,Re)),Re=R,write(chose(R)).

differentTerms(A,B):- dif:dif(A,B).



/*
:- kb_local(baseKB:admittedArgument/3).
baseKB:admittedArgument(P,N,A):-var_non_attvar(P),!,freeze(P,admittedArgument(P,N,A)).
baseKB:admittedArgument(P,N,A):-var_non_attvar(A),!,freeze(A,admittedArgument(P,N,A)).
baseKB:admittedArgument(P,N,A):-var_non_attvar(N),!,freeze(N,(number(N),admittedArgument(P,N,A))).
baseKB:admittedArgument(P,N,A):-nop(wdmsg(admittedArgument(P,N,A))).
*/

%= 	 	 

%% callable_tf( ?F, ?A) is semidet.
%
% Callable True/false.
%

callable_tf(F,A):- functor_safe(P,F,A),predicate_property(P,_),!.
%callable_tf(P,2):- cheaply_u(mpred_arity_pred(P)),!,fail.



%= 	 	 

%% call_whichlist_t( ?UPARAM1, :GoalGOAL2, ?List) is semidet.
%
% Call Whichlist True Stucture.
%
call_whichlist_t(dac(d,_,_,_),CALL,_):- call_u(t(CALL)).
call_whichlist_t(dac(_,a,_,_),_,List):- assertion_t(List).
call_whichlist_t(dac(_,_,c,_),CALL,_):- xcall_t(CALL).
call_whichlist_t(dac(_,_,_,holds_t),CALL,_):- holds_t(CALL).


%= 	 	 

%% call_which_t( ?DBS, ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7) is semidet.
%
% Call Which True Stucture.
%
call_which_t(DBS,P,A1,A2,A3,A4,A5,A6,A7):- callable_tf(P,7),List= [P,A1,A2,A3,A4,A5,A6,A7], CALL=..List, call_whichlist_t(DBS,CALL,List).
call_which_t(dac(_,_,_,h),P,A1,A2,A3,A4,A5,A6,A7):- holds_t(P,A1,A2,A3,A4,A5,A6,A7).


%= 	 	 

%% call_which_t( ?UPARAM1, :PRED6P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6) is semidet.
%
% Call Which True Stucture.
%
call_which_t(dac(d,_,_,_),P,A1,A2,A3,A4,A5,A6):- t(P,A1,A2,A3,A4,A5,A6).
call_which_t(dac(_,a,_,_),P,A1,A2,A3,A4,A5,A6):- assertion_t([P,A1,A2,A3,A4,A5,A6]).
call_which_t(dac(_,_,c,_),P,A1,A2,A3,A4,A5,A6):- callable_tf(P,6),xcall_t(P,A1,A2,A3,A4,A5,A6).
call_which_t(dac(_,_,_,holds_t),P,A1,A2,A3,A4,A5,A6):- holds_t(P,A1,A2,A3,A4,A5,A6).


%= 	 	 

%% call_which_t( ?UPARAM1, :PRED5P, ?A1, ?A2, ?A3, ?A4, ?A5) is semidet.
%
% Call Which True Stucture.
%
call_which_t(dac(d,_,_,_),P,A1,A2,A3,A4,A5):- t(P,A1,A2,A3,A4,A5).
call_which_t(dac(_,a,_,_),P,A1,A2,A3,A4,A5):- assertion_t([P,A1,A2,A3,A4,A5]).
call_which_t(dac(_,_,c,_),P,A1,A2,A3,A4,A5):- callable_tf(P,5),xcall_t(P,A1,A2,A3,A4,A5).
call_which_t(dac(_,_,_,holds_t),P,A1,A2,A3,A4,A5):- holds_t(P,A1,A2,A3,A4,A5).


%= 	 	 

%% call_which_t( ?UPARAM1, :PRED4P, ?A1, ?A2, ?A3, ?A4) is semidet.
%
% Call Which True Stucture.
%
call_which_t(dac(d,_,c,_),P,A1,A2,A3,A4):- t(P,A1,A2,A3,A4).
call_which_t(dac(_,a,_,_),P,A1,A2,A3,A4):- assertion_t([P,A1,A2,A3,A4]).
call_which_t(dac(_,_,c,_),P,A1,A2,A3,A4):- callable_tf(P,4),xcall_t(P,A1,A2,A3,A4).
call_which_t(dac(_,_,_,holds_t),P,A1,A2,A3,A4):- holds_t(P,A1,A2,A3,A4).


%= 	 	 

%% call_which_t( ?UPARAM1, :PRED3P, ?A1, ?A2, ?A3) is semidet.
%
% Call Which True Stucture.
%
call_which_t(dac(d,_,_,_),P,A1,A2,A3):- t(P,A1,A2,A3).
call_which_t(dac(_,a,_,_),P,A1,A2,A3):- assertion_t([P,A1,A2,A3]).
call_which_t(dac(_,_,c,_),P,A1,A2,A3):- callable_tf(P,3),xcall_t(P,A1,A2,A3).
call_which_t(dac(_,_,_,holds_t),P,A1,A2,A3):- holds_t(P,A1,A2,A3).


%= 	 	 

%% call_which_t( ?UPARAM1, :PRED2P, ?A1, ?A2) is semidet.
%
% Call Which True Stucture.
%
call_which_t(dac(d,_,_,_),P,A1,A2):- t(P,A1,A2).
call_which_t(dac(_,a,_,_),P,A1,A2):- assertion_t([P,A1,A2]).
call_which_t(dac(_,_,c,_),P,A1,A2):- callable_tf(P,2),xcall_t(P,A1,A2).
call_which_t(dac(_,_,_,holds_t),P,A1,A2):- holds_t(P,A1,A2).


%= 	 	 

%% call_which_t( ?UPARAM1, :PRED1P, ?A1) is semidet.
%
% Call Which True Stucture.
%
call_which_t(dac(d,_,_,_),P,A1):- call_u(t(P,A1)).
call_which_t(dac(_,a,_,_),P,A1):- assertion_t([P,A1]).
call_which_t(dac(_,_,c,_),P,A1):- callable_tf(P,1),xcall_t(P,A1).
call_which_t(dac(_,_,_,holds_t),P,A1):- holds_t(P,A1).


%= 	 	 

%% call_mt_t( ?VALUE1, ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7, ?A8, ?A9) is semidet.
%
% Call User Microtheory True Stucture.
%
call_mt_t(dac(_,_,_,mt),P,A1,A2,A3,A4,A5,A6,A7,A8,A9):- callable_tf(P,9),CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8,A9],xcall_t(CALL).

%= 	 	 

%% call_mt_t( ?VALUE1, ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7, ?A8) is semidet.
%
% Call User Microtheory True Stucture.
%
call_mt_t(dac(_,_,_,mt),P,A1,A2,A3,A4,A5,A6,A7,A8):- callable_tf(P,8),CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8],xcall_t(CALL).

%= 	 	 

%% call_mt_t( ?VALUE1, ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7) is semidet.
%
% Call User Microtheory True Stucture.
%
call_mt_t(dac(_,_,_,mt),P,A1,A2,A3,A4,A5,A6,A7):- callable_tf(P,7),CALL=..[P,A1,A2,A3,A4,A5,A6,A7],xcall_t(CALL).

%= 	 	 

%% call_mt_t( ?UPARAM1, :PRED6P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6) is semidet.
%
% Call User Microtheory True Stucture.
%
call_mt_t(dac(_,_,_,mt),P,A1,A2,A3,A4,A5,A6):- callable_tf(P,6),xcall_t(P,A1,A2,A3,A4,A5,A6).

%= 	 	 

%% call_mt_t( ?UPARAM1, :PRED5P, ?A1, ?A2, ?A3, ?A4, ?A5) is semidet.
%
% Call User Microtheory True Stucture.
%
call_mt_t(dac(_,_,_,mt),P,A1,A2,A3,A4,A5):- callable_tf(P,5),xcall_t(P,A1,A2,A3,A4,A5).

%= 	 	 

%% call_mt_t( ?UPARAM1, :PRED4P, ?A1, ?A2, ?A3, ?A4) is semidet.
%
% Call User Microtheory True Stucture.
%
call_mt_t(dac(_,_,_,mt),P,A1,A2,A3,A4):- callable_tf(P,4),xcall_t(P,A1,A2,A3,A4).

%= 	 	 

%% call_mt_t( ?UPARAM1, :PRED3P, ?A1, ?A2, ?A3) is semidet.
%
% Call User Microtheory True Stucture.
%
call_mt_t(dac(_,_,_,mt),P,A1,A2,A3):- callable_tf(P,3),xcall_t(P,A1,A2,A3).

%= 	 	 

%% call_mt_t( ?UPARAM1, :PRED2P, ?A1, ?A2) is semidet.
%
% Call User Microtheory True Stucture.
%
call_mt_t(dac(_,_,_,mt),P,A1,A2):- callable_tf(P,3),xcall_t(P,A1,A2).


%= 	 	 

%% xcall_t( ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7, ?A8, ?A9) is semidet.
%
% Extended Call True Stucture.
%
xcall_t(P,A1,A2,A3,A4,A5,A6,A7,A8,A9):- CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8,A9],call(CALL).

%= 	 	 

%% xcall_t( ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7, ?A8) is semidet.
%
% Extended Call True Stucture.
%
xcall_t(P,A1,A2,A3,A4,A5,A6,A7,A8):- CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8],call(CALL).

%= 	 	 

%% xcall_t( ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7) is semidet.
%
% Extended Call True Stucture.
%
xcall_t(P,A1,A2,A3,A4,A5,A6,A7):- CALL=..[P,A1,A2,A3,A4,A5,A6,A7],call(CALL).

%= 	 	 

%% xcall_t( :PRED6P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6) is semidet.
%
% Extended Call True Stucture.
%
xcall_t(P,A1,A2,A3,A4,A5,A6):- call(P,A1,A2,A3,A4,A5,A6).

%= 	 	 

%% xcall_t( :PRED5P, ?A1, ?A2, ?A3, ?A4, ?A5) is semidet.
%
% Extended Call True Stucture.
%
xcall_t(P,A1,A2,A3,A4,A5):- call(P,A1,A2,A3,A4,A5).

%= 	 	 

%% xcall_t( :PRED4P, ?A1, ?A2, ?A3, ?A4) is semidet.
%
% Extended Call True Stucture.
%
xcall_t(P,A1,A2,A3,A4):- call(P,A1,A2,A3,A4).

%= 	 	 

%% xcall_t( :PRED3P, ?A1, ?A2, ?A3) is semidet.
%
% Extended Call True Stucture.
%
xcall_t(P,A1,A2,A3):- call(P,A1,A2,A3).

%= 	 	 

%% xcall_t( :PRED2P, ?A1, ?A2) is semidet.
%
% Extended Call True Stucture.
%
xcall_t(P,A1,A2):- call(P,A1,A2).

%= 	 	 

%% xcall_t( :PRED1P, ?A1) is semidet.
%
% Extended Call True Stucture.
%
xcall_t(P,A1):- call(P,A1).

%= 	 	 

%% xcall_t( :GoalP) is semidet.
%
% Extended Call True Stucture.
%
xcall_t(P):- call(P).

% todo hook into loaded files!
:- was_export(assertion_t/1).

% assertion_t(Call):- t_l:useOnlyExternalDBs,!,baseKB:use_cyc_database,locally_hide(t_l:useOnlyExternalDBs,kb_t(Call)).

%= 	 	 

%% assertion_t( ?Call) is semidet.
%
% Assertion True Stucture.
%
assertion_t(Call):- baseKB:use_cyc_database,!,locally_tl(useOnlyExternalDBs,kb_t(Call)).
% assertion_t(Call):- locally_tl(useOnlyExternalDBs,loop_check(call_u(Call))).

% ================================================================================
% end holds_t
% ================================================================================

% % :- ensure_loaded(logicmoo(plarkc/mpred_cyc_kb)).

% ================================================================================
% begin holds_f
% ================================================================================

%= 	 	 

%% which_f( ?VALUE1) is semidet.
%
% Which False.
%
which_f(dac(d,no_a,no_c,no_mt)).


%= 	 	 

%% holds_f( ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7) is semidet.
%
% Holds False.
%
holds_f(P,A1,A2,A3,A4,A5,A6,A7):- isCycPredArity_ignoreable(P,7),which_f(DBS),(call_f(DBS,P,A1,A2,A3,A4,A5,A6,A7);call_mt_f(DBS,P,A1,A2,A3,A4,A5,A6,A7,_,_);assertion_f([P,A1,A2,A3,A4,A5,A6,A7])).

%= 	 	 

%% holds_f( :PRED6P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6) is semidet.
%
% Holds False.
%
holds_f(P,A1,A2,A3,A4,A5,A6):- isCycPredArity_ignoreable(P,6),which_f(DBS),(call_f(DBS,P,A1,A2,A3,A4,A5,A6);call_mt_f(DBS,P,A1,A2,A3,A4,A5,A6,_,_)).

%= 	 	 

%% holds_f( :PRED5P, ?A1, ?A2, ?A3, ?A4, ?A5) is semidet.
%
% Holds False.
%
holds_f(P,A1,A2,A3,A4,A5):- isCycPredArity_ignoreable(P,5),which_f(DBS),(call_f(DBS,P,A1,A2,A3,A4,A5);call_mt_f(DBS,P,A1,A2,A3,A4,A5,_,_)).

%= 	 	 

%% holds_f( ?P, ?A1, ?A2, ?A3, ?A4) is semidet.
%
% Holds False.
%
holds_f(P,A1,A2,A3,A4):- isCycPredArity_ignoreable(P,4),which_f(DBS),(call_f(DBS,P,A1,A2,A3,A4);call_mt_f(DBS,P,A1,A2,A3,A4,_,_)).

%= 	 	 

%% holds_f( ?P, ?A1, ?A2, ?A3) is semidet.
%
% Holds False.
%
holds_f(P,A1,A2,A3):- isCycPredArity_ignoreable(P,3),which_f(DBS),(call_f(DBS,P,A1,A2,A3);call_mt_f(DBS,P,A1,A2,A3,_,_)).

%= 	 	 

%% holds_f( ?P, ?A1, ?A2) is semidet.
%
% Holds False.
%
holds_f(P,A1,A2):- holds_relaxed_f(P,A1,A2).

%= 	 	 

%% holds_f( :PRED1P, ?A1) is semidet.
%
% Holds False.
%
holds_f(P,A1):- isCycPredArity_ignoreable(P,1),which_f(DBS),(call_f(DBS,P,A1);call_mt_f(DBS,P,A1,_,_)).



%= 	 	 

%% holds_relaxed_f( ?P, ?A1, ?A2) is semidet.
%
% Holds Relaxed False.
%
holds_relaxed_f(P,A1,A2):- isCycPredArity_ignoreable(P,2),which_f(DBS),!,relax_term(P,PR,A1,R1,A2,R2),holds_relaxed_0_f(DBS,PR,R1,R2).

%= 	 	 

%% holds_relaxed_0_f( ?DBS, :PRED4P, ?A1, ?A2) is semidet.
%
% holds relaxed  Primary Helper False.
%
holds_relaxed_0_f(DBS,P,A1,A2):- call_f(DBS,P,A1,A2).
holds_relaxed_0_f(DBS,P,A1,A2):- call_mt_f(DBS,P,A1,A2,_,_).



%= 	 	 

%% holds_f( :TermCALL) is semidet.
%
% Holds False.
%
holds_f([AH,P|LIST]):- is_holds_true(AH),!,holds_f_p2(P,LIST).
holds_f([AH,P|LIST]):- is_holds_false(AH),!,holds_plist_t(P,LIST).
holds_f([P|LIST]):- !, holds_f_p2(P,LIST).
holds_f(CALL):- CALL=..[P|LIST],holds_f([P|LIST]).

%= 	 	 

%% holds_f_p2( ?P, ?LIST) is semidet.
%
% Holds False Pred Extended Helper.
%
holds_f_p2(P,LIST):- CALL=..[holds_f,P|LIST],call(CALL).


%= 	 	 

%% mpred_f( ?List) is semidet.
%
% Managed Predicate False.
%
mpred_f(List):- is_list(List),!,Call=..[mpred_f|List],call_u(Call).
mpred_f(List):- holds_f(List).



%= 	 	 

%% call_f( ?VALUE1, ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7) is semidet.
%
% Call False.
%
call_f(_,P,A1,A2,A3,A4,A5,A6,A7):- callable_tf(P,7),List= [P,A1,A2,A3,A4,A5,A6,A7], CALL=..List,(assertion_f(List);mpred_f(CALL);xcall_f(CALL)).

%= 	 	 

%% call_f( ?UPARAM1, :PRED6P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6) is semidet.
%
% Call False.
%
call_f(dac(d,_,_,_),P,A1,A2,A3,A4,A5,A6):- call_u(mpred_f([P,A1,A2,A3,A4,A5,A6])).
call_f(dac(_,a,_,_),P,A1,A2,A3,A4,A5,A6):- assertion_f([P,A1,A2,A3,A4,A5,A6]).
call_f(dac(_,_,c,_),P,A1,A2,A3,A4,A5,A6):- callable_tf(P,6),xcall_f(P,A1,A2,A3,A4,A5,A6).

%= 	 	 

%% call_f( ?UPARAM1, :PRED5P, ?A1, ?A2, ?A3, ?A4, ?A5) is semidet.
%
% Call False.
%
call_f(dac(d,_,_,_),P,A1,A2,A3,A4,A5):- call_u(mpred_f(P,A1,A2,A3,A4,A5)).
call_f(dac(_,a,_,_),P,A1,A2,A3,A4,A5):- assertion_f([P,A1,A2,A3,A4,A5]).
call_f(dac(_,_,c,_),P,A1,A2,A3,A4,A5):- callable_tf(P,5),xcall_f(P,A1,A2,A3,A4,A5).

%= 	 	 

%% call_f( ?UPARAM1, :PRED4P, ?A1, ?A2, ?A3, ?A4) is semidet.
%
% Call False.
%
call_f(dac(d,_,_,_),P,A1,A2,A3,A4):- call_u(mpred_f(P,A1,A2,A3,A4)).
call_f(dac(_,a,_,_),P,A1,A2,A3,A4):- assertion_f([P,A1,A2,A3,A4]).
call_f(dac(_,_,c,_),P,A1,A2,A3,A4):- callable_tf(P,4),xcall_f(P,A1,A2,A3,A4).

%= 	 	 

%% call_f( ?UPARAM1, :PRED3P, ?A1, ?A2, ?A3) is semidet.
%
% Call False.
%
call_f(dac(d,_,_,_),P,A1,A2,A3):- call_u(mpred_f(P,A1,A2,A3)).
call_f(dac(_,a,_,_),P,A1,A2,A3):- assertion_f([P,A1,A2,A3]).
call_f(dac(_,_,c,_),P,A1,A2,A3):- callable_tf(P,3),xcall_f(P,A1,A2,A3).

%= 	 	 

%% call_f( ?UPARAM1, :PRED2P, ?A1, ?A2) is semidet.
%
% Call False.
%
call_f(dac(d,_,_,_),P,A1,A2):- call_u(mpred_f(P,A1,A2)).
call_f(dac(_,a,_,_),P,A1,A2):- assertion_f([P,A1,A2]).
call_f(dac(_,_,c,_),P,A1,A2):- callable_tf(P,2),xcall_f(P,A1,A2).

%= 	 	 

%% call_f( ?UPARAM1, :PRED1P, ?A1) is semidet.
%
% Call False.
%
call_f(dac(d,_,_,_),P,A1):- call_u(mpred_f(P,A1)).
call_f(dac(_,a,_,_),P,A1):- assertion_f([P,A1]).
call_f(dac(_,_,c,_),P,A1):- callable_tf(P,1),xcall_f(P,A1).


%= 	 	 

%% call_mt_f( ?VALUE1, ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7, ?A8, ?A9) is semidet.
%
% Call User Microtheory False.
%
call_mt_f(dac(_,_,_,mt),P,A1,A2,A3,A4,A5,A6,A7,A8,A9):- callable_tf(P,9),CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8,A9],xcall_f(CALL).

%= 	 	 

%% call_mt_f( ?VALUE1, ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7, ?A8) is semidet.
%
% Call User Microtheory False.
%
call_mt_f(dac(_,_,_,mt),P,A1,A2,A3,A4,A5,A6,A7,A8):- callable_tf(P,8),CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8],xcall_f(CALL).

%= 	 	 

%% call_mt_f( ?VALUE1, ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7) is semidet.
%
% Call User Microtheory False.
%
call_mt_f(dac(_,_,_,mt),P,A1,A2,A3,A4,A5,A6,A7):- callable_tf(P,7),CALL=..[P,A1,A2,A3,A4,A5,A6,A7],xcall_f(CALL).

%= 	 	 

%% call_mt_f( ?UPARAM1, :PRED6P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6) is semidet.
%
% Call User Microtheory False.
%
call_mt_f(dac(_,_,_,mt),P,A1,A2,A3,A4,A5,A6):- callable_tf(P,6),xcall_f(P,A1,A2,A3,A4,A5,A6).

%= 	 	 

%% call_mt_f( ?UPARAM1, :PRED5P, ?A1, ?A2, ?A3, ?A4, ?A5) is semidet.
%
% Call User Microtheory False.
%
call_mt_f(dac(_,_,_,mt),P,A1,A2,A3,A4,A5):- callable_tf(P,5),xcall_f(P,A1,A2,A3,A4,A5).

%= 	 	 

%% call_mt_f( ?UPARAM1, :PRED4P, ?A1, ?A2, ?A3, ?A4) is semidet.
%
% Call User Microtheory False.
%
call_mt_f(dac(_,_,_,mt),P,A1,A2,A3,A4):- callable_tf(P,4),xcall_f(P,A1,A2,A3,A4).

%= 	 	 

%% call_mt_f( ?UPARAM1, :PRED3P, ?A1, ?A2, ?A3) is semidet.
%
% Call User Microtheory False.
%
call_mt_f(dac(_,_,_,mt),P,A1,A2,A3):- callable_tf(P,3),xcall_f(P,A1,A2,A3).

%= 	 	 

%% call_mt_f( ?UPARAM1, :PRED2P, ?A1, ?A2) is semidet.
%
% Call User Microtheory False.
%
call_mt_f(dac(_,_,_,mt),P,A1,A2):- callable_tf(P,2),xcall_f(P,A1,A2).


%= 	 	 

%% xcall_f( ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7, ?A8, ?A9) is semidet.
%
% Extended Call False.
%
xcall_f(P,A1,A2,A3,A4,A5,A6,A7,A8,A9):- CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8,A9],\+ xcall_t(CALL).

%= 	 	 

%% xcall_f( ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7, ?A8) is semidet.
%
% Extended Call False.
%
xcall_f(P,A1,A2,A3,A4,A5,A6,A7,A8):- CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8],\+ xcall_t(CALL).

%= 	 	 

%% xcall_f( ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7) is semidet.
%
% Extended Call False.
%
xcall_f(P,A1,A2,A3,A4,A5,A6,A7):- CALL=..[P,A1,A2,A3,A4,A5,A6,A7],\+ xcall_t(CALL).

%= 	 	 

%% xcall_f( :PRED6P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6) is semidet.
%
% Extended Call False.
%
xcall_f(P,A1,A2,A3,A4,A5,A6):- \+ xcall_t(P,A1,A2,A3,A4,A5,A6).

%= 	 	 

%% xcall_f( :PRED5P, ?A1, ?A2, ?A3, ?A4, ?A5) is semidet.
%
% Extended Call False.
%
xcall_f(P,A1,A2,A3,A4,A5):- \+ xcall_t(P,A1,A2,A3,A4,A5).

%= 	 	 

%% xcall_f( :PRED4P, ?A1, ?A2, ?A3, ?A4) is semidet.
%
% Extended Call False.
%
xcall_f(P,A1,A2,A3,A4):- \+ xcall_t(P,A1,A2,A3,A4).

%= 	 	 

%% xcall_f( :PRED3P, ?A1, ?A2, ?A3) is semidet.
%
% Extended Call False.
%
xcall_f(P,A1,A2,A3):- \+ xcall_t(P,A1,A2,A3).

%= 	 	 

%% xcall_f( :PRED2P, ?A1, ?A2) is semidet.
%
% Extended Call False.
%
xcall_f(P,A1,A2):- \+ xcall_t(P,A1,A2).

%= 	 	 

%% xcall_f( :PRED1P, ?A1) is semidet.
%
% Extended Call False.
%
xcall_f(P,A1):- \+ xcall_t(P,A1).

%= 	 	 

%% xcall_f( :GoalP) is semidet.
%
% Extended Call False.
%
xcall_f(P):- \+ xcall_t(P).


%= 	 	 

%% assertion_f( :TermAH) is semidet.
%
% Assertion False.
%
assertion_f([AH,P|LIST]):- is_holds_true(AH),!,assertion_f([P|LIST]).
assertion_f([AH,P|LIST]):- is_holds_false(AH),!,assertion_f([P|LIST]).
% todo hook into loaded files!
assertion_f(_):- \+(lmcache:loaded_external_kbs(_)),!,fail.
%MAYBE LATER assertion_f([P|LIST]):- 'TINYKB-ASSERTION'(':FALSE-DEF',_,_UniversalVocabularyMt,_Vars,/*HL*/[P|LIST]).
%MAYBE LATER assertion_f([P|LIST]):- 'TINYKB-ASSERTION'(':FALSE-MON',_,_UniversalVocabularyMt,_Vars,/*HL*/[P|LIST]).


% ================================================================================
% end holds_f 
% ================================================================================

:- fixup_exports.


