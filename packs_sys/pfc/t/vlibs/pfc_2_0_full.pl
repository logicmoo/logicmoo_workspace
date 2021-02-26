/* Part of LogicMOO Base Logicmoo Debug Tools
% ===================================================================
% File '$FILENAME.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: '$FILENAME.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/

%:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).

:- if(current_prolog_flag(xref,true)).
:- module(pfc_lib_core, [
  get_startup_uu/1,
  call_u_no_bc/1,%fix_mp/3,
  fix_mp/4, %fix_mp/3,
  mpred_fwc/1,
  get_mpred_is_tracing/1,
  show_if_debug/1,
  full_transform_warn_if_changed/3,
  full_transform_warn_if_same/3,
  full_transform/3,
  maybe_mpred_break/1,
  each_E/3,
  call_m_g/3,
  same_modules/2,
  throw_depricated/0,
  mpred_post_exactly/1,
  lookup_m_g/3,
  head_to_functor_name/2,
          ain_expanded/1,
  mpred_notrace_exec/0,
  get_unnegated_functor/3,
  mpred_post1_rem/2,
  mpred_post1/1,
  mpred_post1_rem2/2,
  mpred_post2/2,
  mpred_post12/2,
  fwc1s_post1s/2,
  
  mpred_mark_fa_as/5,
  %mpred_te/0,
  %mpred_te/2,
  maybe_updated_value/3,
  log_failure/1,
  code_sentence_op/1,
  quietly_ex/1,
  mpred_compile_rhs_term_consquent/3,
  with_fc_mode/2,
  filter_buffer_n_test/3,
  filter_buffer_get_n/3,
  filter_buffer_trim/2,
  plus_fwc/0,
  plus_fwc/1,
  cut_c/0,
  to_u/2,
  fresh_mode/0,
  mpred_mark_as/3,
  get_first_user_reason/2,
  assert_u_confirm_if_missing/1,
  assert_u_confirmed_was_missing/1,
  mpred_notrace_exec/0,
  remove_negative_version/1,
  listing_u/1,

  call_mp/2,
  call_u_mp_fa/4,
  call_u_mp_lc/4,

  get_source_uu/1,
  get_source_mfl/1,
  is_source_ref1/1,
  get_why_uu/1,
  set_fc_mode/1,

  with_no_breaks/1,
  mpred_remove1/2,
  check_never_assert/1,check_never_retract/1,
  oinfo/1,
  why_was_true/1,
  mpred_fwc0/1,
  with_no_mpred_trace_exec/1,
  mpred_set_default/2,
  mpred_ain/1,mpred_ain/1,mpred_ain/2,
  action_is_undoable/1,
  mpred_assumption/1,mpred_assumptions/2,mpred_axiom/1,bagof_PFC/3,bases_union/2,brake/1,build_rhs/3,
  mpred_BC_CACHE0/2,
  build_neg_test/4,build_rule/3,build_code_test/3,
  build_trigger/4,
  defaultmpred_select/1,fc_eval_action/2,
  % foreach/2,
  get_next_fact/1,
  justification/2,justifications/2,
  call_u/1,
  variant_u/2,
  mpred_BC_CACHE/2,
  call_u_no_bc/1,mpred_METACALL/2,mpred_METACALL/3,mpred_METACALL/3,
  mpred_halt/0,mpred_halt/1,mpred_halt/2,
  mpred_ain_db_to_head/2,mpred_ain_actiontrace/2,mpred_trace_op/2,mpred_add_support/2,mpred_ain_trigger_reprop/2,
  mpred_ain_by_type/2,
  mpred_prompt_ask/2,
  mpred_METACALL/3,mpred_BC_w_cache/2,
  ain_fast/1,
  ain_fast/2,
  setup_mpred_ops/0,
  mpred_assert_w_support/2,mpred_asserta_w_support/2,mpred_assertz_w_support/2,mpred_basis_list/2,mpred_bt_pt_combine/3,mpred_child/2,mpred_children/2,
  mpred_classifyFacts/4,mpred_collect_supports/1,mpred_unhandled_command/3,mpred_compile_rhs_term/3,mpred_conjoin/3,mpred_neg_connective/1,
  mpred_database_item/1,
  % mpred_database_term/3,
  mpred_db_type/2,mpred_set_default/2,mpred_define_bc_rule/3,mpred_descendant/2,
  mpred_descendants/2,mpred_enqueue/2,mpred_error/1,mpred_error/2,mpred_eval_lhs/2,mpred_eval_lhs_1/2,mpred_eval_rhs/2,mpred_fact/1,
  mpred_fact/2,mpred_facts/1,mpred_facts/2,mpred_facts/3,mpred_fwc/1,mpred_get_support/2,lookup_u/1,lookup_u/2,
  mpred_literal/1,mpred_load/1,mpred_make_supports/1,mpred_ain_object/1,mpred_aina/2,mpred_ainz/2,mpred_aina/1,mpred_ainz/1,
  mpred_negated_literal/1,mpred_unnegate/2,mpred_nf/2,mpred_nf1_negation/2,mpred_nf_negation/2,mpred_nf_negations/2,mpred_notrace/0,mpred_nowatch/0,
  mpred_nospy/0,mpred_nospy/1,mpred_nospy/3,mpred_positive_literal/1,mpred_post/2,pp_qu/0,mpred_undo_action/1,
  mpred_rem_support/2,mpred_remove_old_version/1,mpred_remove_supports_whine/1,mpred_remove_supports_quietly/1,mpred_reset_kb_0/0,mpred_retract_i/1,mpred_retract_i_or_warn/1,mpred_retract_supported_relations/1,
  mpred_retract_type/2,mpred_select_justification_node/3,mpred_set_warnings/1,mpred_pp_db_justifications/2,
  mpred_spy/1,mpred_spy/2,mpred_spy/3,mpred_step/0,mpred_support_relation/1,mpred_supported/1,mpred_supported/2,
  mpred_trace/0,mpred_trace/1,mpred_trace/2,mpred_trace_maybe_print/3,mpred_trace_maybe_break/3,mpred_trace_exec/0,mpred_trace_op/3,
  mpred_trace_op/2,mpred_trace_msg/1,mpred_trace_msg/2,mpred_trigger_key/2,mpred_trigger_key/2,mpred_undo/1,mpred_unfwc/1,
  mpred_unfwc_check_triggers/1,mpred_union/3,mpred_unique_u/1,mpred_untrace/0,mpred_untrace/1,mpred_warn/0,mpred_warn/1,
  mpred_warn/2,mpred_watch/0,well_founded_0/2,clear_proofs/0,mpred_why/0,mpred_why/1,mpred_whyBrouse/2,mpred_handle_why_command/3,
  nompred_warn/0, % pfcl_do/1,
  pp_DB/0,pp_db_facts/0,pp_db_facts/1,pp_db_facts/2,pp_db_items/1,
  pp_db_rules/0,pp_db_supports/0,pp_db_triggers/0,mpred_load/1,process_rule/3,
  remove_if_unsupported/1,remove_selection/1,mpred_withdraw1/2,

  mpred_post1/2,get_mpred_assertion_status/3,mpred_post_update4/4,get_mpred_support_status/5,same_file_facts/2,clause_asserted_u/1,


  mpred_run/0,
  
  fa_to_p/3,
  call_u_no_bc/1,
  with_umt/2,
          asserta_u/1,assert_u/1,assertz_u/1,retract_u/1,retractall_u/1,
          retract_u0/1,retractall_u0/1,
  clause_u/1,clause_u/2,clause_u/3,
  % clause_ii/3,

  lookup_u/1,

mpred_load_term/1,
pos_2_neg/2,
not_not_ignore_quietly_ex/1,
mpred_trace_all/0,
really_mpred_mark/4,


unassertable/1,
log_failure_red/0,
convention_to_symbolic_mt/5,
attvar_op_fully/2,
closest_u/2,
pred_check/1,
pp_why/0,
get_unnegated_functor/3,
is_user_reason/1,
mpred_retract_i_or_warn_1/1,
mpred_is_silent/0,
pp_why/1,
bad_head_pred/1,
get_mpred_current_db/1,
mpred_call_no_bc0/1,
to_real_mt/3,
all_closed/1,
convention_to_mt/4,

copy_term_vn/2,
get_assertion_head_unnegated/2,
mpred_undo1/1,
convention_to_symbolic_mt_ec/5,

push_current_choice/1,



 get_fc_mode/3,mpred_rem_support_if_exists/2,get_tms_mode/2,

  stop_trace/1,with_mpred_trace_exec/1,
  select_next_fact/1,supporters_list/2,triggerSupports/2,well_founded/1,well_founded_list/2,
  do_assumpts/2,mpred_do_fcnt/2,mpred_do_fcpt/2,mpred_fwc1/1,mpred_do_rule/1,mpred_descendant1/3,mpred_eval_rhs1/2,mpred_nf1/2,
  mpred_post1/2,mpred_withdraw/1,mpred_withdraw/2,mpred_remove/1,
  mpred_remove/2,mpred_post1/2,
  mpred_pp_db_justification1/3,mpred_pp_db_justifications2/4,mpred_spy1/3,
  mpred_unfwc_check_triggers0/1,mpred_unfwc1/1,mpred_why1/1,mpred_blast/1
  % trigger_trigger1/2  , trigger_trigger/3,
  ]).
:- set_module(class(library)).
:- endif.

nr_lc_ex(G):- no_repeats(loop_check(G,trace_or_throw(looped(G)))).

%:- use_module(mpred_kb_ops).
%:- use_module(library(util_varnames)).
%:- use_module(library(no_repeats)).

:- include('mpred_header.pi').
:- current_prolog_flag(mpred_pfc_silent,false)-> true ; set_prolog_flag(mpred_pfc_silent,true).

:- dynamic(lmcache:mpred_is_spying_pred/2).

:- use_module(library(logicmoo_common)).
:- use_module(library(logicmoo/misc_terms)).

:- system:use_module(library(edinburgh)).
:- system:use_module(library(ordsets)).
:- system:use_module(library(oset)).

%:- include(library(pfc_test)).
:- meta_predicate
      %call_mp(+,*,+),
      call_u(*),
      call_u_mp_lc(*,*,*,*),
      call_u_no_bc(+),
      clause_asserted_u(+),
      clause_u(*),
      clause_u(*,*,-),
      clause_u(*,-),
      each_E(*,+,+),
      fc_eval_action(*,*),
      fix_mp(+,+,-,-),
      %foreach(*,?),
      %lookup_kb(?,*),
      %lookup_kb(?,*,?),
      quietly_ex(*),
      ain_expanded(:),
      mpred_add(:),
      mpred_ain(:),
      %mpred_BC_CACHE(+,+),
      %mpred_BC_CACHE0(+,+),
      mpred_call_no_bc0(*),
      mpred_fact_mp(?,*),      
      mpred_METACALL(*,+),
      mpred_METACALL(*,-,+), % 1,-,+
      
      % pfcl_do(*), % not all arg1s are callable
      retract_u0(+),
      with_no_breaks(*),
      with_umt(+,+),
      brake(*),
      with_no_mpred_trace_exec(*),
      with_mpred_trace_exec(*),
      with_fc_mode(+,*).
      

:- meta_predicate mpred_retract_i_or_warn(*).
:- meta_predicate mpred_retract_i_or_warn_1(*).
:- meta_predicate not_not_ignore_quietly_ex(*).
:- meta_predicate must_notrace_pfc(*).
:- multifile(baseKB:safe_wrap/4).
:- dynamic(baseKB:safe_wrap/4).


:- op(700,xfx,('univ_safe')).


:- system:use_module(library(lists)).

:- module_transparent lookup_u/1,lookup_u/2,mpred_unfwc_check_triggers0/1,mpred_unfwc1/1,mpred_why1/1,mpred_blast/1.

quietly_must_ex(G):- tracing -> (notrace,call_cleanup(must_ex(G),trace)); quietly_must(G).
must_ex(G):- (catch(quietly(G),Error,(wdmsg(error_must_ex(G,Error)),fail))*->true;(wdmsg_pretty(must_ex(G)),if_interactive((ignore(rtrace(G)),wdmsg_pretty(must_ex(G)), break)))).

must_notrace_pfc(G):- must_ex((G)).

:- thread_local(t_l:assert_dir/1).

/*

  ?- dynamic(f2/2),gensym(nnn,N),sanity_attvar_08:attr_bind([name_variable(A, 'ExIn'), form_sk(A, 'SKF-66')], true),
   IN=f2(N,A),OUT=f2(N,B),copy_term_vn(IN,OUT),
  asserta_u(IN),clause_asserted_u(OUT),!. % ,nl,writeq(A=@=B).
*/
:- meta_predicate with_each_item(:,+,+).
%% with_each_item(:P2,+EleList,+ArgList) is nondet.
%
% Call apply(P,[Ele|ArgList]) on each Ele(ment) in the EleList.
%
% EleList is a  List, a Conjuction Terms or a single element.
%
with_each_item(P,HV,S):- var(HV),!, apply(P,[HV|S]).
with_each_item(P,M:HT,S) :- !, must_be(atom,M), M:with_each_item(P,HT,S).
with_each_item(P,[H|T],S) :- !, apply(P,[H|S]), with_each_item(P,T,S).
with_each_item(P,(H,T),S) :- !, with_each_item(P,H,S), with_each_item(P,T,S).
with_each_item(P,H,S) :- apply(P,[H|S]).




%% mpred_database_term(:PI, -TYPE) is nondet.
%
% is true iff F/A is something that Pfc adds to
% the database and should not be present in an empty Pfc database
%

:- nodebug(logicmoo(pfc)).

/*
% mined from program database

:- dynamic(baseKB:pt/2).                   
:- system:import(baseKB:pt/2).

:- dynamic(baseKB:pm/1).                   
:- system:import(baseKB:pm/1).

:- dynamic(baseKB:nt/3).                   
:- system:import(baseKB:nt/3).

:- dynamic(baseKB:spft/3).                   
:- system:import(baseKB:spft/3).

:- dynamic(baseKB:bt/2).                   
:- system:import(baseKB:bt/2).

:- dynamic(baseKB:do_and_undo/2).
:- system:import(baseKB:do_and_undo/2).

:- dynamic(baseKB:tms/1).                   
:- system:import(baseKB:tms/1).

*/

/*
*/
:- dynamic(baseKB:mpred_is_tracing_exec/0).
:- export(baseKB:mpred_is_tracing_exec/0).

mpred_database_term_syntax(do_and_undo,2,rule(_)).

mpred_database_term_syntax(('::::'),2,rule(_)).
mpred_database_term_syntax((<-),2,rule(_)).
mpred_database_term_syntax((<==>),2,rule(_)).
mpred_database_term_syntax((==>),2,rule(_)).

mpred_database_term_syntax(mdefault,1,fact(_)).
mpred_database_term_syntax((==>),1,fact(_)).
mpred_database_term_syntax((~),1,fact(_)).


baseKB:mpred_database_term(F,A,syntaxic(T)):- pfc_lib:mpred_database_term_syntax(F,A,T).
baseKB:mpred_database_term(F,A,T):- pfc_lib:mpred_core_database_term(F,A,T).

mpred_core_database_term(genlPreds,2,fact(_)).
% mpred_core_database_term(rtArgsVerbatum,1,fact(_)).

% forward,backward chaining database
mpred_core_database_term(spft,3,support).

mpred_core_database_term(nt,3,trigger(pt)).
mpred_core_database_term(pt,2,trigger(nt)).
mpred_core_database_term(bt,2,trigger(bt)).

% transient state
mpred_core_database_term(actn,1,state).
mpred_core_database_term(que,2,state).
mpred_core_database_term(hs,1,state).



% forward,backward settings
mpred_core_database_term(mpred_current_db,1,setting).
mpred_core_database_term(pfcSelect,1,setting).
mpred_core_database_term(tms,1,setting).
mpred_core_database_term(pm,1,setting).

% debug settings
mpred_core_database_term(mpred_is_tracing_exec,0,debug).
%mpred_core_database_term(lmcache:mpred_is_spying_pred,2,debug).
mpred_core_database_term(mpred_warnings,1,debug).
% mpred_core_database_term(t_l:whybuffer,2,debug).

%mpred_core_database_term(mpred_prop,4,fact(_)).

mpred_core_database_term(predicateConventionMt,2,fact(_)).
% mpred_core_database_term(genlMt,2,fact(_)).
%mpred_core_database_term(arity,2,fact(_)).
%mpred_core_database_term(rtArgsVerbatum,1,fact(_)).

                         
declare_pfc_support(Dyn,M):- forall(mpred_database_term(F,A,_),M:call(Dyn,M:F/A)).
declare_pfc_support(M):- declare_pfc_support(dynamic,M).
export_pfc_support(M):- declare_pfc_support(export,M).
import_pfc_support(M,Into):- declare_pfc_support(Into:import,M).

import_everywhere(BaseKB):-   
   declare_pfc_support(BaseKB),
   export_pfc_support(BaseKB),
   import_pfc_support(BaseKB,system).

import_everywhere:- 
  forall(mpred_database_term(F,A,_),
    (dynamic(baseKB:F/A),baseKB:export(baseKB:F/A),
     system:import(baseKB:F/A))).

%:- import_everywhere.

:- thread_local(t_l:whybuffer/2).
% :- dynamic(baseKB:que/2).

:- meta_predicate show_mpred_success(*,*).
show_mpred_success(Type,G):- G*->mpred_trace_msg(success(Type,G)) ; fail.

% :- ensure_loaded(library(logicmoo_utils)).

:- module_transparent((assert_u_confirmed_was_missing/1,mpred_trace_exec/0, % pfcl_do/1,
  call_u_mp_fa/4,call_u_mp_lc/4,
  mpred_post1/2,get_mpred_assertion_status/3,mpred_post_update4/4,get_mpred_support_status/5,same_file_facts/2,
 
                       asserta_u/1,assert_u/1,assertz_u/1,retract_u/1,retractall_u/1,

                       retract_u0/1,retractall_u0/1,
  mpred_trace_op/3)).

:- thread_local(t_l:no_breaks/0).

decl_rt(RT) :- 
 '@'(((
   sanity(atom(RT)),
   univ_safe(Head , [RT,FP]),
   AIN = ((Head :- cwc, /* dmsg_pretty(warn(call(Head))), */ mpred_prop(M,FP,_,RT))),
   (clause_asserted(AIN) -> 
    (nop(listing(RT)),
     sanity((predicate_property(RHead,number_of_clauses(CL)),predicate_property(RHead,number_of_rules(RL)),CL=RL)));
     
  ((
   (current_predicate(RT/1)->
   ( nop(listing(RT)),
     RHead univ_safe [RT,F/A],
     forall(retract(RHead),ain(mpred_prop(M,F,A,RT))),
     forall(retract(Head),(get_arity(FP,F,A),sanity(atom(F)),sanity(integer(A)),ain(mpred_prop(M,F,A,RT)))),
     sanity((predicate_property(RHead,number_of_clauses(CL)),CL==0)),
     sanity((predicate_property(RHead,number_of_rules(RL)),RL==0)),
     abolish(RT,1));true),

   asserta(AIN),
  % compile_predicates([Head]),
   nop(decl_rt(RT))))))),baseKB).

%quietly_ex(G):- !,G,!.
quietly_ex(G):- quietly(G).

trace_or_throw_ex(G):- trace_or_throw(G).

% =================================================
% ==============  UTILS BEGIN        ==============
% =================================================
% copy_term_vn(A,A):- current_prolog_flag(unsafe_speedups , true) ,!.
copy_term_vn(B,A):- notrace(copy_term_vn0(B,A)).
copy_term_vn0(B,A):- ground(B),!,A=B.
copy_term_vn0(B,A):- !,copy_term(B,A).
copy_term_vn0(B,A):- need_speed,!,copy_term(B,A).
copy_term_vn0(B,A):- get_varname_list(Vs),length(Vs,L),L<30, shared_vars(B,Vs,Shared),Shared\==[],!,copy_term(B+Vs,A+Vs2),append(Vs,Vs2,Vs3),set_varname_list(Vs3),!.
copy_term_vn0(B,A):- nb_current('$old_variable_names',Vs),length(Vs,L),L<30, shared_vars(B,Vs,Shared),Shared\==[],!,copy_term(B+Vs,A+Vs2),append(Vs,Vs2,Vs3),b_setval('$old_variable_names',Vs3),!.
copy_term_vn0(B,A):- copy_term(B,A).


setup_mpred_ops:-
          op(500,fx,'-'),
          op(300,fx,'~'),
          op(1050,xfx,('==>')),
          op(1050,xfx,'<==>'),
          op(1050,xfx,('<-')),
          op(1100,fx,('==>')),
          op(1150,xfx,('::::')),
          op(500,fx,user:'-'),
          op(300,fx,user:'~'),
          op(1050,xfx,(user:'==>')),
          op(1050,xfx,user:'<==>'),
          op(1050,xfx,(user:'<-')),
          op(1100,fx,(user:'==>')),
          op(1150,xfx,(user:'::::')).
:- setup_mpred_ops.


% :- mpred_ain_in_thread.
% :- current_thread_pool(ain_pool)->true;thread_pool_create(ain_pool,20,[]).
:- multifile thread_pool:create_pool/1.
:- dynamic thread_pool:create_pool/1.
thread_pool:create_pool(ain_pool) :-
    thread_pool_create(ain_pool, 50, [detached(true)] ).

:- use_module(library(http/thread_httpd)).
:- use_module(library(thread_pool)).

is_ain_pool_empty:- thread_pool_property(ain_pool,running(N)),!,N==0.
is_ain_pool_empty.

show_ain_pool:- forall(thread_pool_property(ain_pool,PP),fmt(show_ain_pool(PP))).

await_ain_pool:- is_ain_pool_empty->true;(repeat, sleep(0.005), is_ain_pool_empty).

ain_in_thread(MAIN):- strip_module(MAIN,M,AIN), call_in_thread(M:ain(AIN)).

call_in_thread(MG):- strip_module(MG,M,G), copy_term(M:G,GG,_),numbervars(GG,0,_),term_to_atom(GG,TN), call_in_thread(TN,M,G).

call_in_thread(TN,M,G):- thread_property(_,alias(TN)),!,dmsg_pretty(already_queued(M,G)).
call_in_thread(TN,M,G):- current_why(Why), thread_create_in_pool(ain_pool,call_in_thread_code(M,G,Why,TN),_Id,[alias(TN)]).

call_in_thread_code(M,G,Why,TN):- 
 with_only_current_why(Why,
   catch(( M:G-> nop(dmsg_pretty(suceeded(exit,TN)));dmsg_pretty(failed(exit,TN))),E,dmsg_pretty(error(E-->TN)))).
       
% why_dmsg(Why,Msg):- with_current_why(Why,dmsg_pretty(Msg)).

u_to_uu(U,(U,ax)):- var(U),!.
u_to_uu(U,U):- nonvar(U),U=(_,_),!.
u_to_uu([U|More],UU):-list_to_conj([U|More],C),!,u_to_uu(C,UU).
u_to_uu(U,(U,ax)):-!.

%% get_source_uu( :TermU) is det.
%
% Get Source Ref (Current file or User)
%
:- module_transparent((get_source_uu)/1).
get_source_uu(UU):- must(((get_source_ref1(U),u_to_uu(U,UU)))),!.

get_source_ref1(U):- quietly_ex(((current_why(U),nonvar(U)));ground(U)),!.
get_source_ref1(U):- quietly_ex(((get_source_mfl(U)))),!.


:- module_transparent((get_why_uu)/1).
get_why_uu(UU):- findall(U,current_why(U),Whys),Whys\==[],!,u_to_uu(Whys,UU).
get_why_uu(UU):- get_source_uu(UU),!.


get_startup_uu(UU):-u_to_uu((isRuntime,mfl4(VarNameZ,baseKB, user_input, _)),UU),varnames_load_context(VarNameZ).

is_user_reason((_,U)):-atomic(U).
only_is_user_reason((U1,U2)):- freeze(U2,is_user_reason((U1,U2))).

is_user_fact(P):-get_first_user_reason(P,UU),is_user_reason(UU).


get_first_real_user_reason(P,UU):- nonvar(P), UU=(F,T),
  quietly_ex((  ((((lookup_spft(P,F,T))),is_user_reason(UU))*-> true;
    ((((lookup_spft(P,F,T))), \+ is_user_reason(UU))*-> (!,fail) ; fail)))).

get_first_user_reason(P,(F,T)):-
  UU=(F,T),
  ((((lookup_spft(P,F,T))),is_user_reason(UU))*-> true;
    ((((lookup_spft(P,F,T))), \+ is_user_reason(UU))*-> (!,fail) ;
       (clause_asserted_u(P),get_source_uu(UU),is_user_reason(UU)))),!.
get_first_user_reason(_,UU):- get_why_uu(UU),is_user_reason(UU),!.
get_first_user_reason(_,UU):- get_why_uu(UU),!.
get_first_user_reason(P,UU):- must_ex(ignore(((get_first_user_reason0(P,UU))))),!.
get_first_user_reason0(_,(M,ax)):-get_source_mfl(M).

%get_first_user_reason(_,UU):- get_source_uu(UU),\+is_user_reason(UU). % ignore(get_source_uu(UU)).

%:- export(mpred_at_box:defaultAssertMt/1).
%:- system:import(defaultAssertMt/1).
%:- pfc_lib:import(mpred_at_box:defaultAssertMt/1).

:- module_transparent((get_source_mfl)/1).
get_source_mfl(M):- current_why(M), nonvar(M) , M =mfl4(_VarNameZ,_,_,_).
get_source_mfl(mfl4(VarNameZ,M,F,L)):- defaultAssertMt(M), current_source_location(F,L),varnames_load_context(VarNameZ).

get_source_mfl(mfl4(VarNameZ,M,F,L)):- defaultAssertMt(M), current_source_file(F:L),varnames_load_context(VarNameZ).
get_source_mfl(mfl4(VarNameZ,M,F,_L)):- defaultAssertMt(M), current_source_file(F),varnames_load_context(VarNameZ).
get_source_mfl(mfl4(VarNameZ,M,_F,_L)):- defaultAssertMt(M), varnames_load_context(VarNameZ).
%get_source_mfl(M):- (defaultAssertMt(M)->true;(atom(M)->(module_property(M,class(_)),!);(var(M),module_property(M,class(_))))).
get_source_mfl(M):- fail,dtrace,
 ((defaultAssertMt(M) -> !;
 (atom(M)->(module_property(M,class(_)),!);
    mpred_error(no_source_ref(M))))).

is_source_ref1(_).

unassertable(Var):-var(Var),!.
unassertable((M:V)):-nonvar(M),!,unassertable(V).
unassertable((_;_)).
unassertable((_,_)).

:- style_check(+discontiguous).

to_real_mt(_Why,abox,ABOX):- defaultAssertMt(ABOX),!.
to_real_mt(_Why,tbox,TBOX):- get_current_default_tbox(TBOX),!.
to_real_mt(_Why,BOX,BOX).

is_ftVarq(V):-notrace(is_ftVar(V)).

%% fix_mp(+Why,+I,-O) is det.
%
% Ensure modules are correct when asserting/calling information into the correct MTs
%
%fix_mp(Why,I,UO):- compound(UO),dtrace,UO=(U:O),!,quietly_must_ex(fix_mp(Why,I,U,O)).
% fix_mp(Why,I,MT:UO):- current_prolog_flag(unsafe_speedups , true) , !, strip_module(I,_,UO),defaultAssertMt(MT).
fix_mp(Why,I,UO):- quietly_must_ex(fix_mp(Why,I,U,O)),maybe_prepend_mt(U,O,UO).


fix_mp(Why,G,M,GOO):-
  must_ex((quietly_ex((fix_mp0(Why,G,M,GO),strip_module(GO,_,GOO))))).

meta_split(PQ,P,OP,Q):-PQ  univ_safe [OP,P,Q],arg(_,v('<-','==>','<==>','==>',(','),(';')),OP).

fix_mp0(Nonvar,Var,ABox,VarO):- sanity(nonvar(Nonvar)), is_ftVarq(Var),!,Var=VarO,defaultAssertMt(ABox),!.
fix_mp0(Why, '~'(G), M, '~'(GO)):-nonvar(G),!,fix_mp0(Why,G,M,GO).
fix_mp0(Why,'?-'(G),M, '?-'(GO)):-nonvar(G),!,fix_mp0(Why,G,M,GO).
fix_mp0(Why,':-'(G),M, ':-'(GO)):-nonvar(G),!,fix_mp0(Why,G,M,GO).
fix_mp0(Why,(:- G),M,(:- GO)):- !, fix_mp0(Why,G,M,GO).
fix_mp0(Why,(G :- B),M,( GO :- B)):- !, fix_mp0(Why,G,M,GO).
% fix_mp0(Why,(G <- B),M,( GO <- B)):- !, fix_mp0(Why,G,M,GO).
fix_mp0(Why,CM:(G :- B),M,( GO :- B)):- !, CM:fix_mp0(Why,G,M,GO).

%fix_mp0(_Why,spft(P,(mfl4(VarNameZ,FromMt,File,Lineno),UserWhy)),FromMt,spft(P,(mfl4(VarNameZ,FromMt,File,Lineno),UserWhy))):-!.

fix_mp0(Why,M:P,MT,P):- to_real_mt(Why,M,MT)->M\==MT,!,fix_mp0(Why,MT:P,MT,P).

% fix_mp0(Why,PQ,M,PPQQ):- meta_split(PQ,P,OP,Q),!,fix_mp(Why,P,M1,PP),fix_mp(Why,Q,M2,QQ),(M1\==M2 -> (QQ\==Q->M=M2;M=M1) ; M=M1),!,meta_split(PPQQ,PP,OP,QQ).

fix_mp0(_Why,Mt:P,Mt,P):- clause_bq(mtExact(Mt)),!.


fix_mp0(Why,G,M,GO):- /*Why = change(_,_),*/ strip_module(G,WAZ,GO),
  %  ((G==GO; (context_module(CM),CM==WAZ) ; (defaultAssertMt(ABox),ABox==WAZ) ; \+ clause_bq(mtHybrid(WAZ)) ; (header_sane==WAZ); (abox==WAZ))),
   must_ex(get_unnegated_functor(GO,F,A)) 
     -> % nr_lc_ex
     (WAZ:convention_to_mt(WAZ,Why,F,A,M)),!.


fix_mp0(_Why,Mt:P,Mt,P):- clause_bq(mtHybrid(Mt)),!.

fix_mp0(_Why,I,ABox,I):- defaultAssertMt(ABox),!.

/*
fix_mp(Why,Junct,ABox,Result):- fail, (mpred_db_type(Junct,rule(_));(safe_functor(Junct,F,_),bad_head_pred(F))),!,
   must_ex((mpred_rule_hb(Junct,HC,BC),nonvar(HC))),
   Junct univ_safe [F|List],
   must_maplist(fix_mp(call(hb(HC,BC,Op))),List,ListO),
   Result univ_safe [F|ListO],
   defaultAssertMt(ABox),!.

%fix_mp(call(hb(HC,_BC,Op)),H,M,HH):- contains_var(H,HC),!,
%   fix_mp(change(assert,Op),H,M,HH).

fix_mp(call(hb(_HC,BC,Op)),B,M,BB):- contains_var(B,BC),B\=@=BC,!,
   fix_mp(call(Op),B,M,BB).



% fix_mp(Why,Unassertable,_,_):- Why = clause(_,_), unassertable(Unassertable),!,trace_or_throw_ex(unassertable_fix_mp(Why,Unassertable)).

*/
system_between(A,B,C):-call(call,between,A,B,C).

clause_bq(G):-notrace(clause_b(G)),!.

mpred_truth_value(Call,vTrue,vAsserted):-clause_b(Call),!.
mpred_truth_value(Call,vTrue,vDeduced):-call_u(Call),!.
mpred_truth_value(_Call,vUnknown,vFailed).

convention_to_mt(From,Why,F,A,RealMt):-notrace((convention_to_symbolic_mt_ec(From,Why,F,A,Mt),to_real_mt(Why,Mt,RealMt))).

get_unnegated_mfa(M:G,M,F,A):-!,get_unnegated_functor(G,F,A).
get_unnegated_mfa(G,M,F,A):- strip_module(G,M0,_),get_unnegated_functor(G,F,A),
                 convention_to_mt(M0,get_unnegated_mfa(G,M,F,A),F,A,M).

get_unnegated_functor(G,F,A):-notrace(( strip_module(G,_,GO),
   get_assertion_head_unnegated(GO,Unwrap),
   nonvar(Unwrap),
   safe_functor(Unwrap,F,A),
   ignore(show_failure(\+ bad_head_pred(F))))),!.
   

:- module_transparent( (get_assertion_head_unnegated)/2).

get_assertion_head_unnegated(Head,Unwrap):-
  notrace((get_assertion_head(Head,Mid),
  maybe_unnegated(Mid,Unwrap))).


maybe_unnegated(Head,Unwrap):- notrace((maybe_unnegated0(Head,Unwrap))).
maybe_unnegated0(Head,Head):- \+ compound(Head),!.
maybe_unnegated0(~ Head,Unwrap):- \+ is_ftVarq(Head),!, get_assertion_head(Head,Unwrap).
maybe_unnegated0( \+ Head,Unwrap):- \+ is_ftVarq(Head),!, get_assertion_head(Head,Unwrap).
maybe_unnegated0(Head,Unwrap):- get_assertion_head(Head,Unwrap).


get_assertion_head(Head,Head):- \+ compound(Head),!.
get_assertion_head(Head,Unwrap):- is_ftVarq(Head),!,Head=Unwrap.
get_assertion_head( ( Head :- _ ),Unwrap):- nonvar(Head), !, get_assertion_head(Head,Unwrap).
get_assertion_head(Head,Unwrap):- strip_module(Head,_,HeadM),Head\=@=HeadM,!,get_assertion_head(HeadM,Unwrap).
% Should?
get_assertion_head( ( _,Head),Unwrap):- \+ is_ftVarq(Head),!, get_assertion_head(Head,Unwrap).
% Should?
get_assertion_head((P/_),PP):- \+ is_ftVarq(P),!,get_assertion_head(P,PP).
% Should?
% NOOOO get_assertion_head((P<-_),PP):-compound(P),!,get_assertion_head(P,PP).
% disabled
get_assertion_head( Head,UnwrapO):- fail, mpred_rule_hb(Head,Unwrap,_),nonvar(Unwrap),
  Head \=@= Unwrap,!,get_assertion_head(Unwrap,UnwrapO).
get_assertion_head(P,P).


get_head_term(Form,Form):-var(Form),!.
get_head_term(F/A,Form):- integer(A),safe_functor(Form,F,A),!.
get_head_term(Form0,Form):- get_assertion_head_unnegated(Form0,Form).


bad_head_pred([]).
bad_head_pred('[]').
bad_head_pred((.)).
bad_head_pred('{}').
bad_head_pred('[|]').
bad_head_pred(',').
bad_head_pred(':').
bad_head_pred('/').
bad_head_pred(':-').
bad_head_pred(';').
bad_head_pred( \+ ).
bad_head_pred_neg('~').

% bad_head_pred('=>').
% bad_head_pred('<-').
% bad_head_pred('==>').
% Probably bad_head_pred('==>').

% the next line transforms to pfc_lib:convention_to_symbolic_mt(_From,_Why,A, _, B) :- call(ereq, predicateConventionMt(A, B)), !.

convention_to_symbolic_mt_ec(From,Why,F,A,Mt):- notrace(convention_to_symbolic_mt(From,Why,F,A,Mt)).

/*convention_to_symbolic_mt(_From,_Why,predicateConventionMt,2,baseKB):-!.
convention_to_symbolic_mt(_From,_Why,genlMt,2,baseKB):-!.
convention_to_symbolic_mt(_From,_Why,mtNonAssertable,1,baseKB):-!.
convention_to_symbolic_mt(_From,_Why,mtProlog,1,baseKB):-!.
convention_to_symbolic_mt(_From,_Why,functorDeclares,1,baseKB):-!.
convention_to_symbolic_mt(_From,_Why,functorIsMacro,1,baseKB):-!.
*/

convention_to_symbolic_mt(_From,_Why,mtHybrid,1,baseKB):-!.
convention_to_symbolic_mt(From,_Why,F,_,Mt):-  clause_bq(From:predicateConventionMt(F,Mt)),!.
convention_to_symbolic_mt(_From,_Why,F,A,M):- lmcache:already_decl(kb_global,M,F,A),!.




% convention_to_symbolic_mt(From,Why,F,A,Error):- bad_head_pred(F),!,dumpST,dmsg_pretty(bad_head_pred(F)),break,trace_or_throw_ex(error_convention_to_symbolic_mt(From,Why,F,A,Error)).
convention_to_symbolic_mt(_From,_Why,F,A,M):- lmcache:already_decl(kb_global,M,F,A),!.
convention_to_symbolic_mt(_From,_Why,F,A,abox):- mpred_database_term_syntax(F,A,_).
convention_to_symbolic_mt(_From,_Why,F,A,abox):- lmcache:already_decl(kb_shared,_,F,A),!.
convention_to_symbolic_mt(_From,_Why,F,A,abox):- lmcache:already_decl(kb_local,_,F,A),!.

convention_to_symbolic_mt(_From,_Why,F,A,Mt):-  safe_functor(P,F,A),predicate_property(P,imported_from(Mt)),!.
convention_to_symbolic_mt(_From,_Why,F,A,   M):- lmcache:already_decl(kb_global,M,F,A),!.
convention_to_symbolic_mt(_From,_Why,F,A,abox):- mpred_database_term(F,A,_).
convention_to_symbolic_mt(_From,_Why,F,A,abox):- clause_bq(safe_wrap(_M,F,A,ereq)).


convention_to_symbolic_mt(From,Why,F,A,Error):- bad_head_pred(F),!,Error = From,
  if_interactive((
   dumpST,dmsg_pretty(bad_head_pred(F)),break,trace_or_throw_ex(error_convention_to_symbolic_mt(From,Why,F,A,Error)))).


% convention_to_symbolic_mt(_From,_Why,_,_,M):- atom(M),!.

full_transform_warn_if_changed(_,MH,MHH):-!,MH=MHH.
full_transform_warn_if_changed(Why,MH,MHH):- full_transform(Why,MH,MHH),!,sanity(MH=@=MHH).
full_transform_warn_if_same(Why,MH,MHH):- full_transform(Why,MH,MHH),!,sanity(MH \=@= MHH).

/*
full_transform_and_orignal(Why,MH,MHO):- full_transform(Why,MH,MHH),
      (MH=@=MHH -> MHO=MH ; (MHO = MHH ; MHO = MH )).



full_transform(Op,ISA,SentO):- nonvar(ISA),isa(I,C)=ISA,!, must_ex(fully_expand_real(Op,isa(I,C),SentO)),!.
full_transform(Op,Sent,SentO):- safe_functor(Sent,F,A),may_fully_expand(F,A),!,
   must_ex(fully_expand_real(Op,Sent,SentO)),!.

*/
%:- use_module(mpred_expansion).

/*
full_transform(Why,MH,MHH):- has_skolem_attrvars(MH),!,
 rtrace(fully_expand_real(change(assert,skolems(Why)),MH,MHH)),!,
   nop(sanity(on_f_debug(same_modules(MH,MHH)))),!.
*/
%full_transform(Why,MH,MHH):- \+ compound(MH),!,
%   must_det(fully_expand_real(change(assert,Why),MH,MHH)),!.
     % nop(sanity(on_f_debug(same_modules(MH,MHH)))).
%full_transform(Op,==> CI,SentO):- nonvar(CI),!, full_transform(Op,CI,SentO).
%full_transform(Op,isa(I,C),SentO):- nonvar(C),!,must_ex(fully_expand_real(Op,isa(I,C),SentO)),!.
%full_transform(_,CI,SentO):- CI univ_safe [_C,I], atom(I),!,if_defined(do_renames(CI,SentO),CI=SentO),!.
full_transform(Why,MH,MHH):-
 must_det(fully_expand_real(change(assert,Why),MH,MHH)),!,
 nop(sanity(on_f_debug(same_modules(MH,MHH)))).

same_modules(MH,MHH):- strip_module(MH,HM,_),strip_module(MHH,HHM,_),!,
   HM==HHM.

%full_transform_compound(Op,ISA,SentO):- compound(ISA),isa(I,C)=ISA,!, must_ex(fully_expand_real(Op,isa(I,C),SentO)),!.
%full_transform_compound(Why,MH,MHH):-
% must_det(fully_expand_real(change(assert,Why),MH,MHH)),!.
   % nop(sanity(on_f_debug(same_modules(MH,MHH)))).


%:- if(\+ current_prolog_flag(umt_local,false)).

listing_i(MP):- % strip_module(MP,M,P),!,
 forall(to_mpi_matcher(MP,MM:PI),
   listing_mpi(MP,MM:PI)). 

:- reconsult(library(listing)).
%:- system:reexport(library(xlisting)).

%listing_mpi(_MP,MMPI):-  (predicate_property(MMPI,number_of_clauses(NC))->NC==0;true),!,
%  unify_listing_header(MMPI),prolog_listing_list_clauses(MMPI, none),!.
%listing_mpi(_MP,MMPI):- !,unify_listing_header(MMPI), 
%   prolog_listing:list_clauses(MMPI, none).
listing_mpi(_MP,MM:PI):- forall(clause_u(MM:PI,B,R),foo:once(portray_hbr(MM:PI,B,R))).

listing_u(P):-call_u_no_bc(xlisting((P,-lmcache,/*-spft,*/-xlisting))),!.

attvar_op_fully(What,MH):- !, attvar_op(What,MH).
%attvar_op_fully(What,M:H):- must_notrace_pfc(full_transform_warn_if_changed(change(What,attvar_op_fully),H,true,HH,true)),!,each_E(attvar_op(What),M:HH,[]).
%attvar_op_fully(What,MH):- full_transform_warn_if_changed(What, MH,MHH),each_E(attvar_op(What),MHH,[]).

throw_depricated:- trace_or_throw_ex(throw_depricated).

do_db_checks:- fail.

assert_u(MH):- assert_u_no_dep(MH).

assert_u_no_dep(X):- do_db_checks, check_never_assert(X),fail.
assert_u_no_dep(MH):- fix_mp(change(assert,assert_u),MH,MHA),
    attvar_op_fully(db_op_call(assert,assert_i), MHA),expire_tabled_list(MHA).

asserta_u(X):-  do_db_checks, check_never_assert(X),fail.
asserta_u(MH):- fix_mp(change(assert,asserta_u),MH,MHA),attvar_op_fully(db_op_call(asserta,asserta_i),MHA).

assertz_u(X):- do_db_checks, check_never_assert(X),fail.
assertz_u(MH):- fix_mp(change(assert,assertz_u),MH,MHA),attvar_op_fully(db_op_call(asserta,assertz_i),MHA).

% retract_u((H:-B)):- !, show_failure(retract((H:-B))).
retract_u(H):- retract_u0(H) *-> true; ((fail,attvar_op_fully(db_op_call(retract,retract_u0),H))).

retract_u0(X):- do_db_checks, check_never_retract(X),fail.
retract_u0(H0):- strip_module(H0,_,H),(H = ( \+ _ )),!,trace_or_throw_ex(mpred_warn(retract_u(H0))),expire_tabled_list(H).
retract_u0(M:(H:-B)):- atom(M),!, M:clause_u(H,B,R),erase(R),expire_tabled_list(H).
retract_u0(M:(H)):- atom(M),!, M:clause_u(H,true,R),erase(R),expire_tabled_list(H).
retract_u0((H:-B)):-!,clause_u(H,B,R),erase(R),expire_tabled_list(H).
retract_u0(H):- clause_u(H,true,R),erase(R),expire_tabled_list(H).

:- lmcache:import(retract_u0/1).

retractall_u(X):- do_db_checks, check_never_retract(X),fail.
retractall_u(H):- attvar_op_fully(db_op_call(retractall,retractall_u0),H).

retractall_u0(X):- do_db_checks, check_never_retract(X),fail.
retractall_u0(H):- forall(clause_u(H,_,R),erase(R)),expire_tabled_list(H).



clause_u(C):- expand_to_hb(C,H,B),!,clause_u(H,B).


%% clause_u( ?H, ?B) is semidet.
%

% clause_u(H,B):-  current_prolog_flag(unsafe_speedups , true) , ground(H:B),!,clause(H,B).
clause_u(H,B):- clause_u(H,B,_).
%clause_u(H,B):- clause_true( ==>( B , H) ).
%clause_u(H,B):- clause_true( <-( H , B) ).

match_attvar_clauses(HH,BB,H,B):- 
    matrialize_clause((H:-B),C),
 matrialize_clause((HH:-BB),CC),!,
 C=CC.

matrialize_clause((H:-B),(H:-B)):- \+ compound(B),!.
matrialize_clause((H:-attr_bind(Attribs,B)),(H:-B)):-!, attr_bind(Attribs).
matrialize_clause((H:-attr_bind(Attribs)),(H:-true)):-!, attr_bind(Attribs).

:- set_prolog_flag(clause_u_h_exact,false).
:- set_prolog_flag(clause_u_mh_inherit,false).

should_inherit(_, M,_,TF):- clause_bq(mtInherits(M)),!,TF=true.
should_inherit(_, M,_,TF):- clause_bq(mtNotInherits(M)),!,TF=false.
should_inherit(h, _,_,TF):- current_prolog_flag(clause_u_h_exact,false) -> TF = true ; TF = false.
should_inherit(mh,_,_,TF):- current_prolog_flag(clause_u_mh_inherit,TF).

%% clause_u( +H, ?B, ?Why) is semidet.
%
% PFC Clause.
%
clause_u(MH,B,R):- nonvar(R),!,must_ex(clause_i(M:H,B,R)),must_ex((MH=(M:H);MH=(H))),!.
clause_u(H,B,Ref):-var(H),!,trace_or_throw_ex(var_clause_u(H,B,Ref)).
clause_u((H:-BB),B,Ref):- is_true(B),!, trace_or_throw_ex(malformed(clause_u((H:-BB),B,Ref))),clause_u(H,BB,Ref).
clause_u((H:-B),BB,Ref):- is_true(B),!, trace_or_throw_ex(malformed(clause_u((H:-B),BB,Ref))),clause_u(H,BB,Ref).

clause_u(H,B,R):-clause_u_visible(H,B,R),B \= inherit_above(_,_).

module_clause(MHB):- strip_module(MHB,M,HB), expand_to_hb(HB,H,B),clause(M:H,B,R),clause_property(R,module(CM)),CM==M.
module_clause(MHH,BM):- strip_module(MHH,M,HH), HB=(HH:-BM), expand_to_hb(HB,H,B),clause(M:H,B,R),clause_property(R,module(CM)),CM==M.

clause_u_visible(M:H,B,R):- !, clause_i(M:H,B,R),clause_ref_module(R). % need? \+ reserved_body_helper(B) 
clause_u_visible(MH,B,R):- Why = clause(clause,clause_u),
 quietly_ex(fix_mp(Why,MH,M,H)),
   (clause(M:H,B,R)*->true;clause_i(M:H,B,R)).
   
% clause_u(H,B,Why):- has_cl(H),clause_u(H,CL,R),mpred_pbody(H,CL,R,B,Why).
%clause_u(H,B,backward(R)):- R=(<-(H,B)),clause_u(R,true).
%clause_u(H,B,equiv(R)):- R=(<==>(LS,RS)),clause_u(R,true),(((LS=H,RS=B));((LS=B,RS=H))).
%clause_u(H,true, pfcTypeFull(R,Type)):-is_ftNonvar(H),!,pfcDatabaseTerm(F/A),make_functor(R,F,A),pfcRuleOutcomeHead(R,H),clause(R,true),pfcTypeFull(R,Type),Type\=rule.
%clause_u(H,true, pfcTypeFull(R)):-pfcDatabaseTerm(F/A),make_functor(R,F,A),pfcTypeFull(R,Type),Type\=rule,clause(R,true),once(pfcRuleOutcomeHead(R,H)).
%clause_u('nesc'(H),B,forward(Proof)):- is_ftNonvar(H),!, clause_u(H,B,Proof).
%clause_u(H,B,forward(R)):- R=(==>(B,H)),clause_u(R,true).

clause_uu(H,B,Ref):- var(H),var(Ref),!,trace_or_throw_ex(var_clause_u(H,B,Ref)).
clause_uu(M:H,B,R):- safe_functor(H,F,A),safe_functor(HH,F,A),!,should_inherit(mh,M,H,TF),clause_u_attv_m(mh,TF,M,HH,BB,R),match_attvar_clauses(HH,BB,H,B).
clause_uu(  H,B,R):- safe_functor(H,F,A),safe_functor(HH,F,A),!,defaultAssertMt(M),should_inherit(h,M,H,TF),clause_u_attv_m(mh,TF,M,HH,BB,R),match_attvar_clauses(HH,BB,H,B).


clause_u_attv_m(MP,Herit,M,H,B,Ref):-var(H),var(Ref),!,trace_or_throw_ex(var_clause_u_attv_m(MP,Herit,M,H,B,Ref)).
clause_u_attv_m(_,_,M,H,B,R):- nonvar(R),!,must_ex(clause_i(M:H,B,R)),!. % must_ex((MH=(M:H);MH=(H))),!.
clause_u_attv_m(MP,Herit,M,(H:-BB),B,Ref):- is_true(B),!, trace_or_throw_ex(malformed(clause_u(MP,Herit,M,(H:-BB),B,Ref))),clause_u(H,BB,Ref).
clause_u_attv_m(MP,Herit,M,(H:-B),BB,Ref):- is_true(B),!, trace_or_throw_ex(malformed(clause_u(MP,Herit,M,(H:-B),BB,Ref))),clause_u(H,BB,Ref).
clause_u_attv_m(MP,Herit,M,H,B,Ref):- clause_u_attv_b(MP,Herit,M,H,B,Ref),
   B \= inherit_above(M,_), (Herit->clause_ref_module(Ref);clause_ref_module(M,Ref)).

clause_u_attv_b(mh,false,M,H,B,R):- !, clause_i(M:H,B,R), B \= inherit_above(M,_).
clause_u_attv_b(mh,true,IM,H,B,R):- genlMt_each(IM,M),clause_i(M:H,B,R), B \= inherit_above(M,_).
clause_u_attv_b(mh,_,M,H,B,R):- !, clause_u_attv_mhbr(M:H,B,R).
clause_u_attv_b(h,false,M,H,B,R):- clause_i(M:H,B,R).
clause_u_attv_b(h,_,M,H,B,R):- clause_u_attv_mhbr(M:H,B,R).
clause_u_attv_b(h,true,M,H,B,R):- clause_i(M:H,B,R).

genlMt_each(M,M).
genlMt_each(M,O):- clause_b(genlMt(M,P)),(O=P;clause_b(genlMt(P,O))).

clause_u_attv_mhbr(MH,B,R):-
  Why = clause(clause,clause_u),
 ((quietly_ex(fix_mp(Why,MH,M,H)),
  clause(M:H,B,R))*->true;
           (fix_mp(Why,MH,M,CALL)->clause_i(M:CALL,B,R))).

%% clause_u( +VALUE1, ?H, ?B, ?Proof) is semidet.
%
% Hook To [baseKB:clause_u/4] For Module Mpred_pfc.
% PFC Provide Storage Clauses.
%
%clause_u(pfc,H,B,Proof):-clause_u(H,B,Proof).


clause_ref_module(M,Ref):- (clause_property(Ref,module(CM))-> M=CM; false).  % clause_ref_module(Ref) ?
clause_ref_module(Ref):- clause_property(Ref,module(CM)),module_direct(CM).

module_direct(CM):- t_l:exact_kb(M)*->CM=M; true.

with_exact_kb(MM,Call):- locally_tl(exact_kb(MM),Call).


lookup_kb(MM,MHB):- strip_module(MHB,M,HB),
     expand_to_hb(HB,H,B),
      (MM:clause(M:H,B,Ref)*->true; M:clause(MM:H,B,Ref)),
      %clause_ref_module(Ref),
      clause_property(Ref,module(MM)).

% lookup_u/cheaply_u/call_u/clause_bq
lookup_m(SPFT):- callable(SPFT),!,clause_b(SPFT).
lookup_m(SPFT):- callable(SPFT),!,baseKB:on_x_rtrace(SPFT).


lookup_u(SPFT):- callable(SPFT),on_x_rtrace(call_u(SPFT)).
% baseKB:SPFT:- current_prolog_flag(unsafe_speedups , true) , !,baseKB:mtHybrid(MT),call(MT:SPFT).
% lookup_u(H):-lookup_u(H,_).

lookup_u(MH,Ref):- nonvar(Ref),!,
                   must_ex(clause(H,B,Ref)),
                   clause_ref_module(Ref),
                   must_ex(hb_to_clause(H,B,MHI)),!,
                   MH=MHI.

lookup_u((MH,H),Ref):- nonvar(MH),!,lookup_u(MH),lookup_u(H,Ref).
lookup_u(MH,Ref):- clause_u(MH,true,Ref),clause_ref_module(Ref).


:- thread_local(t_l:current_defaultAssertMt/1).
:- was_module_transparent(with_umt/2).
:- was_export(with_umt/2).
%% with_umt( +ABOX, ?G) is semidet.
%
% Using User Microtheory.
%

with_umt(mud_telnet,P):- !,with_umt(baseKB,P).
with_umt(U,G):- sanity(stack_check(5000)),
  (t_l:current_defaultAssertMt(W)->W=U,!,call_from_module(U,G)).
%with_umt(user,P):- !,with_umt(baseKB,P).
with_umt(M,P):-
  (clause_bq(mtHybrid(M))-> W=M;defaultAssertMt(W)),!,
   locally_tl(current_defaultAssertMt(W),
     call_from_module(W,P)).


/*
listing_u(P):- (listing(P)).
assert_u(A):- assert(A).
asserta_u(A):- asserta(A).
assertz_u(A):- assertz(A).
retract_u((H:-B)):-!, clause_u(H,B,R),erase(R).
retract_u(H):-!, clause_u(H,true,R),erase(R).
retractall_u(H):- forall(clause_u(H,_,R),erase(R)).
clause_u(H,B):- clause_u(H,B,_).
clause_u(H,B,R):- clause_i(H,B,R).
call_u_no_bc(G):- G.
*/

%% each_E(+P2,+HT,+S) semidet.
%
% Call P(E,S). each Element in the list.
%
each_E(P,HV,S):- check_context_module, var(HV),!,apply(P,[HV|S]).
each_E(P,M:(H,T),S) :- must_be(atom,M),!,each_E(P,M:H,S), each_E(P,M:T,S).
each_E(P,M:[H],S) :- must_be(atom,M),!,each_E(P,M:H,S).
each_E(P,M:[H|T],S) :- must_be(atom,M),!,each_E(P,M:H,S), each_E(P,M:T,S).
each_E(P,M:HT,S) :- M=='$si$',!,apply(P,[M:HT|S]).
each_E(P,M:HT,S) :- !, must_be(atom,M),M:each_E(P,HT,S).
each_E(P,[H],S) :- !, each_E(P,H,S).
each_E(P,[H|T],S) :- !, each_E(P,H,S), each_E(P,T,S).
each_E(P,(H,T),S) :- !, each_E(P,H,S), each_E(P,T,S).
each_E(P,H,S) :- apply(P,[H|S]).


% =================================================
% ==============  UTILS END          ==============
% =================================================

:- style_check(+singleton).
%   File   : mpred_syntax.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Purpose: syntactic sugar for Pfc - operator definitions and term expansions.

:- op(500,fx,'-').
:- op(300,fx,'~').
:- op(1050,xfx,('==>')).
:- op(1050,xfx,'<==>').
:- op(1050,xfx,('<-')).
:- op(1100,fx,('==>')).
:- op(1150,xfx,('::::')).

:- export('__aux_maplist/2_call+0'/1).
:- meta_predicate('__aux_maplist/2_call+0'(0)).
'__aux_maplist/2_call+0'([]).
'__aux_maplist/2_call+0'([A|B]) :-!,
        call(A),
        '__aux_maplist/2_call+0'(B).
'__aux_maplist/2_call+0'(_:[]).
'__aux_maplist/2_call+0'(M:[A|B]) :-
        M:call(A),
        '__aux_maplist/2_call+0'(M:B).


:- use_module(library(lists)).



%  predicates to examine the state of mpred_


pp_qu:- call_u_no_bc(listing(que/1)).

%   File   : pfc_lib.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated: 10/11/87, ...
%            4/2/91 by R. McEntire: added calls to valid_dbref as a
%                                   workaround for the Quintus 3.1
%                                   bug in the recorded database.
%   Purpose: core Pfc predicates.


% ============================================
% % initialization of global assertons
% ============================================

%%   mpred_set_default(P,Q) is det.
%
%  if there is any fact unifying with P,
% via lookup_u/1 then do
%  nothing, else assert_u Q.
%
mpred_set_default(GeneralTerm,Default):-
  clause_u(GeneralTerm,true) -> true ; assert_u_no_dep(Default).

%  tms is one of {none,local,cycles} and controles the tms alg.
% :- mpred_set_default(tms(_),tms(cycles)).

% Pfc Propagation strategy. pm(X) where P is one of {direct,depth,breadth}
% :- must_ex(mpred_set_default(pm(_), pm(direct))).


ain_expanded(IIIOOO):- mpred_ain((IIIOOO)).

ain_expanded(IIIOOO,S):- mpred_ain((IIIOOO),S).


%% mpred_ainz(+G, ?S) is semidet.
%
% PFC Ainz.
%
mpred_ainz(G):- locally_tl(assert_dir(z),mpred_ain(G)).
mpred_ainz(G,S):- locally_tl(assert_dir(z),mpred_ain(G,S)).

%% mpred_aina(+G, ?S) is semidet.
%
% PFC Aina.
%
mpred_aina(G):- locally_tl(assert_dir(a),mpred_ain(G)).
mpred_aina(G,S):- locally_tl(assert_dir(a),mpred_ain(G,S)).

%%  mpred_ain(P,S)
%
%  asserts P into the dataBase with support from S.
%
%  mpred_ain/2 and mpred_post/2 are the proper ways to add new clauses into the
%  database and have forward reasoning done.
%
mpred_ain(_:P):- retractall(t_l:busy(_)), P==end_of_file,!.
mpred_ain(_:props(_,EL)):- EL==[],!.
mpred_ain(M:P):- M:get_source_uu(UU),M:mpred_ain(M:P,UU).

mpred_add(P):- mpred_ain(P).

%%  mpred_ain(P,S)
%
%  asserts P into the dataBase with support from S.
%

decl_assertable_module(AM):-  must_ex(ensure_abox_support(AM,baseKB)).

% mpred_ain_cm(SM:(==>(AM:P)),P,AM,SM):- SM\==AM, current_predicate(SM:spft/3),!,decl_assertable_module(SM).
mpred_ain_cm(AM:P,P,AM,SM):- nonvar(P),nonvar(P),decl_assertable_module(AM),guess_pos_source_to(SM),!.
mpred_ain_cm(SM:(==>(AM:P)),==>P,AM,SM):- AM==SM,!,decl_assertable_module(AM).
mpred_ain_cm(SM:(==>(_:(AM:P :- B))),==>(AM:P :- SM:B),AM,SM):- nonvar(P), decl_assertable_module(AM).
mpred_ain_cm(SM:(==>(AM:P)),==>P,AM,AM):- decl_assertable_module(AM),!,decl_assertable_module(SM).
mpred_ain_cm((==>(AM:P)),==>P,AM,AM):- decl_assertable_module(AM),!.
mpred_ain_cm((==>(P)),==>P,AM,SM):- get_assert_to(AM), guess_pos_source_to(SM),!.
mpred_ain_cm(M:(==>(P)),==>P,AM,AM):- safe_context_module(M),get_assert_to(AM),!. %  guess_pos_source_to(SM).
mpred_ain_cm(AM:(==>(P)),==>P,AM,AM):- !.

mpred_ain_cm(AM:P,P,SM,AM):- !, safe_context_module(SM).
mpred_ain_cm(   P,P,SM,AM):- get_assert_to(AM), safe_context_module(SM).


guess_pos_assert_to(ToMt):- 
  notrace((  ((guess_pos_source_to(ToMt), \+ is_code_module(ToMt), is_mtCanAssert(ToMt))*-> true; 
    ((guess_pos_source_to(ToMt), \+ is_code_module(ToMt))*-> true ;
    ((guess_pos_source_to(ToMt), is_mtCanAssert(ToMt))*-> true;    
    guess_pos_source_to(ToMt)))))).

:- dynamic(baseKB:mtExact/1).

as_safe_cm(M,OSM):- notrace(M==user;M==system;M==pfc_lib),!,to_osm(OSM).
as_safe_cm(M,M).

to_osm(OSM):- prolog_load_context(module,M),( (M==user;M==system;M==pfc_lib)->OSM=baseKB;OSM=M).

safe_context_module(ToMt):- context_module(UToMt),as_safe_cm(UToMt,ToMt),!.

% guess_pos_source_to(ToMt):- t_l:current_defaultAssertMt(ToMt).

guess_pos_source_to(ToMt):- no_repeats(ToMt,(guess_pos_source_to0(UToMt),as_safe_cm(UToMt,ToMt))).

guess_pos_source_to0(ToMt):- t_l:current_defaultAssertMt(ToMt).
guess_pos_source_to0(ToMt):- '$current_source_module'(ToMt).
guess_pos_source_to0(ToMt):- safe_context_module(ToMt).
guess_pos_source_to0(ToMt):- '$current_typein_module'(ToMt).
guess_pos_source_to0(ToMt):- guess_pfc_file(File),module_property(ToMt,file(File)),File\==ToMt.
guess_pos_source_to0(ToMt):- prolog_load_context(module,ToMt).
guess_pos_source_to0(ToMt):- defaultAssertMt(ToMt).
guess_pos_source_to0(baseKB).

guess_pfc_file(File):- which_file(File).
guess_pfc_file(File):- loading_source_file(File),get_file_type_local(File,pfc).

get_assert_to(ABox):- (var(ABox)->guess_pos_assert_to(ABox);(guess_pos_assert_to(ABoxVar),ABox=ABoxVar)),!.

% get_query_from(SM):- '$current_source_module'(SM).
get_query_from(SM):- guess_pos_assert_to(SM), \+ is_code_module(SM),!.
get_query_from(baseKB).

:- baseKB:import(is_code_module/1).
is_code_module(M):-notrace(is_code_module0(M)).
is_code_module0(system).
is_code_module0(user).
is_code_module0(baseKB):-!,fail.
is_code_module0(pfc_lib).
is_code_module0(M):- clause_bq(mtHybrid(M)),!,fail.
is_code_module0(M):- clause_bq(mtProlog(M)),!.

is_code_module0(M):- module_property(M,class(system)).
is_code_module0(M):- module_property(M,file(FileName)), sub_string(FileName, _, _, _, '.pfc'), !, fail.
is_code_module0(M):- module_property(M,class(library)).
is_code_module0(M):- module_property(M,class(user)).
%call_mp(user, P1 ):- !,  call_mp(baseKB,P1).


mpred_ain(MTP,S):- quietly_ex(is_ftVarq(MTP)),!,trace_or_throw_ex(var_mpred_ain(MTP,S)).
mpred_ain(MTP,S):- mpred_ain_cm(MTP,P,AM,SM),mpred_ain_now4(SM,AM,P,S).


mpred_ain_now4(SM,ToMt,P,(mfl4(VarNameZ,FromMt,File,Lineno),UserWhy)):- sanity(stack_check),ToMt \== FromMt,!,
  mpred_ain_now4(SM,ToMt,P,(mfl4(VarNameZ,ToMt,File,Lineno),UserWhy)).

mpred_ain_now4(SM0,AM0,PIn,S):- SM0==AM0, is_code_module(AM0),!,
  notrace((get_assert_to(AM),get_query_from(SM))),!,mpred_ain_now4(SM,AM,PIn,S).
  
mpred_ain_now4(SM,AM,PIn,S):- % module_sanity_check(SM),
  nop(module_sanity_check(AM)),
  call_from_module(AM, 
    with_source_module(SM,
      locally_tl(current_defaultAssertMt(AM), SM:mpred_ain_now(PIn,S)))).

mpred_ain_now(PIn,S):-
  PIn=P, % must_ex(add_eachRulePreconditional(PIn,P)),  
  must_ex(full_transform(ain,P,P0)),!, % P=P0,  
  must_ex(ain_fast(P0,S)),!,
  nop(ignore((P\=@=P0, mpred_db_type(P,fact(_)),show_failure(mpred_fwc(P))))).

mpred_ain_now(P,S):- mpred_warn("mpred_ain(~p,~p) failed",[P,S]),!,fail.

:- thread_local(t_l:is_repropagating/1).
ain_fast(P):-  \+ t_l:is_repropagating(_),clause_asserted(P),!.
ain_fast(P):- call_u((( get_source_uu(UU), ain_fast(P,UU)))).

ain_fast(P,S):- quietly_ex((maybe_updated_value(P,RP,OLD),subst(S,P,RP,RS))),!,ain_fast(RP,RS),ignore(mpred_retract_i(OLD)).

% ain_fast(P,S):- loop_check_term(ain_fast0(P,S),ain_fast123(P),(trace,ain_fast0(P,S))).

ain_fast(P,S):-
  %retractall(t_l:busy(_)),
  fwc1s_post1s(One,Two),
  filter_buffer_trim('$last_mpred_fwc1s',One),
  filter_buffer_trim('$last_mpred_post1s',Two),
  each_E(mpred_post1,P,[S]),!,
  mpred_run.

:- abolish(lmconf:eachRule_Preconditional/1).
:- abolish(lmconf:eachFact_Preconditional/1).
:- dynamic(lmconf:eachRule_Preconditional/1).
:- dynamic(lmconf:eachFact_Preconditional/1).
lmconf:eachRule_Preconditional(true).
lmconf:eachFact_Preconditional(true).

add_eachRulePreconditional(A,A):-var(A),!.
add_eachRulePreconditional(B::::A,B::::AA):-add_eachRulePreconditional(A,AA).
add_eachRulePreconditional(A==>B,AA==>B):-!,add_eachRulePreconditional_now(A,AA).
add_eachRulePreconditional(A<==>B, ('==>'(AA , B) , (BB ==> A)) ):-!,add_eachRulePreconditional_now(A,AA),add_eachRulePreconditional_now(B,BB).
add_eachRulePreconditional((B <- A), (B <- AA)) :-!,add_eachRulePreconditional_now(A,AA).
add_eachRulePreconditional(A,AA):-add_eachFactPreconditional_now(A,AA).

add_eachFactPreconditional_now(A,A):- lmconf:eachFact_Preconditional(true),!.
add_eachFactPreconditional_now(A,(Was==>A)):- lmconf:eachFact_Preconditional(Was),!.

add_eachRulePreconditional_now(A,A):- lmconf:eachRule_Preconditional(true),!.
add_eachRulePreconditional_now(A,(Was,A)):- lmconf:eachRule_Preconditional(Was),!.




remove_negative_version(_P):- current_prolog_flag(unsafe_speedups , true) ,!.
remove_negative_version((H:-B)):- !,
  % TODO extract_predciates((H:-B),Preds),trust(Preds),
  with_no_mpred_trace_exec((
  once((get_why_uu(S),!,
  must_ex(mpred_ain(\+ (~(H) :- B), S)))))),!.
remove_negative_version(P) :- \+ mpred_non_neg_literal(P),!.

remove_negative_version(P):-
  % TODO extract_predciates(P,Preds),trust(Preds),
  with_no_mpred_trace_exec((
  once((get_why_uu(S),!,
  must_ex(mpred_ain(\+ (~(P)), S)))))),!.

%fwc1s_post1s(0,0):-!.
fwc1s_post1s(1,1):-!.
%fwc1s_post1s(1,2):-!.
/*
fwc1s_post1s(3,0):-!.
fwc1s_post1s(3,0):-!.
%fwc1s_post1s(1,2):- flag_call(unsafe_speedups == false) ,!.

fwc1s_post1s(1,3):- fresh_mode,!.
fwc1s_post1s(1,2):- current_prolog_flag(pfc_booted,true),!.
% fwc1s_post1s(10,20):- defaultAssertMt(Mt)->Mt==baseKB,!.
fwc1s_post1s(1,2).
*/

fresh_mode :- \+ current_prolog_flag(pfc_booted,true), \+ flag_call(unsafe_speedups == false) .
plus_fwc :- \+ fresh_mode.

plus_fwc(P):- is_ftVarq(P),!,trace_or_throw_ex(var_plus_fwc(P)).
plus_fwc(support_hilog(_,_)):-!.
plus_fwc('==>'(_,_)):-!.
plus_fwc(P):- gripe_time(0.6,
  (plus_fwc
    ->
      loop_check_term(must_ex(mpred_fwc(P)),plus_fwc(P),true);true)),!.


maybe_updated_value(UP,R,OLD):- % \+ current_prolog_flag(unsafe_speedups , true) ,
    compound(UP),
    get_assertion_head_unnegated(UP,P),!,
    compound(P),
    once((arg(N,P,UPDATE),is_relative(UPDATE))),
    must_ex(flag_call(unsafe_speedups == false) ),
    replace_arg(P,N,Q_SLOT,Q),
    must_ex(call_u(Q)), update_value(Q_SLOT,UPDATE,NEW), must_ex( \+ is_relative(NEW)),
    replace_arg(Q,N,NEW,R),!,R\=@=UP,subst(UP,P,Q,OLD).



implicitly_true(Var):- is_ftVarq(Var),!,fail.
implicitly_true(true).
implicitly_true(end_of_file).
implicitly_true(props(_,L)):- L ==[].

abby_normal_ERR(Var):- is_ftVarq(Var),!.
abby_normal_ERR( isa(_,_,_),   _).
abby_normal_ERR( tCol(COMMA),   _):- COMMA==','.
abby_normal_ERR( tCol(VAR),   _):- var(VAR).
abby_normal_ERR( P, _):- \+ \+ P = props(_,[]).   

%% mpred_post(+Ps,+S)
%
% tries to assert a fact or set of fact to the database.  For
% each fact (or the singleton) mpred_post1 is called. It always succeeds.
%
mpred_post(P, S):- must(full_transform(post,P,P0)),each_E(mpred_post1,P0,[S]).

mpred_post( P):- get_why_uu(UU), mpred_post( P,   UU).
mpred_post1( P):- get_why_uu(UU), mpred_post1( P,   UU).

%% mpred_post1(+P,+S) is det.
%
% tries to add a fact to the database, and, if it succeeded,
% adds an entry to the Pfc queue for subsequent forward chaining.
% It always succeeds.
%

mpred_post1(P, S) :- show_success(abby_normal_ERR(P,S)),break_ex,!,fail.
mpred_post1(P, S):- each_E(mpred_post2,P,[S]).


mpred_post2( P,   S):- quietly_ex(( sanity(nonvar(P)),fixed_negations(P,P0),P\=@=P0)),!, mpred_post2( P0,   S).

mpred_post2(Fact, _S):-  fail,
  quietly_ex(((true;current_prolog_flag(unsafe_speedups , true)) , ground(Fact),
   \+ t_l:is_repropagating(_),
   fwc1s_post1s(One,_Two),Three is One * 1,
   filter_buffer_n_test('$last_mpred_post1s',Three,Fact))),!.

%mpred_post2(P,S):- gripe_time(0.6,loop_check_early(mpred_post12(P,S),true)).
mpred_post2(P,S):- gripe_time(16,(must(mpred_post12(P,S)),true)).


mpred_post_exactly(P):- current_why(S),mpred_enqueue(P,S).
mpred_remove_exactly(P):- remove_if_unsupported(P).

:- module_transparent(mpred_post_exactly/1).
:- module_transparent(mpred_post1/2).
:- module_transparent(mpred_post12/2).
:- export(mpred_post12/2).

leave_some_vars_at_el(action_rules).
leave_some_vars_at_el(agent_text_command).
leave_some_vars_at_el(rtArgsVerbatum).
leave_some_vars_at_el(==>).

is_ftOpen(A):- member(A,['$VAR'('????????????'),'$VAR'(_)]).

is_ftOpenSentence(P):- compound(P), safe_functor(P,F,N), \+ leave_some_vars_at_el(F),
   (arg(N,P,A);(N\==1,arg(1,P,A))),is_ftOpen(A).
is_ftOpenSentence(P):- is_ftOpen(P).


mpred_post12_withdraw( P,   S):- show_call(mpred_withdraw(P,S)), \+ mpred_supported(P),!.
%mpred_post12_withdraw( P,   S):- is_user_reason(S), show_call(mpred_withdraw(P)), \+ mpred_supported(P),!.
%mpred_post12_withdraw( P,   S):- is_user_reason(S),!, (mpred_withdraw_fail_if_supported(P,S) -> true ;  show_call(mpred_remove2(P,S))).
mpred_post12_withdraw( P,   S):- ignore(show_call(mpred_withdraw_fail_if_supported(P,S))),!.

mpred_post12_negated( P,   S):- mpred_withdraw_fail_if_supported(P,S), mpred_post13(~P,S),!.
mpred_post12_negated( P,   S):- mpred_remove2(P,S), show_call( \+ mpred_supported(P)),!, show_call((nop(2), mpred_post13(~P,S))),!.
mpred_post12_negated( P,   S) :- mpred_get_support(P,S2), 
    color_line(magenta,2),
    dmsg_pretty((mpred_post12( ~ P,   S) :- mpred_get_support(P,S2))),
    color_line(magenta,1),color_line(green,1),color_line(yellow,1),
    color_line(magenta,1),color_line(green,1),color_line(yellow,1),
    color_line(magenta,1),color_line(green,1),color_line(yellow,1),
    mpred_trace_op(blast,P),
    mpred_why_1(P),
    must(mpred_unfwc(P)),
    must(mpred_post13(~P,S)),!.




mpred_post12(P, _):- must_be(nonvar,P),P==true,!.
% mpred_post12(P, S):- quietly_ex((is_ftOpenSentence(P)->wdmsg_pretty((warn((var_mpred_post1(P, S))))))),fail.
mpred_post12( \+  P,   S):- mpred_post12_withdraw( P,   S),!.
mpred_post12(  ~  P,   S):- mpred_post12_negated( P,   S),!.

/*
mpred_post12( \+ P,   S):- (must_be(nonvar,P)), !,doall( must_ex(mpred_post1_rem(P,S))).

% TODO - FIGURE OUT WHY THIS IS NEEDED - WELL THINKING AOBUT IT AND UIT SEEMS WRONG
mpred_post12( ~ P,   S):- fail, (must_be(nonvar,P)), sanity((ignore(show_failure(\+ is_ftOpenSentence(P))))),
   quietly_ex((  \+ mpred_unique_u(P))),
   with_current_why(S,with_no_breaks((nonvar(P),doall(mpred_remove(P,S)),must_ex(mpred_undo(P))))),fail.
*/

mpred_post12(P,S):- quietly_ex((maybe_updated_value(P,RP,OLD))),!,subst(S,P,RP,RS),mpred_post13(RP,RS),ignore(mpred_retract_i(OLD)).

%  TODO MAYBE 
mpred_post12(actn(P),S):- !, 
  with_current_why(S,call(P)), mpred_post13(actn(P),S).

mpred_post12(P,S):- mpred_post13(P,S).

% Two versions exists of this function one expects for a clean database (fresh_mode) and adds new information.
% tries to assert a fact or set of fact to the database.
% The other version is if the program is been running before loading this module.
%
mpred_post13_unused(P,S):- fail,
  fresh_mode,!,
  % db mpred_ain_db_to_head(P,P2),
  % mpred_remove_old_version(P),
 \+ \+ mpred_add_support(P,S),
  ( (\+ mpred_unique_u(P)) -> true ;
  ( assert_u_confirm_if_missing(P),
     !,
     mpred_trace_op(add,P,S),
     !,
     mpred_enqueue(P,S),
     !)),
  plus_fwc(P),!.


% this would be the very inital by Tim Finnin...
mpred_post13_unused(P,S):- fail, fresh_mode,
 ignore(( %  db mpred_ain_db_to_head(P,P2),
  % mpred_remove_old_version(P),
  mpred_add_support(P,S),
  mpred_unique_u(P),
  assert_u_confirm_if_missing(P),
  mpred_trace_op(add,P,S),
  !,
  mpred_enqueue(P,S))),
  !.


/*
% Expects a clean database and adds new information.
mpred_post13_unused(P,S):-  fail,!,
  % db mpred_ain_db_to_head(P,P2),
  % mpred_remove_old_version(P),
  must_ex( \+ \+ mpred_add_support(P,S)),
  ( \+ mpred_unique_u(P)
    -> clause_asserted_u(P)
    ; ( assert_u_confirmed_was_missing(P),
        !,
        mpred_trace_op(add,P,S),
        !,
        mpred_enqueue(P,S),
        !)).
*/

/*
mpred_post13((H:-B),S):- 
  with_current_why(S,
    show_call(mpred_do_hb_catchup_now_maybe(H,B))),
  fail.
*/

% this for complete repropagation
mpred_post13(P,S):- t_l:is_repropagating(_),!,
 ignore(( %  db mpred_ain_db_to_head(P,P2),
  % mpred_remove_old_version(P),
  mpred_add_support(P,S),
  (mpred_unique_u(P)->
     assert_u_confirmed_was_missing(P);
     assert_u_confirm_if_missing(P)),
  mpred_trace_op(add,P,S),
  !,
  mpred_enqueue(P,S))),
  !.

/*
mpred_post13(P,S):- true, !,
 ignore(( %  db mpred_ain_db_to_head(P,P2),
  % mpred_remove_old_version(P),
  mpred_add_support(P,S),
  (mpred_unique_u(P)->
     assert_u_confirmed_was_missing(P);
     assert_u_confirm_if_missing(P)),
  mpred_trace_op(add,P,S),
  !,
  mpred_enqueue(P,S))),
  !.
*/


% Expects a *UN*clean database and adds new information.
% (running the program is been running before loading this module)
%
%  (gets the status in Support and in Database)
mpred_post13(P,S):- !,
 %  set_varname_list([]),!,
   copy_term_vn((P,S),(PP,SS)),
 %  checks to see if we have forward chain the knowledge yet or
  gripe_time(0.1, must_ex(get_mpred_support_status(P,S,PP,SS,Was))),!,
  mpred_post123(P,S,PP,Was).


:- thread_local(t_l:exact_assertions/0).

with_exact_assertions(Goal):-
  locally_tl(exact_assertions,Goal).
 

% The cyclic_break is when we have regressions arouind ~ ~ ~ ~ ~

get_mpred_support_status(_P,_S, PP,(F,T),Was):- 
  t_l:exact_assertions,!,
  (clause_asserted_u(spft(PP,F,T)) -> Was = exact ; Was = none).

get_mpred_support_status(_P,_S, PP,(F,T),Was):- 
  % t_l:exact_assertions,
  !,
  (clause_asserted_u(spft(PP,F,T)) -> Was = exact ; Was = none).

get_mpred_support_status(P,_S, PP,(FF,TT),Was):-
  Simular=simular(none),
  copy_term(PP,PPP),
  ((((lookup_spft(PPP,F,T),variant_u(P,PP))) *->
     ((variant_u(TT,T),same_file_facts0(F,FF)) -> (Was = exact , ! ) ; 
      (nb_setarg(1,Simular,(F,T)),!,fail))
    ; Was = none) -> true ; ignore(Was=Simular)),!.

% mpred_post123(_P,_S,_PP,exact):- current_prolog_flag(pfc_cheats,true), !.

mpred_post123(P,S,PP,Was):-
 % cyclic_break((P,S,PP,Was)),
 %  if we''ve asserted what we''ve compiled  
  gripe_time(0.22, must_ex(get_mpred_assertion_status(P,PP,AStatus))),!,
  gripe_time(0.44, must_ex(mpred_post_update4(AStatus,P,S,Was))),!.

get_mpred_assertion_status(P,_PP,Was):-
 (t_l:exact_assertions ; mpred_db_type(P,rule(_))),!,
  quietly(((clause_asserted_u(P)-> Was=identical; Was= unique))).
 
get_mpred_assertion_status(P,PP,Was):-
  quietly(((clause_asserted_u(P)-> Was=identical;
    (
      (((locally(set_prolog_flag(occurs_check,true),clause_u(PP)),cyclic_break((PPP)))-> (Was= partial(PPP));Was= unique)))))).


same_file_facts(S1,S2):-reduce_to_mfl(S1,MFL1),reduce_to_mfl(S2,MFL2),!,same_file_facts0(MFL1,MFL2).
same_file_facts0(mfl4(VarNameZ,M,F,_),mfl4(VarNameZ,M,FF,_)):-nonvar(M),!, FF=@=F.
same_file_facts0(F,FF):- FF=@=F,!.

reduce_to_mfl(MFL,MFL):- MFL=mfl4(_VarNameZ,_,_,_),!.
reduce_to_mfl((MFL,_),MFLO):- !,reduce_to_mfl(MFL,MFLO).

%% mpred_post_update4(++AssertionStatus, +Ps, +S, ++SupportStatus) is det.
%
% Physically assert the Knowledge+Support Data based on statuses
%
mpred_post_update4(Was,P,S,What):-
  not_not_ignore_quietly_ex(( (get_mpred_is_tracing(P);get_mpred_is_tracing(S)),
  fix_mp(change(assert,post),P,M,PP),
  must_ex(S=(F,T)),wdmsg_pretty(call_mpred_post4:- (Was,post1=M:PP,fact=F,trig=T,What)))),
  fail.

mpred_post_update4(identical,_P,_S,exact):-!.
mpred_post_update4(unique,P,S,none):- !,
   must_det(mpred_add_support_fast(P,S)),
   must_det(assert_u_confirmed_was_missing(P)),
   must_det(mpred_trace_op(add,P,S)),
   must_ex(mpred_enqueue(P,S)),!.

mpred_post_update4(Identical,P,S,Exact):- !,
  ((Exact\==exact ->mpred_add_support_fast(P,S);true),
  (Identical==identical-> true ; 
           (assert_u_confirmed_was_missing(P),mpred_trace_op(add,P,S),mpred_enqueue(P,S)))),!.

mpred_post_update4(identical,P,S,none):-!,mpred_add_support_fast(P,S),
    mpred_enqueue(P,S).

mpred_post_update4(identical,P,S,simular(_)):- !,mpred_add_support_fast(P,S).

/*
mpred_post_update4(Was,P,S,What):-
  not_not_ignore_quietly_ex(( \+ (get_mpred_is_tracing(P);get_mpred_is_tracing(S)),
  fix_mp(change(assert,post),P,M,PP),
  must_ex(S=(F,T)),wdmsg_pretty(mpred_post_update4:- (Was,post1=M:PP,fact=F,trig=T,What)))),
  fail.
*/

mpred_post_update4(partial(_Other),P,S,none):-!,
  mpred_add_support_fast(P,S),
  assert_u_confirmed_was_missing(P),
  mpred_trace_op(add,P,S),
  mpred_enqueue(P,S).

mpred_post_update4(partial(_Other),P,S,exact):-!,
  assert_u_confirmed_was_missing(P),
  mpred_trace_op(add,P,S),
  mpred_enqueue(P,S).

mpred_post_update4(unique,P,S,exact):-!,
  assert_u_confirmed_was_missing(P),
  mpred_trace_op(add,P,S).


mpred_post_update4(partial(_),P,S,exact):- !,
  assert_u_confirmed_was_missing(P),
  mpred_trace_op(add,P,S).


mpred_post_update4(partial(_),P,S,simular(_)):- !,
  mpred_add_support_fast(P,S),
  ignore((mpred_unique_u(P),assert_u_confirmed_was_missing(P),mpred_trace_op(add,P,S))),
  mpred_enqueue(P,S).

mpred_post_update4(unique,P,S,simular(_)):-!,
  mpred_add_support_fast(P,S),
  assert_u_confirmed_was_missing(P),
  mpred_trace_op(add,P,S),
  mpred_enqueue(P,S).


mpred_post_update4(Was,P,S,What):-dmsg_pretty(mpred_post_update4(Was,P,S,What)),dtrace,fail.

mpred_post_update4(Was,P,S,What):-!,trace_or_throw_ex(mpred_post_update4(Was,P,S,What)).

/*
assert_u_confirmed_was_missing(P):- once((get_unnegated_functor(P,F,_),get_functor(P,FF,_))),
 F==FF,
 call_u(prologSingleValued(F)),!,
 \+ \+ must_ex((db_assert_sv(P))),
 \+ \+ sanity((clause_asserted_u(P))),!.
*/

% assert_u_confirmed_was_missing(P):- mpred_enqueue(onChange(P),'was_missing'), fail.

% assert_u_confirmed_was_missing(P):- term_attvars(P,L),L\==[],!,  \+ \+ must_ex(assert_to_mu(P)),!.

assert_u_confirmed_was_missing(P):-
 \+ \+ must_ex(assert_to_mu(P)),!,
  nop((sanity((( (\+ clause_asserted_u(P)) -> (rtrace(assert_to_mu(P)),break) ; true))))),!.

assert_u_confirmed_was_missing(P):-
 copy_term_vn(P,PP),
 must_ex(assert_u_no_dep(P)),!,dtrace,
(nonvar(PP) -> true ; must_ex((P=@=PP,clause_asserted_u(PP),P=@=PP))),!.

assert_to_mu(P):-
  (t_l:assert_dir(Where) ->
   (Where = a -> asserta_mu(P); assertz_mu(P));
  assert_mu(P)).

assert_u_confirm_if_missing(P):-
 must_ex(clause_asserted_u(P)-> true ; assert_u_confirmed_was_missing(P)).

%% get_mpred_current_db(-Db) is semidet.
%
% PFC Current Database.
%
% (was nothing)
%
get_mpred_current_db(Db):-lookup_u(mpred_current_db(Db)),!.
get_mpred_current_db(true).

%%  mpred_ain_db_to_head(+P,-NewP) is semidet.
%
% takes a fact P or a conditioned fact
%  (P:-C) and adds the Db context.
%
mpred_ain_db_to_head(P,NewP):-
  lookup_u(mpred_current_db(Db)),
  (Db=true        -> NewP = P;
   P=(Head:-Body) -> NewP = (Head:- (Db,Body));
   otherwise      -> NewP = (P:- Db)).


%% mpred_unique_u(+P) is semidet.
%
% is true if there is no assertion P in the prolog db.
%
mpred_unique_u(P):- t_l:exact_assertions,!, \+ clause_asserted_u(P).
%mpred_unique_u((Head:-Tail)):- !, \+ clause_u(Head,Tail).
%mpred_unique_u(P):- !, \+ clause_u(P,true).
mpred_unique_u(P):- \+ clause_asserted_u(P).


%% get_fc_mode(+P,+S,-Mode) is semidet.
%
% return Mode to forward assertion P in the prolog db.
%

%get_fc_mode(_P,_S,direct):-!.
get_fc_mode(mpred_prop(_,_,_,_),_S,direct).
get_fc_mode(P,_S,Mode):- notrace(get_unnegated_mfa(P,M,F,A)),mpred_prop(M,F,A,Mode),is_fwc_mode(Mode),!.
get_fc_mode(P,_S,direct):- compound(P),functor(P,_,1).
get_fc_mode(_P,_S,Mode):- get_fc_mode(Mode).

get_fc_mode0(Mode):- t_l:mpred_fc_mode(Mode),!.
get_fc_mode0(Mode):- lookup_m(pm(Mode)),!.
get_fc_mode0(Mode):- !, Mode=direct.
get_fc_mode(Mode):- notrace(get_fc_mode0(Mode)).


:- thread_local(t_l:mpred_fc_mode/1).

%% with_fc_mode(+Mode,:Goal) is semidet.
%
% Temporariliy changes to forward chaining propagation mode while running the Goal
%
with_fc_mode(Mode,Goal):- locally_tl(mpred_fc_mode(Mode),((Goal))).

set_fc_mode(Mode):- asserta(t_l:mpred_fc_mode(Mode)).

%% mpred_enqueue(+P,+S) is det.
%
% PFC Enqueue P for forward chaining
%

mpred_enqueue(P):- mpred_enqueue(P,_S).

mpred_enqueue(P,_):- show_mpred_success(que,lookup_m(que(P,_))),!.
%mpred_enqueue(P,_):- nb_current('$current_why',wp(_,P)),!,trace_or_throw_ex(why(P)).
%mpred_enqueue(P,_):- t_l:busy(P),!,nop(dmsg_pretty(t_l:busy(P))).
%mpred_enqueue(P,S):- locally_each(t_l:busy(P),mpred_enqueue2(P,S)).
mpred_enqueue(P,S):-
 (var(S)->current_why(S);true),
 (notrace(get_fc_mode(P,S,Mode)) 
  -> mpred_enqueue_w_mode(S,Mode,P)
   ; mpred_error("No pm mode")).

mpred_enqueue_w_mode(S,Mode,P):-
       (Mode=direct  -> mpred_enqueue_direct(S,P) ;
        Mode=thread  -> mpred_enqueue_thread(S,P) ;
	Mode=depth   -> mpred_asserta_w_support(que(P,S),S) ;
        Mode=paused  -> mpred_asserta_w_support(que(P,S),S) ;
	Mode=breadth -> mpred_assertz_w_support(que(P,S),S) ;
        Mode=next   -> mpred_asserta_w_support(que(P,S),S) ;
        Mode=last -> mpred_assertz_w_support(que(P,S),S) ;
	true     -> mpred_error("Unrecognized pm mode: ~p", Mode)).

is_fwc_mode(direct).
is_fwc_mode(thread).
is_fwc_mode(depth).
is_fwc_mode(paused).
is_fwc_mode(breadth).
is_fwc_mode(next).
is_fwc_mode(last).


get_support_module(mfl4(_,Module,_,_), Module).
get_support_module((S1,S2),Module):- !, (get_support_module(S1,Module);get_support_module(S2,Module)).
get_support_module((S2:S1),Module):- !, (get_support_module(S1,Module);get_support_module(S2,Module)).

of_queue_module(_, M:_, M):- atom(M), !.
of_queue_module(S, _, Module):- get_support_module(S, Module), !.
of_queue_module(_, _, Module):- get_query_from(Module), !.

mpred_enqueue_direct(S,P):-
  quietly_must_ex(of_queue_module(S,P,Module)),
  loop_check_term(Module:mpred_fwc(P),mpred_enqueueing(P),true).

/*
mpred_enqueue_thread(S,P):- 
      with_only_current_why(S,
        call_in_thread(
           with_fc_mode(direct, % maybe keep `thread` mode?
               loop_check_term(mpred_fwc(P),mpred_enqueueing(P),true)))).

*/

mpred_enqueue_thread(S,P):- 
      with_only_current_why(S,
        call_in_thread(fwc_wlc(P))).

fwc_wlc(P):- in_fc_call(loop_check_term(mpred_fwc(P),mpred_enqueueing(P),true)).

% maybe keep `thread` mode?
% in_fc_call(Goal):- with_fc_mode( thread, Goal).
in_fc_call(Goal):- with_fc_mode( direct, Goal).
% in_fc_call(Goal):- !, call(Goal).

%% mpred_remove_old_version( :TermIdentifier) is semidet.
%
% if there is a rule of the form Identifier ::: Rule then delete it.
%
mpred_remove_old_version((Identifier::::Body)):-
  % this should never happen.
  var(Identifier),
  !,
  mpred_warn("variable used as an  rule name in ~p :::: ~p",
          [Identifier,Body]).


mpred_remove_old_version((Identifier::::Body)):-
  nonvar(Identifier),
  clause_u((Identifier::::OldBody),_),
  \+(Body=OldBody),
  mpred_withdraw((Identifier::::OldBody)),
  !.
mpred_remove_old_version(_).



% mpred_run compute the deductive closure of the current database.
% How this is done depends on the propagation mode:
%    direct -  mpred_fwc has already done the job.
%    depth or breadth - use the queue mechanism.
                                                            
mpred_run :- get_fc_mode(Mode)->Mode=paused,!.
% mpred_run :- repeat, \+ mpred_step, !.
mpred_run:-
  mpred_step,
  mpred_run.
mpred_run:- retractall(t_l:busy(_)).


% mpred_step removes one entry from the queue and reasons from it.

:-thread_local(t_l:busy/1).
:-thread_local(t_l:busy_s/1).

mpred_step:-
  % if hs/1 is true, reset it and fail, thereby stopping inferencing. (hs=halt_signal)
  quietly_ex((lookup_m(hs(Was)))),
  mpred_retract_i(hs(Was)),
  mpred_trace_msg('Stopping on: ~p',[hs(Was)]),
  !,
  fail.

mpred_step:-
  % draw immediate conclusions from the next fact to be considered.
  % fails iff the queue is empty.
  get_next_fact(P),
  %asserta(t_l:busy(P)),
  ignore(mpred_fwc(P)),
 % ignore(retract(t_l:local_current_why(_,P))),
  %retractall(t_l:busy(P)),
  !.

get_next_fact(P):-
  %identifies the nect fact to mpred_fwc from and removes it from the queue.
  select_next_fact(P),
  remove_selection(P).

remove_selection(P):-
  lookup_u(que(P,_),Ref),
  erase(Ref),
  % must_ex(mpred_retract_i(que(P,_))),
  mpred_remove_supports_quietly(que(P,_)),
  !.
remove_selection(P):-
  brake(format("~Nmpred_:get_next_fact - selected fact not on Queue: ~p",
               [P])).


% select_next_fact(P) identifies the next fact to reason from.
% It tries the user defined predicate first and, failing that,
%  the default mechanism.
select_next_fact(P):-  
  lookup_u(pfcSelect(P)),
  !.
select_next_fact(P):-
  defaultmpred_select(P),
  !.

% the default selection predicate takes the item at the froint of the queue.
defaultmpred_select(P):- lookup_m(que(P,_)),!.

pfcQueue(P):- lookup_m(que(P,_)).

% mpred_halt stops the forward chaining.
mpred_halt:-  mpred_halt(anonymous(mpred_halt)).
pfcHalt:- mpred_halt.

mpred_halt(Format,Args):- format_to_message(Format,Args,Info), mpred_halt(Info).

mpred_halt(Now):-
  mpred_trace_msg("New halt signal ",[Now]),
  (lookup_m(hs(Was)) ->
       mpred_warn("mpred_halt finds halt signal already set to: ~p ",[Was])
     ; assert_u_no_dep(hs(Now))).


% stop_trace(Info):- quietly_ex((tracing,leash(+all),dtrace(dmsg_pretty(Info)))),!,rtrace.
stop_trace(Info):- dtrace(dmsg_pretty(Info)).

%% mpred_ain_trigger_reprop(+Trigger,+Support) is nondet.
%
%  Assert New Trigger and Propigate
%
mpred_ain_trigger_reprop(PT,Support):- PT = pt(Trigger,Body), !,
   mpred_trace_msg('~N~n\tAdding positive~n\t\ttrigger: ~p~n\t\tbody: ~p~n\t Support: ~p~n',[Trigger,Body,Support]),!,
   sanity(quietly_must_ex(( (\+ string(Support)), (\+ string(Trigger)), (\+ string(Body))))),
   mpred_mark_as_confirmed(Support,Trigger,pfcPosTrigger),
 must(((  
  %  (debugging(logicmoo(_))->dtrace;true),
  
  mpred_assert_w_support(PT,Support),
  copy_term(PT,Tcopy),!,
  forall(call_u_no_bc(Trigger), 
   forall(mpred_eval_lhs(Body,(Trigger,Tcopy)),true))))),!.
  


mpred_ain_trigger_reprop(nt(Trigger,Test,Body),Support):- NT = nt(TriggerCopy,Test,Body),!,
  copy_term_vn(Trigger,TriggerCopy),  
  mpred_mark_as_confirmed(Support,Trigger,pfcNegTrigger),
  mpred_trace_msg('~N~n\tAdding negative~n\t\ttrigger: ~p~n\t\ttest: ~p~n\t\tbody: ~p~n\t Support: ~p~n',[Trigger,Test,Body,Support]),
  mpred_assert_w_support(NT,Support),
  %stop_trace(mpred_assert_w_support(NT,Support)),
  !,
  ignore((\+ call_u_no_bc(Test),
  mpred_eval_lhs(Body,((\+Trigger),NT)))).

mpred_ain_trigger_reprop(BT,Support):- BT = bt(Trigger,Body),!,

  % UNEEDED Due to a trigger that creates it?
  % get_bc_clause(Trigger,Post),mpred_post1(Post),
  mpred_mark_as_confirmed(Support,Trigger,pfcBcTrigger),
  % UNEEDED Due to a trigger that does it?
  % if_defined(kb_shared(Trigger),true),
  mpred_trace_msg('~N~n\tAdding backwards~n\t\ttrigger: ~p~n\t\tbody: ~p~n\t Support: ~p~n',[Trigger,Body,Support]),
  mpred_assert_w_support(BT,Support),
  !,
  mpred_bt_pt_combine(Trigger,Body,Support).

mpred_ain_trigger_reprop(X,Support):-
  mpred_warn("Unrecognized trigger to mpred_ain_trigger_reprop: ~p\n~~p~n",[X,Support]).


mpred_bt_pt_combine(Head,Body,Support):-
  %  a backward trigger (bt) was just added with head and Body and support Support
  %  find any pt''s with unifying heads and add the instantied bt body.
  lookup_u(pt(Head,Body)),
  mpred_eval_lhs(Body,Support),
  fail.
mpred_bt_pt_combine(_,_,_):- !.



%
%  predicates for manipulating action traces.
%   (Undoes side-effects)
%

mpred_ain_actiontrace(Action,Support):-
  % adds an action trace and it''s support.
  mpred_add_support(actn(Action),Support).

mpred_undo_action(actn(Did)):-
  (clause_asserted_u(do_and_undo(Did,Undo))->true;lookup_u(do_and_undo(Did,Undo))),
  call_u_no_bc(Undo),
  !.

%%  mpred_prolog_retractall(X) is nondet.
mpred_prolog_retractall(X):-
 get_assertion_head_unnegated(X,P),
 mpred_prolog_retract(P),fail.
mpred_prolog_retractall(_).

%%  mpred_prolog_retract(X) is nondet.
mpred_prolog_retract(X):-
  %  retract an arbitrary thing.
  mpred_db_type(X,Type),!,
  mpred_retract_type(Type,X).
  


%%  mpred_retract_i(X) is det.
%
%  predicates to remove Pfc facts, triggers, action traces, and queue items
%  from the database.
%
mpred_retract_i(X):-
  %  retract an arbitrary thing.
  mpred_db_type(X,Type),!,
  mpred_retract_type(Type,X),
  !.

mpred_retract_type(fact(_FT),X):-
  %  db mpred_ain_db_to_head(X,X2), retract_u(X2).
  % stop_trace(mpred_retract_type(fact(FT),X)),
  (retract_u(X)
   *-> mpred_unfwc(X) ; (mpred_unfwc(X),!,fail)).

mpred_retract_type(rule(_RT),X):-
  %  db  mpred_ain_db_to_head(X,X2),  retract_u(X2).
  (retract_u(X)
      *-> mpred_unfwc(X) ; (mpred_unfwc(X),!,fail)).

mpred_retract_type(trigger(_TT),X):-
  retract_u(X)
    -> mpred_unfwc(X)
     ; mpred_warn("Trigger not found to retract_u: ~p",[X]).

mpred_retract_type(action,X):- mpred_undo_action(X).


%%  mpred_ain_object(X)
%
% adds item P to some database
%
mpred_ain_object(X):-
  % what type of P do we have?
  mpred_db_type(X,Type),
  % call the appropriate predicate.
  mpred_ain_by_type(Type,X).

mpred_ain_by_type(fact(_FT),X):-
  mpred_unique_u(X),
  assert_u_confirmed_was_missing(X),!.
mpred_ain_by_type(rule(_RT),X):-
  mpred_unique_u(X),
  assert_u_confirmed_was_missing(X),!.
mpred_ain_by_type(trigger(_TT),X):-
  assert_u_confirmed_was_missing(X).
mpred_ain_by_type(action,_ZAction):- !.



%% mpred_withdraw(P,S) removes support S from P and checks to see if P is still supported.
%% If it is not, then the fact is retreactred from the database and any support
%% relationships it participated in removed.

mpred_withdraw(P):- mpred_reduced_chain(mpred_withdraw,P),!.

mpred_withdraw(mfl4(_VarNameZ,_,_,_)):-!.
mpred_withdraw(P) :- 
  only_is_user_reason(UU),
  % iterate down the list of facts to be mpred_withdraw''ed.
  (is_list(P)->
  mpred_withdraw_list(P,UU);
    % mpred_withdraw/1 is the user's interface - it withdraws user support for P.
  mpred_withdraw(P,UU)).
  
  
mpred_withdraw_list(P) :- 
  only_is_user_reason(UU),
  mpred_withdraw_list(P,UU).

mpred_withdraw_list([H|T],UU) :-
  % mpred_withdraw each element in the list.
  mpred_withdraw(H,UU),
  mpred_withdraw_list(T,UU).

maybe_user_support(P,S,SS):- 
  (mpred_get_support(P,S) ->
  (frozen(S,Goals),
  (Goals == true  -> SS=S ; SS = freeze(S,Goals))); SS = unKnown_suppoRt).

mpred_withdraw(P,S) :-
  maybe_user_support(P,S,SS),
  (SS \== unKnown_suppoRt ->
  % pfcDebug(format("~Nremoving support ~p from ~p",[SS,P])),
  (mpred_trace_msg('\n    Removing support: ~p~n',[SS]),
  mpred_trace_msg('     Which was for: ~p~n',[P])); 
    nop(dmsg_pretty(mpred_withdraw(P,S)))),
  ignore(mpred_withdraw_fail_if_supported(P,S)).

mpred_withdraw_fail_if_supported(mfl4(_VarNameZ,_,_,_),_):-!.
mpred_withdraw_fail_if_supported(P,S):-
  maybe_user_support(P,S,SS),
  (((lookup_spft(P,F,T), S= (F,T), mpred_rem_support(P,S), nop(dmsg_pretty(found(mpred_rem_support1(P,S)))))
     -> (remove_if_unsupported(P),retractall(t_l:busy(_)))
      ; ((mpred_withdraw_fail_if_supported_maybe_warn(SS,P),
            \+ show_still_supported(P))))).

mpred_withdraw_fail_if_supported_maybe_warn(_,P):- P== singleValuedInArg(arity, 2).
mpred_withdraw_fail_if_supported_maybe_warn(_,P):- P= prologSingleValued(_Arity).
% mpred_withdraw_fail_if_supported_maybe_warn(_,~P):- nonvar(P),!.
mpred_withdraw_fail_if_supported_maybe_warn(unKnown_suppoRt,P):- 
  maybe_user_support(P,S,SS),
        (((lookup_spft(P,F,T), S= (F,T), call(mpred_rem_support(P,S)),
           nop(dmsg_pretty(found(mpred_rem_support2(P,S)))))
           -> (remove_if_unsupported(P),retractall(t_l:busy(_)))
            ; (( nop(mpred_withdraw_fail_if_supported_maybe_warn(SS,P)),
                  \+ show_still_supported(P))))).
mpred_withdraw_fail_if_supported_maybe_warn(S,P):- 
  mpred_get_support(P,S),SS=S,
        (((lookup_spft(P,F,T), S= (F,T), mpred_rem_support(P,S),dmsg_pretty(found(mpred_rem_support3(P,S))))
           -> (remove_if_unsupported(P),retractall(t_l:busy(_)))
            ; (( nop(mpred_withdraw_fail_if_supported_maybe_warn(SS,P)),
                  \+ show_still_supported(P))))).
mpred_withdraw_fail_if_supported_maybe_warn(SS,P):-
  mpred_trace_msg("mpred_withdraw/2 Could not find support ~p to remove (fact): ~p",[SS,P]).

show_still_supported(P):-  ((mpred_supported(P),mpred_trace_msg('~p',[still_supported(P)]))).


%% mpred_remove(P) is det.
%% mpred_remove2(P) is det.
%% mpred_remove2(P,S) is det.
%
% mpred_remove2 is like mpred_withdraw, but if P is still in the DB after removing the
% user's support, it is retracted by more forceful means (e.g. mpred_blast).
%
mpred_remove(P):- mpred_withdraw(P), (mpred_supported(P) -> mpred_blast(P); true).

mpred_remove2(P):- mpred_reduced_chain(mpred_remove2,P),!.

mpred_remove2(P) :-  only_is_user_reason(UU),
  % mpred_remove2/1 is the user's interface - it withdraws user support for P.
  mpred_remove2(P,UU).

mpred_remove2(P,S) :-
  mpred_withdraw(P,S),
  (call_u(P)
     -> ( mpred_blast(P) )
      ; true).

mpred_retract_is_complete(mfl4(_VarNameZ,_,_,_)):-!.
mpred_retract_is_complete(P) :- \+ mpred_supported(local,P), \+ call_u(P).

mpred_retract(P):- mpred_withdraw(P), mpred_retract_is_complete(P),!,mpred_trace_msg('    Withdrew: ~p',[P]).
mpred_retract(P):- mpred_retract_preconds(P), mpred_retract_is_complete(P),!,mpred_trace_msg('    Retracted: ~p~n',[P]).
mpred_retract(P):- listing(P),mpred_why_1(P),show_call(mpred_blast(P)),mpred_retract_is_complete(P),!,mpred_trace_msg('    Blasted: ~p~n',[P]).
mpred_retract(P):- ok_left_over(P),mpred_trace_msg('    Still True (ok_left_over): ~p~n',[P]),!,ignore((with_no_retry_undefined((mpred_why_1(P),listing(P))))).
mpred_retract(P):- listing(P),mpred_why_1(P),!,with_no_retry_undefined(P),mpred_warn('    Still True: ~p~n',[P]),
  log_failure_red,sleep(2),!,ok_left_over(P).
  

ok_left_over(P):- strip_module(P,M,H),ok_left_over(M,H).
ok_left_over(_,arity(_,_)).

mpred_retract_preconds(P):- mpred_retract_1preconds(P).

mpred_retract_1preconds(P):- 
  supporters_list0(P,WhyS),
  member(S,WhyS),
  mpred_db_type(S,fact(_)),
  mpred_children(S,Childs),
  Childs=[C],C=@=P,
  mpred_trace_msg('    Removing support1: ~p~n',[S]),
  mpred_trace_msg('       Which was for: ~p~n',[P]),
  show_call(mpred_retract(S)).  

mpred_retract_1preconds(P):- 
  supporters_list0(P,WhyS),
  member(S,WhyS),
  mpred_db_type(S,fact(_)),
  mpred_children(S,Childs),
  mpred_trace_msg('    Removing support2: ~p~n',[S]),
  mpred_trace_msg(' Childs: ~p~n',[Childs]),
  show_call(mpred_retract(S)).



mpred_retract1(P):- 
  supporters_list0(P,WhyS),
  must_maplist(mpred_retract_if_fact,WhyS).


mpred_retract_if_fact(P):-  mpred_db_type(P,fact(_)),!, mpred_retract1(P).
mpred_retract_if_fact(_).

%
%  mpred_blast(+F) retracts fact F from the DB and removes any dependent facts
%

mpred_blast(F) :- 
  mpred_remove_supports_whine(F),
  mpred_undo(F).

mpred_retract_all(P):- 
  repeat, \+ mpred_retract(P).

% removes any remaining supports for fact F, complaining as it goes.

mpred_remove_supports_whine(P) :- 
  lookup_spft(P,F,S),
  mpred_trace_msg("~p was still supported by ~p",[F,S]),
  %  mpred_retract_i_or_warn(spft(P,F,S)).
  fail.
mpred_remove_supports_whine(_).

mpred_remove_supports_quietly(F) :- 
  mpred_rem_support(F,_),
  fail.
mpred_remove_supports_quietly(_).


%% mpred_undo(X) undoes X.
%
% - a positive or negative trigger.
% - an action by finding a method and successfully executing it.
% - or a random fact, printing out the trace, if relevant.
%

mpred_undo(P):- mpred_reduced_chain(mpred_undo,P),!.
mpred_undo(X):- mpred_undo1(X),!.


% maybe still un-forward chain?
mpred_undo_unfwd(Fact):-
  % undo a random fact, printing out the dtrace, if relevant.
  (mpred_unfwc(Fact) *-> mpred_trace_msg(mpred_unfwc(Fact));mpred_trace_msg( \+ mpred_unfwc(Fact))).
% mpred_undo(X):- doall(mpred_undo1(X)).

mpred_undo1((H:-B)):- reduce_clause(unpost,(H:-B),HB), HB\=@= (H:-B),!,mpred_undo1((HB)).
mpred_undo1(actn(A)):-
  % undo an action by finding a method and successfully executing it.
  !,
  show_call(mpred_undo_action(actn(A))).

mpred_undo1(pt(Key,Head,Body)):-
  % undo a positive trigger 3.
  %
  !,
  (show_mpred_success(mpred_undo1_pt_unfwc_3,retract_u(pt(Key,Head,Body)))
    -> mpred_unfwc(pt(Head,Body))
     ; mpred_warn("Trigger not found to undo: ~p",[pt(Head,Body)])).

mpred_undo1(pt(Head,Body)):- 
  % undo a positive trigger.
  %
  !,
  (show_mpred_success(mpred_undo1_pt_unfwc_2,retract_u(pt(Head,Body)))
    -> mpred_unfwc(pt(Head,Body))
     ; mpred_warn("Trigger not found to undo: ~p",[pt(Head,Body)])).

mpred_undo1(nt(Head,Condition,Body)):-
  % undo a negative trigger.
  !,
  (
   show_mpred_success(mpred_undo1_nt_unfwc,(nt(Head,Condition,Body),
       dmsg_pretty(mpred_undo1(nt(Head,Condition,Body))),retract_u(nt(Head,Condition,Body))))
    -> (mpred_unfwc(nt(Head,Condition,Body))->true;show_call(assert_u(nt(Head,Condition,Body))))
     ; mpred_trace_msg("WARNING?? Trigger not found to undo: ~p",[nt(Head,Condition,Body)])).

mpred_undo1(P):- mpred_reduced_chain(mpred_undo1,P),!.

mpred_undo1(Fact):-
  % undo a random fact, printing out the dtrace, if relevant.
  (retract_u(Fact)*->true; mpred_trace_msg(show_failure(mpred_undo1,retract_u(Fact)))),
  mpred_trace_op(rem,Fact),
  mpred_unfwc(Fact).



%%  mpred_unfwc(+P)
%
% "un-forward-chains" from fact P.  That is, fact P has just
%  been removed from the database, so remove all support relations it
%  participates in and check the things that they support to see if they
%  should stay in the database or should also be removed.
%

mpred_unfwc(P):- mpred_reduced_chain(mpred_unfwc,P),!.
mpred_unfwc(F):-
  show_failure(mpred_retract_supported_relations(F)),
  mpred_unfwc1(F).

mpred_unfwc1(F):-
  mpred_unfwc_check_triggers(F),
  % is this really the right place for mpred_run<?
  mpred_run,!.


mpred_unfwc_check_triggers(F):- 
 loop_check(mpred_unfwc_check_triggers0(F), 
    (mpred_warn(looped_mpred_unfwc_check_triggers0(F)), mpred_run)).

mpred_unfwc_check_triggers0(F):-
  mpred_db_type(F,_),
 doall(( copy_term_vn(F,Fcopy),
  lookup_u(nt(Fcopy,Condition,Action)),
  \+ call_u_no_bc(Condition),
  mpred_eval_lhs(Action,((\+F),nt(F,Condition,Action))))),
 !.


mpred_unfwc_check_triggers0(F):-
  mpred_db_type(F,FT),
  dmsg_pretty(unknown_rule_type(mpred_db_type(F,FT))),!.



mpred_retract_supported_relations(Fact):-
  mpred_db_type(Fact,Type),Type=trigger(_),
  mpred_rem_support_if_exists(P,(_,Fact)),
  must_ex(nonvar(P)),
  remove_if_unsupported(P),
  fail.

mpred_retract_supported_relations(Fact):-
  mpred_rem_support_if_exists(P,(Fact,_)),
  must_ex(nonvar(P)),
  remove_if_unsupported(P),
  fail.

mpred_retract_supported_relations(_).



%  remove_if_unsupported(+Ps) checks to see if all Ps are supported and removes
%  it from the DB if they are not.
remove_if_unsupported(P):-
  loop_check(remove_if_unsupported0(P),true).
remove_if_unsupported0(P):- \+ mpred_supported(P),!,doall((mpred_undo(P))).
remove_if_unsupported0(P):- \+ is_single_valued(P),!,mpred_trace_msg('~p',[still_supported(P)]).
remove_if_unsupported0(P):- mpred_trace_msg('~p',[sv_still_supported(P)]), doall((mpred_undo(P))).

is_single_valued(P):- get_unnegated_functor(P,F,_)->call_u(prologSingleValued(F)).



%%  mpred_fwc(+X) is det.
%
% forward chains from a fact or a list of facts X.
%
mpred_fwc(Ps):- each_E(mpred_fwc0,Ps,[]).
:- module_transparent((mpred_fwc0)/1).

%%  mpred_fwc0(+X) is det.
%
%  Avoid loop while calling mpred_fwc1(P)
%
% this line filters sequential (and secondary) dupes
 % mpred_fwc0(genls(_,_)):-!.
mpred_fwc0(Fact):- fail, quietly_ex(ground(Fact)),
   \+ t_l:is_repropagating(_),
   quietly_ex((once(((fwc1s_post1s(_One,Two),Six is Two * 1))))), 
   show_mpred_success(filter_buffer_n_test,(filter_buffer_n_test('$last_mpred_fwc1s',Six,Fact))),!.
mpred_fwc0(Fact):- quietly_ex(copy_term_vn(Fact,FactC)),
      loop_check(mpred_fwc1(FactC),true).


filter_buffer_trim(Name,N):- quietly_ex((
  filter_buffer_get_n(Name,List,N),
  nb_setval(Name,List))).

filter_buffer_get_n(Name,FactS,N):-
  nb_current(Name,Fact1s),
  length(Fact1s,PFs),!,
  ((PFs =< N)
    -> FactS=Fact1s;
   (length(FactS,N),append(FactS,_,Fact1s))).
filter_buffer_get_n(_,[],_).


% filter_buffer_n_test(_Name,_,_Fact):- \+ need_speed, !,fail.
filter_buffer_n_test(Name,N,Fact):- filter_buffer_get_n(Name,FactS,N),
   (memberchk(Fact,FactS)-> true ; (nb_setval(Name,[Fact|FactS]),fail)).

:- meta_predicate(mpred_reduced_chain(1,*)).
:- meta_predicate(mpred_reduced_chain(*,*)).
mpred_reduced_chain(P1,(Fact:- (FWC, BODY))):- FWC==fwc,!,call(P1,{BODY}==>Fact).
mpred_reduced_chain(P1,(Fact:- (BWC, BODY))):- BWC==bwc,!,call(P1,(Fact<-BODY)).
mpred_reduced_chain(P1,(P:-AB)):- compound(AB),AB=attr_bind(L,R),!,must_ex(attr_bind(L)),call(P1,(P:-R)).
mpred_reduced_chain(P1,(P:-True)):- True==true,call(P1,P).

mpred_reduced_chain(P1,==>(Fact),P1):- sanity(nonvar(Fact)),!,
  must_ex(full_transform(mpred_fwc1,==>(Fact),ExpandFact)),!,  
  mpred_trace_msg((expanding_mpred_chain(P1,Fact) ==> ExpandFact)),
  sanity(ExpandFact\== (==>(Fact))),
  each_E(P1,ExpandFact,[]).


%% mpred_fwc1(+P) is det.
%
% forward chains for a single fact.
%  Avoids loop while calling mpred_fwc1(P)
mpred_fwc1(clause_asserted_u(Fact)):-!,sanity(clause_asserted_u(Fact)).
mpred_fwc1(P):- mpred_reduced_chain(mpred_fwc1,P),!.
mpred_fwc1(support_hilog(_,_)):-!.
mpred_fwc1(mpred_unload_option(_,_)):-!.

% mpred_fwc1(singleValuedInArg(_, _)):-!.
% this line filters sequential (and secondary) dupes
% mpred_fwc1(Fact):- current_prolog_flag(unsafe_speedups , true) , ground(Fact),fwc1s_post1s(_One,Two),Six is Two * 3,filter_buffer_n_test('$last_mpred_fwc1s',Six,Fact),!.
mpred_fwc1(Prop):-'$current_source_module'(Sm),mpred_m_fwc1(Sm,Prop).


:-thread_local(t_l:busy_f/1).
:-thread_local(t_l:busy_s/1).

mpred_m_fwc1(Sm,Prop):- fixed_syntax(Prop,After),!,must(Prop\=@=After),mpred_m_fwc1(Sm,After).
mpred_m_fwc1(Sm,Prop):- clause_asserted(t_l:busy_s(Prop)),dmsg_pretty(Sm:warn(busy_mpred_m_fwc1(Prop))),!.
mpred_m_fwc1(Sm,Prop):- clause_asserted(t_l:busy_f(Prop)),dmsg_pretty(Sm:warn(busy_mpred_m_fwc1_f(Prop))),!.
mpred_m_fwc1(Sm,Prop):- % clause_asserted(t_l:busy_f(Prop)),!,
   setup_call_cleanup(
     asserta(t_l:busy_s(Prop),R),
     ignore(mpred_m_fwc2(Sm,Prop)),
     ignore(catch(erase(R),_,fail))).
% mpred_m_fwc1(Sm,Prop):- mpred_m_fwc2(Sm,Prop).

mpred_m_fwc2(Sm,Prop):-   
  mpred_trace_msg(Sm:mpred_fwc1(Prop)),
  %ignore((mpred_non_neg_literal(Prop),remove_negative_version(Prop))),
  \+ \+ ignore(mpred_do_rule(Prop)),
  setup_call_cleanup(
    asserta(t_l:busy_f(Prop),R),
    ignore(mpred_do_fact(Prop)),
    ignore(catch(erase(R),_,fail))).



%% mpred_do_rule(P) is det.
% does some special, built in forward chaining if P is
%  a rule.

mpred_do_rule((P==>Q)):-
  !,
  process_rule(P,Q,(P==>Q)).
mpred_do_rule((Name::::P==>Q)):-
  !,
  process_rule(P,Q,(Name::::P==>Q)).
mpred_do_rule((P<==>Q)):-
  !,
  process_rule(P,Q,(P<==>Q)),
  process_rule(Q,P,(P<==>Q)).
mpred_do_rule((Name::::P<==>Q)):-
  !,
  process_rule(P,Q,((Name::::P<==>Q))),
  process_rule(Q,P,((Name::::P<==>Q))).

mpred_do_rule(('<-'(P,Q))):-
  !,
  mpred_define_bc_rule(P,Q,('<-'(P,Q))).

mpred_do_rule(('<=='(P,Q))):-
  !,
  mpred_define_bc_rule(P,Q,('<-'(P,Q))).

mpred_do_rule((H:-B)):- fail, 
  !,
  mpred_do_hb_catchup(H,B).


is_head_LHS(H):- nonvar(H),get_functor(H,F,A),must_ex(suggest_m(M)),lookup_u(mpred_prop(M,F,A,pfcLHS)).
body_clause(SK,Cont):-nonvar(SK),SK=Cont.

mpred_do_hb_catchup(H, _B):- \+ is_head_LHS(H),!.
mpred_do_hb_catchup(_H, B):- \+ \+ (B=true),!.
mpred_do_hb_catchup(_H, B):- compound(B), \+ \+ reserved_body_helper(B),!. 

% prolog_clause mpred_do_rule VAR_H
mpred_do_hb_catchup(H,B):- sanity(nonvar(B)),
  var(H),!,dmsg_pretty(warn(is_VAR_H((H:-B)))),
  trace,   % THe body needs to sanify (bind) the Head
  forall(call_u(B),
     (sanity(nonvar(H)),mpred_ain(H))),!.

mpred_do_hb_catchup(H,Body):- is_head_LHS(H),
   body_clause(Body,attr_bind(AG,B)),
% Should we repropagate(H) ?
   attr_bind(AG),!,
   mpred_do_hb_catchup_now(H,B).


% prolog_clause mpred_do_rule pfcLHS
mpred_do_hb_catchup(H,B):- %is_head_LHS(H),  
% Should we repropagate(H) if body failed?
   mpred_do_hb_catchup_now(H,B).
                     
% mpred_do_hb_catchup(H,B):- !,mpred_do_hb_catchup_now(H,B).

% mpred_do_hb_catchup_now_maybe(_,_):-!.
mpred_do_hb_catchup_now_maybe(H,B):- B\=(cwc,_),
  mpred_do_hb_catchup_now(H,B).

% mpred_do_hb_catchup_now(_,_):-!.
mpred_do_hb_catchup_now(H,B):- B\=(cwc,_),nonvar(B),
  with_exact_assertions(catch( (forall(call_u(B),mpred_fwc(H));true),_,true)),!.


% prolog_clause mpred_do_clause COMMENTED
% mpred_do_clause(Fact,H,B):- nonvar(H),mpred_do_fact({clause(H,B)}),fail.

% prolog_clause mpred_do_clause (_ :- _)

mpred_do_clause(H,B):-
 with_exact_assertions(mpred_do_clause0(H,B)).

mpred_do_clause0(Var, B):- is_ftVarq(Var),!,trace_or_throw(var_mpred_do_clause0(Var, B)).
mpred_do_clause0((=>(_,_)),_):-!.
mpred_do_clause0((==>(_,_)),_):-!.
mpred_do_clause0(H,B):-
  % Fact = {clause(H,B)},
  Fact = (H :- B),  B\=(cwc,_),!,
  copy_term(Fact,Copy),
  % check positive triggers
   mpred_do_fcpt(Fact,Copy), % dmsg_pretty(trace_or_throw_ex(mpred_do_rule(Copy)))),

  %nr_lc_ex(mpred_do_fcpt(Fact,Copy),true), % dmsg_pretty(trace_or_throw_ex(mpred_do_clause(Copy)))),
  % check negative triggers
  mpred_do_fcnt(Fact,Copy),
  mpred_do_hb_catchup(H,B).

:- dynamic(baseKB:todo_later/1).
is_cutted(Cutted):- contains_var(!,Cutted).
do_later(G):- !, call_u(G).
do_later(mpred_do_clause(_,Cutted)):- is_cutted(Cutted),!.
do_later(mpred_do_clause(~_H,_B)):- !.
do_later(G):- assertz(baseKB:todo_later(G)),nop(dmsg(do_later(G))).

% prolog_clause mpred_do_fact (_ :- _)
mpred_do_fact(Fact):-
  Fact = (_:-_), 
  copy_term_vn(Fact,(H:-B)),
  B\=(cwc,_),!,
  do_later(mpred_do_clause(H,B)),!.

mpred_do_fact(Fact):-
  copy_term_vn(Fact,Copy),
  % check positive triggers
   mpred_do_fcpt(Fact,Copy), 
  % dmsg_pretty(trace_or_throw_ex(mpred_do_rule(Copy)))),
  % check negative triggers
  mpred_do_fcnt(Fact,Copy),
  nop(mpred_do_clause(Fact,true)).


get_tms_mode(_P,Mode):- lookup_m(tms(ModeO)),!,ModeO=Mode.
get_tms_mode(_P,Mode):- Mode=local.


% do all positive triggers
mpred_do_fcpt(mpred_prop(swish_help, index_json, 2, kb_shared),_):- dumpST, break.
mpred_do_fcpt(Copy,Trigger):-
  forall((call_u(pt(Trigger,Body)),
  mpred_trace_msg('~N~n\tFound positive trigger: ~p~n\t\tbody: ~p~n',
		[Trigger,Body])),
    forall(mpred_eval_lhs_no_nc(Body,(Copy,pt(Trigger,Body))),
     true)),!.
  
%mpred_do_fcpt(Trigger,F):-
%  lookup_u(pt(presently(F),Body)),
%  mpred_e val_lhs(Body,(presently(Fact),pt(presently(F),Body))),
%  fail.
% mpred_do_fcpt(_,_).

% do all negative triggers
mpred_do_fcnt(_ZFact,Trigger):-
  NT = nt(Trigger,Condition,Body),
  (call_u(NT)*-> lookup_spft(X,F1,NT) ; lookup_spft(X,F1,NT)),
  %clause(SPFT,true),
  mpred_trace_msg('~N~n\tFound negative trigger: ~p~n\t\tcond: ~p~n\t\tbody: ~p~n\tSupport: ~p~n',
                 [Trigger,Condition,Body,spft(X,F1,NT)]),  
  call_u_no_bc(Condition),
  mpred_withdraw(X,(F2,NT)),
  sanity(F1=F2),
  fail.
mpred_do_fcnt(_Fact,_Copy).


%% mpred_define_bc_rule(+Head,+Body,+Parent_rule)
%
% defines a backward chaining rule and adds the
% corresponding bt triggers to the database.
%
mpred_define_bc_rule(Head,_ZBody,Parent_rule):-
  (\+ mpred_literal_nonvar(Head)),
  mpred_warn("Malformed backward chaining rule.  ~p not atomic.",[Head]),
  mpred_error("caused by rule: ~p",[Parent_rule]),
  !,
  fail.

mpred_define_bc_rule(Head,Body,Parent_rule):-
  must_notrace_pfc(get_source_mfl(U)),!,
  copy_term(Parent_rule,Parent_ruleCopy),
  quietly_must_ex(build_rhs(U,Head,Rhs)),
  % kb_local(Head),
  % UNEEDED Due to a trigger that creates it?
  % get_bc_clause(Head,Post),ain(Post),
  foreach(mpred_nf(Body,Lhs),
          ignore((quietly_must_ex(build_trigger(Parent_ruleCopy,Lhs,rhs(Rhs),Trigger)),
           ain_fast(bt(Head,Trigger),(Parent_ruleCopy,U))))).
   
get_bc_clause(Head,(HeadC:- BodyC)):- quietly(get_bc_clause(Head,HeadC,BodyC)).

get_bc_clause(HeadIn, ~HeadC, Body):- compound(HeadIn), HeadIn = ~Head,!,
     Body = ( awc, 
            ( nonvar(HeadC)-> (HeadC = Head,!) ; (HeadC = Head)), 
              mpred_bc_and_with_pfc(~Head)).
get_bc_clause(Head, Head, Body):-  % % :- is_ftNonvar(Head).
     Body = ( awc, !, mpred_bc_and_with_pfc(Head)).

:- thread_initialization(nb_setval('$pfc_current_choice',[])).

push_current_choice:- current_prolog_flag(pfc_support_cut,false),!.
push_current_choice:- prolog_current_choice(CP),push_current_choice(CP),!.
push_current_choice(CP):- nb_current('$pfc_current_choice',Was)->b_setval('$pfc_current_choice',[CP|Was]);b_setval('$pfc_current_choice',[CP]).

cut_c:- current_prolog_flag(pfc_support_cut,false),!.
cut_c:- must_ex(nb_current('$pfc_current_choice',[CP|_WAS])),prolog_cut_to(CP).

%% mpred_eval_lhs(X,Support) is nondet.
%
%  eval something on the LHS of a rule.
%


mpred_eval_lhs(X,S):-
   push_current_choice,
   Loop = _,
   with_current_why(S,
     loop_check(mpred_eval_lhs_0(X,S),Loop=true)),
   (nonvar(Loop)-> (fail,dumpST,break) ; true).

mpred_eval_lhs_no_nc(X,S):- mpred_eval_lhs_0(X,S).


%% mpred_eval_lhs_0(X,Support) is det.
%
%  Helper of evaling something on the LHS of a rule.
%
mpred_eval_lhs_0(rhs(X),Support):- !, mpred_eval_rhs(X,Support).
mpred_eval_lhs_0(X,Support):- mpred_eval_lhs_1(X,Support).


%% mpred_eval_lhs_1(X,Support) is det.
%
%  Helper Secondary of evaling something on the LHS of a rule.
%
mpred_eval_lhs_1(Var,Support):- var(Var),!,trace_or_throw_ex(var_mpred_eval_lhs_0(Var,Support)).
mpred_eval_lhs_1((Test *-> Body),Support):-  % Noncutted *->
  !,
  (call_u_no_bc(Test) *-> mpred_eval_lhs_0(Body,Support)).

mpred_eval_lhs_1((Test -> Body),Support):- !,  % cutted ->
  call_u_no_bc(Test) -> mpred_eval_lhs_0(Body,Support).


%mpred_eval_lhs_1(snip(X),Support):-
%  snip(Support),
%  mpred_eval_lhs_1(X,Support).

mpred_eval_lhs_1(X,Support):- mpred_db_type(X,trigger(_TT)),!,must(mpred_ain_trigger_reprop(X,Support)),!.

mpred_eval_lhs_1(X,_):- mpred_warn("Unrecognized item found in trigger body, namely ~p.",[X]).


args_swapped(~P1,~P2):-!,args_swapped(P1,P2).
args_swapped(P1,P2):- P1  univ_safe  [F,Y,X], P2  univ_safe  [F,X,Y].
fxy_args_swapped(F,X,Y,P1,P2):- P1  univ_safe  [F,X,Y], P2  univ_safe  [F,Y,X].


%%  mpred_eval_rhs1(What,Support) is nondet.
%
%  eval something on the RHS of a rule.
%
mpred_eval_rhs([],_):- !.
mpred_eval_rhs([Head|Tail],Support):-
  mpred_eval_rhs1(Head,Support),
  mpred_eval_rhs(Tail,Support).

mpred_eval_rhs1(Action,Support):- is_ftVarq(Action),throw(mpred_eval_rhs1(Action,Support)).
mpred_eval_rhs1([X|Xrest],Support):-
 % embedded sublist.
 !,
 mpred_eval_rhs([X|Xrest],Support).

mpred_eval_rhs1({Action},Support):-
 % evaluable Prolog code.
 !,
 fc_eval_action(Action,Support).

mpred_eval_rhs1( \+ ~P, _Support):-  nonvar(P), !, 
  %mpred_trace_msg('~N~n~n\t\tRHS-Withdrawing: ~p \n\tSupport: ~p~n',[~P,Support]),
   mpred_withdraw(~P).


% if negated litteral \+ P
mpred_eval_rhs1(\+ P,Support):- nonvar(P),
 % predicate to remove.
  \+ mpred_negated_literal( P),
  %TODO Shouldn''t we be mpred_withdrawing the Positive version?
  % perhaps we aready negated here dirrent nf1_*
  mpred_trace_msg('~N~n~n\t\tRHS-Withdrawing-Neg: ~p \n\tSupport: ~p~n',[P,Support]),
  !,
  mpred_withdraw(P).


% Dmiles replaced with this
mpred_eval_rhs1( P,Support):- 
 % predicate to remove.
  P\= ~(_),
  quietly(mpred_unnegate( P , PN)),!,
  %TODO Shouldn''t we be mpred_withdrawing the Positive version?  (We are)
  % perhaps we aready negated here from mpred_nf1_negation?!
  mpred_trace_msg('~N~n~n\t\tNegation causes RHS-Withdrawing: ~p \n\tSupport: ~p~n',[P,Support]),
  !,
  mpred_withdraw(PN).


% if negated litteral \+ P
mpred_eval_rhs1( P,Support):-
 % predicate to remove.
  P \= ~(_),
  \+ \+ mpred_negated_literal( P),
  %TODO Shouldn''t we be mpred_withdrawing the Positive version?
  % perhaps we aready negated here dirrent nf1_*
  mpred_trace_msg('~N~n~n\t\tRHS-Withdrawing-mpred_negated_literal: ~p \n\tSupport: ~p~n',[P,Support]),
  !,
  mpred_withdraw(P).

mpred_eval_rhs1(Assertion,Support):- !,
 % an assertion to be added.
 mpred_trace_msg('~N~n~n\tRHS-Post1: ~p \n\tSupport: ~p~n',[Assertion,Support]),!,
 ((mpred_post(Assertion,Support)) *->
    true;
    mpred_warn("\n\t\t\n\t\tMalformed rhs of a rule (mpred_post1 failed)\n\t\tPost1: ~p\n\t\tSupport=~p.",[Assertion,Support])).

% mpred_eval_rhs1(X,_):-  mpred_warn("Malformed rhs of a rule: ~p",[X]).


%% fc_eval_action(+Action,+Support)
%
%  evaluate an action found on the rhs of a rule.
%

fc_eval_action(CALL,Support):-
  mpred_METACALL(fc_eval_action_rev(Support),CALL).

fc_eval_action_rev(Support,Action):-
  (call_u_no_bc(Action)),
  (show_success(action_is_undoable(Action))
     -> mpred_ain_actiontrace(Action,Support)
      ; true).

/*
%
%
%

trigger_trigger(Trigger,Body,_ZSupport):-
 trigger_trigger1(Trigger,Body).
trigger_trigger(_,_,_).


%trigger_trigger1(presently(Trigger),Body):-
%  !,
%  copy_term_vn(Trigger,TriggerCopy),
%  call_u(Trigger),
%  mpred_eval_lhs(Body,(presently(Trigger),pt(presently(TriggerCopy),Body))),
%  fail.

trigger_trigger1(Trigger,Body):-
  copy_term_vn(Trigger,TriggerCopy),
  call_u(Trigger),
  mpred_eval_lhs(Body,(Trigger,pt(TriggerCopy,Body))),
  fail.
*/


call_m_g(To,_M,G):- To:call(G).
lookup_m_g(To,_M,G):- clause(To:G,true).

%%  call_u(F) is det.
%
%  is true iff F is a fact available *for* forward chaining
%  (or *from* the backchaining rules)
%  Note: a bug almost fixed is that this sometimes the side effect of catching
%  facts and not assigning the correct justifications
%
% call_u(P):- predicate_property(P,number_of_rules(N)),N=0,!,lookup_u(P).

% :- table(call_u/1).


% call_u(M:G):- !,module_sanity_check(M),call_mp(M,G).

%call_u(G):- \+  current_prolog_flag(retry_undefined, kb_shared),!,
%   strip_module(G,M,P), no_repeats(gripe_time(5.3,on_x_rtrace(call_mp(M,P)))).
%call_u(M:G):- !, M:call(G).

% prolog_clause call_u ?
%call_u(G):- G \= (_:-_), !, quietly_ex(defaultAssertMt(M)),!,call_mp(M,G).
call_u(G):- notrace(strip_module(G,M,P)),call_u_mp(M,P).

get_var_or_functor(H,F):- compound(H)->get_functor(H,F);H=F.

%call_u(G):- strip_module(G,M,P), no_repeats(gripe_time(5.3,on_x_rtrace(call_mp(M,P)))).

call_u_mp(Var, P):- notrace(var(Var)),!,clause_b(mtHybrid(Var)),call_mp(Var,P).
call_u_mp(assert, P):- !, notrace(must(get_assert_to(SM))),call_mp(SM,P).
call_u_mp(M, P):- notrace(M==query;M==pfc_lib;is_code_module(M)),!, notrace((get_query_from(SM),sanity(pfc_lib\==SM))),call_mp(SM,P).
call_u_mp(M, P):- call_mp(M, P).

%call_mp(M,Var):- notrace(var(P)),!, M:mpred_call_with_no_triggers(P).
call_mp(M,P):- notrace(var(P)),!,call((clause_bq(mtExact(M))->mpred_fact_mp(M,P);(defaultAssertMt(W),with_umt(W,mpred_fact_mp(W,P))))).
call_mp(M,M:P):-!,sanity(atom(M)),call_mp(M,P).

call_mp(_,functorDeclares(H)):-  get_var_or_functor(H,F),!,clause_b(functorDeclares(F)).
call_mp(_,singleValuedInArg(H,A)):- get_var_or_functor(H,F),!,clause_b(singleValuedInArg(F,A)).
call_mp(_,singleValuedInArgAX(H,A,N)):- get_var_or_functor(H,F),!,clause_b(singleValuedInArgAX(F,A,N)).
call_mp(_,ttRelationType(C)):- !, clause_b(ttRelationType(C)).
call_mp(M,ttExpressionType(P)):-!,clause_b(M:ttExpressionType(P)).
call_mp(M,mtHybrid(P)):-!,clause_b(M:mtHybrid(P)).
%call_mp(pfc_lib, P1 ):- !, call_mp(query, P1 ).
% call_mp(pfc_lib, P1 ):- !, break_ex,'$current_source_module'(SM),SM\==pfc_lib,!,  call_mp(SM,P1).
call_mp(_, M:P1):-!,call_u_mp(M,P1).

call_mp(M, (P1,P2)):-!,call_mp(M,P1),call_mp(M,P2).
call_mp(M, (P1*->P2;P3)):-!,(call_mp(M,P1)*->call_mp(M,P2);call_mp(M,P3)).
call_mp(M, (P1->P2;P3)):-!,(call_mp(M,P1)->call_mp(M,P2);call_mp(M,P3)).
call_mp(M, (P1->P2)):-!,(call_mp(M,P1)->call_mp(M,P2)).
call_mp(M, (P1*->P2)):-!,(call_mp(M,P1)*->call_mp(M,P2)).
call_mp(M, (P1;P2)):- !,(call_mp(M,P1);call_mp(M,P2)).
call_mp(M,( \+ P1)):-!, \+ call_mp(M,P1).
call_mp(M,clause(H,B,Ref)):-!,M:clause_u(H,B,Ref).
call_mp(M,clause(H,B)):-!,M:clause_u(H,B).
call_mp(M,clause(HB)):-expand_to_hb(HB,H,B),!,M:clause_u(H,B).
call_mp(M,asserta(X)):- !, M:mpred_aina(X).
call_mp(M,assertz(X)):- !, M:mpred_ainz(X).
call_mp(M,assert(X)):- !, M:mpred_ain(X).
call_mp(M,retract(X)):- !, M:mpred_prolog_retract(X).
call_mp(M,retractall(X)):- !, M:mpred_prolog_retractall(X).

call_mp(M,must_ex(P)):-!, M:must_ex( call_mp(M,P)).
call_mp(M, 't'(P)):-!, call_mp(M,P).
call_mp(M,t(A,B)):-(atom(A)->true;(no_repeats(arity_no_bc(A,1)),atom(A))),ABC=..[A,B],call_mp(M,ABC).
call_mp(M,isa(B,A)):-(atom(A)->true;(call_u(tCol(A)),atom(A))),ABC=..[A,B],call_mp(M,ABC).
%call_mp(M,t(A,B)):-!,(atom(A)->true;(no_repeats(arity_no_bc(A,1)),atom(A))),ABC=..[A,B],call_mp(M,ABC).
call_mp(M,t(A,B,C)):-!,(atom(A)->true;(no_repeats(arity_no_bc(A,2)),atom(A))),ABC=..[A,B,C],call_mp(M,ABC).
call_mp(M,t(A,B,C,D)):-!,(atom(A)->true;(no_repeats(arity_no_bc(A,3)),atom(A))),ABC=..[A,B,C,D],call_mp(M,ABC).
call_mp(M,t(A,B,C,D,E)):-!,(atom(A)->true;(no_repeats(arity_no_bc(A,4)),atom(A))),ABC=..[A,B,C,D,E],call_mp(M,ABC).
call_mp(M,'{}'(P)):-!, call_mp(M,P).
%call_mp(_,is_string(P)):- !, logicmoo_util_bugger:is_string(P).
call_mp(M,call_u(P)):- !, call_mp(M,P).
call_mp(_,call_mp(Mt,P)):- !, call_mp(Mt,P).
call_mp(M,call(P)):- !, call_mp(M,P).
call_mp(M,call(P1,A)):- append_term(P1,A,P),!,call_mp(M,P).
% call_mp(MaseKB,call_u_no_bc(P)):- !, call_mp(MaseKB,P).

% prolog_clause call_u
% call_mp(M, (H:-B)):- B=@=call(BA),!,B=call(BA),!, (M:clause_u(H,BA);M:clause_u(H,B)),sanity(\+ reserved_body(B)).
call_mp(M, (H:-B)):- !,call_mp(M,clause_u(H,B)),(\+ reserved_body(B)).
% call_mp(M, (H:-B)):- !,call_mp(M,clause_u(H,B)),sanity(\+ reserved_body(B)).

%call_mp(M,H):- !, locally_tl(infAssertedOnly(H),call_u(H)).
%call_mp(M,argIsa(mpred_isa,2,mpred_isa/2)):-  trace_or_throw_ex(call_mp(M,argIsa(mpred_isa,2,mpred_isa/2))),!,fail.
% TODO: test removal
% call_mp(M,isa(H,B)):-!,isa_asserted(H,B).

% call_mp(M,P1):- predicate_property(M:P1,foreign),!,M:call(P1).
% call_mp(M,P1):- predicate_property(M:P1,static),!,M:call(P1).
%call_mp(M,P1):- predicate_property(M:P1,built_in),!, M:call(P1).
%call_mp(M,P1):- predicate_property(M:P1,dynamic),!, M:call(P1).

%call_mp(M,HB):-quietly((full_transform_warn_if_changed(mpred_call_0,HB,HHBB))),!,call_mp(M,HHBB).
call_mp(M,H):- notrace(fixed_negations(H,O)),!, call_mp(M,O).

call_mp(M,P1):- notrace(predicate_property(M:P1,defined)),!, M:call(P1).


%call_mp(M,H):- is_static_predicate(H),!,M:call(H).
%call_mp(M,H):- is_static_predicate(H),!,show_pred_info(H),dtrace(call_mp(M,H)).

call_mp(M,P1):- trace, !,M:call(P1).
% @TODO NEVER GETS HERE 
call_mp(M,P):- safe_functor(P,F,A), call_u_mp_fa(M,P,F,A).


%% mpred_call_1( +VALUE1, ?G, ?VALUE3) is semidet.
%
% PFC call  Secondary Helper.
%
mpred_call_1(_,G,_):- is_side_effect_disabled,!,mpred_call_with_no_triggers(G).

mpred_call_1(M,G,F):- sanity(\+  is_side_effect_disabled),
               (ground(G); \+ current_predicate(_,M:G) ; \+ (predicate_property(M:G,number_of_clauses(CC)),CC>1)), 
    
                ignore((nr_lc_ex(call_with_bc_triggers(M:G)),maybeSupport(G,(g,ax)),fail)),
                 \+ current_predicate(F,M:G),\+ current_predicate(_,_:G),
                 doall(show_call(predicate_property(_UM:G,_PP))),
                 debug_logicmoo(logicmoo(_)),
                 fail,
                 %TODO remove this failure
                 must(show_call(kb_shared(M:G))),
                 kb_shared(M:G),!,fail.
mpred_call_1(_,G,_):- mpred_call_with_no_triggers(G).



make_visible(R,M:F/A):- wdmsg_pretty(make_visible(R,M:F/A)),fail.
make_visible(_,_):- !.
make_visible(M,M:F/A):- quietly_ex(M:export(M:F/A)).
make_visible(R,M:F/A):- must_det_l((M:export(M:F/A),R:import(M:F/A),R:export(M:F/A))).

make_visible(R,M,F,A):- wdmsg_pretty(make_visible(R,M,F,A)),fail.
make_visible(system,M,F,A):- trace_or_throw_ex(unexpected(make_visible(system,M,F,A))).
make_visible(user,M,F,A):- trace_or_throw_ex(unexpected(make_visible(user,M,F,A))).
make_visible(TM,M,F,A):- 
   must_ex((TM:import(M:F/A),TM:export(TM:F/A))),
   must_ex((TM:module_transparent(M:F/A))). % in case this has clauses th

reserved_body(B):-var(B),!,fail.
reserved_body(attr_bind(_)).
reserved_body(attr_bind(_,_)).
reserved_body(B):-reserved_body_helper(B).

reserved_body_helper(B):- \+ compound(B),!,fail.
reserved_body_helper((ZAWC,_)):- atom(ZAWC),is_pfc_chained(ZAWC).
%reserved_body_helper(inherit_above(_,_)).
%reserved_body_helper(Body):- get_bc_clause(_Head,_Head2,BCBody),!,Body=BCBody.
%reserved_body_helper((_,Body)):-!,reserved_body_helper(Body).

call_u_mp_fa(M,P,F,A):- !,nr_lc_ex(call_u_mp_lc(M,P,F,A)).
% @TODO NEVER GETS HERE 
call_u_mp_fa(_,P,F,_):- (F==t; ( \+ clause_bq(prologBuiltin(F)),
  F \= isT,F \= isTT, \+ predicate_property(P,file(_)))),if_defined(t_ify0(P,TGaf),fail), if_defined(isT(TGaf),false).
call_u_mp_fa(M,P,F,A):- nr_lc_ex(call_u_mp_lc(M,P,F,A)).

%call_u_mp_lc(pfc_lib,P,F,A):-!, call_u_mp_lc(baseKB,P,F,A).
%call_u_mp_lc(M,P,F,A):- current_predicate(M:F/A),!,throw(current_predicate(M:F/A)),catch(M:P,E,(wdmsg_pretty(call_mp(M,P)),wdmsg_pretty(E),dtrace)).
% call_u_mp_lc(baseKB,P,F,A):- kb_shared(F/A),dmsg_pretty(kb_shared(F/A)),!, call(P).

/*
call_u_mp_lc(M,P,_,_):- predicate_property(M:P,file(_)),!,call(M:P).
call_u_mp_lc(M,P,_,_):- source_file(M:P,_),!,call(M:P).
call_u_mp_lc(R,P,F,A):- source_file(M:P,_),!,make_visible(R,M:F/A),call(R:P).
call_u_mp_lc(R,P,F,A):- find_module(R:P,M),dmsg_pretty(find_module(R:P,M)),make_visible(R,M:F/A),!,catch(R:call(P),E,(wdmsg_pretty(call_mp(R,M:P)),wdmsg_pretty(E),dtrace)).
%call_u_mp_lc(M,P):- \+ clause_bq(mtHybrid(M)),!,clause_bq(mtHybrid(MT)),call_mp(MT,P).
call_u_mp_lc(M,P,F,A):- wdmsg_pretty(dynamic(M:P)),must_det_l((dynamic(M:F/A),make_visible(user,M:F/A),multifile(M:F/A))),!,fail.
*/
/*       
Next
call_mp(_G,M,P):- var(P),!,call((baseKB:mtExact(M)->mpred_fact_mp(M,P);(defaultAssertMt(W),with_umt(W,mpred_fact_mp(W,P))))).
% call_mp(mtHybrid(P),_,mtHybrid(P)):-!,baseKB:mtHybrid(P).
call_mp((P),M,(P)):-!,catch(call(P),E,(wdmsg_pretty(M:call_mp(P)),wdmsg_pretty(E),dtrace)).
% call_mp(P,M,P):- !,catch(M:call(P),E,(wdmsg_pretty(M:call_mp(P)),wdmsg_pretty(E),dtrace)).
call_mp(_G,M,P):- call((baseKB:mtExact(M)->M:call(P);call(P))).
*/
call_u_mp_lc(M,P,_,_):- !, M:call_mp(M,P).
call_u_mp_lc(M,P,_,_):- !, M:call(P).




mpred_BC_w_cache(W,P):- must_ex(mpred_BC_CACHE(W,P)),clause_u(P,true).

mpred_BC_CACHE(M,P0):-  ignore( \+ loop_check_early(mpred_BC_CACHE0(M,P0),trace_or_throw_ex(mpred_BC_CACHE(P0)))).

mpred_BC_CACHE0(_,P00):- var(P00),!.
mpred_BC_CACHE0(M,must_ex(P00)):-!,mpred_BC_CACHE0(M,P00).
mpred_BC_CACHE0(_,P):- predicate_property(P,static),!.
% mpred_BC_CACHE0(_,P):- predicate_property(P,built_in),!.
mpred_BC_CACHE0(_, :-(_,_)):-!.
mpred_BC_CACHE0(_,bt(_,_)):-!.
mpred_BC_CACHE0(_,clause(_,_)):-!.
mpred_BC_CACHE0(_,spft(_,_,_)):-!.
mpred_BC_CACHE0(_,P):-
 ignore((
  cyclic_break(P),
 % trigger any bc rules.
  lookup_u(bt(P,Trigger)),
  copy_term_vn(bt(P,Trigger),bt(CP,CTrigger)),
  must_ex(lookup_spft(bt(CP,_Trigger),F,T)),
  mpred_eval_lhs(CTrigger,(F,T)),
  fail)).



% I''d like to remove this soon
%call_u_no_bc(P0):- strip_module(P0,M,P), sanity(stack_check),var(P),!, M:mpred_fact(P).
%call_u_no_bc(_:true):-!.
call_u_no_bc(P):- no_repeats(call_u(P)).
% call_u_no_bc(P):- !, call_u(P).
%call_u_no_bc(G):- !, call(G).
% call_u_no_bc(P):- nr_lc_ex((mpred_METACALL(call_u, P))).

% mpred_call_no_bc0(P):- lookup_u(P).
% mpred_call_no_bc0(P):-  defaultAssertMt(Mt), Mt:lookup_u(P).
% mpred_call_no_bc0((A,B)):-!, mpred_call_no_bc0(A),mpred_call_no_bc0(B).
%mpred_call_no_bc0(P):-  defaultAssertMt(Mt),current_predicate(_,Mt:P),!,Mt:call(P).
%mpred_call_no_bc0(P):-  defaultAssertMt(Mt),rtrace(Mt:call(P)).
% TODO .. mpred_call_no_bc0(P):-  defaultAssertMt(Mt), clause_bq(genlMt(Mt,SuperMt)), call_umt(SuperMt,P).
%mpred_call_no_bc0(P):- mpred_call_with_no_triggers(P).
% mpred_call_no_bc0(P):- nonvar(P),predicate_property(P,defined),!, P.
mpred_call_no_bc0(P):- current_prolog_flag(unsafe_speedups , true) ,!,call(P).
mpred_call_no_bc0(P):- nr_lc_ex(mpred_METACALL(ereq, P)).

pred_check(A):- var(A),!.
% catch module prefix issues
pred_check(A):- nonvar(A),must_ex(atom(A)).

%mpred_METACALL(How,P):- current_prolog_flag(unsafe_speedups , true) ,!,call(How,P).
mpred_METACALL(How,P):- mpred_METACALL(How, Cut, P), (var(Cut)->true;(Cut=cut(CutCall)->(!,CutCall);call_u_no_bc(Cut))).

mpred_METACALL(How, Cut, Var):- var(Var),!,trace_or_throw_ex(var_mpred_METACALL_MI(How,Cut,Var)).
mpred_METACALL(How, Cut, (H:-B)):-!,mpred_METACALL(How, Cut, clause_asserted_call(H,B)).
%  this is probably not advisable due to extreme inefficiency.
mpred_METACALL(_How,_Cut, Var):-is_ftVarq(Var),!,mpred_call_with_no_triggers(Var).
mpred_METACALL(How, Cut, call_u_no_bc(G0)):- !,mpred_METACALL(How, Cut, (G0)).
mpred_METACALL(_How, Cut, mpred_METACALL(How2, G0)):- !,mpred_METACALL(How2, Cut, (G0)).
mpred_METACALL(How, Cut, mpred_METACALL(G0)):- !,mpred_METACALL(How, Cut, (G0)).
mpred_METACALL(_How, cut(true), !):- !.

mpred_METACALL(How, Cut, (C1->C2;C3)):-!,(mpred_METACALL(How, Cut, C1)->mpred_METACALL(How, Cut, C2);mpred_METACALL(How, Cut, C3)).
mpred_METACALL(How, Cut, (C1*->C2;C3)):-!,(mpred_METACALL(How, Cut, C1)*->mpred_METACALL(How, Cut, C2);mpred_METACALL(How, Cut, C3)).

mpred_METACALL(How, Cut, (C1->C2)):-!,(mpred_METACALL(How, Cut, C1)->mpred_METACALL(How, Cut, C2)).
mpred_METACALL(How, Cut, (C1*->C2)):-!,(mpred_METACALL(How, Cut, C1)*->mpred_METACALL(How, Cut, C2)).
mpred_METACALL(How, Cut, (C1,C2)):-!,mpred_METACALL(How, Cut, C1),mpred_METACALL(How, Cut, C2).
mpred_METACALL(How, Cut, (C1;C2)):-!,(mpred_METACALL(How, Cut, C1);mpred_METACALL(How, Cut, C2)).
%  check for system predicates first
% mpred_METACALL(_How, _SCut, P):- predicate_property(P,built_in),!, call(P).


mpred_METACALL(How, Cut, M):- fixed_syntax(M,O),!,mpred_METACALL(How, Cut, O).
mpred_METACALL(How, Cut, U:X):-U==user,!,mpred_METACALL(How, Cut, X).
% mpred_METACALL(How, Cut, t(A,B)):-(atom(A)->true;(no_repeats(arity(A,1)),atom(A))),ABC univ_safe [A,B],mpred_METACALL(How, Cut, ABC).
% mpred_METACALL(How, Cut, isa(B,A)):-(atom(A)->true;(no_repeats(tCol(A)),atom(A))),ABC univ_safe [A,B],mpred_METACALL(How, Cut, ABC).
%mpred_METACALL(How, Cut, t(A,B)):-!,(atom(A)->true;(no_repeats(arity(A,1)),atom(A))),ABC univ_safe [A,B],mpred_METACALL(How, Cut, ABC).
mpred_METACALL(How, Cut, t(A,B,C)):-!,(atom(A)->true;(no_repeats(arity(A,2)),atom(A))),ABC univ_safe [A,B,C],mpred_METACALL(How, Cut, ABC).
mpred_METACALL(How, Cut, t(A,B,C,D)):-!,(atom(A)->true;(no_repeats(arity(A,3)),atom(A))),ABC univ_safe [A,B,C,D],mpred_METACALL(How, Cut, ABC).
mpred_METACALL(How, Cut, t(A,B,C,D,E)):-!,(atom(A)->true;(no_repeats(arity(A,4)),atom(A))),ABC univ_safe [A,B,C,D,E],mpred_METACALL(How, Cut, ABC).
mpred_METACALL(How, Cut, call(X)):- !, mpred_METACALL(How, Cut, X).
mpred_METACALL(How, Cut, call_u(X)):- !, mpred_METACALL(How, Cut, X).
mpred_METACALL(How, Cut, once(X)):- !, once(mpred_METACALL(How, Cut, X)).
mpred_METACALL(How, Cut, must_ex(X)):- !, must_ex(mpred_METACALL(How, Cut, X)).
mpred_METACALL(How, Cut, \+(X)):- !, \+ mpred_METACALL(How, Cut, X).
mpred_METACALL(How, Cut, not(X)):- !,\+ mpred_METACALL(How, Cut, X).
mpred_METACALL(_How, _Cut, clause(H,B,Ref)):-!,clause_u(H,B,Ref).
mpred_METACALL(_How, _Cut, clause(H,B)):-!,clause_u(H,B).
mpred_METACALL(_How, _Cut, clause(HB)):-expand_to_hb(HB,H,B),!,clause_u(H,B).
mpred_METACALL(_How, _Cut, asserta(X)):- !, aina(X).
mpred_METACALL(_How, _Cut, assertz(X)):- !, ainz(X).
mpred_METACALL(_How, _Cut, assert(X)):- !, mpred_ain(X).
mpred_METACALL(_How, _Cut, retract(X)):- !, mpred_prolog_retract(X).
mpred_METACALL(_How, _Cut, retractall(X)):- !, mpred_prolog_retractall(X).
% TODO: test removal
%mpred_METACALL(How, Cut, prologHybrid(H)):-get_functor(H,F),!,isa_asserted(F,prologHybrid).
% mpred_METACALL(How, Cut, HB):-quietly_ex((full_transform_warn_if_changed(mpred_METACALL,HB,HHBB))),!,mpred_METACALL(How, Cut, HHBB).
%mpred_METACALL(How, Cut, argIsa(mpred_isa,2,mpred_isa/2)):-  trace_or_throw_ex(mpred_METACALL(How, Cut, argIsa(mpred_isa,2,mpred_isa/2))),!,fail.
% TODO: test removal
% mpred_METACALL(How, Cut, isa(H,B)):-!,isa_asserted(H,B).
mpred_METACALL(_How, _Cut, (H:-B)):- !, clause_u((H :- B)).
mpred_METACALL(_How, _Cut, M:(H:-B)):- !, clause_u((M:H :- B)).

% TODO: mpred_METACALL(_How, _Cut, M:HB):- current_prolog_flag(unsafe_speedups , true) ,!, call(M:HB).

%mpred_METACALL(_How, _SCut, P):- fail, predicate_property(P,built_in),!, call(P).
%mpred_METACALL(How, Cut, (H)):- is_static_pred(H),!,show_pred_info(H),dtrace(mpred_METACALL(How, Cut, (H))).
mpred_METACALL( How,   Cut, P) :- fail, predicate_property(P,number_of_clauses(_)),!,
     clause_u(P,Condition),
     mpred_METACALL(How,Cut,Condition),
       (var(Cut)->true;(Cut=cut(CutCall)->(!,CutCall);call_u_no_bc(Cut))).

% mpred_METACALL(_How,_SCut, P):- must_ex(current_predicate(_,M:P)),!, call_u(M:P).
%mpred_METACALL(How, Cut, H):- !, locally_tl(infAssertedOnly(H),call_u(H)).
mpred_METACALL(How, _SCut, P):- call(How,P).




%% action_is_undoable(+G)
%
% an action is action_is_undoable if there exists a method for undoing it.
%
action_is_undoable(G):- lookup_u(do_and_undo(G,_)).
action_is_undoable(G):- safe_functor(G,F,_),lookup_u(do_and_undo(F,Undo)),atom(Undo).



%% mpred_nf(+In,-Out)
%
% maps the LHR of a Pfc rule In to one normal form
%  Out.  It also does certain optimizations.  Backtracking into this
%  predicate will produce additional clauses.
%

/*
mpred_nf({LHS},List):- !,
  mpred_nf((nondet,{LHS}),List).
*/

mpred_nf(LHS,List):-
  mpred_nf1(LHS,List2),
  mpred_nf_negations(List2,List).


%%  mpred_nf1(+In,-Out)
%
% maps the LHS of a Pfc rule In to one normal form
%  Out.  Backtracking into this predicate will produce additional clauses.

% handle a variable.

mpred_nf1(P,[P]):- is_ftVarq(P), !.

% these next two rules are here for upward compatibility and will go
% away eventually when the P/Condition form is no longer used anywhere.

mpred_nf1(P/Cond,[(\+P)/Cond]):- mpred_negated_literal(P), !, 
  nop(dmsg_pretty(warn(mpred_nf1(P/Cond,[(\+P)/Cond])))).

mpred_nf1(P/Cond,[P/Cond]):- var(P),!.
mpred_nf1(P/Cond,[P/Cond]):- ((mpred_db_type(P,trigger(_));mpred_literal_nonvar(P))), !.

%  handle a negated form

mpred_nf1(NegTerm,NF):-
  quietly(mpred_unnegate(NegTerm,Term)),
  !,
  mpred_nf1_negation(Term,NF).

%  disjunction.

mpred_nf1((P;Q),NF):-
  !,
  (mpred_nf1(P,NF) ;   mpred_nf1(Q,NF)).


%  conjunction.

mpred_nf1((P,Q),NF):-
  !,
  mpred_nf1(P,NF1),
  mpred_nf1(Q,NF2),
  append(NF1,NF2,NF).

mpred_nf1([P|Q],NF):-
  !,
  mpred_nf1(P,NF1),
  mpred_nf1(Q,NF2),
  append(NF1,NF2,NF).


% prolog_clause mpred_nf1
mpred_nf1((H :- B)  , [(H :- B)]):-  
  mpred_positive_literal(H),!.

/*
% prolog_clause mpred_nf1 COMMENTED
mpred_nf1((H :- B)  ,[P]):-   
  mpred_positive_literal(H),
  P={clause(H , B)},
  dmsg_pretty(warn(mpred_nf1((H :- B)  ,[P]))),!.

% prolog_clause mpred_nf1 COMMENTED
mpred_nf1((H :- B)  ,[P]):-   
  mpred_positive_literal(H),
  P={clause(H , B)},
  dmsg_pretty(warn(mpred_nf1((H :- B)  ,[P]))),!.
*/

%  handle a random literal.

mpred_nf1(P,[P]) :- is_ftVarq(P), !.
mpred_nf1(P,[P]):-
  mpred_literal_nonvar(P),
  !.

mpred_nf1(Term,[Term]):- mpred_trace_msg("mpred_nf Accepting ~p",[Term]),!.


%=% shouldn''t we have something to catch the rest as errors?
mpred_nf1(Term,[Term]):-
  mpred_warn("mpred_nf doesn't know how to normalize ~p",[Term]),dtrace,!,fail.

notiffy_p(P,\+(P)):- var(P),!. % prevents next line from binding
notiffy_p(\+(P),P):- dmsg_pretty(notiffy_p(\+(P),P)), !.
notiffy_p(P,\+(P)).

%% mpred_nf1_negation(+P, ?NF) is semidet.
%
% is true if NF is the normal form of \+P.
%
mpred_nf1_negation(P,[\+P]):- is_ftVarq(P),!.
mpred_nf1_negation((P/Cond),[NOTP/Cond]):- notiffy_p(P,NOTP), !.

mpred_nf1_negation((P;Q),NF):-
  !,
  mpred_nf1_negation(P,NFp),
  mpred_nf1_negation(Q,NFq),
  append(NFp,NFq,NF).

mpred_nf1_negation((P,Q),NF):-
  % this code is not correct! twf.
  !,
  mpred_nf1_negation(P,NF)
  ;
  (mpred_nf1(P,Pnf),
   mpred_nf1_negation(Q,Qnf),
   append(Pnf,Qnf,NF)).

mpred_nf1_negation(P,[\+P]).


%%  mpred_nf_negations(List2,List) is det.
%
% sweeps through List2 to produce List,
%  changing -{...} to {\+...}
% % ? is this still needed? twf 3/16/90

%% mpred_nf_negations( :TermX, :TermX) is semidet.
%
% PFC Normal Form Negations.
%
mpred_nf_negations(X,X) :- !.  % I think not! twf 3/27/90

mpred_nf_negations([],[]).

mpred_nf_negations([H1|T1],[H2|T2]):-
  mpred_nf_negation(H1,H2),
  mpred_nf_negations(T1,T2).


%% mpred_nf_negation(+X, ?X) is semidet.
%
% PFC Normal Form Negation.
%
mpred_nf_negation(Form,{\+ X}):-
  nonvar(Form),
  Form=(-({X})),
  !.
mpred_nf_negation(X,X).



%%  build_rhs(+Sup,+Conjunction,-Rhs)
%

build_rhs(_Sup,X,[X]):-
  var(X),
  !.

build_rhs(Sup,(A,B),[A2|Rest]):-
  !,
  mpred_compile_rhs_term(Sup,A,A2),
  build_rhs(Sup,B,Rest).

build_rhs(Sup,X,[X2]):-
   mpred_compile_rhs_term(Sup,X,X2).


mpred_compile_rhs_term(_Sup,P,P):-is_ftVarq(P),!.

% TODO confirm this is not reversed (mostly confirmed this is correct now)
mpred_compile_rhs_term(Sup, \+ ( P / C), COMPILED) :- nonvar(C), !,
  mpred_compile_rhs_term(Sup, ( \+ P ) / C , COMPILED).

% dmiles added this to get PFC style lazy BCs
mpred_compile_rhs_term(Sup,(P/C),((P0 <- C0))) :- fail, !,mpred_compile_rhs_term(Sup,P,P0),
   build_code_test(Sup,C,C0),!.

mpred_compile_rhs_term(Sup,(P/C),((P0 :- C0))) :- !,mpred_compile_rhs_term(Sup,P,P0),
   build_code_test(Sup,C,C0),!.

mpred_compile_rhs_term(Sup,I,O):- quietly(mpred_compile_rhs_term_consquent(Sup,I,O)).





     %% mpred_unnegate(+N, ?P) is semidet.
     %
     %  is true if N is a negated term and P is the term
     %  with the negation operator stripped.  (not Logical ~ negation however)
     %
     mpred_unnegate(P,_):- notrace(is_ftVar(P)),!,fail.
     mpred_unnegate((\+(P)),P).
     mpred_unnegate((-P),P).
     mpred_unnegate((~P),P).



     %% mpred_negated_literal(+P) is semidet.
     %
     % PFC Negated Literal.
     %
     %mpred_negated_literal(P):- is_ftVarq(P),!,fail.
     mpred_negated_literal(P):-
       notrace((mpred_unnegate(P,Q),
       mpred_positive_literal(Q))).
     %mpred_negated_literal(~(_)).


     mpred_literal_or_var(X):- notrace((is_ftVar(X); mpred_negated_literal(X);mpred_positive_literal(X))).

     mpred_literal(X):- notrace((is_ftVar(X); mpred_negated_literal(X);mpred_positive_literal(X))).

     pfcAtom(P):- mpred_literal(P).

     mpred_literal_nonvar(X):- notrace((\+ is_ftVarq(X),!,(mpred_negated_literal(X);mpred_positive_literal(X)))).

     
     mpred_positive_literal(X):-
      notrace(( is_ftNonvar(X),
       X \= ~(_), % MAYBE COMMENT THIS OUT
       \+ mpred_db_type(X,rule(_RT)),
       get_functor(X,F,_),
       \+ mpred_neg_connective(F))),
       !.

     mpred_positive_fact(X):-  notrace(( mpred_positive_literal(X), X \= ~(_), 
        mpred_db_type(X,fact(_FT)), \+ mpred_db_type(X,trigger(_)))).

     mpred_is_trigger(X):-  notrace( mpred_db_type(X,trigger(_))).



     mpred_connective(Var):-var(Var),!,fail.
     mpred_connective(';').
     mpred_connective(',').
     mpred_connective('/').
     mpred_connective('{}').
     mpred_connective('|').
     mpred_connective(('==>')).
     mpred_connective(('<-')).
     mpred_connective('<==>').
     mpred_connective('-').
     % mpred_connective('~').
     mpred_connective(('\\+')).


     mpred_neg_connective('-').
     % mpred_neg_connective('~').
     mpred_neg_connective('\\+').

     is_simple_lhs(ActN):- is_ftVarq(ActN),!,fail.
     is_simple_lhs( \+ _ ):-!,fail.
     is_simple_lhs( ~ _ ):-!,fail.
     is_simple_lhs( _  / _ ):-!,fail.
     is_simple_lhs((Lhs1,Lhs2)):- !,is_simple_lhs(Lhs1),is_simple_lhs(Lhs2).
     is_simple_lhs((Lhs1;Lhs2)):- !,is_simple_lhs(Lhs1),is_simple_lhs(Lhs2).
     is_simple_lhs(ActN):- is_active_lhs(ActN),!,fail.
     is_simple_lhs((Lhs1/Lhs2)):- !,fail, is_simple_lhs(Lhs1),is_simple_lhs(Lhs2).
     is_simple_lhs(_).


     is_active_lhs(ActN):- var(ActN),!,fail.
     is_active_lhs(!).
     is_active_lhs(cut_c).
     is_active_lhs(actn(_Act)).
     is_active_lhs('{}'(_Act)).
     is_active_lhs((Lhs1/Lhs2)):- !,is_active_lhs(Lhs1);is_active_lhs(Lhs2).
     is_active_lhs((Lhs1,Lhs2)):- !,is_active_lhs(Lhs1);is_active_lhs(Lhs2).
     is_active_lhs((Lhs1;Lhs2)):- !,is_active_lhs(Lhs1);is_active_lhs(Lhs2).


     add_lhs_cond(Lhs1/Cond,Lhs2,Lhs1/(Cond,Lhs2)):-!.
     add_lhs_cond(Lhs1,Lhs2,Lhs1/Lhs2).


     %% constrain_meta(+Lhs, ?Guard) is semidet.
     %
     % Creates a somewhat sane Guard.
     %
     % To turn this feature off...
     % ?- set_prolog_flag(constrain_meta,false).  
     %
     %
     constrain_meta(_,_):- current_prolog_flag(constrain_meta,false),!,fail.
     % FACT
     constrain_meta(P,mpred_positive_fact(P)):- is_ftVarq(P),!.
     % NEG chaining
     constrain_meta(~ P, CP):- !,  constrain_meta(P,CP).
     constrain_meta(\+ P, CP):- !,  constrain_meta(P,CP).
     % FWD chaining
     constrain_meta((_==>Q),nonvar(Q)):- !, is_ftVarq(Q).
     % EQV chaining
     constrain_meta((P<==>Q),(nonvar(Q);nonvar(P))):- (is_ftVarq(Q);is_ftVarq(P)),!.
     % BWD chaining
     constrain_meta((Q <- _),mpred_literal_nonvar(Q)):- is_ftVarq(Q),!.
     constrain_meta((Q <- _),CQ):- !, constrain_meta(Q,CQ).
     % CWC chaining
     constrain_meta((Q :- _),mpred_literal_nonvar(Q)):- is_ftVarq(Q),!.
     constrain_meta((Q :- _),CQ):- !, constrain_meta(Q,CQ).




%% process_rule(+Lhs, ?Rhs, ?Parent_rule) is semidet.
%
% Process Rule.
%

/*

Next Line converts:
((prologHybrid(F),arity(F,A))==>{kb_shared(F/A)}).

To:
arity(F,A)/prologHybrid(F)==>{kb_shared(F/A)}.
prologHybrid(F)/arity(F,A)==>{kb_shared(F/A)}.

In order to reduce the number of postivie triggers (pt/2s)
*/

process_rule(LhsIn,Rhs,Parent_rule):- constrain_meta(LhsIn,How),!,
  process_rule0(LhsIn/How,Rhs,Parent_rule).

process_rule(LhsIn,Rhs,Parent_rule):- is_simple_lhs(LhsIn),LhsIn = (Lhs1,Lhs2),
  Lhs2\=(_,_),
  add_lhs_cond(Lhs1,Lhs2,LhsA),
  add_lhs_cond(Lhs2,Lhs1,LhsB),
  process_rule0(LhsA,Rhs,Parent_rule),
  process_rule0(LhsB,Rhs,Parent_rule).
process_rule(Lhs,Rhs,Parent_rule):-process_rule0(Lhs,Rhs,Parent_rule).

process_rule0(Lhs,Rhs,Parent_rule):-
  must_notrace_pfc(get_source_mfl(U)),!,
  copy_term(Parent_rule,Parent_ruleCopy),
  build_rhs(U,Rhs,Rhs2),
  foreach(mpred_nf(Lhs,Lhs2),
    ignore(build_rule(Lhs2,rhs(Rhs2),(Parent_ruleCopy,U)))).


%% build_rule(+Lhs, ?Rhs, ?Support) is semidet.
%
% Build Rule.
%
build_rule(Lhs,Rhs,Support):-
  copy_term_vn(Support,WS),
  mpred_mark_as_confirmed(WS,Lhs,pfcLHS),
  quietly_must_ex(build_trigger(WS,Lhs,Rhs,Trigger)),
  cyclic_break((Lhs,Rhs,WS,Trigger)),
  doall(mpred_eval_lhs_no_nc(Trigger,Support)).

build_trigger(WS,[],Consequent,ConsequentO):-
   mpred_compile_rhs_term_consquent(WS,Consequent,ConsequentO).

build_trigger(WS,[V|Triggers],Consequent,pt(V,X)):-
  is_ftVarq(V),
  !,
  build_trigger(WS,Triggers,Consequent,X).

% T1 is a negation in the next two clauses
build_trigger(WS,[TT|Triggers],Consequent,nt(T2,Test2,X)):- 
  compound(TT),
  TT=(T1/Test),
  mpred_unnegate(T1,T2),
  !,
  build_neg_test(WS,T2,Test,Test2),
  build_trigger(WS,Triggers,Consequent,X).

build_trigger(WS,[(T1)|Triggers],Consequent,nt(T2,Test,X)):-
  mpred_unnegate(T1,T2),
  !,
  build_neg_test(WS,T2,true,Test),
  build_trigger(WS,Triggers,Consequent,X).

build_trigger(WS,[{Test}|Triggers],Consequent,(Test*->Body)):- % Noncutted ->
  !,
  build_trigger(WS,Triggers,Consequent,Body).

build_trigger(WS,[T/Test|Triggers],Consequent,pt(T,X)):-
  !,
  build_code_test(WS, Test,Test2),
  build_trigger(WS,[{Test2}|Triggers],Consequent,X).


%build_trigger(WS,[snip|Triggers],Consequent,snip(X)):-
%  !,
%  build_trigger(WS,Triggers,Consequent,X).


build_trigger(WS,[T|Triggers],Consequent,Reslt):- 
  constrain_meta(T,Test)->
  build_trigger(WS,[T/Test|Triggers],Consequent,Reslt),!.

build_trigger(WS,[T|Triggers],Consequent,pt(T,X)):-
  !,
  build_trigger(WS,Triggers,Consequent,X).


%%  build_neg_test(+WhyBuild,+Test,+Testin,-Testout).
%
%  builds the test used in a negative trigger (nt/3).  This test is a
%  conjunction of the check than no matching facts are in the db and any
%  additional test specified in the rule attached to this - term.
%

build_neg_test(WS,T,Testin,Testout):-
  build_code_test(WS,Testin,Testmid),
  mpred_conjoin((call_u_no_bc(T)),Testmid,Testout).

%% check_never_assert(+Pred) is semidet.
%
% Check Never Assert.
%

%check_never_assert(_Pred):-!.
%:-dumpST.
check_never_assert(MPred):- strip_module(MPred,M,_Pred),
  quietly_ex(ignore((check_db_sanity(never_assert_u,M,MPred)))).

check_db_sanity(Checker,CModule,Pred):- 
 (current_predicate(CModule:Checker/2)->Module=CModule;Module=baseKB),!,
 copy_term_and_varnames(Pred,Pred_2),
 CheckerCall  univ_safe [ Checker,Pred_2,_Why],
 call_u_no_bc(Module:CheckerCall),
 sanity(variant_u(Pred,Pred_2)),
 trace_or_throw_ex(Module:CheckerCall).

%check_never_assert(Pred):- quietly_ex(ignore(( copy_term_and_varnames(Pred,Pred_2),call_u_no_bc(never_assert_u(Pred_2)),variant_u(Pred,Pred_2),trace_or_throw_ex(never_assert_u(Pred))))).
%check_never_assert(Pred):- quietly_ex((( copy_term_and_varnames(Pred,Pred_2),call_u_no_bc(never_assert_u(Pred_2,Why)), variant_u(Pred,Pred_2),trace_or_throw_ex(never_assert_u(Pred,Why))))),fail.

%% check_never_retract(+Pred) is semidet.
%
% Check Never Retract.
%

%check_never_retract(_Pred):-!.
check_never_retract(MPred):- strip_module(MPred,M,_Pred),
  quietly_ex(ignore((check_db_sanity(never_retract_u,M,MPred)))).


%% pos_2_neg(+P, ?P) is semidet.
%
% pos  Extended Helper Negated.
%
pos_2_neg(p,n):-!.
pos_2_neg(n,p):-!.
pos_2_neg(P,~(P)):- (var(P); P \= '~'(_)),!.
% pos_2_neg(P,~(P)).


mpred_mark_as_confirmed(Sup, A, Type):- retractall(t_l:busy(_)),  quietly_must_ex(mpred_mark_as(Sup, A, Type)), mpred_run.

%% mpred_mark_as(+VALUE1,  :TermP, ?VALUE4) is semidet.
%
% PFC Mark Converted To.
%
mpred_mark_as(_,P,_):- notrace(is_ftVar(P) ; P == [] ),!.
mpred_mark_as(Sup,M:P,Type):- atom(M),clause_bq(mtHybrid(M)),!,M:mpred_mark_as(Sup,P,Type).
mpred_mark_as(Sup,_:P,Type):- !, mpred_mark_as(Sup,P,Type).
mpred_mark_as(Sup,\+(P),Type):- !,mpred_mark_as(Sup,P,Type).
mpred_mark_as(Sup,~(P),Type):- !,mpred_mark_as(Sup,P,Type).
mpred_mark_as(Sup,-(P),Type):- !,mpred_mark_as(Sup,P,Type).
mpred_mark_as(Sup,not(P),Type):- !,mpred_mark_as(Sup,P,Type).
mpred_mark_as(Sup,( P / CC ),Type):- !, mpred_mark_as(Sup,P,Type),mpred_mark_as(Sup,( CC ),pfcCallCode).
mpred_mark_as(Sup,( P :- _CC), Type):- !, mpred_mark_as(Sup,P, Type) /* , mpred_mark_as(Sup, ( CC ), pfcCallCode) */ .
mpred_mark_as(Sup,'{}'(  CC ), _Type):- mpred_mark_as(Sup,( CC ),pfcCallCode).
mpred_mark_as(Sup,[ A | B], Type):- !, mpred_mark_as(Sup,A, Type),mpred_mark_as(Sup,B, Type).
mpred_mark_as(Sup,( A , B), Type):- !, mpred_mark_as(Sup,A, Type),mpred_mark_as(Sup,B, Type).
mpred_mark_as(Sup,( A ; B), Type):- !, mpred_mark_as(Sup,A, Type),mpred_mark_as(Sup,B, Type).
mpred_mark_as(Sup,( A ==> B), Type):- !, mpred_mark_as(Sup,A, Type),mpred_mark_as(Sup,B, pfcRHS).
mpred_mark_as(Sup,( B <- A), Type):- !, mpred_mark_as(Sup,A, Type),mpred_mark_as(Sup,B, pfcRHS).
mpred_mark_as(Sup,P,Type):-get_functor(P,F,A),ignore(mpred_mark_fa_as(Sup,P,F,A,Type)),!.


%% mpred_mark_fa_as(+Sup, ?P, ?F, ?A, ?Type) is semidet.
%
% PFC Mark Functor-arity Converted To.
%

% mpred_mark_fa_as(_Sup,_P,'\=',2,_):- dtrace.
% BREAKS SIMPLE CASES
% mpred_mark_fa_as(_Sup,_P,_,_,Type):- Type \== pfcLHS, Type \== pfcRHS, current_prolog_flag(unsafe_speedups , true) ,!.
mpred_mark_fa_as(_Sup,_P,isa,_,_):- !.
mpred_mark_fa_as(_Sup,_P,t,_,_):- !.
mpred_mark_fa_as(_Sup,_P,argIsa,N,_):- !,must_ex(N=3).
mpred_mark_fa_as(_Sup,_P,arity,N,_):- !,must_ex(N=2).
%mpred_mark_fa_as(_Sup,_P,mpred_isa,N,_):- must_ex(N=2).
mpred_mark_fa_as(_Sup,_P,'[|]',N,_):- dtrace,must_ex(N=2).

%mpred_mark_fa_as(_Sup,_P,_,_,pfcCallCode):- !.
%mpred_mark_fa_as(_Sup,_P,mpred_prop,N,_):- !,must_ex(N=4).
%mpred_mark_fa_as(_Sup,_P,_:mpred_prop,N,_):- must_ex(N=4).

mpred_mark_fa_as(Sup, _P,F,A,Type):- really_mpred_mark(Sup,Type,F,A),!.

% i hope i am not exagerating but anniepoo used to enter this yearly contest for whom could build graphical assets the most pretty and complex the quickest in secondlife.. (now it makes sense she used a 3d mouse)  she won so much, they and she had to ban herself becasue she always won hands down.. so with this agility to create the physical aspects of a wolrd veery easily .. we realized we could make a fun leanring inpiring world for AIs .. however 

really_mpred_mark(_  ,Type,F,A):- call_u_no_bc(mpred_prop(_M,F,A,Type)),!.
really_mpred_mark(Sup,Type,F,A):-
  current_assertion_module(M),
  MARK = mpred_prop(M,F,A,Type),
  check_never_assert(MARK),
  why_marked(M,Sup,WM),
  with_fc_mode(direct,mpred_post1(MARK,(WM,ax))),
  % with_no_mpred_trace_exec(with_fc_mode(direct,mpred_post1(MARK,(WM,ax)))),
  !.
  %with_no_mpred_trace_exec(with_fc_mode(direct,mpred_post1(MARK,(why_marked(Sup),ax)))).
  % with_no_mpred_trace_exec(with_fc_mode(direct,mpred_fwc1(MARK,(why_marked(Sup),ax)))),!.

why_marked(M,_Sup,mfl4(VarNameZ,M,F,L)):- source_location(F,L),!,varnames_load_context(VarNameZ).
why_marked(_,Sup,Sup).

%% fa_to_p(+F, ?A, ?P) is semidet.
%
% Functor-arity Converted To Pred.
%
fa_to_p(F,A,P):-is_ftNameArity(F,A),safe_functor(P,F,A),
  ( P \= call_u_no_bc(_) ),( P \= '$VAR'(_)).


%% build_code_test(+WS, ?Test, ?TestO) is semidet.
%
% Build Code Test.
%
% what this does...
%
%   strips away any currly brackets
%   converts cuts to cut_c/0
%   converts variable Ps to call_u_no_bc(P)
%
build_code_test(_Support,Test,TestO):- is_ftVarq(Test),!,must_ex(TestO=call_u_no_bc(Test)).
build_code_test(WS,{Test},TestO) :- !,build_code_test(WS,Test,TestO).
build_code_test(_Sup,!,cut_c):-!.
build_code_test(WS,rhs(Test),rhs(TestO)) :- !,build_code_test(WS,Test,TestO).
build_code_test(WS,Test,TestO):- is_list(Test),must_maplist(build_code_test(WS),Test,TestO).
build_code_test(_WS,(H:-B),clause_asserted_u(H,B)):- !.
build_code_test(_WS,M:(H:-B),clause_asserted_u(M:H,B)):- !.
build_code_test(WS,Test,TestO):- code_sentence_op(Test),Test univ_safe [F|TestL],must_maplist(build_code_test(WS),TestL,TestLO),TestO univ_safe [F|TestLO],!.
build_code_test(WS,Test,Test):- must_ex(mpred_mark_as(WS,Test,pfcCallCode)),!.
build_code_test(_,Test,Test).


%% mpred_compile_rhs_term_consquent(+Support, +TestIn, -TestOut) is semidet.
%
% Build Consequent.
%
mpred_compile_rhs_term_consquent(_      ,Test,Test):- is_ftVarq(Test),!.
mpred_compile_rhs_term_consquent(_      ,Test,TestO):-is_ftVarq(Test),!,TestO=added(Test).
mpred_compile_rhs_term_consquent(_Sup,!,{cut_c}):-!.
mpred_compile_rhs_term_consquent(WS,'{}'(Test),'{}'(TestO)) :- !,build_code_test(WS,Test,TestO).
mpred_compile_rhs_term_consquent(WS,rhs(Test),rhs(TestO)) :- !,mpred_compile_rhs_term_consquent(WS,Test,TestO).
mpred_compile_rhs_term_consquent(WS,Test,TestO):- is_list(Test),must_maplist(mpred_compile_rhs_term_consquent(WS),Test,TestO).

mpred_compile_rhs_term_consquent(_WS,(H:-B),(H:-B)):-!.

mpred_compile_rhs_term_consquent(WS,Test,TestO):-
   code_sentence_op(Test),Test univ_safe [F|TestL],
   must_maplist(mpred_compile_rhs_term_consquent(WS),TestL,TestLO),
   TestO univ_safe [F|TestLO],!.

mpred_compile_rhs_term_consquent(Sup,I,O):-
  % TODO replace the next line with  I=O,
    full_transform_warn_if_changed(compile_rhs,I,O),
    mpred_mark_as_confirmed(Sup,O,pfcRHS),!.



%% code_sentence_op( :TermVar) is semidet.
%
% Code Sentence Oper..
%
code_sentence_op(Var):-is_ftVarq(Var),!,fail.
code_sentence_op(rhs(_)).
code_sentence_op(~(_)).
code_sentence_op(-(_)).
code_sentence_op(-(_)).
code_sentence_op((_,_)).
code_sentence_op((_;_)).
code_sentence_op(\+(_)).
code_sentence_op(call(_)).
code_sentence_op(call_u(_)).
code_sentence_op(call_u_no_bc(_,_)).
code_sentence_op(Test:-_):-!,code_sentence_op(Test).
code_sentence_op(Test):-
  predicate_property(Test,built_in),
  predicate_property(Test,meta_predicate(PP)), \+ (( arg(_,PP,N), N \= 0)).


%% all_closed(+C) is semidet.
%
% All Closed.
%
all_closed(C):- \+is_ftCompound(C)->true;(safe_functor(C,_,A),A>1,\+((arg(_,C,Arg),is_ftVarq(Arg)))),!.


%head_to_functor_name(I,F):- is_ftCompound(I),get_head(I,H),is_ftCompound(I),get_functor_name(I,F).
head_to_functor_name(I,F):- is_ftCompound(I),get_functor(I,F).


%% mpred_db_type(+VALUE1, ?Type) is semidet.
%
% PFC Database Type.
%
%  simple typeing for Pfc objects
%
mpred_db_type(Var,Type):- var(Var),!, Type=fact(_FT).
mpred_db_type(_:X,Type):- !, mpred_db_type(X,Type).
mpred_db_type(~_,Type):- !, Type=fact(_FT).
mpred_db_type(('==>'(_,_)),Type):- !, Type=rule(fwd).
mpred_db_type(('<==>'(_,_)),Type):- !, Type=rule(<==>).
mpred_db_type(('<-'(_,_)),Type):- !, Type=rule(bwc).
mpred_db_type((':-'(_,_)),Type):- !, Type=rule(cwc).
mpred_db_type(pt(_,_,_),Type):- !, Type=trigger(pt).
mpred_db_type(pt(_,_),Type):- !, Type=trigger(pt).
mpred_db_type(nt(_,_,_),Type):- !,  Type=trigger(nt).
mpred_db_type(bt(_,_),Type):- !,  Type=trigger(bt).
mpred_db_type(actn(_),Type):- !, Type=action.
mpred_db_type((('::::'(_,X))),Type):- !, mpred_db_type(X,Type).
mpred_db_type(_,fact(_FT)):-
  %  if it''s not one of the above, it must_ex be a fact!
  !.

mpred_assert_w_support(P,Support):-
  (clause_asserted_u(P) ; assert_u_confirmed_was_missing(P)),
  !,
  mpred_add_support(P,Support).

mpred_asserta_w_support(P,Support):-
  (clause_asserted_u(P) ; asserta_u(P)),
  !,
  mpred_add_support(P,Support).

mpred_assertz_w_support(P,Support):-
  (clause_asserted_u(P) ; assertz_u(P)),
  !,
  mpred_add_support(P,Support).



%% clause_asserted_u(+Head) is semidet.
%
% PFC Clause For User Interface.
%

:- module_transparent(clause_asserted_call/2).
clause_asserted_call(H,B):-clause_asserted(H,B).

clause_asserted_u(P):- call_u(clause_asserted(P)),!.
  

/*
clause_asserted_u0(P):-clause_asserted(P),!,sanity(clause_asserted_u1(P)),!.
clause_asserted_u0(P):- sanity( \+ clause_asserted_u1(P)),fail.
clause_asserted_u1(M:(H:-B)):- nonvar(M),!, clause_asserted_u0(M,H,B). 
clause_asserted_u1((M:H):-B):- nonvar(M),!, clause_asserted_u0(M,H,B). 
clause_asserted_u1(MH):- strip_module(MH,M,H),clause_asserted_u0(M,H,true),!. 

clause_asserted_u0(M,H,_):- sanity((nonvar(H), ignore(show_failure(\+ is_static_predicate(M:H))))),fail.
% clause_asserted_u0(MH,_):- \+ ground(MH),must_notrace_pfc(full_transform(change(assert,assert_u),MH,MA)),MA\=@=MH,!,clause_asserted_u(MA).
% clause_asserted_u0(M,H,B):- current_prolog_flag(unsafe_speedups, true), !,clause_asserted_ii(M,H,B).
clause_asserted_u0(M,H,B):- must_ex(quietly_ex(fix_mp(clause(clause,clause_asserted_u),M:H,M,H))),clause_asserted_ii(M,H,B).
*/
clause_asserted_ii(M,H,B):- system:clause(M:H,B,Ref),system:clause(_:HH,BB,Ref),H=@=HH,B=@=BB,!.

variant_m(_:H,_:HH):-!,H=@=HH.
variant_m(H,_:HH):-!,H=@=HH.
variant_m(_:H,HH):-!,H=@=HH.
variant_m(H,HH):-!,H=@=HH.

variant_u(HeadC,Head_copy):-variant_i(HeadC,Head_copy).

/*
%% foreach(+Binder, ?Body) is det.
%
% Foreachl Do.
%
foreach(Binder,Body):- Binder,pfcl_do(Body),fail.
foreach(_,_).


%% pfcl_do(+X) is semidet.
%
% executes P once and always succeeds.
%
pfcl_do(X):- X,!.
pfcl_do(_).
*/

%% mpred_union(L1,L2,L3) is semidet.
%
%  true if set L3 is the result of appending sets
%  L1 and L2 where sets are represented as simple lists.
%
mpred_union([],L,L).
mpred_union([Head|Tail],L,Tail2):-
  memberchk(Head,L),
  !,
  mpred_union(Tail,L,Tail2).
mpred_union([Head|Tail],L,[Head|Tail2]):-
  mpred_union(Tail,L,Tail2).


%  mpred_conjoin(+Conjunct1,+Conjunct2,?Conjunction).
%  arg3 is a simplified expression representing the conjunction of
%  args 1 and 2.

mpred_conjoin(True,X,X):- True==true, !.
mpred_conjoin(X,True,X):- True==true, !.
mpred_conjoin(C1,C2,(C1,C2)).


%   File   : pfcdb.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Author :  Dan Corpron
%   Updated: 10/11/87, ...
%   Purpose: predicates to manipulate a Pfc database (e.ax. save,
% 	restore, reset, etc.)

%% mpred_reset_kb() is det.
%
% removes all forward chaining rules, facts and justifications from each db.
%
mpred_reset:- 
  mpred_reset_kb,
  forall((clause_b(mtHybrid(Module)),Module\==baseKB),
       mpred_reset_kb(Module)).

%% mpred_reset_kb() is det.
%% mpred_reset_kb(+Module) is det.
%
% removes all forward chaining rules, facts and justifications from db.
%
mpred_reset_kb:- defaultAssertMt(Module),
  (Module\==baseKB->mpred_reset_kb(Module);true).

mpred_reset_kb_facts(Module):- nop(Module).

mfl_module(mfl4(_VarNameZ,M,_,_),Module):- Module==M,!.
mfl_module(mfl4(_VarNameZ,_,F,_),Module):- atom(F),
   module_property(M,file(F)),  
   \+ ((module_property(M2,file(F)),M\==M2)),
   Module==M.

mpred_reset_kb(Module):-
  with_exact_kb(Module,mpred_reset_kb_0(Module)).

mpred_reset_kb_0(Module):- mpred_reset_kb_facts(Module),fail.
mpred_reset_kb_0(Module):- 
  only_is_user_reason((ZF,ZTrigger)),
  clause(Module:spft(P,ZF,ZTrigger),_,Ref),
  nonvar(P),
  once(clause_property(Ref,module(Module)); mfl_module(ZF,Module)),
  must_ex(mpred_reset_mp(Module,P)), 
  ( \+ clause(Module:spft(P,ZF,ZTrigger),_,Ref) -> true;
     (must_ex((clause(_SPFT,_SB,Ref),erase(Ref))))),  %     must_ex((mpred_retract_i_or_warn_1(P);(fail,mpred_retract_i_or_warn(SPFT)))),
  fail.
mpred_reset_kb_0(Module):- 
  clause(Module:spft(P,ZF,ZTrigger),_,Ref),
  nonvar(P),
  once(clause_property(Ref,module(Module)); mfl_module(ZF,Module)),
  must_ex(mpred_reset_mp(Module,P)), 
  ( \+ clause(Module:spft(P,ZF,ZTrigger),_,Ref) -> true;
     (must_ex((clause(_SPFT,_SB,Ref),erase(Ref))))),  %     must_ex((mpred_retract_i_or_warn_1(P);(fail,mpred_retract_i_or_warn(SPFT)))),
  fail.

mpred_reset_kb_0(Module):- mpred_reseted_kb_check(Module),!.


mpred_reseted_kb_check(Module):- with_exact_kb(Module,mpred_reseted_kb_check_0(Module)).

mpred_reseted_kb_check_0(Module):- \+ mpred_database_item(Module,_),!,mpred_trace_msg("Reset DB complete for ~p",[Module]).
mpred_reseted_kb_check_0(Module):- mpred_trace_msg("Couldn't full mpred_reseted_kb_check(~w).~n",[Module]),
  pp_DB,mpred_database_item(Module,T),
  wdmsg_pretty(mpred_database_item(Module,T)),!.
  %mpred_warn("Pfc database [~w] not empty: ~p.~n",[Module,T]),!,
  %mpred_error("Pfc database [~w] not empty: ~p.~n",[Module,T]),!.
  
mpred_reset_mp(Module,P):- P \= ( _:-_ ), mpred_retract(Module:P),!.
mpred_reset_mp(Module,P):-
     doall((
     expand_to_hb(P,H,B),
     clause_asserted(Module:H,B,PRef1),
     clause_property(PRef1,module(Module)),
     % show_failure((((lookup_u(Module:P,PRef2),PRef2==PRef1)))),
  (must_ex(mpred_retract_i(Module:P))->true;mpred_warn("Couldn't retract ~p: ~p.~n",[Module,P])),
  sanity(\+ clause_asserted(_H0,_B0,PRef1)))).


% true if there is some Pfc crud still in the database.
mpred_database_item(Module,P):- 
   current_module(Module),
  mpred_database_term(F,A,Type),
  Type\=debug,Type\=setting,
  safe_functor(H,F,A),
  % H \= ~(_),  
  P = (H:-B),
  Module:clause(H,B,Ref),
  clause_property(Ref,module(Module)),
  \+ reserved_body_helper(B).


mpred_retract_i_or_warn(X):- ignore(show_failure((mpred_retract_i_or_warn_1(X) *-> true; mpred_retract_i_or_warn_2(X)))).

mpred_retract_i_or_warn_1(X):- sanity(is_ftNonvar(X)), 
  ((((X=spft(_,_,_), call_u(X), retract_u(X))) *-> true ; retract_u(X))),
  nop((mpred_trace_msg('~NSUCCESS: ~p~n',[retract_u(X)]))).

% mpred_retract_i_or_warn_2(SPFT):- \+ \+ SPFT = spft(_,a,a),!,fail.
% mpred_retract_i_or_warn_2(X):- fail,mpred_warn("Couldn't retract_u ~p.~n",[X]),(debugging_logicmoo(logicmoo(pfc))->rtrace(retract_u(X));true),!.
mpred_retract_i_or_warn_2(X):- mpred_trace_msg("Couldn't retract_i: ~p.~n",[X]),fail.
%mpred_retract_i_or_warn_2(X):- mpred_warn("Couldn't retract_i: ~p.~n",[X]),!.




%   File   : pfcdebug.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: provides predicates for examining the database and debugginh
%   for Pfc.

%:- mpred_set_default(baseKB:mpred_warnings(_), baseKB:mpred_warnings(true)).
%  tms is one of {none,local,cycles} and controles the tms alg.
% :- during_boot(mpred_set_default(mpred_warnings(_), mpred_warnings(true))).

%  mpred_fact(P) is true if fact P was asserted into the database via add.

mpred_fact_mp(M,G):- current_predicate(_,M:G),\+ predicate_property(M:G,imported_from(_)),
  mpred_fact(G),ignore((lookup_u(G,Ref),clause_property(Ref,module(MW)))),MW=M.

mpred_fact(P):- mpred_fact(P,true).

%  mpred_fact(P,C) is true if fact P was asserted into the database via
%  add and contdition C is satisfied.  For example, we might do:
%
%   mpred_fact(X,mpred_userFact(X))
%

mpred_fact(P,C):- mpred_fact0(P,C).
mpred_fact(P,C):- compound(P),safe_functor(P,F,2),clause_bq(rtSymmetricBinaryPredicate(F)),args_swapped(P,Q),mpred_fact0(Q,C).
mpred_fact(~P,C):- compound(P),safe_functor(P,F,2),clause_bq(rtSymmetricBinaryPredicate(F)),args_swapped(P,Q),mpred_fact0(~Q,C).
mpred_fact0(P,C):-
  mpred_get_support(P,_),
  mpred_db_type(P,fact(_FT)),
  call_u_no_bc(C).

%  mpred_facts_in_kb(MM,-ListofPmpred_facts) returns a list of facts added.

mpred_facts(L):- mpred_facts_in_kb(_,L).
mpred_facts_in_kb(MM,L):- mpred_facts_in_kb(MM,_,true,L).

mpred_facts(P,L):- mpred_facts_in_kb(_,P,L).
mpred_facts(KB,P,L):- mpred_facts_in_kb(KB,P,L).
mpred_facts_in_kb(MM,P,L):- mpred_facts_in_kb(MM,P,true,L).

%  mpred_facts_in_kb(MM,Pattern,Condition,-ListofPmpred_facts) returns a list of facts added.

%% mpred_facts_in_kb(MM,+P, ?C, ?L) is semidet.
%
% PFC Facts.
%
mpred_facts_in_kb(MM,P,C,L):- with_exact_kb(MM,setof(P,mpred_fact(P,C),L)).


%% brake(+X) is semidet.
%
% Brake.
%
brake(X):-  X, break.


%
%  predicates providing a simple tracing facility
%

% this is here for upward compat. - should go away eventually.
mpred_trace_op(Add,P):- 
  not_not_ignore_quietly_ex((get_why_uu(Why), !, mpred_trace_op(Add,P,Why))).


mpred_trace_op(Add,P,S):-
   not_not_ignore_quietly_ex((mpred_trace_maybe_print(Add,P,S),
      mpred_trace_maybe_break(Add,P,S))).


mpred_trace_maybe_print(Add,P,S):-
  not_not_ignore_quietly_ex((
  \+ get_mpred_is_tracing(P) -> true;
  (
   ((to_u(S,U),atom(U))
       -> wdmsg_pretty("~NOP: ~p (~p) ~p",[Add,U,P])
        ; wdmsg_pretty("~NOP: ~p (:) ~p~N\tSupported By: ~q",[Add,P,S]))))),!.

to_u(S,U):-S=(U,ax),!.
to_u(S,U):-S=(U,_),!.
to_u(S,U):-S=(U),!.


mpred_trace_maybe_break(Add,P0,_ZS):-
  get_head_term(P0,P),
   (
  \+ call(lmcache:mpred_is_spying_pred(P,Add)) -> true;
   (wdmsg_pretty("~NBreaking on ~p(~p)",[Add,P]),
    break)).

:- dynamic(lmcache:mpred_is_spying_pred/2).

pfc_hide(P):-call(P).

mpred_trace:- mpred_trace(_).

mpred_trace(Form0):-  get_head_term(Form0,Form),
  assert_u_no_dep(lmcache:mpred_is_spying_pred(Form,print)).

%% get_mpred_is_tracing(:PRED) is semidet.
%
% PFC If Is A Tracing.
%
get_mpred_is_tracing(_):-!,fail.
get_mpred_is_tracing(Form0):- get_head_term(Form0,Form), t_l:hide_mpred_trace_exec,!,
  \+ \+ ((quietly_ex(call(lmcache:mpred_is_spying_pred(Form,print))))).
get_mpred_is_tracing(Form0):- get_head_term(Form0,Form),
  once(t_l:mpred_debug_local ; tracing ; clause_asserted_u(mpred_is_tracing_exec) ;
     call(lmcache:mpred_is_spying_pred(Form,print))).


%% mpred_trace(+Form, ?Condition) is semidet.
%
% PFC Trace.
%
mpred_trace(Form0,Condition):- get_head_term(Form0,Form),
  assert_u_no_dep((lmcache:mpred_is_spying_pred(Form,print):- Condition)).

mpred_spy(Form):- mpred_spy(Form,[add,rem],true).

mpred_spy(Form,Modes):- mpred_spy(Form,Modes,true).

mpred_spy(Form0,List,Condition):- is_list(List),!,get_head_term(Form0,Form),
  !,must_maplist(mpred_spy1(Condition,Form),List).

mpred_spy(Form0,Mode,Condition):- get_head_term(Form0,Form),
  mpred_spy1(Condition,Form,Mode).
 
mpred_spy1(Condition,Form0,Mode):- get_head_term(Form0,Form),
  assert_u_no_dep((lmcache:mpred_is_spying_pred(Form,Mode):- Condition)).

mpred_nospy:- mpred_nospy(_,_,_).

mpred_nospy(Form):- mpred_nospy(Form,_,_).

mpred_nospy(Form0,Mode,Condition):- get_head_term(Form0,Form),
  clause(lmcache:mpred_is_spying_pred(Form,Mode), Condition, Ref),
  erase(Ref),
  fail.
mpred_nospy(_,_,_).

mpred_notrace:- mpred_untrace.
mpred_untrace:- mpred_untrace(_).
mpred_untrace(Form0):- get_head_term(Form0,Form), retractall(lmcache:mpred_is_spying_pred(Form,print)).


% not_not_ignore_quietly_ex(G):- ignore(quietly(\+ \+ G)).
% not_not_ignore_quietly_ex(G):- ignore( \+ (G)).
not_not_ignore_quietly_ex(G):- notrace(ignore(quietly_ex(\+ \+ G))).

% needed:  mpred_trace_rule(Name)  ...

log_failure(ALL):- quietly_ex((log_failure_red,maybe_mpred_break(ALL),log_failure_red)).

log_failure_red:- quietly(doall((
  show_current_source_location,
  between(1,3,_),
  ansifmt(red,"%%%%%%%%%%%%%%%%%%%%%%%%%%% find log_failure_red in srcs %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"),
  ansifmt(yellow,"%%%%%%%%%%%%%%%%%%%%%%%%%%% find log_failure_red in srcs %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")))).

%% with_no_breaks(+P) is semidet.
%
% Dont break even if PFC Test fails
%
:- thread_local(t_l:no_breaks/0).
with_no_breaks(G):- locally_tl(no_breaks,G). 

break_ex:- quietly((log_failure_red,dumpST,log_failure_red)),
  (t_l:no_breaks -> ansifmt(red,"NO__________________DUMP_BREAK/0") ;dbreak).

maybe_mpred_break(Info):- (t_l:no_breaks->true;(debugging(logicmoo(pfc))->dtrace(dmsg_pretty(Info));(dmsg_pretty(Info)))),break_ex.
%maybe_mpred_break(Info):- (t_l:no_breaks->true;(debugging(logicmoo(pfc))->dtrace(dmsg_pretty(Info));(dmsg_pretty(Info)))),break_ex.

% if the correct flag is set, dtrace exection of Pfc
mpred_trace_msg(_):- current_prolog_flag(mpred_pfc_silent,true).
mpred_trace_msg(Info):- not_not_ignore_quietly_ex(((((clause_asserted_u(mpred_is_tracing_exec);tracing)->(show_wdmsg(Info));true)))).
mpred_trace_msg(Format,Args):- not_not_ignore_quietly_ex((((clause_asserted_u(mpred_is_tracing_exec);tracing)-> (show_wdmsg(Format,Args))))),!.
% mpred_trace_msg(Format,Args):- not_not_ignore_quietly_ex((((format_to_message(Format,Args,Info),mpred_trace_msg(Info))))).

show_wdmsg(A,B):- current_prolog_flag(mpred_pfc_silent,true)-> true; wdmsg_pretty(A,B).
show_wdmsg(A):- current_prolog_flag(mpred_pfc_silent,true)-> true; wdmsg_pretty(A).

mpred_warn(Info):- not_not_ignore_quietly_ex((((color_line(red,1), lookup_u(mpred_warnings(true));tracing) ->
  wdmsg_pretty(warn(logicmoo(pfc),Info)) ; mpred_trace_msg('WARNING/PFC:  ~p ',[Info])),
  nop(maybe_mpred_break(Info)))).

mpred_warn(Format,Args):- not_not_ignore_quietly_ex((((format_to_message(Format,Args,Info),mpred_warn(Info))))).

mpred_error(Info):- not_not_ignore_quietly_ex(((tracing -> wdmsg_pretty(error(logicmoo(pfc),Info)) ; mpred_warn(error(Info))))).
mpred_error(Format,Args):- not_not_ignore_quietly_ex((((format_to_message(Format,Args,Info),mpred_error(Info))))).

mpred_pfc_silent(TF):-set_prolog_flag(mpred_pfc_silent,TF).


mpred_watch:- mpred_trace_exec,mpred_pfc_silent(false).
mpred_nowatch:-  mpred_notrace_exec.

mpred_trace_exec:- assert_u_no_dep(mpred_is_tracing_exec),mpred_pfc_silent(false).
mpred_notrace_exec:- retractall_u(mpred_is_tracing_exec).

mpred_trace_all:- mpred_trace_exec,mpred_trace,mpred_set_warnings(true),mpred_pfc_silent(false).
mpred_notrace_all:- mpred_notrace_exec,mpred_notrace,mpred_set_warnings(false).


:- thread_local(t_l:hide_mpred_trace_exec/0).

%% with_mpred_trace_exec( +P) is semidet.
%
% Using Trace exec.
%

% with_mpred_trace_exec(P):- locally_each(-t_l:hide_mpred_trace_exec,locally_each(t_l:mpred_debug_local, must_ex(show_if_debug(P)))).

with_mpred_trace_exec(P):- notrace(lookup_u(mpred_is_tracing_exec)),!,show_if_debug(P).
with_mpred_trace_exec(P):-
   locally_each(-t_l:hide_mpred_trace_exec,
       locally_each(t_l:mpred_debug_local,
           must_ex(show_if_debug(P)))).


%% with_mpred_trace_exec( +P) is semidet.
%
% Without Trace exec.
%
with_no_mpred_trace_exec(P):-
 with_no_dmsg((
   locally_each(-t_l:mpred_debug_local,locally_each(t_l:hide_mpred_trace_exec, must_ex(/*show_if_debug*/(P)))))).

%% show_if_debug( :GoalA) is semidet.
%
% Show If Debug.
%
:- meta_predicate(show_if_debug(0)).
% show_if_debug(A):- !,show_call(why,A).
show_if_debug(A):-  get_mpred_is_tracing(A) -> show_call(mpred_is_tracing,call_u(A)) ; call_u(A).

:- thread_local(t_l:mpred_debug_local/0).

%% mpred_is_silent is det.
%
% If Is A Silient.
%
mpred_is_silent :- t_l:hide_mpred_trace_exec,!, \+ tracing.
mpred_is_silent :- quietly_ex(( \+ t_l:mpred_debug_local, \+ lookup_u(mpred_is_tracing_exec), \+ call(lmcache:mpred_is_spying_pred(_,_)),
  current_prolog_flag(debug,false), is_release)) ,!.

oinfo(O):- xlisting((O, - spft, - ( ==> ), - pt , - nt , - bt , - mdefault, - lmcache)).

mpred_must(\+ G):-!, ( \+ call_u(G) -> true ; (log_failure(failed_mpred_test(\+ G)),!,ignore(why_was_true(G)),!,break_ex)).
mpred_must(G):- (call_u(G) -> true ; (ignore(sanity(why_was_true(\+ G))),(log_failure(failed_mpred_test(G))),!,break_ex)).


why_was_true((A,B)):- !,mpred_why(A),mpred_why(B).
why_was_true(P):- predicate_property(P,dynamic),mpred_why(P),!.
why_was_true(P):- dmsg_pretty(justfied_true(P)),!.


mpred_load_term(:- module(_,L)):-!, call_u_no_bc(maplist(export,L)).
mpred_load_term(:- TermO):- call_u_no_bc(TermO).
mpred_load_term(TermO):-mpred_ain_object(TermO).


%
%  These control whether or not warnings are printed at all.
%    mpred_warn.
%    nompred_warn.
%
%  These print a warning message if the flag mpred_warnings is set.
%    mpred_warn(+Message)
%    mpred_warn(+Message,+ListOfArguments)
%

mpred_warn:-
  retractall_u(mpred_warnings(_)),
  assert_u_no_dep(mpred_warnings(true)).

nompred_warn:-
  retractall_u(mpred_warnings(_)),
  assert_u_no_dep(mpred_warnings(false)).


%%  mpred_set_warnings(+TF) is det.
%   true = sets flag to cause Pfc warning messages to print.
%   false = sets flag to cause Pfc warning messages not to print.
%
mpred_set_warnings(True):-
  retractall_u(mpred_warnings(_)),
  assert_u_no_dep(mpred_warnings(True)).
mpred_set_warnings(false):-
  retractall_u(mpred_warnings(_)).


%%  mpred_trigger_key(+Trigger,-Key)
%
%  Arg1 is a trigger.  Key is the best term to index it on.
%
%  Get a key from the trigger that will be used as the first argument of
%  the trigger base clause that stores the trigger.

mpred_trigger_key(X,X):- var(X), !.
mpred_trigger_key(pt(Key,_),Key).
mpred_trigger_key(pk(Key,_,_),Key).
mpred_trigger_key(nt(Key,_,_),Key).
mpred_trigger_key(Key,Key).

% For chart parser
mpred_trigger_key(chart(word(W),_ZL),W):- !.
mpred_trigger_key(chart(stem([Char1|_ZRest]),_ZL),Char1):- !.
mpred_trigger_key(chart(Concept,_ZL),Concept):- !.
mpred_trigger_key(X,X).


:-module_transparent(mpred_ain/1).
:-module_transparent(mpred_aina/1).
:-module_transparent(mpred_ainz/1).
:-system:import(mpred_ain/1).
:-system:import(mpred_ain/2).

/*
:-module_transparent(mpred_ain/1).
:-module_transparent(mpred_aina/1).
:-module_transparent(mpred_ainz/1).
*/

% :- '$current_source_module'(M),forall(mpred_database_term(F,A,_),(abolish(pfc_lib:F/A),abolish(user:F/A),abolish(M:F/A))).
% :- initialization(ensure_abox(baseKB)).


% % :- set_prolog_flag(mpred_pfc_file,true).
% local_testing

:- set_prolog_flag(expect_pfc_file,never).

:- fixup_exports.
%:- prolog_load_context(module,M),declare_pfc_support(M).
%:- import_everywhere(baseKB).


% :- kb_shared(lmcache:mpred_is_spying_pred/2).

 
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.



:- must_ex(mpred_reset_kb_0).

:- defaultAssertMt(M),dynamic((M:current_ooZz/1,M:default_ooZz/1,M:if_mooZz/2)).

:- mpred_trace.
:- mpred_watch.


% this should have been ok
% (if_mooZz(Missing,Create) ==> ((\+ Missing/(Missing\==Create), \+ Create , \+ ~(Create)) ==> Create)).
:- ((mpred_ain((if_mooZz(Missing,Create) ==>
 ( ( \+ Missing/ \+ (variant_u(Missing,Create))) ==> Create))))).

:- mpred_ain((default_ooZz(X) ==> if_mooZz(current_ooZz(_),current_ooZz(X)))).

:- mpred_ain(default_ooZz(booZz)).

:- mpred_test(current_ooZz(booZz)).

% :- pp_DB.

:- (mpred_ain(current_ooZz(fooZz))).

:- mpred_test(\+current_ooZz(booZz)).

:- (mpred_ain(\+ current_ooZz(fooZz))).

:- mpred_test(current_ooZz(booZz)).

:- (mpred_withdraw( default_ooZz(booZz) )).

:- listing([current_ooZz,default_ooZz]).

:- mpred_test( \+current_ooZz(booZz)).

:- mpred_ain(~ current_ooZz(fooZz)).

% :- pp_DB.

:- mpred_test(~current_ooZz(fooZz)).

:- mpred_ain(default_ooZz(booZz)).

:- mpred_test(current_ooZz(booZz)).

:- mpred_reset_kb_0.



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
:- if(current_prolog_flag(xref,true)).  % XREF

:- module(mpred_at_box,[
         assert_setting01/1,
         make_module_name_local/2,
         make_module_name_local0/2,
         (make_shared_multifile)/1,
         (make_shared_multifile)/3,
         (make_shared_multifile)/4,
         % (kb_global)/1,
         add_import_predicate/3,
         autoload_library_index/4,
         ensure_abox_hybrid/1,
         ensure_abox/1,
         baseKB_hybrid_support/2,
         correct_module/3,
         correct_module/5,
         defaultAssertMt/1,
         ensure_imports/1,

         set_fileAssertMt/1,
          setup_module_ops/1,

         in_mpred_kb_module/0,
         
         makeConstant/1,
         is_mtCanAssert/1,
         %registerCycPred/4,
         %registerCycPred/5,

         set_defaultAssertMt/1,
         set_fileAssertMt/1,
         transitive_path/3,
         which_file/1,
         user_m_check/1,


         % add_abox_module/1,

         ensure_tbox/1,
          get_file_type_local/2,

         fixup_modules/0,
         import_predicate/2,
         skip_user/1,
         inherit_into_module/2,
         box_type/3,
         make_reachable/2,
         % clause_bq/1,
         fixup_module/2,
         is_undefaulted/1,
         ensure_imports_tbox/2,
         map_inheritance/1,


         which_file/1
    ]).
:- set_module(class(library)).
:- endif.

:- module_transparent((
     baseKB_hybrid_support/2,
         correct_module/3,
         correct_module/5,
         defaultAssertMt/1,
         ensure_imports/1,
         fileAssertMt/1,

         in_mpred_kb_module/0)).

:-dynamic(unused_predicate/4).

:- include('mpred_header.pi').
:- flag_call(runtime_debug=false).

baseKB:pfc_load_lib.

:- set_how_virtualize_file(bodies).

user_m_check(_Out).

:- meta_predicate make_shared_multifile(+,+,+), mpred_op_each(3).
:- meta_predicate make_shared_multifile(*,*,*,*).


:- meta_predicate transitive_path(2,*,*).

% add_abox_module(baseKB):-!.
/*add_abox_module(ABox):- must(atom(ABox)),
  must(is_mtCanAssert(ABox)),
  baseKB:ain(baseKB:mtHybrid(ABox)).
*/
/*
:- dynamic(baseKB:mtProlog/1).
*/

:- kb_global(baseKB:mtNoPrologCode/1).
baseKB:mtNoPrologCode(mpred_userkb).

:- kb_global(baseKB:mtProlog/1).
baseKB:mtProlog(Mt):- baseKB_mtProlog(Mt).

%:- kb_global(lmcache:mpred_is_spying_pred/2).

baseKB_mtProlog(Mt):- \+ atom(Mt),!,var(Mt),!,current_module(Mt),baseKB:mtProlog(Mt).
baseKB_mtProlog(Mt):- \+ current_module(Mt),!,fail.
baseKB_mtProlog(Mt):- clause_bq(mtHybrid(Mt)),!,fail.
baseKB_mtProlog(Mt):- module_property(Mt,class(library)).
baseKB_mtProlog(Mt):- module_property(Mt,class(system)).
baseKB_mtProlog(Mt):- arg(_,v(lmcache,t_l,system),Mt).
% baseKB_mtProlog(user).

:- multifile(lmcache:has_pfc_database_preds/1).
:- dynamic(lmcache:has_pfc_database_preds/1).


%% assert_setting01( ?X) is semidet.
% :- srtrace.
assert_setting01(M:P):-safe_functor(P,_,A),dupe_term(P,DP),setarg(A,DP,_),system:retractall(M:DP),system:asserta(M:P).

% :- break.

%% which_file( ?F) is semidet.
%
% Which File.
%
which_file(F):- prolog_load_context(source,F) -> true; once(loading_source_file(F)).

:- module_transparent

         assert_setting01/1,
         make_module_name_local/2,
         make_module_name_local0/2,

         defaultAssertMt/1,
         set_defaultAssertMt/1,
         with_no_retry_undefined/1,
         which_file/1,
         fileAssertMt/1,
         set_fileAssertMt/1,

         correct_module/3,
         correct_module/5,
         ensure_imports/1,
         in_mpred_kb_module/0,
         which_file/1,
         user_m_check/1 .


%% in_mpred_kb_module is semidet.
%
% In Managed Predicate Knowledge Base Module.
%
in_mpred_kb_module:- source_context_module(MT),defaultAssertMt(MT2),!,MT==MT2.


map_inheritance(Child):-forall(import_module(Child,Parent),inherit_into_module(Child,Parent)).


%% box_type( ?F, ?A, ?VALUE3) is semidet.
%
% Datalog Type.
%
box_type(F,A,tbox):-current_predicate(baseKB:F/A).
box_type(_,_,abox).




:- thread_local(t_l:current_defaultAssertMt/1).
:- dynamic(baseKB:file_to_module/2).




:- dynamic(lmcache:mpred_directive_value/3).


% :- '$hide'(defaultAssertMt(_)).


%% get_file_type_local( ?File, ?Type) is det.
%
% Get File Type.
%
get_file_type_local(File,Type):-var(File),!,quietly_must_ex(loading_source_file(File)),get_file_type_local(File,Type),!.
get_file_type_local(File,pfc):-file_name_extension(_,'.pfc.pl',File),!.
get_file_type_local(File,pfc):-file_name_extension(_,'.clif',File),!.
get_file_type_local(File,Type):-file_name_extension(_,Type,File),!.
get_file_type_local(File,Type):-clause_bq(lmcache:mpred_directive_value(File,language,Type)).

mtCanAssert(M):- is_mtCanAssert(M).

is_mtCanAssert(Module):- clause_bq(mtNonAssertable(Module)),!,fail.
is_mtCanAssert(ABox):- \+ \+ (ABox=abox),!,trace_or_throw_ex(is_mtCanAssert(ABox)),fail.
is_mtCanAssert(Module):- clause_bq(mtHybrid(Module)).
is_mtCanAssert(user):-  is_user_pfc.
is_mtCanAssert(Module):- \+ pfc_lib:is_code_module(Module),!.
 % is_mtCanAssert(Module):- clause_bq(mtExact(Module)).
% is_mtCanAssert(Module):-  module_property(Module,file(_)),!,fail.
is_mtCanAssert(Module):- (loading_source_file(File),get_file_type_local(File,pfc),prolog_load_context(module,Module)).
is_mtCanAssert(Module):- clause_bq(mtProlog(Module)),!,fail.

is_user_pfc:- clause_bq(mtHybrid(user)).



%% fileAssertMt(-ABox) is det.
%
% Gets ABox is an "assertion component" Prolog Module
% within a knowledge base.
%
% not just user modules

fileAssertMt(M):- nonvar(M), fileAssertMt(ABoxVar),!,M=@=ABoxVar.
fileAssertMt(M):- loading_source_file(File),clause_bq(baseKB:file_to_module(File,M)),!.
fileAssertMt(M):- loading_source_file(File),clause_bq(lmcache:mpred_directive_value(File,module,M)),!.
fileAssertMt(M):- fileAssertMt0(M), (source_location(_,_)->show_call(set_fileAssertMt(M));true).

fileAssertMt0(M):- prolog_load_context(module,M),is_mtCanAssert(M),!.
fileAssertMt0(M):- '$current_typein_module'(M),is_mtCanAssert(M),!.
fileAssertMt0(M):- 'strip_module'(module,M,module),is_mtCanAssert(M),!.
fileAssertMt0(M):- must(get_fallBackAssertMt(M)),!.


:- initialization(fix_baseKB_imports,now).



%% set_fileAssertMt( ABox) is semidet.
%
% Sets the File''s Module.
%

% set_fileAssertMt(M):- '$current_source_module'(M),!.
set_fileAssertMt(M):-  (M==user;M==system;M==pfc_lib),!,set_fileAssertMt(baseKB).
set_fileAssertMt(M):-
  ensure_abox_hybrid(M),
  sanity(is_mtCanAssert(M)),
  must(which_file(File)),
  assert_setting(baseKB:file_to_module(File,M)),
  assert_setting(lmcache:mpred_directive_value(File,module,M)),
  asserta_until_eof(t_l:current_defaultAssertMt(M)),!,
  ((pfc_lib:is_pfc_file) -> must(set_current_modules_until_eof(M)) ; true).


%:- import(pfc_lib:is_pfc_file/0).
% :- '$hide'(set_fileAssertMt(_)).


set_current_modules_until_eof(M):- 
 '$current_typein_module'(CM),'$set_typein_module'(M),call_on_eof('$set_typein_module'(CM)),
 '$current_source_module'(SM),'$set_source_module'(M),call_on_eof('$set_source_module'(SM)).


%% set_defaultAssertMt( ?M) is semidet.
%
% Sets Current Module.
%
set_defaultAssertMt(M):-  (M==user;M==system;M==pfc_lib),!,set_defaultAssertMt(baseKB).
set_defaultAssertMt(M):-
  ignore(show_failure(is_mtCanAssert(M))),
   ensure_abox_hybrid(M),!,
   % assert_setting(t_l:current_defaultAssertMt(M)),
   asserta_until_eof(t_l:current_defaultAssertMt(M)),
  (source_location(_,_)-> ((fileAssertMt(M) -> true; set_fileAssertMt(M)))  ;set_current_modules_until_eof(M)).

% :- '$hide'(set_defaultAssertMt(_)).



%% defaultAssertMt(-Ctx) is det.
%
% M is an "assertion component" Prolog Module
% within a knowledge base.
%
% not just user modules

defaultAssertMt(M):- nonvar(M), defaultAssertMt(ABoxVar),!,M=@=ABoxVar.
defaultAssertMt(M):- quietly(defaultAssertMt0(M)),!.

defaultAssertMt0(M):- t_l:current_defaultAssertMt(M).
defaultAssertMt0(M):- get_fallBackAssertMt(M),!.

get_fallBackAssertMt(M):- loading_source_file(File),clause_bq(baseKB:file_to_module(File,M)).
get_fallBackAssertMt(M):- loading_source_file(File),clause_bq(lmcache:mpred_directive_value(File,module,M)).
get_fallBackAssertMt(M):- guess_maybe_assertMt(M),clause_bq(mtHybrid(M)),!.
get_fallBackAssertMt(M):- guess_maybe_assertMt(M),is_mtCanAssert(M),!.
get_fallBackAssertMt(M):- guess_maybe_assertMt(M).

guess_maybe_assertMt(M):- '$current_source_module'(M).
guess_maybe_assertMt(M):- context_module(M).
guess_maybe_assertMt(M):- loading_source_file(File),clause_bq(baseKB:file_to_module(File,M)).
guess_maybe_assertMt(M):- loading_source_file(File),clause_bq(lmcache:mpred_directive_value(File,module,M)).
guess_maybe_assertMt(M):-  which_file(File)->current_module(M),module_property(M,file(File)),File\==M.
guess_maybe_assertMt(M):- '$current_typein_module'(M).
guess_maybe_assertMt(M):- nb_current(defaultQueryMt,M),!.
guess_maybe_assertMt(M):- which_file(File)->make_module_name_local(File,M),current_module(M),File\==M.   
guess_maybe_assertMt(M):- (loading_source_file(File),get_file_type_local(File,pfc)),prolog_load_context(module,M).





defaultQueryMt(M):- nonvar(M), defaultQueryMt(ABoxVar),!,M=@=ABoxVar.
defaultQueryMt(M):- nb_current(defaultQueryMt,M)->true;(defaultQueryMt0(M)->nb_setval(defaultQueryMt,M)),!.


defaultQueryMt0(M):- 'strip_module'(module,M,module),clause_bq(mtHybrid(M)),!.
defaultQueryMt0(M):- prolog_load_context(module,M),clause_bq(mtHybrid(M)),!.
defaultQueryMt0(M):- '$current_typein_module'(M),clause_bq(mtHybrid(M)),!.
defaultQueryMt0(M):- guess_maybe_assertMt(M),clause_bq(mtHybrid(M)),!.
defaultQueryMt0(M):- guess_maybe_assertMt(M),is_mtCanAssert(M),!.
defaultQueryMt0(M):- guess_maybe_assertMt(M).







% baseKB:mtGlobal
% mtCore



makeConstant(_Mt).

is_pfc_module_file(M):- is_pfc_module_file(M,F,TF),!, (F \== (-)), TF = true.

is_pfc_module_file(M,F,TF):- (module_property(M,file(F)),pfc_lib:is_pfc_file(F)) *-> TF=true ; 
  (module_property(M,file(F))*->TF=false ; (F= (-), TF=false)).

maybe_ensure_abox(M):- is_pfc_module_file(M,F,_), (F \== (-)), !,   
  (pfc_lib:is_pfc_file(F)->show_call(pfc_lib:is_pfc_file(F),ensure_abox_hybrid(M));dmsg_pretty(not_is_pfc_module_file(M,F))).
maybe_ensure_abox(M):- show_call(not_is_pfc_file,ensure_abox_hybrid(M)).


:- module_transparent((ensure_abox_hybrid)/1).
ensure_abox_hybrid(M):- ensure_abox(M),must(ain(baseKB:mtHybrid(M))),must(M\==baseKB->ain(genlMt(M,baseKB));true).
ensure_abox_prolog(M):- ensure_abox(M),must(ain(baseKB:mtProlog(M))).

ensure_abox(M):- clause_bq(M:defaultTBoxMt(_)),!.
ensure_abox(M):- 
  ignore(((M==user;M==pfc_lib;M==baseKB)->true;add_import_module(M,pfc_lib,end))),
  dynamic(M:defaultTBoxMt/1),
  must(ensure_abox_support(M,baseKB)),!.
  
setup_database_term(M:F/A):-dynamic(M:F/A),multifile(M:F/A),public(M:F/A),module_transparent(M:F/A),
  discontiguous(M:F/A),
  ignore((M\==baseKB,functor(P,F,A),assertz_new(M:(P :- zwc, inherit_above(M, P))))).

:- module_transparent((ensure_abox_support)/2).
ensure_abox_support(M,TBox):- (M==user;M==system;M==pfc_lib),!,ensure_abox_support(baseKB,TBox).
ensure_abox_support(M,TBox):- clause_bq(M:defaultTBoxMt(TBox)),!.
ensure_abox_support(M,TBox):- 
   asserta(M:defaultTBoxMt(TBox)),
   set_prolog_flag(M:unknown,error),  
   must(forall(mpred_database_term(F,A,_PType), setup_database_term(M:F/A))),
   must(system:add_import_module(M,system,end)),
   (M\==user->must(ignore(system:delete_import_module(M,user)));true),!,
   must(setup_module_ops(M)),
   (M == baseKB -> true ; ensure_abox_support_pt2_non_baseKB(M)).
   
ensure_abox_support(M,TBox):- 
       % system:add_import_module(M,user,end),
       must(ignore(system:delete_import_module(M,system))),
       must(ignore(system:delete_import_module(M,baseKB))),
       system:add_import_module(M,system,end),
       retractall(M:defaultTBoxMt(TBox)),
       throw(failed_ensure_abox_support(M,TBox)).


ensure_abox_support_pt2_non_baseKB(M):-
   M:use_module(library(pfc_lib)),
   '$current_typein_module'(TM),
   '$current_source_module'(SM),
   '$set_typein_module'(M),
   '$set_source_module'(M),
   pfc_iri:include_module_file(M:library('pfclib/system_each_module.pfc'),M),!,
   '$set_typein_module'(TM),
   '$set_source_module'(SM),!.
   

setup_module_ops(M):- mpred_op_each(mpred_op_unless(M)).

mpred_op_unless(M,A,B,C):- op_safe(A,B,M:C).

mpred_op_each(OpEach):-
            call(OpEach,1199,fx,('==>')), % assert
            call(OpEach,1199,fx,('?->')), % ask
            call(OpEach,1199,fx,('?=>')),
            call(OpEach,1190,xfy,('::::')), % Name something
            call(OpEach,1180,xfx,('==>')), % Forward chaining
            call(OpEach,1170,xfx,('<==>')), % Forward and backward chaining
            call(OpEach,1160,xfx,('<==')), % backward chain PFC sytle
            call(OpEach,1160,xfx,('<-')), % backward chain PTTP sytle (currely really PFC)
            call(OpEach,1160,xfx,('<=')), % backward chain DRA sytle
            call(OpEach,1150,xfx,('=>')), % Logical implication
            call(OpEach,1130,xfx,('<=>')), % Logical bi-implication
            call(OpEach,600,yfx,('&')), 
            call(OpEach,600,yfx,('v')),
            call(OpEach,400,fx,('~')),
            % call(OpEach,300,fx,('-')),
            call(OpEach,350,xfx,('xor')),
            % replicate user:op/3s in case we remove inheritance
            forall(current_op(X,Y,user:Z),
              call(OpEach,X,Y,Z)).







%:- multifile(get_current_default_tbox/1).
%:- dynamic(get_current_default_tbox/1).
%get_current_default_tbox(baseKB).
:- if(current_predicate(get_current_default_tbox/1)).
:- redefine_system_predicate(get_current_default_tbox/1).
:- endif.
:- module_transparent(get_current_default_tbox/1).
get_current_default_tbox(TBox):- defaultAssertMt(ABox)->current_module(ABox)->clause(ABox:defaultTBoxMt(TBox),B),call(B),!.
get_current_default_tbox(baseKB).
:- sexport(get_current_default_tbox/1).





make_module_name_local(A,B):- make_module_name_local0(A,B), \+ exists_file(B),!.

make_module_name_local0(Source,KB):- clause_bq(mtProlog(Source)),t_l:current_defaultAssertMt(KB),!.
make_module_name_local0(Source,KB):- clause_bq(mtGlobal(Source)),t_l:current_defaultAssertMt(KB),!.
make_module_name_local0(Source,SetName):- clause_bq(baseKB:file_to_module(Source,SetName)),!.
make_module_name_local0(Source,Source):- lmcache:has_pfc_database_preds(Source).
make_module_name_local0(Source,Source):- clause_bq(mtHybrid(Source)),!.
make_module_name_local0(user,KB):- t_l:current_defaultAssertMt(KB),!.
make_module_name_local0(user,baseKB):-!.
make_module_name_local0(Source,GetName):- make_module_name00(Source,GetName).


ensure_tbox(_ABox).


%% mtCore( ?VALUE1) is semidet.
%
% If Is A Datalog System Core Microtheory.
%
:- dynamic(baseKB:mtCore/1).
baseKB:mtCore(baseKB).




%% baseKB:mtGlobal(M,Box).
%
% Boot Modules.
%
%baseKB:mtGlobal(mpred_loader).

:- dynamic(baseKB:mtGlobal/1).
baseKB:mtGlobal(boot_system).
baseKB:mtGlobal(system_markers).
baseKB:mtGlobal(system_singleValued).
baseKB:mtGlobal(system_genls).
baseKB:mtGlobal(system_if_missing).
baseKB:mtGlobal(common_logic_clif).
baseKB:mtGlobal(system_mdefault).

:- dynamic(baseKB:mtCycLBroad/1).

baseKB:mtCycLBroad(baseKB).

is_undefaulted(user).

/*
:- dynamic(call_a/0).
call_a:- arity(tCol,1),arity(arity,2).
:- must(((clause(call_a,
        (ereq(arity(tCol,1)),ereq(arity(arity,2))),Ref),erase(Ref)))).
*/

%% ensure_imports( ?M) is semidet.
%
% Ensure Imports.
%
ensure_imports(baseKB):-!.
ensure_imports(M):- ain(genlMt(M,baseKB)).
% ensure_imports(M):- ain(M:genlMt(M,baseKB)).

:-multifile(lmcache:is_ensured_imports_tbox/2).
:-dynamic(lmcache:is_ensured_imports_tbox/2).


%% skip_user( ?M) is semidet.
%
% Skip over 'user' module and still see 'system'.
%
skip_user(Mt):- Mt==user,!.
skip_user(Mt):- import_module(Mt,system), \+ default_module(Mt,user), !.
skip_user(Mt):- add_import_module(Mt,system,start),ignore(delete_import_module(Mt,user)),
  forall((import_module(Mt,X),default_module(X,user)),skip_user(X)).

inherit_into_module(Child,Parent):- ==(Child,Parent),!.
%TODO inherit_into_module(Child,Parent):- ain(Child:genlMt(Child,Parent)).
inherit_into_module(Child,Parent):- ain(baseKB:genlMt(Child,Parent)).

%% ensure_imports_tbox( ?M, ?TBox) is semidet.
%
% Ensure Imports Tbox.
%
ensure_imports_tbox(M,TBox):- trace_or_throw_ex(unexpected_ensure_imports_tbox(M,TBox)), M==TBox,!.
ensure_imports_tbox(M,TBox):-
  lmcache:is_ensured_imports_tbox(M,TBox),!.
ensure_imports_tbox(M,TBox):-
  asserta(lmcache:is_ensured_imports_tbox(M,TBox)),

  must((
   skip_user(TBox),
   ignore(maybe_delete_import_module(M,TBox)),
   ignore(maybe_delete_import_module(TBox,M)),
   forall((user:current_predicate(_,TBox:P),
      \+  /*ex*/predicate_property(TBox:P,imported_from(_))),
      add_import_predicate(M,P,TBox)),
   inherit_into_module(M,user),
   skip_user(M),
   ignore(maybe_delete_import_module(M,user)),
   inherit_into_module(user,M),
   ignore(maybe_delete_import_module(user,system)), % gets from M now
   !)).



% :- inherit_into_module(logicmoo_user,system).

fixup_module(_,_):-!.
fixup_module(system,_).
fixup_module(M,_L):- clause_bq(tGlobal(M)),skip_user(M).
fixup_module(system,_L):-skip_user(system).
fixup_module(_,[user]).
fixup_module(M,_L):- skip_user(M).


fixup_modules:-  trace_or_throw_ex(unexpected(fixup_modules)),
   doall((current_module(M),once((findall(I,import_module(M,I),L))),once(fixup_module(M,L)))).

% :- autoload([verbose(false)]).
:- flag_call(runtime_debug=true).

% :- fixup_modules.







% ============================================

%% correct_module( ?M, ?X, ?T) is semidet.
%
% Correct Module.
%
correct_module(M,G,T):-safe_functor(G,F,A),quietly_must_ex(correct_module(M,G,F,A,T)),!.

%% correct_module( ?M, ?Goal, ?F, ?A, ?T) is semidet.
%
% Correct Module.
%
correct_module(abox,G,F,A,T):- !, defaultAssertMt(M),correct_module(M,G,F,A,T).
correct_module(tbox,G,F,A,T):- !, get_current_default_tbox(M),correct_module(M,G,F,A,T).
correct_module(user,G,F,A,T):- fail,!,defaultAssertMt(M),correct_module(M,G,F,A,T).

correct_module(HintMt,Goal,F,A,OtherMt):-var(Goal),safe_functor(Goal,F,A),!,correct_module(HintMt,Goal,F,A,OtherMt).
correct_module(HintMt,Goal,_,_,OtherMt):-  /*ex*/predicate_property(HintMt:Goal,imported_from(OtherMt)).
correct_module(_,Goal,_,_,OtherMt):-  /*ex*/predicate_property(Goal,imported_from(OtherMt)).
correct_module(HintMt,_,_,_,HintMt):- call_u(mtExact(HintMt)).
correct_module(HintMt,Goal,_,_,HintMt):-  /*ex*/predicate_property(HintMt:Goal,exported).
correct_module(_,Goal,_,_,OtherMt):- var(OtherMt),!,  /*ex*/predicate_property(OtherMt:Goal,file(_)).
correct_module(_,Goal,_,_,OtherMt):- clause_bq(mtGlobal(OtherMt)),  /*ex*/predicate_property(OtherMt:Goal,file(_)).
correct_module(MT,_,_,_,MT):-!.



:- dynamic(lmcache:how_registered_pred/4).
:- module_transparent(lmcache:how_registered_pred/4).

add_import_predicate(Mt,Goal,OtherMt):- fail,
   clause_bq(mtGlobal(Mt)),
   clause_bq(mtGlobal(OtherMt)),
   \+ import_module(OtherMt,Mt),
   catch(add_import_module(Mt,OtherMt,end),
       error(permission_error(add_import,module,baseKB),
       context(system:add_import_module/3,'would create a cycle')),fail),
   must( /*ex*/predicate_property(Mt:Goal,imported_from(OtherMt))),!.


add_import_predicate(Mt,Goal,OtherMt):- trace_or_throw_ex(add_import_predicate(Mt,Goal,OtherMt)),
   catch(Mt:import(OtherMt:Goal),_,fail),!.
add_import_predicate(Mt,Goal,OtherMt):-
   safe_functor(Goal,F,A),
   make_as_dynamic(imported_from(OtherMt),Mt,F,A),
   assert_if_new(( Mt:Goal :- OtherMt:Goal)).


transitive_path(F,[Arg1,Arg2],Arg2):-
  dif(Arg1,Arg2),call(F,Arg1,Arg2),!.
transitive_path(F,[Arg1,SecondNodeMt|REST],Arg2):-
  dif(Arg1,Arg2),dif(Arg1,SecondNodeMt),
  call(F,Arg1,SecondNodeMt),sanity(stack_check),
  transitive_path(F,[SecondNodeMt|REST],Arg2).



autoload_library_index(F,A,PredMt,File):- safe_functor(P,F,A),'$autoload':library_index(P,PredMt,File).


:- multifile(baseKB:hybrid_support/2).
:- dynamic(baseKB:hybrid_support/2).
baseKB_hybrid_support(F,A):-suggest_m(M),clause_bq(baseKB:safe_wrap(M,F,A,_)).
baseKB_hybrid_support(F,A):-clause_bq(hybrid_support(F,A)).

baseKB:hybrid_support(predicateConventionMt,2).

baseKB:hybrid_support(functorDeclares,1).
baseKB:hybrid_support(arity,2).

%baseKB:hybrid_support(spft,3).

baseKB:hybrid_support(mtHybrid,1).
baseKB:hybrid_support(mtCycLBroad,1).
baseKB:hybrid_support(genlMt,2).


%=

%% kb_global( +PI) is semidet.
%
% Shared Multifile.
%
make_shared_multifile(PredMt:MPI):-
   context_module_of_file(CallerMt),!,
   with_pfa_group(make_shared_multifile,CallerMt,PredMt, MPI),!.


%% make_shared_multifile( ?CallerMt, ?PredMt, :TermPI) is semidet.
%
% Make Shared Multifile.
%
make_shared_multifile(CallerMt, PredMt,FA):- get_fa(FA,F,A), make_shared_multifile(CallerMt, PredMt,F,A),!.

make_shared_multifile(CallerMt,    t_l,F,A):-!,CallerMt:thread_local(t_l:F/A),CallerMt:multifile(t_l:F/A).
% make_shared_multifile(CallerMt,baseKB ,F,A):-!,CallerMt:multifile(baseKB:F/A),CallerMt:dynamic(baseKB:F/A),!.
make_shared_multifile(CallerMt,lmcache,F,A):-!,CallerMt:multifile(lmcache:F/A),CallerMt:volatile(lmcache:F/A),CallerMt:dynamic(lmcache:F/A),!.

make_shared_multifile(CallerMt,PredMt,F,A):-
  safe_functor(Goal,F,A),
  correct_module(PredMt,Goal,F,A,HomeM),
  HomeM\==PredMt,!,
  make_shared_multifile(CallerMt,HomeM,F,A).

make_shared_multifile(CallerMt,Home,F,A):- clause_bq(mtProlog(Home)),!,
     wdmsg_pretty(mtSharedPrologCodeOnly_make_shared_multifile(CallerMt,Home:F/A)),!.

make_shared_multifile(_CallerMt, baseKB,F,A):-  kb_global(baseKB:F/A),!.

make_shared_multifile(_CallerMt,PredMt,F,A):-!,
 dmsg_pretty(make_shared_multifile(PredMt:F/A)),
 locally(set_prolog_flag(access_level,system),
  PredMt:(
   sanity( \+ ((PredMt:F/A) = (qrTBox:p/1))),
   PredMt:check_never_assert(declared(PredMt:F/A)),
   decl_mpred(PredMt:F/A))).




%% make_reachable( ?UPARAM1, ?Test) is semidet.
%
% Make Reachable.
%
make_reachable(_,Test):- \+ \+ ((Test= (_:F/_), is_ftVar(F))),!.
make_reachable(CM,M:F/A):-  atom(CM),ignore(CM=M),quietly_must_ex(atom(CM)),quietly_must_ex(atom(M)),
   safe_functor(G,F,A),
   correct_module(M,G,F,A,TT), !,import_predicate(CM,TT:F/A).



%% import_predicate( ?CM, :TermM) is semidet.
%
% Import Predicate.
%
import_predicate(CM,M:_):- CM==M,!.
import_predicate(CM,M:_):- default_module(CM,M),!.
import_predicate(CM,M:F/A):- show_call(nop(CM:z333import(M:F/A))),CM:multifile(M:F/A),
  on_xf_cont(CM:discontiguous(M:F/A)).



/*
system:call_expansion(T,(mpred_at_box:defaultAssertMt(NewVar),NewT)):- current_predicate(_,get_lang(pfc)), compound(T),
   subst(T,abox,NewVar,NewT),NewT\=@=T.

system:body_expansion(T,(mpred_at_box:defaultAssertMt(NewVar),NewT)):- current_predicate(_,get_lang(pfc)), compound(T),
   subst(T,abox,NewVar,NewT),NewT\=@=T.
*/


:- fixup_exports.

/*  

% File used as storage place for all predicates which change as
% the world is run.
%
% props(Obj,height(ObjHt))  == k(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == height(Obj,ObjHt)
% padd(Obj,height(ObjHt))  == padd(height,Obj,ObjHt,...) == add(QueryForm)
% kretract[all](Obj,height(ObjHt))  == kretract[all](Obj,height,ObjHt) == pretract[all](height,Obj,ObjHt) == del[all](QueryForm)
% keraseall(AnyTerm).
%
%
% Dec 13, 2035
% Douglas Miles
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_kb_ops.pl
%:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
:- if(current_prolog_flag(xref,true)).
:- module(mpred_kb_ops,[]).
:- include('mpred_header.pi').
:- endif.

:- user:use_module(library(clpfd),['#='/2]).
%% get_arity( :TermTerm, ?F, ?A) is semidet.
%
% Get Arity.
%
get_arity(Term,F,A):- atom(Term),F=Term,!,ensure_arity(F,A).
get_arity(F/A,F,A):-!,atom(F),ensure_arity(F,A),!,(A>0).
get_arity('//'(F , A),F,A2):- must(integer(A)),!, atom(F), is(A2 , A+2), ensure_arity(F,A2),!,(A2>0).  
get_arity('//'(F , A),F,A2):- use_module(library(clpfd),['#='/2]),!, atom(F), clpfd:call('#='(A2 , A+2)), ensure_arity(F,A2),!,(A2>0). 
get_arity(M:FA,F,A):-atom(M),!,get_arity(FA,F,A).
get_arity(FA,F,A):- get_functor(FA,F,A),must(A>0).

% arity_no_bc(F,A):- call_u(arity(F,A)).
arity_no_bc(F,A):- clause_b(arity(F,A)).
arity_no_bc(F,A):- clause_b(support_hilog(F,A)).
arity_no_bc(F,A):- clause_b(functorDeclares(F)),!,A=1.
arity_no_bc(completeExtentAsserted,1).
arity_no_bc(home,2).
arity_no_bc(record,2).
arity_no_bc(F,A):- suggest_m(M),clause_b(mpred_prop(M,F,AA,_)),nonvar(AA),A=AA.
%arity_no_bc(F,A):- current_predicate(F/A)
% arity_no_bc(F,A):- current_predicate(_:F/A),\+(current_predicate(_:F/AA),AA\=A). =

%% ensure_arity( ?VALUE1, ?VALUE2) is semidet.
%
% Ensure Arity.
%
ensure_arity(F,A):- 
 one_must(
   arity_no_bc(F,A),
   one_must(
    (current_predicate(F/A),(A>0),assert_arity(F,A)),
    (ground(F:A),(A>0),assert_arity(F,A)))),
  !.


%=

%% assert_arity( ?F, :PRED2A) is semidet.
%
% Assert Arity.
%

assert_arity(F,A):- sanity(\+ ((bad_arity(F,A), trace_or_throw_ex(assert_arity(F,A))))), arity_no_bc(F,A),!.
assert_arity(F,A):- arity_no_bc(F,AA), A\=AA,dmsg_pretty(assert_additional_arity(F,AA->A)),!,ain_fast(arity(F,A)).
assert_arity(F,A):- ain_fast(arity(F,A)),!.

bad_arity(F,_):- \+ atom(F).
bad_arity(_,A):- \+ integer(A).
bad_arity('[|]',_).
bad_arity(typeProps,0).
bad_arity(argIsa,2).
bad_arity(isEach,_).
bad_arity(_,0).
bad_arity(prologDynamic,2).
bad_arity(F,A):- \+ good_pred_relation_name(F,A).


%=

%% good_pred_relation_name( ?F, ?A) is semidet.
%
% Good Predicate Relation Name.
%
good_pred_relation_name(F,A):- \+ bad_pred_relation_name0(F,A).


%=

%% bad_pred_relation_name0( ?V, ?VALUE2) is semidet.
%
% Bad Predicate Relation Name Primary Helper.
%
bad_pred_relation_name0(V,_):- \+ atom(V),!.
bad_pred_relation_name0('[]',_).
bad_pred_relation_name0('',_).
bad_pred_relation_name0('!',_).
bad_pred_relation_name0('{}',_).
bad_pred_relation_name0(',',_).
bad_pred_relation_name0('[|]',_).

%=

%% bad_pred_relation_name1( ?X, ?Y) is semidet.
%
% Bad Predicate Relation Name Secondary Helper.
%
bad_pred_relation_name1(X,Y):-bad_pred_relation_name0(X,Y).
bad_pred_relation_name1(F,A):-must_det((atom_codes(F,[C|_]),to_upper(C,U))),!, U == C, A>1.
bad_pred_relation_name1(F,A):-arity_no_bc(F,AO), A \= AO.

% :-after_boot(writeq("Seen Mpred_props at start!\n")),!.

%=

%% functor_check_univ( ?G1, ?F, ?List) is semidet.
%
% Functor Check Univ.
%
functor_check_univ(M:G1,F,List):-atom(M),member(M,[dbase,user]),!,functor_check_univ(G1,F,List),!.
functor_check_univ(G1,F,List):-must_det(compound(G1)),must_det(G1 \= _:_),must_det(G1 \= _/_),G1=..[F|List],!.


%:- endif.
% :- ensure_loaded(library('logicmoo/util/logicmoo_util_bugger.pl')).
%:- ensure_loaded(pfc_lib).
%:- use_module(mpred_type_isa).
%:- use_module(library(util_varnames)).

/*
:- module_transparent retract_mu/1,
         assert_mu/4,
         asserta_mu/2,
         assertz_mu/2,
         assert_u/1,
         asserta_u/1,
         assertz_u/1,
         attempt_side_effect/1.
*/
:- module_transparent(attvar_op/2).


:- meta_predicate 
      pred_head(1,*),
      attempt_side_effect(+),
      call_s(*),
      oncely(*),
      naf(*),
      call_s2(*),
      mpred_update_literal(*,*,0,*),
      mpred_retry(*),
%      mpred_op(?, ?),
      mpred_facts_only(*),
      map_unless(1,:,*,*),      
      is_callable(*),     
%      deducedSimply(*),
      cnstrn0(:,+),
      cnstrn(*),
      cnstrn(+,:),
      attvar_op(*,*),
      % clause_u(+,+,-),
      % call_u(+),
      assertz_mu(+),      
      assertz_mu(+,+),
      if_missing1(*),
      assert_mu(+),
      assert_mu(+,+,+,+),
      ain_minfo_2(1,*),
      ain_minfo(1,*),                                    
%      whenAnd(0,0),
      % mpred_call_0(*),
      mpred_bc_only(*),
      mpred_bc_only0(*),
      mpred_prove_neg(*),
      call_u_req(*),
      pfcBC_NoFacts(*).

 :- meta_predicate mpred_get_support_one(0,*).
 :- meta_predicate mpred_get_support_precanonical_plus_more(0,*).
 % :- meta_predicate '__aux_maplist/2_cnstrn0+1'(*,0).
 :- meta_predicate repropagate_0(*).
 :- meta_predicate trigger_supporters_list(0,*).
 :- meta_predicate repropagate_meta_wrapper(*).
 :- meta_predicate repropagate_0(*).


% oncely later will throw an error if there where choice points left over by call
:- meta_predicate(oncely(*)).
:- was_export(oncely/1).


%% oncely( :Goal) is semidet.
%
% Oncely.
%
oncely(:-(Call)):-!,Call,!.
oncely(:-(Call)):-!,call_u(Call).
oncely(Call):-once(Call).
% ================================================
% mpred_op/2
% ================================================

/*
query(t, call_u, G):- call_u(G).
query(_, _, Op, G):- dtrace(call_u(call(Op,G))).
once(A,B,C,D):-trace_or_throw_ex(once(A,B,C,D)).
*/




% ================================================
% is_callable/call_u/naf
% ================================================

%:- was_dynamic(naf/1).
:- meta_predicate(naf(*)).
:- was_export(naf/1).



%% naf( :Goal) is semidet.
%
% Negation-By-Faliure.
%
naf(Goal):- (\+ call_u(Goal)).

:- meta_predicate(is_callable(*)).
:- was_export(is_callable/1).



%% is_callable( :GoalC) is semidet.
%
% If Is A Callable.
%
is_callable(C):-current_predicate(_,C),!.


:- style_check(+singleton).

% TODO READD
%:- foreach(arg(_,isEach(prologMultiValued,prologOrdered,prologNegByFailure,prologPTTP,prologKIF,pfcControlled,ttRelationType,
%     prologHybrid,predCanHaveSingletons,prologDynamic,prologBuiltin,functorIsMacro,prologListValued,prologSingleValued),P),.. )


% TODO ISSUE https://github.com/logicmoo/PrologMUD/issues/7

%% check_context_module is semidet.
%
% Check Context Module. (throws if it turns out wrong)
%

check_context_module:- !.
% check_context_module:- is_release,!.
check_context_module:- 
  sanity((source_context_module(M1),clause_b(mtHybrid(M1)))),
  sanity((defaultAssertMt(M2),clause_b(mtHybrid(M2)))).

%% check_real_context_module is semidet.
%
% Check Real Context Module (throws if it turns out wrong)
%
check_real_context_module:- is_release,!.
check_real_context_module:-!.
check_real_context_module:- sanity((context_module(M1),defaultAssertMt(M2),must(M1==M2))).


% ======================= mpred_file('pfcsyntax').	% operator declarations.

:-
 op(1199,fx,('==>')), 
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),  
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'), 
 op(600,yfx,'&'), 
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'),
 op(300,fx,'-').




%% mreq( +G) is semidet.
%
% Mreq.
%
mreq(G):- if_defined(call_u(G),fail).

% ======================= mpred_file('pfccore').	% core of Pfc.

%   File   : pfccore.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated: 10/11/87, ...
%            4/2/91 by R. McEntire: added calls to valid_dbref as a
%                                   workaround for the Quintus 3.1
%                            bug in the recorded database.
%   Purpose: core Pfc predicates.

/*

LogicMOO is mixing Mark Stickel's PTTP (prolog techn theorem prover) to create horn clauses that 
 PFC forwards and helps maintain in visible states )  in prolog knowledge baseable.. We use spft/3 to track deductions
Research~wise LogicMOO has a main purpose is to prove that grounded negations (of contrapostives) are of first class in importance in helping
with Wff checking/TMS 
Also alows an inference engine constrain search.. PFC became important since it helps memoize and close off (terminate) transitive closures

*/


%% is_side_effect_disabled is semidet.
%
% If Is A Side Effect Disabled.
%
is_side_effect_disabled:- t_l:no_attempt_side_effects,!.
is_side_effect_disabled:- t_l:side_effect_ok,!,fail.
is_side_effect_disabled:- t_l:noDBaseMODs(_),!.



%% f_to_mfa( +EF, ?R, ?F, ?A) is semidet.
%
% Functor Converted To Module-functor-arity.
%
f_to_mfa(EF,R,F,A):-w_get_fa(EF,F,A),
              (((current_predicate(F/A),functor(P,F,A),predicate_property(_M:P,imported_from(R)))*->true;
              current_predicate(F/A),functor(P,F,A),source_file(R:P,_SF))),
              current_predicate(R:F/A).


%% w_get_fa( +PI, ?F, ?A) is semidet.
%
% W Get Functor-arity.
%
w_get_fa(PI,_F,_A):-is_ftVar(PI),!.
w_get_fa(F/A,F,A):- !.
w_get_fa(PI,PI,_A):- atomic(PI),!.
w_get_fa(PI,F,A):- is_ftCompound(PI),!,functor(PI,F,A).
w_get_fa(Mask,F,A):-get_functor(Mask,F,A).



:- multifile(baseKB:mpred_hook_rescan_files/0).
:- dynamic(baseKB:mpred_hook_rescan_files/0).
:- use_module(library(logicmoo_common)).
%:- was_dynamic(use_presently/0).
% used to annotate a predciate to indicate PFC support


%% is_mpred_action( :TermP) is semidet.
%
% If Is A Managed Predicate Action.
%
is_mpred_action('$VAR'(_)):-!,fail.
is_mpred_action(remove_if_unsupported(_,_)).
is_mpred_action(P):-is_static_predicate(P).

%% mpred_is_builtin( +P) is semidet.
%
% PFC If Is A Builtin.
%
mpred_is_builtin(P):- predicate_property(P,built_in), \+ predicate_property(P,dynamic).
mpred_is_builtin(P):- callable(P),functor(P,F,_),clause_b(prologBuiltin(F)).
mpred_is_builtin(F):- current_predicate(F/A),A>0,functor(P,F,A),mpred_is_builtin(P).

/* UNUSED TODAY

:- use_module(library(mavis)).
:- use_module(library(type_check)).
:- use_module(library(typedef)).
*/



:- thread_local((t_l:use_side_effect_buffer , t_l:verify_side_effect_buffer)).

%% record_se is semidet.
%
% Record Se.
%
record_se:- (t_l:use_side_effect_buffer ; t_l:verify_side_effect_buffer).



%% add_side_effect( +Op, ?Data) is semidet.
%
% Add Side Effect.
%
add_side_effect(_,_):- ( \+  record_se ),!.
add_side_effect(Op,Data0):- current_why(Why),serialize_attvars(Data0,Data),assert(t_l:side_effect_buffer(Op,Data,Why)).


%% attvar_op( +:PRED1, ?Data) is semidet.
%
% Attribute Variable Oper..
%


listing_s(P):-call_s(xlisting(P)).

assert_s(H):- assertz_s(H).
retractall_s(H):- forall(clause_s(H,_,R),erase(R)).
clause_s(H,B):- clause_s(H,B,_).

retract_s(H):- lookup_s(H,R),erase(R).

lookup_s(H):- lookup_s(H,_). 

lookup_s(M:(H:-B),R):- !,clause_s(M:H,B,R).
lookup_s((H:-B),R):-  !,clause_s(H,B,R).
lookup_s(H,R):- clause_s(H,true,R).

lookq_s(X):-lookq_s(X,_Ref).

lookq_s(M:(H:-B),R):- !,clauseq_s(M:H,B,R).
lookq_s((H:-B),R):- !, clauseq_s(H,B,R).
lookq_s(H,R):- clauseq_s(H,true,R).

asserta_s(H):- fix_mp(clause(assert,asserta_s),H,M,H0),asserta_i(M:H0).
assertz_s(H):- fix_mp(clause(assert,assertz_s),H,M,H0),assertz_i(M:H0).
clause_s(H,B,R):- fix_mp(clause(clause,clause_s),H,M,H0),clause_u(M:H0,B,R).
clauseq_s(H,B,R):- fix_mp(clause(clause,clauseq_s),H,M,H0),clause_u(M:H0,B,R),clause(M:HC,BC,R),H0=@=HC,BC=@=B.

call_s(G0):-
  strip_module(G0,_,G),functor(G,F,A),
  (memberchk(F/A,[(',')/2])->
  mpred_METACALL(call_s,G);
  call_s2(G0)).

call_s2(G0):-
  strip_module(G0,WM,G),
  defaultAssertMt(U),  
  must(current_predicate(_,U:G)->(CALL=U:G);(current_predicate(_,WM:G0)->CALL=WM:G0; fail)),
  call(call,(
 '$set_source_module'(S,U),'$module'(M,U),
  setup_call_cleanup( % _each
    ('$set_source_module'(U),'$set_typein_module'(U)),
       call(CALL),
     ('$set_source_module'(S),'$set_typein_module'(M))))).


:- module_transparent(attvar_op/2).

% % attvar_op(Op,Data):- deserialize_attvars(Data,Data0), attvar_op(Op,Data0).
attvar_op(Op,MData):-
 must_det_l((
   strip_module(Op,_,OpA), sanity( \+ atom(OpA)),
   fix_mp(clause(assert,OpA),MData,M,Data),
   add_side_effect(OpA,M:Data),
   quietly(current_prolog_flag(assert_attvars,true)->deserialize_attvars(Data,Data0);Data=Data0))),!,
   attempt_side_effect_mpa(M,OpA,Data0).


:- thread_local(t_l:no_attempt_side_effects/0).

%% attempt_side_effect( +PSE) is semidet.
%
% Physical Side Effect.
%
attempt_side_effect(PSE):- to_physical_mpa(PSE,M,P,A),!,attempt_side_effect_mpa(M,P,A).

to_physical_mpa(PSE,M,P,A):- strip_module(PSE,M,PA),to_physical_pa(PA,P,A).
to_physical_pa(PA,P,A):-PA=..[P,A],!. to_physical_pa(PA,call,PA).


:- meta_predicate(db_op_call(*,1,?)).
db_op_call(_What,How,Data):- call(How,Data).

% attempt_side_effect_mpa(M,OpA,Data):- record_se,!,add_side_effect(OpA,M:Data).
attempt_side_effect_mpa(M,db_op_call(_,retract_u0),Data0):- \+ lookup_u(M:Data0),!,fail.
attempt_side_effect_mpa(M,OpA,Data0):- \+ record_se, is_side_effect_disabled,!,mpred_warn('no_attempt_side_effects ~p',attempt_side_effect_mpa(M,OpA,Data0)).
% @TODO BROKEN phys ical_side_effect_call(M,assertz_i,Data0):- must((compile_aux_clauses(M:Data0))),!.
attempt_side_effect_mpa(M,OpA,Data0):- show_failure(M:call(M:OpA,M:Data0)).


/*

  b_setval(th_asserts,[]),
  call_u(G),
  b_getval(th_asserts,List).

attempt_side_effect_mpa(C) :- 
   b_getval(th_asserts,List),
   b_setval(th_asserts,[C|List]),!.



*/
%% erase_w_attvars( +Data0, ?Ref) is semidet.
%
% Erase W Attribute Variables.
%
erase_w_attvars(Data0,Ref):- attempt_side_effect(erase(Ref)),add_side_effect(erase,Data0).


%% mpred_nochaining( +Goal) is semidet.
%
% PFC No Chaining.
%
mpred_nochaining(Goal):- locally_tl(no_attempt_side_effects,call(Goal)).


%% with_chaining( +Goal) is semidet.
%
% PFC No Chaining.
%
with_chaining(Goal):- locally(- t_l:no_attempt_side_effects,call(Goal)).

% TODO ISSUE https://github.com/logicmoo/PrologMUD/issues/7


%% match_source_ref1( :TermARG1) is semidet.
%
% Match Source Ref Secondary Helper.
%
match_source_ref1(ax):-!.
match_source_ref1(mfl4(_VarNameZ,_,_,_)).

%% make_uu_remove( :TermU) is semidet.
%
% Make Uu Remove.
%
make_uu_remove((_,ax)).


%% has_functor( :TermC) is semidet.
%
% Has Functor.
%

% -- % has_functor(_):-!,fail.
has_functor(F/A):-!,is_ftNameArity(F,A),!.
has_functor(C):- (\+ is_ftCompound(C)),!,fail.
has_functor(C):- is_ftCompound(C),\+is_list(C).


%% mpred_each_literal( +P, ?E) is semidet.
%
% PFC Each Literal.
%
mpred_each_literal(P,E):-is_ftNonvar(P),P=(P1,P2),!,(mpred_each_literal(P1,E);mpred_each_literal(P2,E)).
mpred_each_literal(P,P). %:-conjuncts_to_list(P,List),member(E,List).



%% retract_eq_quitely( +H) is semidet.
%
% Retract Using (==/2) (or =@=/2) ) Quitely.
%
retract_eq_quitely(H):- call_u(retract_eq_quitely_f(H)).

%% retract_eq_quitely_f( +H) is semidet.
%
% Retract Using (==/2) (or =@=/2) ) Quitely False.
%
retract_eq_quitely_f((H:-B)):- !,clause_asserted_i(H,B,Ref),erase(Ref).
retract_eq_quitely_f(pfclog(H)):- retract_eq_quitely_f(H),fail.
retract_eq_quitely_f((H)):- clause_asserted_i(H,true,Ref),erase(Ref).


%% assert_eq_quitely( +H) is semidet.
%
% Assert Using (==/2) (or =@=/2) ) Quitely.
%
assert_eq_quitely(H):- attvar_op(db_op_call(assert,assert_if_new),H).


%% mpred_is_tautology( +Var) is semidet.
%
% PFC If Is A Tautology.
%
% :- module_transparent( (mpred_is_tautology)/1).
mpred_is_tautology(V):- (is_ftVar(V) -> true;(copy_term_nat(V,VC),numbervars(VC),mpred_is_taut(VC))),!.



%% mpred_is_taut( :TermA) is semidet.
%
% PFC If Is A Taut.
%
mpred_is_taut(A):-var(A),!.
mpred_is_taut(A:-B):-!,mpred_is_taut(B==>A).
mpred_is_taut(A<-B):-!,mpred_is_taut(B==>A).
mpred_is_taut(A<==>B):-!,(mpred_is_taut(A==>B);mpred_is_taut(B==>A)).
mpred_is_taut(A==>B):- A==B,!.
mpred_is_taut((B,_)==>A):- mpred_is_assertable(B),mpred_is_taut(A==>B),!.
mpred_is_taut((_,B)==>A):- mpred_is_assertable(B),mpred_is_taut(A==>B),!.
mpred_is_taut(B==>(A,_)):- mpred_is_assertable(A),mpred_is_taut(A==>B),!.
mpred_is_taut(B==>(_,A)):- mpred_is_assertable(A),mpred_is_taut(A==>B),!.


% baseKB:decl_database_hook(Op,Hook):- loop_check_nr(pfc_provide_storage_op(Op,Hook)).


%% is_retract_first( +VALUE1) is semidet.
%
% If Is A Retract First.
%
is_retract_first(one).
is_retract_first(a).


%% pfc_provide_storage_op( +Op, ?I1) is semidet.
%
% Prolog Forward Chaining Provide Storage Oper..
%
pfc_provide_storage_op(Op,(I1,I2)):-!,pfc_provide_storage_op(Op,I1),pfc_provide_storage_op(Op,I2).
pfc_provide_storage_op(Op,(nesc(P))):-!,pfc_provide_storage_op(Op,P).
%pfc_provide_storage_op(change(assert,_AorZ),Fact):- loop_check_nr(ainPreTermExpansion(Fact)).
% pfcRem1 to just get the first
pfc_provide_storage_op(change(retract,OneOrA),FactOrRule):- is_retract_first(OneOrA),!,
            loop_check_nr(mpred_withdraw(FactOrRule)),
  ignore((ground(FactOrRule),mpred_remove(FactOrRule))).
% mpred_remove should be forcefull enough
pfc_provide_storage_op(change(retract,all),FactOrRule):- loop_check_nr(mpred_remove(FactOrRule)),!.
% pfc_provide_storage_op(clause_u,FactOrRule):- is_ftNonvar(FactOrRule),!,loop_check_nr(clause_u(FactOrRule)).


% pfcDatabaseGoal(G):-is_ftCompound(G),get_functor(G,F,A),pfcDatabaseTerm(F/A).




%% mpred_pbody( +H, ?B, ?R, ?BIn, ?WHY) is semidet.
%
% PFC Pbody.
%
%mpred_pbody(H,B,_R,fail,deduced(backchains)):- get_bc_clause(H,_H,B),!.
%mpred_pbody(H,infoF(INFO),R,B,Why):-!,mpred_pbody_f(H,INFO,R,B,Why).
%mpred_pbody(H,B,R,BIn,WHY):- is_true(B),!,BIn=B,get_why(H,H,R,WHY).
%mpred_pbody(H,B,R,B,asserted(R,(H:-B))).


%% get_why( +VALUE1, ?CL, ?R, :TermR) is semidet.
%
% Get Generation Of Proof.
%
get_why(_,CL,R,asserted(R,CL:-U)):- clause_u(spft(CL, U, ax),true),!.
get_why(H,CL,R,deduced(R,WHY)):- (mpred_get_support(H,WH)*->WHY=(H=WH);(mpred_get_support(CL,WH),WHY=(CL=WH))).



%% mpred_pbody_f( +H, ?CL, ?R, ?B, ?WHY) is semidet.
%
% PFC Pbody False.
%
mpred_pbody_f(H,CL,R,B,WHY):- CL=(B==>HH),sub_term_eq(H,HH),!,get_why(H,CL,R,WHY).
mpred_pbody_f(H,CL,R,B,WHY):- CL=(HH<-B),sub_term_eq(H,HH),!,get_why(H,CL,R,WHY).
mpred_pbody_f(H,CL,R,B,WHY):- CL=(HH<==>B),sub_term_eq(H,HH),get_why(H,CL,R,WHY).
mpred_pbody_f(H,CL,R,B,WHY):- CL=(B<==>HH),sub_term_eq(H,HH),!,get_why(H,CL,R,WHY).
mpred_pbody_f(H,CL,R,fail,infoF(CL)):- trace_or_throw_ex(mpred_pbody_f(H,CL,R)).


%% sub_term_eq( +H, ?HH) is semidet.
%
% Sub Term Using (==/2) (or =@=/2) ).
%
sub_term_eq(H,HH):-H==HH,!.
sub_term_eq(H,HH):-each_subterm(HH,ST),ST==H,!.


%% sub_term_v( +H, ?HH) is semidet.
%
% Sub Term V.
%
sub_term_v(H,HH):-H=@=HH,!.
sub_term_v(H,HH):-each_subterm(HH,ST),ST=@=H,!.

%% all_different_head_vals(+Clause) is det.
%
% Enforces All Different Head Vals.
%
all_different_head_vals(HB):- (\+ compound(HB) ; ground(HB)),!.
all_different_head_vals(HB):- 
  mpred_rule_hb(HB,H,B),
  term_slots(H,Slots),  
  (Slots==[]->
     all_different_head_vals(B);
    (lock_vars(Slots),all_different_head_vals_2(H,Slots),unlock_vars(Slots))),!.
  

all_different_head_vals_2(_H,[]):-!.
all_different_head_vals_2(H,[A,R|EST]):-get_assertion_head_arg(_,H,E1),E1 ==A,dif(A,E2),get_assertion_head_arg(_,H,E2),\+ contains_var(A,E2),all_different_vals(dif_matrix,[A,E2,R|EST]),!.
all_different_head_vals_2(_H,[A,B|C]):-all_different_vals(dif_matrix,[A,B|C]),!.
all_different_head_vals_2(HB,_):- \+ compound(HB),!.
all_different_head_vals_2(H,[A]):-get_assertion_head_arg(_,H,E1),E1 ==A, H=..[_|ARGS], all_different_vals(dif_matrix,ARGS),!.
all_different_head_vals_2(H,[A]):-get_assertion_head_arg(_,H,E1),E1 ==A,  get_assertion_head_arg(_,H,E2), A\==E2, \+ contains_var(A,E2), dif(A,E2),!.
all_different_head_vals_2(H,[A]):-get_assertion_head_arg(_,H,E1),E1\==A, compound(E1), contains_var(A,E1), all_different_head_vals_2(E1,[A]),!.
all_different_head_vals_2(_,_).
   	 

%% mpred_rule_hb( +Outcome, ?OutcomeO, ?AnteO) is semidet.
%
% Calculate PFC Rule Head+body.
%
mpred_rule_hb(Outcome,OutcomeO,Body):- nonvar(OutcomeO),!,mpred_rule_hb(Outcome,OutcomeN,Body),must(OutcomeO=OutcomeN).
mpred_rule_hb(Outcome,OutcomeO,BodyO):- nonvar(BodyO),!,mpred_rule_hb(Outcome,OutcomeO,BodyN),must(BodyN=BodyO).
mpred_rule_hb(Outcome,OutcomeO,AnteO):- 
  quietly((mpred_rule_hb_0(Outcome,OutcomeO,Ante),
  mpred_rule_hb_0(Ante,AnteO,_))).
% :-mpred_trace_nochilds(mpred_rule_hb/3).


%% mpred_rule_hb_0( +Rule, -Head, -Body) is nondet.
%
% Calculate PFC rule Head+Body  Primary Helper.
%


mpred_rule_hb_0(Outcome,OutcomeO,true):-is_ftVar(Outcome),!,OutcomeO=Outcome.
mpred_rule_hb_0(Outcome,OutcomeO,true):- \+compound(Outcome),!,OutcomeO=Outcome.
mpred_rule_hb_0((Outcome1,Outcome2),OutcomeO,AnteO):- mpred_rule_hb(Outcome1,Outcome1O,Ante1),mpred_rule_hb(Outcome2,Outcome2O,Ante2),
                   conjoin(Outcome1O,Outcome2O,OutcomeO),
                   conjoin(Ante1,Ante2,AnteO).
mpred_rule_hb_0((Ante1==>Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Ante1=>Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Ante1->Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Ante1*->Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
% mpred_rule_hb_0((Outcome/Ante1),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(rhs([Outcome]),OutcomeO,Ante2):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
% mpred_rule_hb_0(rhs([OutcomeH|OutcomeT]),OutcomeO,Ante2):- !, mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0({Outcome},OutcomeO,Ante2):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Outcome<-Ante1),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Ante1 & Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Ante1 , Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Outcome<==>Ante1),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Ante1<==>Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(_::::Outcome,OutcomeO,Ante2):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb_0(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(bt(Outcome,Ante1),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(pt(Ante1,Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(pk(Ante1a,Ante1b,Outcome),OutcomeO,(Ante1a,Ante1b,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(nt(Ante1a,Ante1b,Outcome),OutcomeO,(Ante1a,Ante1b,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(spft(Outcome,Ante1a,Ante1b),OutcomeO,(Ante1a,Ante1b,Ante2)):- (nonvar(Outcome)-> ! ; true),mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(que(Outcome,_),OutcomeO,Ante2):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
% mpred_rule_hb_0(pfc Default(Outcome),OutcomeO,Ante2):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Outcome:-Ante),Outcome,Ante):-(nonvar(Outcome)-> ! ; true).
mpred_rule_hb_0(Outcome,Outcome,true).


%% ain_minfo( +G) is semidet.
%
% Assert If New Metainformation.
%
:- module_transparent(ain_minfo/1).
ain_minfo(G):-ain_minfo(assertz_if_new,G).

%% ain_minfo( :PRED1How, ?H) is semidet.
%
% Assert If New Metainformation.
%
:- module_transparent(ain_minfo/2).
ain_minfo(How,(H:-True)):-is_true(True),must(is_ftNonvar(H)),!,ain_minfo(How,H).
ain_minfo(How,(H<-B)):- !,ain_minfo(How,(H:-infoF(H<-B))),!,get_bc_clause(H,Post),ain_minfo(How,Post),ain_minfo_2(How,(B:-infoF(H<-B))).
ain_minfo(How,(B==>H)):- !,ain_minfo(How,(H:-infoF(B==>H))),!,ain_minfo_2(How,(B:-infoF(B==>H))).
ain_minfo(How,(B<==>H)):- !,ain_minfo(How,(H:-infoF(B<==>H))),!,ain_minfo(How,(B:-infoF(B<==>H))),!.
ain_minfo(How,((A,B):-INFOC)):-mpred_is_info(INFOC),(is_ftNonvar(A);is_ftNonvar(B)),!,ain_minfo(How,((A):-INFOC)),ain_minfo(How,((B):-INFOC)),!.
ain_minfo(How,((A;B):-INFOC)):-mpred_is_info(INFOC),(is_ftNonvar(A);is_ftNonvar(B)),!,ain_minfo(How,((A):-INFOC)),ain_minfo(How,((B):-INFOC)),!.
ain_minfo(How,(-(A):-infoF(C))):-is_ftNonvar(C),is_ftNonvar(A),!,ain_minfo(How,((A):-infoF((C)))). % attvar_op(How,(-(A):-infoF(C))).
ain_minfo(How,(~(A):-infoF(C))):-is_ftNonvar(C),is_ftNonvar(A),!,ain_minfo(How,((A):-infoF((C)))). % attvar_op(How,(-(A):-infoF(C))).
ain_minfo(How,(A:-INFOC)):- is_ftNonvar(INFOC), get_bc_clause(A,AA,INFOCC),A=AA,INFOC==INFOCC,!,attvar_op(How,(A:-INFOC)),!.
ain_minfo(How,bt(_ABOX,H,_)):-!,get_bc_clause(H,Post),attvar_op(How,Post).
ain_minfo(How,nt(H,Test,Body)):-!,attvar_op(How,(H:-fail,nt(H,Test,Body))).
ain_minfo(How,pt(H,Body)):-!,attvar_op(How,(H:-fail,pt(H,Body))).
ain_minfo(How,(A0:-INFOC0)):- mpred_is_info(INFOC0), copy_term_and_varnames((A0:-INFOC0),(A:-INFOC)),!,must((mpred_rewrap_h(A,AA),imploded_copyvars((AA:-INFOC),ALLINFO), attvar_op(How,(ALLINFO)))),!.
%ain_minfo(How,G):-mpred_trace_msg(skipped_add_meta_facts(How,G)).
ain_minfo(_,_).

:- was_export(ain_minfo_2/2).

%% ain_minfo_2( :PRED1How, ?G) is semidet.
%
% Assert If New Metainformation  Extended Helper.
%
:- module_transparent(ain_minfo_2/2).
ain_minfo_2(How,G):-ain_minfo(How,G).


%% mpred_is_info( :TermC) is semidet.
%
% PFC If Is A Info.
%
mpred_is_info((CWC,Info)):- (atom(CWC),is_a_info(CWC));mpred_is_info(Info).
mpred_is_info(mpred_bc_only(C)):-is_ftNonvar(C),!.
mpred_is_info(infoF(C)):-is_ftNonvar(C),!.
mpred_is_info(inherit_above(_,_)).


is_a_info(fail).
is_a_info(CWC):- is_pfc_chained(CWC).

is_pfc_chained(cwc).
is_pfc_chained(awc).
is_pfc_chained(zwc).
is_pfc_chained(fwc).
is_pfc_chained(bwc).
is_pfc_chained(wac).



:- module_transparent(is_ain_clause/2).
is_ain_clause( _, Var):- var(Var),!, fail.
is_ain_clause( M,(:- Body)):- !, is_ain_body(M,Body),!.
is_ain_clause( M,(P:- Body)):- !,(is_ain_head(M,P);is_ain_body(M,Body)),!.
is_ain_clause( M,(P)):- !, is_ain_head(M, P).

:- module_transparent(is_ain_head/2).
is_ain_head(_, P):- var(P),!.
is_ain_head(_,(_,_)):- !.
is_ain_head(_,(_;_)):- !.
is_ain_head(_,not(_)):- !.
is_ain_head(_,\+(_)):- !.
is_ain_head(M, P):- is_ain_body(M, P),!.
is_ain_head(_,==>(_)):- !.
is_ain_head(_,==>(_,_)):- !.
is_ain_head(_,<==>(_,_)):- !.
is_ain_head(_,<==(_)):- !.
is_ain_head(_,<==(_,_)):- !.
is_ain_head(_,'::::'(_,_)):- !.
is_ain_head(baseKB,_).
is_ain_head(_,=>(_)):- !.
is_ain_head(_,=>(_,_)):- !.
is_ain_head(_,_):- get_how_virtualize_file(Lang),!,Lang=heads.

:- module_transparent(is_ain_body/2).
is_ain_body(_, P):- var(P),!,fail.
is_ain_body(M, (P,_)):- !, nonvar(P), is_ain_body(M, P).
is_ain_body(_, CWC):- atom(CWC),  is_pfc_chained(CWC).
is_ain_body(M, P):- functor(P,F,A), \+ \+ mpred_prop(M,F,A,_), !,
  \+ (mpred_prop(M,F,A,Prop), is_pfc_prolog_only_prop(Prop)).
is_ain_body(M, MP):- strip_module(MP,M2,P), M2\==M, !,is_ain_body(M2,P).

is_pfc_prolog_only_prop(prologOnly).
is_pfc_prolog_only_prop(prologBuiltin).


%cwc(Call):- callable(Call),Call.

%:- was_dynamic(not_not/1).

%% mpred_rewrap_h( +A, ?A) is semidet.
%
% PFC Rewrap Head.
%
mpred_rewrap_h(A,A):- is_ftNonvar(A),\+ is_static_predicate(A).
mpred_rewrap_h(A,F):- functor(A,F,_),\+ is_static_predicate(F),!.
%mpred_rewrap_h(A,not_not(A)):-!.


%% cwc is det.
%
% Cwc.
%
cwc:-true.

%% fwc is det.
%
% Fwc.
%
fwc:-true.

%% bwc is semidet.
%
% Bwc.
%
bwc:-true.

%% wac is semidet.
%
% Wac.
%
wac:-true.

awc:-true.
zwc:-true.


%% is_fc_body( +P) is semidet.
%
% If Is A Forward Chaining Body.
%
is_fc_body(P):- has_body_atom(fwc,P).

%% is_bc_body( +P) is semidet.
%
% If Is A Backchaining Body.
%
is_bc_body(P):- has_body_atom(bwc,P).

%% is_action_body( +P) is semidet.
%
% If Is A Action Body.
%
is_action_body(P):- has_body_atom(wac,P).



%% has_body_atom( +WAC, ?P) is semidet.
%
% Has Body Atom.
%
has_body_atom(WAC,P):- call(
   WAC==P -> true ; (is_ftCompound(P),get_assertion_head_arg(1,P,E),has_body_atom(WAC,E))),!.

/*
has_body_atom(WAC,P,Rest):- call(WAC==P -> Rest = true ; (is_ftCompound(P),functor(P,F,A),is_atom_body_pfa(WAC,P,F,A,Rest))).
is_atom_body_pfa(WAC,P,F,2,Rest):-get_assertion_head_arg(1,P,E),E==WAC,get_assertion_head_arg(2,P,Rest),!.
is_atom_body_pfa(WAC,P,F,2,Rest):-get_assertion_head_arg(2,P,E),E==WAC,get_assertion_head_arg(1,P,Rest),!.
*/

:- module_transparent( (get_assertion_head_arg)/3).
get_assertion_head_arg(N,P,E):-get_assertion_head_unnegated(P,PP),!,arg(N,PP,E).

same_functors(Head1,Head2):-must_det(get_unnegated_functor(Head1,F1,A1)),must_det(get_unnegated_functor(Head2,F2,A2)),!,F1=F2,A1=A2.


%% mpred_update_literal( +P, ?N, ?Q, ?R) is semidet.
%
% PFC Update Literal.
%
mpred_update_literal(P,N,Q,R):-
    get_assertion_head_arg(N,P,UPDATE),call(replace_arg(P,N,Q_SLOT,Q)),
    must(call_u(Q)),update_value(Q_SLOT,UPDATE,NEW), 
    replace_arg(Q,N,NEW,R).


% spft(5,5,5).

%% update_single_valued_arg(+Module, +P, ?N) is semidet. 
%
% Update Single Valued Argument.
%
:- module_transparent( (update_single_valued_arg)/3).

update_single_valued_arg(M,M:Pred,N):-!,update_single_valued_arg(M,Pred,N).
update_single_valued_arg(_,M:Pred,N):-!,update_single_valued_arg(M,Pred,N).

update_single_valued_arg(world,P,N):- !, update_single_valued_arg(baseKB,P,N).
update_single_valued_arg(M,P,N):- break, \+ clause_b(mtHybrid(M)), trace, clause_b(mtHybrid(M2)),!,
   update_single_valued_arg(M2,P,N).

update_single_valued_arg(M,P,N):- 
  get_assertion_head_arg(N,P,UPDATE),
  is_relative(UPDATE),!,
  dtrace,
  break,
  replace_arg(P,N,OLD,Q),
  must_det_l((clause_u(Q),update_value(OLD,UPDATE,NEW),\+ is_relative(NEW), replace_arg(Q,N,NEW,R))),!,
  update_single_valued_arg(M,R,N).


update_single_valued_arg(M,P,N):- 
 call_u((must_det_l((

  call_u(mtHybrid(M)),
  mpred_type_args \= M,
  mpred_kb_ops \= M,
  get_assertion_head_arg(N,P,UPDATE),
  replace_arg(P,N,Q_SLOT,Q),
  var(Q_SLOT),
  same_functors(P,Q),
  % current_why(U),
  must_det_l((
     % rtrace(attvar_op(assert_if_new,M:spft(P,U,ax))),
     % (call_u(P)->true;(assertz_mu(P))),
     assertz_mu(M,P),
     doall((
          lookup_u(M:Q,E),
          UPDATE \== Q_SLOT,
          erase(E),
          mpred_unfwc1(M:Q))))))))).

% ======================= 
% utils
% ======================= 

%% map_literals( +P, ?G) is semidet.
%
% Map Literals.
%
map_literals(P,G):-map_literals(P,G,[]).


%% map_literals( +VALUE1, :TermH, ?VALUE3) is semidet.
%
% Map Literals.
%
map_literals(_,H,_):-is_ftVar(H),!. % skip over it
map_literals(_,[],_) :- !.
map_literals(Pred,(H,T),S):-!, apply(Pred,[H|S]), map_literals(Pred,T,S).
map_literals(Pred,[H|T],S):-!, apply(Pred,[H|S]), map_literals(Pred,T,S).
map_literals(Pred,H,S):- mpred_literal(H),must(apply(Pred,[H|S])),!.
map_literals(_Pred,H,_S):- \+ is_ftCompound(H),!. % skip over it
map_literals(Pred,H,S):-H=..List,!,map_literals(Pred,List,S),!.



%% map_unless( :PRED1Test, ?Pred, ?H, ?S) is semidet.
%
% Map Unless.
%
map_unless(Test,Pred,H,S):- call(Test,H),ignore(apply(Pred,[H|S])),!.
map_unless(_Test,_,[],_) :- !.
map_unless(_Test,_Pred,H,_S):- \+ is_ftCompound(H),!. % skip over it
map_unless(Test,Pred,(H,T),S):-!, apply(Pred,[H|S]), map_unless(Test,Pred,T,S).
map_unless(Test,Pred,[H|T],S):-!, apply(Pred,[H|S]), map_unless(Test,Pred,T,S).
map_unless(Test,Pred,H,S):-H=..List,!,map_unless(Test,Pred,List,S),!.


:- meta_predicate(map_first_arg(:,+)).
%% map_first_arg( +Pred, ?List) is semidet.
%
% PFC Maptree.
%
map_first_arg(CM:Pred,List):-map_first_arg(CM,Pred,List,[]).

:- meta_predicate(map_first_arg(+,*,+,+)).
%% map_first_arg( +Pred, :TermH, ?S) is semidet.
%
% PFC Maptree.
%
map_first_arg(CM,Pred,H,S):-is_ftVar(H),!,CM:apply(Pred,[H|S]).
map_first_arg(_,_,[],_) :- !.
map_first_arg(CM,Pred,(H,T),S):-!, map_first_arg(CM,Pred,H,S), map_first_arg(CM,Pred,T,S).
map_first_arg(CM,Pred,(H;T),S):-!, map_first_arg(CM,Pred,H,S) ; map_first_arg(CM,Pred,T,S).
map_first_arg(CM,Pred,[H|T],S):-!, CM:apply(Pred,[H|S]), map_first_arg(CM,Pred,T,S).
map_first_arg(CM,Pred,H,S):- CM:apply(Pred,[H|S]). 

:- fixup_exports.

% % :- ensure_loaded(logicmoo(util/rec_lambda)).

%example pfcVerifyMissing(mpred_isa(I,D), mpred_isa(I,C), ((mpred_isa(I,C), {D==C});-mpred_isa(I,C))). 
%example pfcVerifyMissing(mudColor(I,D), mudColor(I,C), ((mudColor(I,C), {D==C});-mudColor(I,C))). 


%% pfcVerifyMissing( +GC, ?GO, ?GO) is semidet.
%
% Prolog Forward Chaining Verify Missing.
%
pfcVerifyMissing(GC, GO, ((GO, {D==C});\+ GO) ):-  GC=..[F,A|Args],append(Left,[D],Args),append(Left,[C],NewArgs),GO=..[F,A|NewArgs],!.

%example mpred_freeLastArg(mpred_isa(I,C),~(mpred_isa(I,C))):-is_ftNonvar(C),!.
%example mpred_freeLastArg(mpred_isa(I,C),(mpred_isa(I,F),C\=F)):-!.

%% mpred_freeLastArg( +G, ?GG) is semidet.
%
% PFC Free Last Argument.
%
mpred_freeLastArg(G,GG):- G=..[F,A|Args],append(Left,[_],Args),append(Left,[_],NewArgs),GG=..[F,A|NewArgs],!.
mpred_freeLastArg(_G,false).


%% mpred_current_op_support( +VALUE1) is semidet.
%
% PFC Current Oper. Support.
%
mpred_current_op_support((p,p)):-!.


%% pfcVersion( +VALUE1) is semidet.
%
% Prolog Forward Chaining Version.
%
pfcVersion(6.6).


% % :- '$set_source_module'(mpred_kb_ops).

%% correctify_support( +S, ?S) is semidet.
%
% Correctify Support.
%
correctify_support(U,(U,ax)):-var(U),!.
correctify_support((U,U),(U,ax)):-!.
correctify_support((S,T),(S,T)):-!.
correctify_support((U,_UU),(U,ax)):-!.
correctify_support([U],S):-correctify_support(U,S).
correctify_support(U,(U,ax)).


%% clause_asserted_local( :TermABOX) is semidet.
%
% Clause Asserted Local. 
%
clause_asserted_local(MCL):-
  strip_module(MCL,_,CL),
  must(CL=spft(P,Fact,Trigger )),!,
  clause_u(spft(P,Fact,Trigger),true,Ref),
  clause_u(spft(UP,UFact,UTrigger),true,Ref),
  (((UP=@=P,UFact=@=Fact,UTrigger=@=Trigger))).



%% is_already_supported( +P, ?S, ?UU) is semidet.
%
% If Is A Already Supported.
%
is_already_supported(P,(S,T),(S,T)):- clause_asserted_local(spft(P,S,T)),!.
is_already_supported(P,_S,UU):- clause_asserted_local(spft(P,US,UT)),must(get_source_uu(UU)),UU=(US,UT).

% TOO UNSAFE 
% is_already_supported(P,_S):- copy_term_and_varnames(P,PC),sp ftY(PC,_,_),P=@=PC,!.


if_missing1(Q):- mpred_literal_nv(Q), call_u( \+ ~ Q), if_missing_mask(Q,R,Test),!, lookup_u(R), Test.


%% if_missing_mask( +Q, ?R, ?Test) is semidet.
%
% If Missing Mask.
%

if_missing_mask(M:Q,M:R,M:Test):- nonvar(Q),!,if_missing_mask(Q,R,Test).
if_missing_mask(Q,~Q,\+Q):- \+ is_ftCompound(Q),!.

%if_missing_mask(ISA, ~ ISA, \+ ISA):- functor(ISA,F,1),(F==tSwim;call_u(functorDeclares(F))),!.
if_missing_mask(HB,RO,TestO):- once(mpred_rule_hb(HB,H,B)),B\==true,HB\==H,!,
     if_missing_mask(H,R,TestO),subst(HB,H,R,RO).

if_missing_mask(ISA, ISA, \+ ISA):- functor(ISA, _F,1),!.% (F==tSwim;call_u(functorDeclares(F))),!.

if_missing_mask(Q,R,Test):-
   which_missing_argnum(Q,N),
   if_missing_n_mask(Q,N,R,Test),!.

if_missing_mask(ISA, ~ ISA, \+ ISA).

%% if_missing_n_mask( +Q, ?N, ?R, ?Test) is semidet.
%
% If Missing Mask.
%
if_missing_n_mask(Q,N,R,Test):-
  get_assertion_head_arg(N,Q,Was),
  (nonvar(R)-> (which_missing_argnum(R,RN),get_assertion_head_arg(RN,R,NEW));replace_arg(Q,N,NEW,R)),!,
   Test=dif:dif(Was,NEW).

/*
Old version
if_missing_mask(Q,N,R,dif:dif(Was,NEW)):- 
 must((is_ftNonvar(Q),acyclic_term(Q),acyclic_term(R),functor(Q,F,A),functor(R,F,A))),
  (singleValuedInArg(F,N) -> 
    (get_assertion_head_arg(N,Q,Was),replace_arg(Q,N,NEW,R));
    ((get_assertion_head_arg(N,Q,Was),is_ftNonvar(Was)) -> replace_arg(Q,N,NEW,R);
        (N=A,get_assertion_head_arg(N,Q,Was),replace_arg(Q,N,NEW,R)))).
*/


%% which_missing_argnum( +VALUE1, ?VALUE2) is semidet.
%
% Which Missing Argnum.
%
which_missing_argnum(Q,N):- compound(Q),\+ compound_name_arity(Q,_,0),
 must((acyclic_term(Q),is_ftCompound(Q),get_functor(Q,F,A))),
 F\=t,
  (call_u(singleValuedInArg(F,N)) -> true; which_missing_argnum(Q,F,A,N)).

which_missing_argnum(_,_,1,_):-!,fail.
which_missing_argnum(Q,_F,A,N):- between(A,1,N),get_assertion_head_arg(N,Q,Was),is_ftNonvar(Was).

mpred_run_pause:- asserta(t_l:mpred_run_paused).
mpred_run_resume:- retractall(t_l:mpred_run_paused).

without_running(G):- (t_l:mpred_run_paused->G;locally_tl(mpred_run_pause,G)).

mpred_remove_file_support(_File):- !.
mpred_remove_file_support(File):- 
  forall((filematch(File,File0),freeze(Match,contains_var(File0,Match))),
      forall(lookup_u(spft( W, Match, ax)),forall(retract_u(spft( W, Match, ax)),mpred_remove(W)))).

/*

%% remove_if_unsupported( +Why, ?P) is semidet.
%
% Remove If Unsupported.
%
remove_if_unsupported(Why,P) :- is_ftVar(P),!,trace_or_throw_ex(warn(var_remove_if_unsupported(Why,P))).
remove_if_unsupported(Why,P) :- ((\+ ground(P), P \= (_:-_) , P \= ~(_) ) -> mpred_trace_msg(warn(nonground_remove_if_unsupported(Why,P))) ;true),
   (((mpred_tms_supported(local,P,How),How\=unknown(_)) -> mpred_trace_msg(still_supported(How,Why,local,P)) ; (  mpred_undo(Why,P)))),!.
   % mpred_run.

*/

%= mpred_tms_supported(+P,-How) succeeds if P is "supported". What "How" means
%= depends on the TMS mode selected.


%% mpred_tms_supported( +P, ?How) is semidet.
%
% PFC Truth Maintainence/wff Supported.
%
mpred_tms_supported(P,How) :-
  lookup_u(tms(Mode)),
  mpred_tms_supported0(Mode,P,How).



%% mpred_tms_supported( +Mode, ?P, ?How) is semidet.
%
% PFC Truth Maintainence/wff Supported.
%
mpred_tms_supported(Mode,P,How) :- is_ftVar(Mode),get_tms_mode(P,tms(Mode)),!,mpred_tms_supported0(Mode,P,How).
mpred_tms_supported(Mode,P,How) :- mpred_tms_supported0(Mode,P,How).
mpred_tms_supported(How,_P,unknown(How)).

:- module_transparent((mpred_wfflist)/2).
:- module_transparent((mpred_wff)/3).


%% mpred_tms_supported0( +TmsMode, ?P, ?How) is semidet.
%
% PFC Truth Maintainence/wff Supported Primary Helper.
%
mpred_tms_supported0(local,P,How) :-  mpred_get_support(P,How). % ,sanity(mpred_deep_support(How,S)).
mpred_tms_supported0(cycles,P,How) :-  well_founded(P,How).
mpred_tms_supported0(deep,P,How) :- mpred_deep_support(How,P).

% baseKB:hook_one_minute_timer_tick:- statistics.

%% well_founded( +Fact, ?How) is semidet.
%
% a fact is well founded if it is supported by the user
% or by a set of facts and a rules, all of which are well founded.
%
well_founded(Fact,How) :- mpred_wff(Fact,[],How).



%% mpred_wff( ?F, ?VALUE2, :TermHow) is semidet.
%
% PFC Well-formed Formula.
%
mpred_wff(F,_,How) :-
  % supported by user (mpred_axiom) or an "absent" fact (assumption).
  ((mpred_axiom(F),How =mpred_axiom(F) ); (mpred_assumption(F),How=mpred_assumption(F))),
  !.

mpred_wff(F,Descendants,wff(Supporters)) :-
  % first make sure we aren''t in a loop.
  (\+ memberchk(F,Descendants)),
  % find a justification.
  supporters_list(F,Supporters),
  % all of whose members are well founded.
  mpred_wfflist(Supporters,[F|Descendants]),
  !.



%% mpred_wfflist(+L1, ?L2) is semidet.
%
%  simply maps mpred_wff over the list.
%
mpred_wfflist([],_).
mpred_wfflist([X|Rest],L) :-
  mpred_wff(X,L,_How),
  mpred_wfflist(Rest,L).


%% mpred_scan_tms( +P) is semidet.
%
% PFC Scan Truth Maintainence/wff.
%
mpred_scan_tms(P):-mpred_get_support(P,(S,SS)),
  (S==SS-> true;
   once((mpred_deep_support(_How,P)->true;
     (mpred_trace_msg(warn(now_maybe_unsupported(mpred_get_support(P,(S,SS)),fail))))))).


%% user_atom( +U) is semidet.
%
% User Atom.
%
user_atom(mfl4(_VarNameZ,_,_,_)):-!.
user_atom(ax).
user_atom(s(_)).


%% mpred_deep_support( +How, ?M) is semidet.
%
% PFC Deep Support.
%
mpred_deep_support(_How,unbound):-!,fail.
mpred_deep_support(How,M):-nr_lc_ex(mpred_deep_support0(How,M)).


%% mpred_deep_support0( +U, ?U) is semidet.
%
% PFC Deep Support Primary Helper.
%
mpred_deep_support0(user_atom(U),(U,ax)):-user_atom(U),!.
mpred_deep_support0(How,(A==>_)):-!,mpred_deep_support(How,A).
mpred_deep_support0(pt(HowA,HowB),pt(A,B)):-!,mpred_deep_support(HowA,A),mpred_deep_support(HowB,B).
mpred_deep_support0(HowA->HowB,(A->B)):-!,mpred_deep_support(HowA,A),mpred_deep_support(HowB,B).
mpred_deep_support0(HowA/HowB,(A/B)):-!,mpred_deep_support(HowA,A),mpred_deep_support(HowB,B).
mpred_deep_support0((HowA,HowB),(A,B)):-!,mpred_deep_support(HowA,A),mpred_deep_support(HowB,B).
mpred_deep_support0(How,rhs(P)):-!,maplist(mpred_deep_support,How,P).
mpred_deep_support0(mpred_call_only_facts(\+ P),\+ call_u(P)):-!,mpred_call_only_facts(\+ P).
mpred_deep_support0(mpred_call_only_facts(P),call_u(P)):-!,mpred_call_only_facts(P).
mpred_deep_support0(mpred_call_only_facts(P),{P}):-!,mpred_call_only_facts(P).
mpred_deep_support0(S==>How,P):-mpred_get_support(P,S),mpred_deep_support(How,S),!.
mpred_deep_support0(mpred_call_only_facts(\+(P)),\+(P)):-!, mpred_call_only_facts(\+(P)).
mpred_deep_support0(user_atom(P),P):-user_atom(P),!.
mpred_deep_support0(mpred_call_only_facts((P)),P):-mpred_call_only_facts(P).


%% mpred_get_support_precanonical_plus_more( +P, ?Sup) is semidet.
%
% PFC Get Support Precanonical Plus More.
%
mpred_get_support_precanonical_plus_more(P,Sup):- 
  mpred_get_support_one(P,Sup)*->true;
  ((fully_expand(mpred_get_support_precanonical_plus_more,P,PE),!,
    P\=@=PE,mpred_get_support_one(PE,Sup))).

%% mpred_get_support_one( +P, ?Sup) is semidet.
%
% PFC Get Support One.
%
mpred_get_support_one(P,Sup):- mpred_get_support(P,Sup)*->true;
  (mpred_get_support_via_clause_db(P,Sup)*->true;
     mpred_get_support_via_sentence(P,Sup)).


%% mpred_get_support_via_sentence( +Var, ?VALUE2) is semidet.
%
% PFC Get Support Via Sentence.
%
mpred_get_support_via_sentence(Var,_):-is_ftVar(Var),!,fail.
mpred_get_support_via_sentence((A,B),(FC,TC)):-!, mpred_get_support_precanonical_plus_more(A,(FA,TA)),mpred_get_support_precanonical_plus_more(B,(FB,TB)),conjoin(FA,FB,FC),conjoin(TA,TB,TC).
mpred_get_support_via_sentence(true,g):-!.
mpred_get_support_via_sentence(G,call_u(G)):- call_u(G).



%% mpred_get_support_via_clause_db( :TermP, ?OUT) is semidet.
%
% PFC Get Support Via Clause Database.
%
mpred_get_support_via_clause_db(\+ P,OUT):- mpred_get_support_via_clause_db(~(P),OUT).
mpred_get_support_via_clause_db(\+ P,(naf(g),g)):- !, predicate_property(P,number_of_clauses(_)),\+ clause(P,_Body).
mpred_get_support_via_clause_db(P,OUT):- predicate_property(P,number_of_clauses(N)),N>0,
   clause_u(P,Body),(Body==true->Sup=(g);
    (support_ok_via_clause_body(P),mpred_get_support_precanonical_plus_more(Body,Sup))),
   OUT=(Sup,g).



%% support_ok_via_clause_body( +H) is semidet.
%
% Support Ok Via Clause Body.
%
support_ok_via_clause_body(_H):-!,fail.
support_ok_via_clause_body(H):- get_functor(H,F,A),support_ok_via_clause_body(H,F,A).


%% support_ok_via_clause_body( +VALUE1, ?F, ?VALUE3) is semidet.
%
% Support Ok Via Clause Body.
%
support_ok_via_clause_body(_,(\+),1):-!,fail.
support_ok_via_clause_body(_,F,_):- lookup_u(rtArgsVerbatum(F)),!,fail.
support_ok_via_clause_body(H,F,A):- should_call_for_facts(H,F,A).




%% mpred_get_support_precanonical( +F, ?Sup) is semidet.
%
% PFC Get Support Precanonical.
%
mpred_get_support_precanonical(F,Sup):-fully_expand(mpred_get_support_precanonical,F,P),mpred_get_support(P,Sup).

%% spft_precanonical( +F, ?SF, ?ST) is semidet.
%
% Spft Precanonical.
%

spft_precanonical(F,SF,ST):- fully_expand(spft_precanonical,F,P),!,mpred_get_support(P,(SF,ST)).
                                                      

%% trigger_supporters_list( +U, :TermARG2) is semidet.
%
% Trigger Supports Functor (list Version).
%
trigger_supporters_list(U,[]) :- match_source_ref1(U),!.
trigger_supporters_list(U,[]) :- atom(U),!.

trigger_supporters_list(Trigger,[Fact|MoreFacts]) :-
  mpred_get_support_precanonical_plus_more(Trigger,(Fact,AnotherTrigger)),
  must(trigger_supporters_list(AnotherTrigger,MoreFacts)).

mpred_retry(G):- fail; quietly(G).


%% { ?G} is semidet.
%
% an escape construct for bypassing the FOL''s salient body goals. 
% 
%
:- meta_predicate('{}'(*)).
:- module_transparent( ({})/1).
'{}'(G):- call_u(G).
:- sexport(({})/1).

%% neg_in_code( +G) is semidet.
%
% Negated In Code.
%
:- meta_predicate neg_in_code(*).
:- export(neg_in_code/1).
neg_in_code(G):- nr_lc_ex((neg_in_code0(G))).
                                                   
% :- kb_shared(baseKB:proven_neg/1).

:- meta_predicate neg_in_code0(*).
:- export(neg_in_code0/1).
/*
neg_in_code0(G):- cwc, nr_lc_ex(proven_neg(G)).
neg_in_code0(G):- cwc, var(G),!,nr_lc_ex(lookup_u(~ G)).
neg_in_code0(call_u(G)):- !,neg_in_code0(G).
neg_in_code0(~(G)):- nonvar(G),!,  \+ nr_lc_ex(~G) ,!.
neg_in_code0(G):-  is_ftNonvar(G), a(prologSingleValued,G),
      must((if_missing_mask(G,R,Test),nonvar(R),nonvar(Test))),call_u(R),!,call_u(Test).
neg_in_code0(G):- cwc, clause(~G,Call)*-> call_u(Call).
*/
neg_in_code0(G):- nr_lc_ex(neg_may_naf(G)), \+ nr_lc_ex(G),!.
% neg_in_code0(_:G):-!,baseKB:neg_in_code0(G).


:- meta_predicate neg_may_naf(*).
:- module_transparent(neg_may_naf/1).
:- export(neg_may_naf/1).

%% neg_may_naf( :GoalP) is semidet.
%
% Negated May Negation-by-faliure.
%
neg_may_naf(P):- mpred_non_neg_literal(P),get_functor(P,F),clause_u(prologNegByFailure(F),true),!.
neg_may_naf(P):- is_ftCompound(P),is_never_pfc(P).
                                          

%% call_u_req( +G) is semidet.
%
% Req.
%
call_u_req(G):- nr_lc_ex(call_u(G)).


%% mpred_call_only_facts(:Fact) is nondet.
%% mpred_call_only_facts(+Why,:Fact) is nondet.
%
% PFC Call Only Facts.
%
% is true iff Fact is a fact available for forward chaining.
%
% Note that this has the side effect [maybe] of catching unsupported facts and
% assigning them support from God. (g,ax)
%
mpred_call_only_facts(_Why,Clause):- mpred_call_only_facts(Clause).
mpred_call_only_facts(Clause) :-  
   strip_module(Clause,_,H), 
   on_x_debug(nr_lc_ex(locally_tl(infAssertedOnly(H),call_u(H)))). 



% TODO: test removal
%mpred_call_0(prologHybrid(H)):-get_functor(H,F),!,isa_asserted(F,prologHybrid).



%% call_with_bc_triggers( +MP) is semidet.
%
% Call Using Backchaining Triggers.
%
call_with_bc_triggers(MP) :- strip_module(MP,_,P), functor(P,F,A), \+ t_l:infBackChainPrevented(F/A), 
  lookup_u(bt(P,Trigger)),
  no_repeats(mpred_get_support(bt(P,Trigger),S)),
  once(no_side_effects(P)),
  locally_tl(infBackChainPrevented(F/A),mpred_eval_lhs(Trigger,S)).


:- thread_local t_l:infBackChainPrevented/1.

%% mpred_call_with_no_triggers( +Clause) is semidet.
%
% PFC Call Using No Triggers.
%
mpred_call_with_no_triggers(Clause) :-  strip_module(Clause,_,F),
  % = this (is_ftVar(F)) is probably not advisable due to extreme inefficiency.
  (is_ftVar(F)    ->  mpred_facts_and_universe(F) ;
     mpred_call_with_no_triggers_bound(F)).


%% mpred_call_with_no_triggers_bound( +F) is semidet.
%
% PFC Call Using No Triggers Bound.
%
mpred_call_with_no_triggers_bound(F):- mpred_call_with_no_triggers_uncaugth(F).

%% mpred_call_with_no_triggers_uncaugth( +Clause) is semidet.
%
% PFC Call Using No Triggers Uncaugth.
%
mpred_call_with_no_triggers_uncaugth(Clause) :-  strip_module(Clause,_,F),
  show_failure(mpred_call_with_no_triggers_bound,no_side_effects(F)),
  (\+ current_predicate(_,F) -> fail;call_u(F)).
  %= we check for system predicates as well.
  %has_cl(F) -> (clause_u(F,Condition),(Condition==true->true;call_u(Condition)));
  %call_u(F).


%% mpred_bc_only( +M) is semidet.
%
% PFC Backchaining Only.
%

%mpred_bc_only(G):- !,defaultAssertMt(W), nr_lc_ex(mpred_BC_w_cache(W,G)).
%mpred_bc_only(M:G):- !, nr_lc_ex(with_umt(M,mpred_bc_only0(G))).
mpred_bc_only(G):- nr_lc_ex((mpred_bc_only0(G))).

%% mpred_bc_only0( +G) is semidet.
%
% PFC Backchaining Only Primary Helper.
%
mpred_bc_only0(G):- mpred_unnegate(G,Pos),!, show_call(why,\+ mpred_bc_only(Pos)).
% mpred_bc_only0(G):- pfcBC_NoFacts(G).
mpred_bc_only0(G):- mpred_BC_w_cache(G,G).

% mpred_bc_only0(G):- mpred_call_only_facts(G).


%% mpred_bc_and_with_pfc( +M) is semidet.
%
% PFC Backchaining + FACTS + Inheritance.
%
mpred_bc_and_with_pfc(G):- mpred_bc_and_with_pfc_0(G).

mpred_bc_and_with_pfc_0(G):- loop_check(mpred_call_only_facts(G)). % was missing
mpred_bc_and_with_pfc_0(G):- mpred_bc_only0(G).
mpred_bc_and_with_pfc_0(G):- strip_module(G,M,P),inherit_above(M,P).




% % :- '$set_source_module'(mpred_kb_ops).

%%
%= pfcBC_NoFacts(F) is true iff F is a fact available for backward chaining ONLY.
%= Note that this has the side effect of catching unsupported facts and
%= assigning them support from God.
%= this Predicate should hide Facts from mpred_bc_only/1
%%

%% pfcBC_NoFacts( +F) is semidet.
%
% Prolog Forward Chaining Backtackable Class No Facts.
%
pfcBC_NoFacts(F):- pfcBC_NoFacts_TRY(F)*-> true ; (mpred_slow_search,pfcBC_Cache(F)).


%% mpred_slow_search is semidet.
%
% PFC Slow Search.
%
mpred_slow_search.


/*
%% ruleBackward( +R, ?Condition) is semidet.
%
% Rule Backward.
%
ruleBackward(R,Condition):- call_u(( ruleBackward0(R,Condition),functor(Condition,F,_),
  \+ consequent_arg(_,v(call_u_no_bc,call,call_u),F))).
%ruleBackward0(F,Condition):-clause_u(F,Condition),\+ (is_true(Condition);mpred_is_info(Condition)).

%% ruleBackward0( +F, ?Condition) is semidet.
%
% Rule Backward Primary Helper.
%
ruleBackward0(F,Condition):- call_u((  '<-'(F,Condition),\+ (is_true(Condition);mpred_is_info(Condition)) )).

%{X}:-dmsg_pretty(legacy({X})),call_u(X).
*/


%% pfcBC_NoFacts_TRY( +F) is semidet.
%
% Prolog Forward Chaining Backtackable Class No Facts Try.
%


pfcBC_NoFacts_TRY(F) :- no_repeats(ruleBackward(F,Condition,Support)),
  % neck(F),
  copy_term((Condition,Support),(CCondition,SupportC)),
  no_repeats(F,call_u(Condition)),  
  maybe_support_bt(F,CCondition,SupportC).

maybe_support_bt(P,_,_):-mpred_ignored(P),!.
maybe_support_bt(F,Condition,Support):-  
  doall((no_repeats(Why,call_u(bt(F,pt(A,Why)))) *-> mpred_add_support_fast(F,(A,Why)))),
  doall((no_repeats(Why,call_u(bt(F,Why))) *-> mpred_add_support_fast(F,(bt(F,Why),Support)))),
  ignore((maybeSupport(F,(Condition,Support)))).

:- meta_predicate mpred_why_all(*).
mpred_why_all(Call):- !,
      call_u(Call),
      doall((
        lookup_u(Call),
        ignore(show_failure(mpred_why(Call))),
        dmsg_pretty(result=Call),nl)),
   forall(Call,ignore(show_failure(mpred_why(Call)))).

mpred_why_all(Call):-
      doall((
        call_u(Call),
        ignore(show_failure(mpred_why(Call))),
        dmsg_pretty(result=Call),nl)).



%% pfcBC_Cache( +F) is semidet.
%
% Prolog Forward Chaining Backtackable Class Cache.
%
pfcBC_Cache(F) :- mpred_call_only_facts(pfcBC_Cache,F),
   ignore((ground(F),( (\+call_u(F)), maybeSupport(F,(g,ax))))).



%% maybeSupport( +P, ?VALUE2) is semidet.
%
% Maybe Support.
%
maybeSupport(P,_):-mpred_ignored(P),!.
maybeSupport(P,S):- fail, ( \+ ground(P)-> true;
  (predicate_property(P,dynamic)->mpred_post(P,S);true)).

maybeSupport(P,S):- 
   mpred_add_support_fast(P,S),
   maybeMaybeAdd(P,S).
 
maybeMaybeAdd(P,_):- \+ predicate_property(P,dynamic),!.
maybeMaybeAdd(P,_):- \+ \+ clause_u(P,true),!.
maybeMaybeAdd(P,S):- 
 locally_tl(assert_dir(a),
    assert_u_confirmed_was_missing(P)),
   mpred_trace_op(add,P,S),
   mpred_enqueue(P,S).


%% mpred_ignored( :TermC) is semidet.
%
% PFC Ignored.
%
mpred_ignored(argIsa(F, A, argIsaFn(F, A))).
mpred_ignored(genls(A,A)).
mpred_ignored(isa(tCol,tCol)).
%mpred_ignored(isa(W,tCol)):-mreq(baseKB:hasInstance_dyn(tCol,W)).
mpred_ignored(isa(W,_)):-is_ftCompound(W),call_u(isa(W,meta_argtypes)).
mpred_ignored(C):-clause_safe(C,true). 
mpred_ignored(isa(_,Atom)):-atom(Atom),atom_concat(ft,_,Atom),!.
mpred_ignored(isa(_,argIsaFn(_, _))).



%% has_cl( +H) is semidet.
%
% Has Clause.
%
has_cl(H):-predicate_property(H,number_of_clauses(_)).

% an action is undoable if there exists a method for undoing it.


%% mpred_negation_w_neg( +P, ?NF) is semidet.
%
% PFC Negation W Negated.
%
mpred_negation_w_neg(~(P),P):-is_ftNonvar(P),!.
mpred_negation_w_neg(P,NF):-mpred_nf1_negation(P,NF).


%% hook_one_minute_timer_tick is semidet.
%
% Hook To [baseKB:hook_one_minute_timer_tick/0] For Module Mpred_pfc.
% Hook One Minute Timer Tick.
%
baseKB:hook_one_minute_timer_tick:-mpred_cleanup.


%% mpred_cleanup is semidet.
%
% PFC Cleanup.
%
mpred_cleanup:- forall((no_repeats(F-A,(call_u(mpred_prop(M,F,A,pfcRHS)),A>1))),mpred_cleanup(M,F,A)).


%% mpred_cleanup(M, +F, ?A) is semidet.
%
% PFC Cleanup.
%
mpred_cleanup(M,F,A):-functor(P,F,A),predicate_property(P,dynamic)->mpred_cleanup_0(M,P);true.


%% mpred_cleanup_0(M, +P) is semidet.
%
% PFC cleanup  Primary Helper.
%
mpred_cleanup_0(M,P):- findall(P-B-Ref,M:clause(P,B,Ref),L),
  M:forall(member(P-B-Ref,L),erase_w_attvars(clause(P,B,Ref),Ref)),forall(member(P-B-Ref,L),M:attvar_op(db_op_call(assertz,assertz_if_new),((P:-B)))).

% :-debug.
%isInstFn(A):-!,trace_or_throw_ex(isInstFn(A)).

%= mpred_unnegate(N,P) is true if N is a negated term and P is the term
%= with the negation operator stripped.

/*
%% mpred_unnegate( +P, ?P) is semidet.
%
% PFC Negation.
%
mpred_unnegate((-P),P).
% mpred_unnegate((~P),P).
mpred_unnegate((\+(P)),P).
*/
/*

%% mpred_negated_literal( +P) is semidet.
%
% PFC Negated Literal.
%
mpred_negated_literal(P):-is_reprop(P),!,fail.
mpred_negated_literal(P):-mpred_negated_literal(P,_).

%% mpred_negated_literal( +P, ?Q) is semidet.
%
% PFC Negated Literal.
%
mpred_negated_literal(P,Q) :- is_ftNonvar(P),
  mpred_unnegate(P,Q),
  mpred_literal(Q).

*/

%% mpred_is_assertable( +X) is semidet.
%
% PFC If Is A Assertable.
%
mpred_is_assertable(X):- mpred_literal_nv(X),\+ functor(X,{},_).

%% mpred_literal_nv( +X) is semidet.
%
% PFC Literal Nv.
%
mpred_literal_nv(X):-is_ftNonvar(X),mpred_literal(X).

/*
%% mpred_literal( +X) is semidet.
%
% PFC Literal.
%
mpred_literal(X) :- is_reprop(X),!,fail.
mpred_literal(X) :- cyclic_term(X),!,fail.
mpred_literal(X) :- atom(X),!.
mpred_literal(X) :- mpred_negated_literal(X),!.
mpred_literal(X) :- mpred_positive_literal(X),!.
mpred_literal(X) :- is_ftVar(X),!.

*/

%% is_reprop( +X) is semidet.
%
% If Is A Reprop.
%
is_reprop(X):- compound(X),is_reprop_0(X).

%% is_reprop_0( +X) is semidet.
%
% If Is A reprop  Primary Helper.
%
is_reprop_0(~(X)):-!,is_reprop(X).
is_reprop_0(X):-get_functor(X,repropagate,_).


%% mpred_non_neg_literal( +X) is semidet.
%
% PFC Not Negated Literal.
%
mpred_non_neg_literal(X):- is_reprop(X),!,fail.
mpred_non_neg_literal(X):- atom(X),!.
mpred_non_neg_literal(X):- sanity(stack_check),
    mpred_positive_literal(X), X \= ~(_), X \= mpred_prop(_,_,_,_), X \= conflict(_).

% ======================= mpred_file('pfcsupport').	% support maintenance


%% is_relative( :TermV) is semidet.
%
% If Is A Relative.
%
is_relative(V):- (\+is_ftCompound(V)),!,fail.
is_relative(update(_)).
is_relative(replace(_)).
is_relative(rel(_)).
is_relative(+(X)):- \+ is_ftVar(X).
is_relative(-(X)):- \+ is_ftVar(X).
is_relative(*(X)):- \+ is_ftVar(X).

/*
% TODO not called yet
%= mpred_get_trigger_key(+Trigger,-Key)
%=
%= Arg1 is a trigger.  Key is the best term to index it on.

mpred_get_trigger_key(pt(Key,_),Key).
mpred_get_trigger_key(pk(Key,_,_),Key).
mpred_get_trigger_key(nt(Key,_,_),Key).
mpred_get_trigger_key(Key,Key).
*/

/*

the FOL i get from SUMO, CycL, UMBEL and many *non* RDF ontologies out there.. i convert to Datalog..  evidently my conversion process is unique as it preserves semantics most by the book conversions gave up on. 


% TODO not called yet
%=^L
%= Get a key from the trigger that will be used as the first argument of
%= the trigger baseable clause that stores the trigger.
%=
mpred_trigger_key(X,X) :- is_ftVar(X), !.
mpred_trigger_key(chart(word(W),_L),W) :- !.
mpred_trigger_key(chart(stem([Char1|_Rest]),_L),Char1) :- !.
mpred_trigger_key(chart(Concept,_L),Concept) :- !.
mpred_trigger_key(X,X).
*/
% ======================= mpred_file('pfcdb').	% predicates to manipulate database.

%   File   : pfcdb.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Author :  Dan Corpron
%   Updated: 10/11/87, ...
%   Purpose: predicates to manipulate a pfc database (e.g. save,
%	restore, reset, etc).






%% clause_or_call( +H, ?B) is semidet.
%
% Clause Or Call.
%
clause_or_call(M:H,B):-is_ftVar(M),!,no_repeats(M:F/A,(f_to_mfa(H,M,F,A))),M:clause_or_call(H,B).
clause_or_call(isa(I,C),true):-!,call_u(isa_asserted(I,C)).
clause_or_call(genls(I,C),true):-!,on_x_log_throw(call_u(genls(I,C))).
clause_or_call(H,B):- clause(src_edit(_Before,H),B).
clause_or_call(H,B):- predicate_property(H,number_of_clauses(C)),predicate_property(H,number_of_rules(R)),((R*2<C) -> (clause_u(H,B)*->!;fail) ; clause_u(H,B)).
clause_or_call(H,true):- call_u(should_call_for_facts(H)),no_repeats(on_x_log_throw(H)).


% as opposed to simply using clause(H,true).

%% should_call_for_facts( +H) is semidet.
%
% Should Call For Facts.
%
should_call_for_facts(H):- get_functor(H,F,A),call_u(should_call_for_facts(H,F,A)).

%% should_call_for_facts( +VALUE1, ?F, ?VALUE3) is semidet.
%
% Should Call For Facts.
%
should_call_for_facts(_,F,_):- a(prologSideEffects,F),!,fail.
should_call_for_facts(H,_,_):- modulize_head(H,HH), \+ predicate_property(HH,number_of_clauses(_)),!.
should_call_for_facts(_,F,A):- clause_b(mpred_prop(_M,F,A,pfcRHS)),!,fail.
should_call_for_facts(_,F,A):- clause_b(mpred_prop(_M,F,A,pfcMustFC)),!,fail.
should_call_for_facts(_,F,_):- a(prologDynamic,F),!.
should_call_for_facts(_,F,_):- \+ a(pfcControlled,F),!.



%% no_side_effects( +P) is semidet.
%
% No Side Effects.
%
no_side_effects(P):-  (\+ is_side_effect_disabled->true;(get_functor(P,F,_),a(prologSideEffects,F))).


:- was_dynamic(functorIsMacro/1).


%% compute_resolve( +NewerP, ?OlderQ, ?SU, ?SU, ?OlderQ) is semidet.
%
% Compute Resolve.
%
compute_resolve(NewerP,OlderQ,SU,SU,(mpred_blast(OlderQ),mpred_ain(NewerP,S),mpred_withdraw(conflict(NewerP)))):-
  must(correctify_support(SU,S)),
  wdmsg_pretty(compute_resolve(newer(NewerP-S)>older(OlderQ-S))).
compute_resolve(NewerP,OlderQ,S1,[U],Resolve):-compute_resolve(OlderQ,NewerP,[U2],S1,Resolve),match_source_ref1(U),match_source_ref1(U2),!.
compute_resolve(NewerP,OlderQ,SU,S2,(mpred_blast(OlderQ),mpred_ain(NewerP,S1),mpred_withdraw(conflict(NewerP)))):-
  must(correctify_support(SU,S1)),
  wdmsg_pretty(compute_resolve((NewerP-S1)>(OlderQ-S2))).



%% compute_resolve( +NewerP, ?OlderQ, ?Resolve) is semidet.
%
% Compute Resolve.
%
compute_resolve(NewerP,OlderQ,Resolve):-
   supporters_list(NewerP,S1),
   supporters_list(OlderQ,S2),
   compute_resolve(NewerP,OlderQ,S1,S2,Resolve).



%% is_resolved( +C) is semidet.
%
% If Is A Resolved.
%
is_resolved(C):- Why= is_resolved, mpred_call_only_facts(Why,C),\+mpred_call_only_facts(Why,~(C)).
is_resolved(C):- Why= is_resolved, mpred_call_only_facts(Why,~(C)),\+mpred_call_only_facts(Why,C).

:- must(nop(_)).


%% mpred_prove_neg( +G) is semidet.
%
% PFC Prove Negated.
%
mpred_prove_neg(G):-  (dtrace), \+ mpred_bc_only(G), \+ mpred_fact(G).


%% pred_head( :PRED1Type, ?P) is semidet.
%
% Predicate Head.
%
pred_head(Type,P):- no_repeats(P,(call(Type,P),\+ nonfact_metawrapper(P),is_ftCompound(P))).


%% pred_head_all( +P) is semidet.
%
% Predicate Head All.
%
pred_head_all(P):- pred_head(pred_all,P).


%% nonfact_metawrapper( :TermP) is semidet.
%
% Nonfact Metawrapper.
%
nonfact_metawrapper(~(_)).
nonfact_metawrapper(pt(_,_)).
nonfact_metawrapper(bt(_,_,_)).
nonfact_metawrapper(nt(_,_)).
nonfact_metawrapper(spft(_,_,_)).
nonfact_metawrapper(added(_)).
% we use the arity 1 forms is why 
nonfact_metawrapper(term_expansion(_,_)).
nonfact_metawrapper(P):- \+ current_predicate(_,P).
nonfact_metawrapper(M:P):-atom(M),!,nonfact_metawrapper(P).
nonfact_metawrapper(P):- get_functor(P,F,_), 
   (a(prologSideEffects,F);a(rtNotForUnboundPredicates,F)).
nonfact_metawrapper(P):-rewritten_metawrapper(P).


%% rewritten_metawrapper( +C) is semidet.
%
% Rewritten Metawrapper.
%
rewritten_metawrapper(_):-!,fail.
%rewritten_metawrapper(isa(_,_)).
rewritten_metawrapper(C):-is_ftCompound(C),functor(C,t,_).


%% meta_wrapper_rule( :TermARG1) is semidet.
%
% Meta Wrapper Rule.
%
meta_wrapper_rule((_<-_)).
meta_wrapper_rule((_<==>_)).
meta_wrapper_rule((_==>_)).
meta_wrapper_rule((_:-_)).



%% pred_all( +P) is semidet.
%
% Predicate All.
%
pred_all(P):-pred_u0(P).
pred_all(P):-pred_t0(P).
pred_all(P):-pred_r0(P).


%% pred_u0( +P) is semidet.
%
% Predicate For User Code Primary Helper.
%
pred_u0(P):-pred_u1(P),has_db_clauses(P).
pred_u0(P):-pred_u2(P).

%% pred_u1( +VALUE1) is semidet.
%
% Predicate For User Code Secondary Helper.
%
pred_u1(P):-a(pfcControlled,F),arity_no_bc(F,A),functor(P,F,A).
pred_u1(P):-a(prologHybrid,F),arity_no_bc(F,A),functor(P,F,A).
pred_u1(P):-a(prologDynamic,F),arity_no_bc(F,A),functor(P,F,A).

%% pred_u2( +P) is semidet.
%
% Predicate For User Code Extended Helper.
%
pred_u2(P):- compound(P),functor(P,F,A),sanity(no_repeats(arity_no_bc(F,A))),!,has_db_clauses(P).
pred_u2(P):- no_repeats(arity_no_bc(F,A)),functor(P,F,A),has_db_clauses(P).



%% has_db_clauses( +PI) is semidet.
%
% Has Database Clauses.
%
has_db_clauses(PI):-modulize_head(PI,P),
   predicate_property(P,number_of_clauses(NC)),\+ predicate_property(P,number_of_rules(NC)), \+ \+ clause_u(P,true).



%% pred_t0(+ ?P) is semidet.
%
% Predicate True Stucture Primary Helper.
%
pred_t0(P):-mreq('==>'(P)).
pred_t0(P):-mreq(pt(P,_)).
pred_t0(P):-mreq(bt(P,_)).
pred_t0(P):-mreq(nt(P,_,_)).
pred_t0(P):-mreq(spft(P,_,_)).

%pred_r0(-(P)):- call_u(-(P)).
%pred_r0(~(P)):- mreq(~(P)).


%% pred_r0( :TermP) is semidet.
%
% Predicate R Primary Helper.
%
pred_r0(P==>Q):- mreq(P==>Q).
pred_r0(P<==>Q):- mreq(P<==>Q).
pred_r0(P<-Q):- mreq(P<-Q).


%% cnstrn( +X) is semidet.
%
% Cnstrn.
%
:- module_transparent(cnstrn/1).
cnstrn(X):-term_variables(X,Vs),maplist(cnstrn0(X),Vs),!.

%% cnstrn( +V, ?X) is semidet.
%
% Cnstrn.
%
:- module_transparent(cnstrn/2).
cnstrn(V,X):-cnstrn0(X,V).

%% cnstrn0( +X, ?V) is semidet.
%
% Cnstrn Primary Helper.
%
:- module_transparent(cnstrn0/2).
cnstrn0(X,V):-when(is_ftNonvar(V),X).


%% rescan_pfc is semidet.
%
% Rescan Prolog Forward Chaining.
%
rescan_pfc:-forall(clause(baseKB:mpred_hook_rescan_files,Body),show_entry(rescan_pfc,Body)).


%% mpred_facts_and_universe( +P) is semidet.
%
% PFC Facts And Universe.
%
mpred_facts_and_universe(P):- (is_ftVar(P)->pred_head_all(P);true),call_u(P). % (meta_wrapper_rule(P)->call_u(P) ; call_u(P)).


%% repropagate( :TermP) is semidet.
%
% Repropagate.
%
repropagate(_):-  notrace((check_context_module,fail)).
repropagate(P):-  repropagate_0(P).
%repropagate(P):-  check_real_context_module,fail.

repropagate_0(P):-  notrace(is_ftVar(P)),!.
repropagate_0(USER:P):- USER==user,!,repropagate_0(P).
repropagate_0(==>P):- !,repropagate_0(P).
repropagate_0(P):-  meta_wrapper_rule(P),!,call_u(repropagate_meta_wrapper(P)).
repropagate_0(P):-  \+ predicate_property(P,_),'$find_predicate'(P,PP),PP\=[],!,
     forall(member(M:F/A,PP),must((functor(Q,F,A),repropagate_0(M:Q)))).
repropagate_0(F/A):- is_ftNameArity(F,A),!,functor(P,F,A),!,repropagate_0(P).
repropagate_0(F/A):- atom(F),is_ftVar(A),!,repropagate_0(F).
repropagate_0(P):-  notrace((\+ predicate_property(_:P,_),dmsg_pretty(undefined_repropagate(P)))),dumpST,dtrace,!,fail.
repropagate_0(P):- repropagate_meta_wrapper(P).

:- export(repropagate_meta_wrapper/1).
:- module_transparent(repropagate_meta_wrapper/1).

:- thread_local(t_l:is_repropagating/1).
%% repropagate_meta_wrapper( +P) is semidet.
%
% Repropagate Meta Wrapper Rule.
%
repropagate_meta_wrapper(P):-
 call_u(doall((no_repeats((mpred_facts_and_universe(P))),
    locally_tl(is_repropagating(P),ignore((once(show_failure(fwd_ok(P))),show_call(mpred_fwc(P)))))))).


predicate_to_goal(P,Goal):-atom(P),get_arity(P,F,A),functor(Goal,F,A).
predicate_to_goal(PF/A,Goal):-atom(PF),get_arity(PF,F,A),functor(Goal,F,A).
predicate_to_goal(G,G):-compound(G),!.


%% fwd_ok( :TermP) is semidet.
%
% Forward Repropigated Ok.
%
fwd_ok(_):-!.
fwd_ok(P):-ground(P),!.
fwd_ok(if_missing1(_,_)).
fwd_ok(idForTest(_,_)).
fwd_ok(clif(_)).
fwd_ok(pfclog(_)).
fwd_ok(X):-compound(X),get_assertion_head_arg(_,X,E),compound(E),functor(E,(:-),_),!.
% fwd_ok(P):-must(ground(P)),!.


%% mpred_facts_only( +P) is semidet.
%
% PFC Facts Only.
%
mpred_facts_only(P):- (is_ftVar(P)->(pred_head_all(P),\+ meta_wrapper_rule(P));true),no_repeats(P).



:- thread_local(t_l:user_abox/1).

% :- ((prolog_load_context(file,F),  prolog_load_context(source,F))-> throw(prolog_load_context(source,F)) ; true). :- include('mpred_header.pi').
:- style_check(+singleton).

% TODO READD
%:- foreach(arg(_,isEach(prologMultiValued,prologOrdered,prologNegByFailure,prologPTTP,prologKIF,pfcControlled,ttRelationType,
%     prologHybrid,predCanHaveSingletons,prologDynamic,prologBuiltin,functorIsMacro,prologListValued,prologSingleValued),P),)

%% get

% =================================================
% ==============  UTILS BEGIN        ==============
% =================================================

%% ruleBackward(+R, ?Condition) is semidet.
%
% Rule Backward.
%
ruleBackward(R,Condition,Support):- ruleBackward0(R,Condition,Support),
  functor(Condition,F,_),\+ arg(_,v(call_u,mpred_bc_only,inherit_above),F).

%% ruleBackward0(+F, ?Condition) is semidet.
%
% Rule Backward Primary Helper.
%
ruleBackward0(F,Condition,Support):- call_u('<-'(FF,Condition)),copy_term('<-'(FF,Condition),Support),FF=F.
%ruleBackward0(F,Condition,(F :- Condition)):- clause_u(F,Condition),\+ (is_true(Condition);mpred_is_info(Condition)).


% ======================= 
% user''s program''s database
% ======================= 


%% assert_mu(+X) is semidet.
%
% Assert For User Code.
%

assert_mu(MH):-  fix_mp(clause(assert,assert_u), MH,M,H),get_unnegated_functor(H,F,A),assert_mu(M,H,F,A).
asserta_mu(MH):- fix_mp(clause(assert,asserta_u),MH,M,H),asserta_mu(M,H).
assertz_mu(MH):- fix_mp(clause(assert,assertz_u),MH,M,H),assertz_mu(M,H).


% :- kb_shared(baseKB:singleValuedInArg/2).
:- thread_local(t_l:assert_dir/1).

%% assert_mu(+Module, +Pred, ?Functor, ?Arity) is semidet.
%
% Assert For User Code.
%
assert_mu(M,M2:Pred,F,A):- M == M2,!, assert_mu(M,Pred,F,A).
% maYBE assert_mu(M,(M2:Pred :- B),F,A):- M == M2,!, assert_mu(M,(Pred :- B),F,A).
assert_mu(M,_:Pred,F,A):- dtrace,sanity(\+ is_ftVar(Pred)),!, assert_mu(M,Pred,F,A).
assert_mu(M,(Pred:- (AWC,More)),_,_):- AWC == awc,!,asserta_mu(M,(Pred:- (AWC,More))).
assert_mu(M,(Pred:- (ZWC,More)),_,_):- ZWC == zwc,!,assertz_mu(M,(Pred:- (ZWC,More))).
%assert_mu(M,Pred,F,_):- clause_b(singleValuedInArg(F,SV)),!,must(update_single_valued_arg(M,Pred,SV)),!.
%assert_mu(M,Pred,F,A):- a(prologSingleValued,F),!,must(update_single_valued_arg(M,Pred,A)),!.
assert_mu(M,Pred,F,_):- a(prologOrdered,F),!,assertz_mu(M,Pred).
assert_mu(M,Pred,_,_):- t_l:assert_dir(Where),!, (Where = a -> asserta_mu(M,Pred); assertz_mu(M,Pred)).
%assert_mu(M,Pred,_,1):- !, assertz_mu(M,Pred),!.
assert_mu(M,Pred,_,_):- assertz_mu(M,Pred).


:-thread_local(t_l:side_effect_ok/0).


%% assertz_mu(+M, ?X) is semidet.
%
% Assertz Module Unit.
%
%assertz_mu(abox,X):-!,defaultAssertMt(M),!,assertz_mu(M,X).
%assertz_mu(M,X):- check_never_assert(M:X), clause_asserted_u(M:X),!.
% assertz_mu(M,X):- correct_module(M,X,T),T\==M,!,assertz_mu(T,X).
% assertz_mu(_,X):- must(defaultAssertMt(M)),!,must((expire_tabled_list(M:X),show_call(attvar_op(db_op_call(assertz,assertz_i),M:X)))).


assertz_mu(_,X):- check_never_assert(X),fail.
%assertz_mu(M,M2:Pred,F,A):- M == M2,!, assertz_mu(M,Pred,F,A).
%assertz_mu(M,spft(P,mfl4(VarNameZ,KB,F,L),T)):-M\==KB,!,assertz_mu(KB,spft(P,mfl4(VarNameZ,KB,F,L),T)).
assertz_mu(M,X):- strip_module(X,_,P), %sanity(check_never_assert(M:P)), 
    must((expire_tabled_list(M:P),show_failure(attvar_op(db_op_call(assertz,assertz_i),M:P)))).
   %(clause_asserted_u(M:P)-> true; must((expire_tabled_list(M:P),show_failure(attvar_op(db_op_call(assertz,assertz_i),M:P))))).

%% asserta_mu(+M, ?X) is semidet.
%
% Asserta Module Unit.
%
%asserta_mu(abox,X):-!,defaultAssertMt(M),!,asserta_mu(M,X).
% asserta_mu(M,X):- correct_module(M,X,T),T\==M,!,asserta_mu(T,X).
% asserta_mu(_,X):- must(defaultAssertMt(M)),!,must((expire_tabled_list(M:X),show_failure(attvar_op(db_op_call(assertz,assertz_i),M:X)))).

asserta_mu(_,X):- check_never_assert(X),fail.
asserta_mu(M,X):- strip_module(X,_,P),!, %sanity(check_never_assert(M:P)), 
    must((expire_tabled_list(M:P),show_failure(attvar_op(db_op_call(asserta,asserta_i),M:P)))).
   %(clause_asserted_u(M:P)-> true; must((expire_tabled_list(M:P),show_failure(attvar_op(db_op_call(asserta,asserta_i),M:P))))).


%% retract_mu( :TermX) is semidet.
%
% Retract For User Code.
%
% retract_mu(que(X,Y)):-!,show_failure(why,retract_eq_quitely_f(que(X,Y))),must((expire_tabled_list(~(X)))),must((expire_tabled_list((X)))).
retract_mu(H0):- throw_depricated, strip_module(H0,_,H),defaultAssertMt(M),show_if_debug(attvar_op(db_op_call(retract,retract_i),M:H)),!,must((expire_tabled_list(H))).
retract_mu(X):- check_never_retract(X),fail.
retract_mu(~(X)):-!,show_success(why,retract_eq_quitely_f(~(X))),must((expire_tabled_list(~(X)))),must((expire_tabled_list((X)))).
retract_mu((X)):-!,show_success(why,retract_eq_quitely_f((X))),must((expire_tabled_list(~(X)))),must((expire_tabled_list((X)))).
retract_mu(M:(H:-B)):- atom(M),!, clause_u(H,B,R),erase(R).
retract_mu((H:-B)):-!, clause_u(H,B,R),erase(R).
%retract_mu(~(X)):-must(is_ftNonvar(X)),!,retract_eq_quitely_f(~(X)),must((expire_tabled_list(~(X)))),must((expire_tabled_list((X)))).
%retract_mu(hs(X)):-!,retract_eq_quitely_f(hs(X)),must((expire_tabled_list(~(X)))),must((expire_tabled_list((X)))).





:- retractall(t_l:mpred_debug_local).
:- thread_local(t_l:in_rescan_mpred_hook/0).

%:- module_transparent(mpred_call_0/1).

 :- meta_predicate update_single_valued_arg(+,+,*).
 :- meta_predicate assert_mu(*,+,*,*).
 :- meta_predicate mpred_facts_and_universe(*).
 :- meta_predicate {*}.
 :- meta_predicate neg_in_code0(*).
 :- meta_predicate repropagate_meta_wrapper(*).
 :- meta_predicate mpred_get_support_via_sentence(*,*).

:- dynamic(infoF/1).



mpred_kb_ops_file.

:- fixup_exports.




% =======================================================
/* 
%
%= predicates to examine the state of pfc 
% interactively exploring Pfc justifications.
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% =======================================================
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_list_triggers.pl
:- if(current_prolog_flag(xref,true)).  % XREF
:- module(mpred_listing,
          [ draw_line/0,
            loop_check_just/1,
            pinfo/1,
            pp_items/2,
            pp_item/2,
            pp_filtered/1,
            pp_facts/2,
            pp_facts/1,
            pp_facts/0,
            mpred_list_triggers_types/1,
            mpred_list_triggers_nlc/1,
            mpred_list_triggers_1/1,
            mpred_list_triggers_0/1,
            mpred_list_triggers/1,
            mpred_contains_term/2,
            mpred_classify_facts/4,
            lqu/0,
            get_clause_vars_for_print/2,
            %mpred_whyBrouse/2,
            %mpred_why1/1,
            %mpred_why/1,
            %mpred_why/0,
            pp_rules/0,
            pp_supports/0,
            pp_triggers/0,            
            print_db_items/1,
            print_db_items/2,
            print_db_items/3,
            print_db_items/4,
            print_db_items_and_neg/3,
            show_pred_info/1,
            show_pred_info_0/1,
            mpred_listing_file/0
          ]).

:- include('mpred_header.pi').

:- endif.

% :- use_module(logicmoo(util/logicmoo_util_preddefs)).



:- multifile((
              user:portray/1,
  	user:prolog_list_goal/1,
  	user:prolog_predicate_name/2,
  	user:prolog_clause_name/2)).

:- dynamic
  	user:portray/1.

% :- dynamic(whybuffer/2).



%= 	 	 

%% lqu is semidet.
%
% Lqu.
%
lqu :- listing(que/2).


 

%= 	 	 

%% pp_facts is semidet.
%
% Pretty Print Facts.
%
pp_facts :- pp_facts(_,true).


%= 	 	 

%% pp_facts( ?Pattern) is semidet.
%
% Pretty Print Facts.
%
pp_facts(Pattern) :- pp_facts(Pattern,true).


%= 	 	 

%% pp_facts( ?P, ?C) is semidet.
%
% Pretty Print Facts.
%
pp_facts(P,C) :-
  mpred_facts(P,C,L),
  mpred_classify_facts(L,User,Pfc,_Rule),
  draw_line,
  fmt("User added facts:",[]),
  pp_items(user,User),
  draw_line,
  draw_line,
  fmt("Pfc added facts:",[]),
  pp_items(system,Pfc),
  draw_line.



%= 	 	 

%% pp_items( ?Type, :TermH) is semidet.
%
% Pretty Print Items.
%
pp_items(_Type,[]):-!.
pp_items(Type,[H|T]) :-
  ignore(pp_item(Type,H)),!,
  pp_items(Type,T).
pp_items(Type,H) :- ignore(pp_item(Type,H)).

:- thread_local(t_l:print_mode/1).

%= 	 	 

%% pp_item( ?MM, :TermH) is semidet.
%
% Pretty Print Item.
%
pp_item(_M,H):-pp_filtered(H),!.
pp_item(MM,(H:-B)):- B ==true,pp_item(MM,H).
pp_item(MM,H):- flag(show_asserions_offered,X,X+1),find_and_call(get_print_mode(html)), ( \+ \+ if_defined(pp_item_html(MM,H))),!.


pp_item(MM,spft(W0,U,ax)):- W = (_KB:W0),!,pp_item(MM,U:W).
pp_item(MM,spft(W0,F,U)):- W = (_KB:W0),atom(U),!,    fmt('~N%~n',[]),pp_item(MM,U:W), fmt('rule: ~p~n~n', [F]),!.
pp_item(MM,spft(W0,F,U)):- W = (_KB:W0),         !,   fmt('~w~nd:       ~p~nformat:    ~p~n', [MM,W,F]),pp_item(MM,U).
pp_item(MM,nt(Trigger0,Test,Body)) :- Trigger = (_KB:Trigger0), !, fmt('~w n-trigger: ~p~ntest: ~p~nbody: ~p~n', [MM,Trigger,Test,Body]).
pp_item(MM,pt(F0,Body)):- F = (_KB:F0),             !,fmt('~w p-trigger:~n', [MM]), pp_item('',(F:-Body)).
pp_item(MM,bt(F0,Body)):- F = (_KB:F0),             !,fmt('~w b-trigger:~n', [MM]), pp_item('',(F:-Body)).


pp_item(MM,U:W):- !,format(string(S),'~w  ~w:',[MM,U]),!, pp_item(S,W).
pp_item(MM,H):- \+ \+ (( get_clause_vars_for_print(H,HH),fmt("~w ~p~N",[MM,HH]))).


%= 	 	 

%% get_clause_vars_for_print( ?HB, ?HB) is semidet.
%
% Get Clause Variables For Print.
%
get_clause_vars_for_print(HB,HB):- ground(HB),!.
get_clause_vars_for_print(I,I):- is_listing_hidden(skipVarnames),!.
get_clause_vars_for_print(H0,MHB):- get_clause_vars_copy(H0,MHB),!.
get_clause_vars_for_print(HB,HB).

%= 	 	 

%% mpred_classify_facts( :TermH, ?User, :TermPfc, ?H) is semidet.
%
% Managed Predicate Classify Facts.
%
mpred_classify_facts([],[],[],[]).

mpred_classify_facts([H|T],User,Pfc,[H|Rule]) :-
  mpred_db_type(H,rule),
  !,
  mpred_classify_facts(T,User,Pfc,Rule).

mpred_classify_facts([H|T],[H|User],Pfc,Rule) :-
  mpred_get_support(H,(mfl4(_VarNameZ,_,_,_),ax)),
  !,
  mpred_classify_facts(T,User,Pfc,Rule).

mpred_classify_facts([H|T],User,[H|Pfc],Rule) :-
  mpred_classify_facts(T,User,Pfc,Rule).



%= 	 	 

%% print_db_items( ?T, ?I) is semidet.
%
% Print Database Items.
%
print_db_items(T, I):- 
    draw_line, 
    fmt("~N~w ...~n",[T]),
    print_db_items(I),
    draw_line,!.


%= 	 	 

%% print_db_items( ?I) is semidet.
%
% Print Database Items.
%
print_db_items(F/A):-number(A),!,safe_functor(P,F,A),!,print_db_items(P).
print_db_items(H):- bagof(H,clause_u(H,true),R1),pp_items((:),R1),R1\==[],!.
print_db_items(H):- \+ current_predicate(_,H),!. 
print_db_items(H):- catch( ('$find_predicate'(H,_),call_u(listing(H))),_,true),!,nl,nl.


%= 	 	 

%% pp_rules is semidet.
%
% Pretty Print Rules.
%
pp_rules :-
   print_db_items("Forward Rules",(_ ==> _)),
   print_db_items("Bidirectional Rules",(_ <==> _)), 
   print_db_items("Implication Rules",(_ => _)),
   print_db_items("Bi-conditional Rules",(_ <=> _)),
   print_db_items("Backchaining Rules",(_ <- _)),
   print_db_items("Positive Facts",(==>(_))),
   print_db_items("Negative Facts",(~(_))).


%= 	 	 

%% pp_triggers is semidet.
%
% Pretty Print Triggers.
%
pp_triggers :-
     print_db_items("Positive triggers", pt(_,_,_)),
     print_db_items("Negative triggers", nt(_,_,_,_)),
     print_db_items("Goal triggers",bt(_,_,_)).


%= 	 	 

%% pp_supports is semidet.
%
% Pretty Print Supports.
%
pp_supports :-
  % temporary hack.
  draw_line,
  fmt("Supports ...~n",[]), 
  setof((P =< S), (mpred_get_support(P,S), \+ pp_filtered(P)),L),
  pp_items('Support',L),
  draw_line,!.


pp_filtered(P):-var(P),!,fail.
pp_filtered(_:P):- !, pp_filtered(P).
pp_filtered(P):- safe_functor(P,F,A),F\==(/),!,pp_filtered(F/A).
pp_filtered(F/_):-F==mpred_prop.



%% draw_line is semidet.
%
% Draw Line.
%
draw_line:- \+ thread_self_main,!.
draw_line:- (t_l:print_mode(H)->true;H=unknown),fmt("~N%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n",[]),H=H.

 :- meta_predicate loop_check_just(0).

%= 	 	 

%% loop_check_just( :GoalG) is semidet.
%
% Loop Check Justification.
%
loop_check_just(G):-loop_check(G,ignore(arg(1,G,[]))).


%= 	 	 

%% show_pred_info( ?F) is semidet.
%
% Show Predicate Info.
%
show_pred_info(PI):-
   ((
       pi_to_head_l(PI,Head),      
       % doall(show_call(why,call_u(isa(Head,_)))),
        safe_functor(Head,F,_),
        doall(show_call(why,call_u(isa(F,_)))),
       ((current_predicate(_,M:Head), (\+ predicate_property(M:Head,imported_from(_))))
          -> show_pred_info_0(M:Head); 
             wdmsg_pretty(cannot_show_pred_info(Head))))),!.


%= 	 	 

%% show_pred_info_0( ?Head) is semidet.
%
% show Predicate info  Primary Helper.
%
show_pred_info_0(Head):- 
        doall(show_call(why,predicate_property(Head,_))),
        (has_cl(Head)->doall((show_call(why,clause(Head,_))));quietly((listing(Head)))),!.


% ===================================================
% Pretty Print Formula
% ===================================================



%= 	 	 

%% print_db_items( ?Title, ?Mask, ?What) is semidet.
%
% Print Database Items.
%
print_db_items(Title,Mask,What):-print_db_items(Title,Mask,Mask,What).

%= 	 	 

%% print_db_items( ?Title, ?Mask, ?SHOW, ?What0) is semidet.
%
% Print Database Items.
%
print_db_items(Title,Mask,SHOW,What0):-
     get_pi(Mask,H),get_pi(What0,What),
     format(atom(Showing),'~p for ~p...',[Title,What]),
     statistics(cputime,Now),Max is Now + 2,!,
       gripe_time(1.0,
         doall((once(statistics(cputime,NewNow)),NewNow<Max,clause_or_call(H,B),
             quietly(mpred_contains_term(What,(H:-B))),
             flag(print_db_items,LI,LI+1),
             ignore(quietly(pp_item(Showing,SHOW)))))),
     ignore(pp_item(Showing,done)),!.


%= 	 	 

%% mpred_contains_term( ?What, ?VALUE2) is semidet.
%
% Managed Predicate Contains Term.
%
mpred_contains_term(What,_):-is_ftVar(What),!.
mpred_contains_term(What,Inside):- compound(What),!,(\+ \+ ((copy_term_nat(Inside,Inside0),snumbervars(Inside0),contains_term(What,Inside0)))),!.
mpred_contains_term(What,Inside):- (\+ \+ once((subst(Inside,What,foundZadooksy,Diff),Diff \=@= Inside ))),!.



%= 	 	 

%% hook_mpred_listing( ?What) is semidet.
%
% Hook To [baseKB:hook_mpred_listing/1] For Module Mpred_listing.
% Hook Managed Predicate Listing.
%
baseKB:hook_mpred_listing(What):- on_x_debug(mpred_list_triggers(What)).

:- thread_local t_l:mpred_list_triggers_disabled.
% listing(L):-locally(t_l:mpred_list_triggers_disabled,listing(L)).


%= 	 	 

%% mpred_list_triggers( ?What) is semidet.
%
% Managed Predicate List Triggers.
%
mpred_list_triggers(_):-t_l:mpred_list_triggers_disabled,!.
mpred_list_triggers(What):-loop_check(mpred_list_triggers_nlc(What)).

:- meta_predicate(mpred_list_triggers_nlc(?)).


%= 	 	 

%% mpred_list_triggers_nlc( ?What) is semidet.
%
% Managed Predicate List Triggers Nlc.
%
mpred_list_triggers_nlc(MM:What):-atom(MM),!,MM:mpred_list_triggers(What).
mpred_list_triggers_nlc(What):-loop_check(mpred_list_triggers_0(What),true).


%= 	 	 

%% mpred_list_triggers_0( ?What) is semidet.
%
% Managed Predicate list triggers  Primary Helper.
%
mpred_list_triggers_0(What):-get_pi(What,PI),PI\=@=What,mpred_list_triggers(PI).
mpred_list_triggers_0(What):-nonvar(What),What= ~(Then),!, \+ \+ mpred_list_triggers_1(Then), \+ \+ mpred_list_triggers_1(What).
mpred_list_triggers_0(What):- \+ \+  mpred_list_triggers_1(~(What)), \+ \+ mpred_list_triggers_1(What).


%= 	 	 

%% mpred_list_triggers_types( ?VALUE1) is semidet.
%
% Managed Predicate list triggers  Types.
%
mpred_list_triggers_types('Triggers').
mpred_list_triggers_types('Instances').
mpred_list_triggers_types('Subclasses').
mpred_list_triggers_types('ArgTypes').
mpred_list_triggers_types('Arity').
mpred_list_triggers_types('Forward').
mpred_list_triggers_types('Bidirectional').
mpred_list_triggers_types('Backchaining').
mpred_list_triggers_types('Negative').
mpred_list_triggers_types('Sources').
mpred_list_triggers_types('Supports').
mpred_list_triggers_types('Edits').

% print_db_items_and_neg(Title,Fact,What):-nonvar(Fact),Fact= ~(_),!,fail.

%= 	 	 

%% print_db_items_and_neg( ?Title, ?Fact, ?What) is semidet.
%
% Print Database Items And Negated.
%
print_db_items_and_neg(Title,Fact,What):-print_db_items(Title,Fact,What).
print_db_items_and_neg(Title,Fact,What):-print_db_items(Title,~(Fact),What).


%= 	 	 

%% mpred_list_triggers_1( ?What) is semidet.
%
% Managed Predicate list triggers  Secondary Helper.
%
mpred_list_triggers_1(~(What)):-var(What),!.
mpred_list_triggers_1(~(_What)):-!.
mpred_list_triggers_1(What):-var(What),!.
mpred_list_triggers_1(What):- 
   print_db_items('Supports User',spft_precanonical(P,mfl4(VarNameZ,_,_,_),ax),spft(P,mfl4(VarNameZ,_,_,_),ax),What),
   print_db_items('Forward Facts',(nesc(F)),F,What),
   print_db_items('Forward Rules',(_==>_),What),
 ignore((What\= ~(_),safe_functor(What,IWhat,_),
   print_db_items_and_neg('Instance Of',isa(IWhat,_),IWhat),
   print_db_items_and_neg('Instances: ',isa(_,IWhat),IWhat),
   print_db_items_and_neg('Subclass Of',genls(IWhat,_),IWhat),
   print_db_items_and_neg('Subclasses: ',genls(_,IWhat),IWhat))),
   forall(suggest_m(M),print_db_items('PFC Watches', mpred_prop(M,_,_,_),What)),
   print_db_items('Triggers Negative', nt(_,_,_,_),What),
   print_db_items('Triggers Goal',bt(_,_,_),What),
   print_db_items('Triggers Positive',pt(_,_,_),What),
   print_db_items('Bidirectional Rules',(_<==>_),What), 
   dif(A,B),print_db_items('Supports Deduced',spft_precanonical(P,A,B),spft(P,A,B),What),
   dif(G,ax),print_db_items('Supports Nonuser',spft_precanonical(P,G,G),spft(P,G,G),What),
   print_db_items('Backchaining Rules',(_<-_),What),
   % print_db_items('Edits',is_disabled_clause(_),What),
   print_db_items('Edits',is_edited_clause(_,_,_),What),
   print_db_items('Instances',isa(_,_),What),
   print_db_items('Subclasses',genls(_,_),What),
   print_db_items('Negative Facts',~(_),What),

   print_db_items('ArgTypes',argGenls(_,_,_),What),
   print_db_items('ArgTypes',argIsa(_,_,_),What),
   print_db_items('ArgTypes',argQuotedIsa(_,_,_),What),
   print_db_items('ArgTypes',meta_argtypes(_),What),
   print_db_items('ArgTypes',predicate_property(G,meta_predicate(G)),What),
   print_db_items('ArgTypes',resultGenls(_,_),What),
   print_db_items('ArgTypes',resultIsa(_,_),What),
   print_db_items('Arity',arity(_,_),What),
   print_db_items('Arity',current_predicate(_),What),
   print_db_items('MetaFacts Predicate',predicate_property(_,_),What),
   print_db_items('Sources',module_property(_,_),What),
   print_db_items('Sources',predicateConventionMt(_,_),What),
   print_db_items('Sources',source_file(_,_),What),
   print_db_items('Sources',_:man_index(_,_,_,_,_),What),
   print_db_items('Sources',_:'$pldoc'(_,_,_,_),What),
   print_db_items('Sources',_:'$pred_option'(_,_,_,_),What),
   print_db_items('Sources',_:'$mode'(_,_),What),
   !.     


pinfo(F/A):- listing(F/A),safe_functor(P,F,A),findall(Prop,predicate_property(P,Prop),List),wdmsg_pretty(pinfo(F/A)==List),!.



%% pp_DB is semidet.
%
% Pretty Print All.
%
%pp_DB:- defaultAssertMt(M),clause_b(mtHybrid(M)),!,pp_DB(M).
%pp_DB:- forall(clause_b(mtHybrid(M)),pp_DB(M)).

pp_DB:- defaultAssertMt(M),pp_DB(M).
 

pp_DB(M):-
 with_exact_kb(M,
 M:must_det_l((
  pp_db_facts,
  pp_db_rules,
  pp_db_triggers,
  pp_db_supports))).

pp_db_facts:- context_module(M), pp_db_facts(M).
pp_db_rules:- context_module(M), pp_db_rules(M).
pp_db_triggers:- context_module(M), pp_db_triggers(M).
pp_db_supports:- context_module(M), pp_db_supports(M).


:- system:import(pp_DB/0).
:- system:export(pp_DB/0).

%  pp_db_facts ...

pp_db_facts(MM):- ignore(pp_db_facts(MM,_,true)).

pp_db_facts(MM,Pattern):- pp_db_facts(MM,Pattern,true).

pp_db_facts(MM,P,C):-
  mpred_facts_in_kb(MM,P,C,L),
  mpred_classifyFacts(L,User,Pfc,_ZRule),
  length(User,UserSize),length(Pfc,PfcSize),
  format("~N~nUser added facts in [~w]: ~w",[MM,UserSize]),
  pp_db_items(User),
  format("~N~nSystem added facts in [~w]: ~w",[MM,PfcSize]),
  pp_db_items(Pfc).

%  printitems clobbers it''s arguments - beware!


pp_db_items(Var):-var(Var),!,format("~N  ~p",[Var]).
pp_db_items([]):-!.
pp_db_items([H|T]):- !,
  % numbervars(H,0,_),
  format("~N  ~p",[H]),
  nonvar(T),pp_db_items(T).

pp_db_items((P >= FT)):- is_hidden_pft(P,FT),!.
  
pp_db_items(Var):-
  format("~N  ~p",[Var]).


is_hidden_pft(_,(mfl4(_VarNameZ,baseKB,_,_),ax)).
is_hidden_pft(_,(why_marked(_),ax)).


pp_mask(Type,MM,Mask):-   
  bagof_PFC(Mask,lookup_kb(MM,Mask),Nts),
  list_to_set_variant(Nts,NtsSet),!,
  pp_mask_list(Type,MM,NtsSet).

pp_mask_list(Type,MM,[]):- !,
  format("~N~nNo ~ws in [~w]...~n",[Type,MM]).
pp_mask_list(Type,MM,NtsSet):- length(NtsSet,Size), !,
  format("~N~n~ws (~w) in [~w]...~n",[Type,Size,MM]),
  pp_db_items(NtsSet).

mpred_classifyFacts([],[],[],[]).

mpred_classifyFacts([H|T],User,Pfc,[H|Rule]):-
  mpred_db_type(H,rule(_)),
  !,
  mpred_classifyFacts(T,User,Pfc,Rule).

mpred_classifyFacts([H|T],[H|User],Pfc,Rule):-
  % get_source_uu(UU),
  get_first_user_reason(H,_UU),
  !,
  mpred_classifyFacts(T,User,Pfc,Rule).

mpred_classifyFacts([H|T],User,[H|Pfc],Rule):-
  mpred_classifyFacts(T,User,Pfc,Rule).


pp_db_rules(MM):- 
 pp_mask("Forward Rule",MM,==>(_,_)),
 pp_mask("Bi-conditional Rule",MM,<==>(_,_)),
 pp_mask("Backward Rule",MM,<-(_,_)),
 % pp_mask("Prolog Rule",MM,:-(_,_)),
 !.


pp_db_triggers(MM):- 
 pp_mask("Positive trigger",MM,pt(_,_)),
 pp_mask("Negative trigger",MM,nt(_,_,_)),
 pp_mask("Goal trigger",MM,bt(_,_)),!.

pp_db_supports(MM):-
  % temporary hack.
  format("~N~nSupports in [~w]...~n",[MM]),
  with_exact_kb(MM, bagof_PFC((P >= S), mpred_get_support(P,S),L)),
  list_to_set_variant(L,LS),
  pp_db_items(LS),!.



:- fixup_exports.

mpred_listing_file.

% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_terms.pl
:- if(current_prolog_flag(xref,true)).
:- module(mpred_terms,
          [ 
          any_to_number/2,
          is_ftText/1,
          any_to_value/2,
          atom_to_value/2
          ]).


/** <module> mpred_terms
% Provides a common set of operators in translation between the several logical languages
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
:- include('mpred_header.pi').
:-endif.

:- export(any_to_number/2).
%% any_to_value( ?Var, ?Var) is semidet.
%
% Any Converted To Value.
%
any_to_value(Var,Var):-var(Var),!.
any_to_value(V,Term):-atom(V),!,atom_to_value(V,Term).
any_to_value(A,V):-any_to_number(A,V).
any_to_value(A,A).


%% any_to_number( :TermN, ?N) is semidet.
%
% Any Converted To Number.
%
any_to_number(N,N):- number(N),!.
any_to_number(ftDiceFn(A,B,C),N):- ground(A),if_defined(roll_dice(A,B,C,N)),!.
any_to_number(A,N):-atom(A),atom_to_value(A,V),A\=V,any_to_number(V,N).
any_to_number(A,N):- catch(number_string(N,A),_,fail).

%% atom_to_value( ?V, :TermTerm) is semidet.
%
% Atom Converted To Value.
%
atom_to_value(V,Term):-not(atom(V)),!,any_to_value(V,Term).
% 56
atom_to_value(V,Term):- catch((read_term_from_atom(V,Term,[variable_names([])])),_,fail),!.
% 18d18+4000
atom_to_value(V,ftDiceFn(T1,T2,+T3)):- atomic_list_concat_safe([D1,'d',D2,'+',D3],V), atom_to_value(D1,T1),atom_to_value(D2,T2),atom_to_value(D3,T3),!.
atom_to_value(V,ftDiceFn(T1,T2,-T3)):- atomic_list_concat_safe([D1,'d',D2,'-',D3],V), atom_to_value(D1,T1),atom_to_value(D2,T2),atom_to_value(D3,T3),!.



%% is_ftText( ?Arg) is semidet.
%
% If Is A Format Type Text.
%
is_ftText(Arg):- string(Arg),!.
is_ftText(Arg):- \+ compound(Arg),!,fail.
is_ftText(Arg):- safe_functor(Arg,s,_),!.
is_ftText([Arg|_]):-string(Arg),!.
is_ftText(Arg):- is_ftVar(Arg),!,fail.
is_ftText(Arg):- text_to_string_safe(Arg,_),!.
is_ftText(Arg):- safe_functor(Arg,S,_), ereq(resultIsa(S,ftText)).

:- kb_global(baseKB:ftText/1).
baseKB:ftText(A):- !, if_defined(term_is_ft(A, ftText),is_ftText(A)),!.

% =======================================================
% term utils
% =======================================================

:- was_export(inverse_args/2).



%% inverse_args( ?AR, ?GS) is semidet.
%
% Inverse Arguments.
%
inverse_args([AR,GS],[GS,AR]):-!.
inverse_args([AR,G,S],[S,G,AR]):-!.
inverse_args([A,R,G,S],[S,R,G,A]):-!.
inverse_args([P,A,R,G,S],[S,A,R,G,P]):-!.

:- was_export(same_vars/2).



%% same_vars( ?T1, ?T2) is semidet.
%
% Same Variables.
%
same_vars(T1,T2):-term_variables(T1,V1),term_variables(T2,V2),!,V1==V2.




%% replace_arg( ?C, :PRED3A, ?VAR, ?CC) is semidet.
%
% Replace Argument.
%
replace_arg(C,A,_VAR,_CC):-sanity((is_ftCompound(C),integer(A))),fail.
replace_arg((C:-B),A,VAR,(CC:-B)):-!,replace_arg(C,A,VAR,CC).
replace_arg(~ (C),A,VAR,~(CC)):-!,replace_arg(C,A,VAR,CC).
replace_arg( \+ (C),A,VAR,~(CC)):-!,replace_arg(C,A,VAR,CC).
replace_arg(M:(C),A,VAR,M:(CC)):-!,replace_arg(C,A,VAR,CC).
replace_arg(C,0,VAR,CC):-!, C=..[_|ARGS],CC=..[VAR|ARGS].
replace_arg(C,1,VAR,CC):-!, C=..[F,_|ARGS],CC=..[F,VAR|ARGS].
replace_arg(C,2,VAR,CC):-!, C=..[F,A,_|ARGS],CC=..[F,A,VAR|ARGS].
replace_arg(C,3,VAR,CC):-!, C=..[F,A,B,_|ARGS],CC=..[F,A,B,VAR|ARGS].
% replace_arg(C,A,VAR,CO):- dupe_term(C,CC),setarg(A,CC,VAR),!,CC=CO.
replace_arg(C,A,VAR,CC):- C=..FARGS,replace_nth_arglist(FARGS,A,VAR,FARGO),!,CC=..FARGO.

% :- mpred_trace_nochilds(replace_arg/4).

%% replace_nth_arglist(+List, +Index, +Element, -NewList) is det[private]
% Replace the Nth (1-based) element of a list.
% :- mpred_trace_nochilds(replace_nth_arglist/4).



%% replace_nth_arglist( :TermARG1, ?VALUE2, ?VAR, :TermVAR) is semidet.
%
% Replace Nth Arglist.
%
replace_nth_arglist([],_,_,[]):- !.
replace_nth_arglist([_|ARGO],0,VAR,[VAR|ARGO]):- !.
replace_nth_arglist([T|FARGS],A,VAR,[T|FARGO]):- 
    A2 is A-1,replace_nth_arglist(FARGS,A2,VAR,FARGO).





%% replace_nth_ref( :TermARG1, ?N, ?OldVar, ?NewVar, :TermARG5) is semidet.
%
% Replace Nth Ref.
%
replace_nth_ref([],_N,_OldVar,_NewVar,[]):- !,trace_or_throw_ex(missed_the_boat).
replace_nth_ref([OldVar|ARGS],1,OldVar,NewVar,[NewVar|ARGS]):- !.
replace_nth_ref([Carry|ARGS],Which,OldVar,NewVar,[Carry|NEWARGS]):- 
 Which1 is Which-1,
 replace_nth_ref(ARGS,Which1,OldVar,NewVar,NEWARGS),!.


% :- mpred_trace_nochilds(update_value/3).



%% update_value( ?OLD, ?NEW, ?NEXT) is semidet.
%
% Update Value.
%
update_value(OLD,NEW,NEXT):- var(NEW),!,trace_or_throw_ex(logicmoo_bug(update_value(OLD,NEW,NEXT))).
update_value(OLD,NEW,NEWV):- var(OLD),!,compute_value_no_dice(NEW,NEWV).
update_value(OLD,X,NEW):- is_list(OLD),!,list_update_op(OLD,X,NEW),!.
update_value(OLDI,+X,NEW):- compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD + X,_,fail),!.
update_value(OLDI,-X,NEW):- compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD - X,_,fail),!.
update_value(OLDI,X,NEW):- number(X),X<0,compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD + X,_,fail),!.
update_value(_,NEW,NEWV):- compute_value_no_dice(NEW,NEWV),!.




%% flatten_append( ?First, ?Last, ?Out) is semidet.
%
% Flatten Append.
%
flatten_append(First,Last,Out):-flatten([First],FirstF),flatten([Last],LastF),append(FirstF,LastF,Out),!.




%% list_update_op( ?OLDI, :TermX, ?NEW) is semidet.
%
% List Update Oper..
%
list_update_op(OLDI,+X,NEW):-flatten_append(OLDI,X,NEW),!.
list_update_op(OLDI,-X,NEW):-flatten([OLDI],OLD),flatten([X],XX),!,list_difference_eq(OLD,XX,NEW),!.




%% compute_value_no_dice( ?NEW, ?NEW) is semidet.
%
% Compute Value No Dice.
%
compute_value_no_dice(NEW,NEW):- compound(NEW),functor_catch(NEW,ftDiceFn,_),!.
compute_value_no_dice(NEW,NEW):- compound(NEW),functor_catch(NEW,ftDice,_),!.
compute_value_no_dice(NEW,NEWV):-compute_value(NEW,NEWV).




%% compute_value( ?NEW, ?NEWV) is semidet.
%
% Compute Value.
%
compute_value(NEW,NEWV):-catch(NEWV is NEW,_,fail),!.
compute_value(NEW,NEWV):-catch(any_to_value(NEW,NEWV),_,fail),!.
compute_value(NEW,NEW).




%% insert_into( :TermARGS, ?VALUE2, ?Insert, :TermInsert) is semidet.
%
% Insert Converted To.
%
insert_into(ARGS,0,Insert,[Insert|ARGS]):- !.
insert_into([Carry|ARGS],After,Insert,[Carry|NEWARGS]):- 
   After1 is After - 1,
   insert_into(ARGS,After1,Insert,NEWARGS).



% ========================================
% is_holds_true/is_holds_false
% ========================================


:- was_export(into_plist/2).

%= 	 	 

%% into_plist( ?In, ?Out) is semidet.
%
% Converted To Plist.
%
into_plist(In,Out):-into_plist_arities(2,12,In,Out).

:- was_export(into_plist_arities/4).

%= 	 	 

%% into_plist_arities( ?Min, ?Max, ?PLIST, ?PLISTO) is semidet.
%
% Converted To Plist Arities.
%
into_plist_arities(Min,Max,PLIST,PLISTO):- var(PLIST),!,between(Min,Max,X),length(PLIST,X),PLISTO=PLIST.
into_plist_arities(_,_,[P|LIST],[P|LIST]):-var(P),!.
into_plist_arities(_,_,[(t)|PLIST],PLIST):-!.  % t is our versuion of '$holds' or call/N
into_plist_arities(_,_,plist(P,LIST),[P|LIST]):-!.
into_plist_arities(_,_,Call,PLIST):- Call=..PLIST. % finally the fallthrue



%= 	 	 

%% never_mpred_tcall( ?VALUE1) is semidet.
%
% Never Managed Predicate Managed Predicate.
%

never_mpred_tcall(mpred_prop).
never_mpred_tcall(isa).
never_mpred_tcall(arity).


local_qh_mpred_prop(M,F,A,C):- call_u(mpred_prop(M,F,A,C)).


% :- setup_mpred_ops.


                   
%= 	 	 

:- meta_predicate(if_result(0,0)).

%= 	 	 

%% if_result( :GoalTF, :Goal) is semidet.
%
% If Result.
%
if_result(TF,Call):-(TF->Call;true).




%= 	 	 

%% mpred_plist_t( ?P, :TermLIST) is semidet.
%
% Managed Predicate Plist True Stucture.
%
/* mpred_plist_t(P,[]):-!,t(P). */
mpred_plist_t(P,LIST):-var(P),!,is_list(LIST),CALL=..[t,P|LIST],on_x_debug((CALL)).
mpred_plist_t(t,[P|LIST]):-!, mpred_plist_t(P,LIST).
%mpred_plist_t(mpred_isa,[C,_A,I]):-!,ground(I:C),local_qh_mpred_isa(C,I).
mpred_plist_t(isa,[I,C]):-!,call(call,t,C,I).
mpred_plist_t(P,_):-never_mpred_tcall(P),!,fail.
mpred_plist_t(P,[L|IST]):-is_holds_true(P),!,mpred_plist_t(L,IST).
mpred_plist_t(P,LIST):-is_holds_false(P),!,call_u(mpred_f(LIST)).
mpred_plist_t(P,LIST):- CALL=..[t,P|LIST],on_x_debug(CALL).


:- meta_predicate(mpred_fa_call(?,?,0)).



%= 	 	 

%% mpred_fa_call( ?F, ?UPARAM2, :Goal) is semidet.
%
% Managed Predicate Functor-arity Call.
%
mpred_fa_call(F,A,Call):- var(F),!,
 no_repeats(F,(clause_b(support_hilog(F,A));clause_b(arity(F,A)))), 
   once((F\==t, 
   \+ a(rtNotForUnboundPredicates,F),current_predicate(F,M:_OtherCall))),
    on_x_debug(M:Call).
mpred_fa_call(M:F,A,Call):- nonvar(M),!,mpred_fa_call(F,A,M:Call).
mpred_fa_call(F,_,Call):-F\==t,current_predicate(F,M:_OtherCall),!,M:Call.


%= 	 	 

%% mpred_fact_arity( ?VALUE1, ?VALUE2) is semidet.
%
% Managed Predicate Fact Arity.
%
mpred_fact_arity(F,A):- call_u(arity(F,A)),
  suggest_m(M),
  once(local_qh_mpred_prop(M,F,A,prologHybrid);
     local_qh_mpred_prop(M,F,A,pfcControlled);
     local_qh_mpred_prop(M,F,A,prologPTTP);
     local_qh_mpred_prop(M,F,A,prologKIF)),!.


%= 	 	 

%% prologHybridFact( ?G) is semidet.
%
% Prolog Hybrid Fact.
%
prologHybridFact(G):- (var(G)->(mpred_fact_arity(F,A),safe_functor(G,F,A));true),into_mpred_form(G,M),!,no_repeats(call_u(M)).





:- fixup_exports.


/* Part of LogicMOO Base Logicmoo Debug Tools
% ===================================================================
%   File   : mpred_why.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated:
%   Purpose: predicates for interactively exploring Pfc justifications.

% ***** predicates for brousing justifications *****
% ===================================================================
*/

%:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).

%:- throw(module(pfcumt,[umt/1])).

:- if(current_prolog_flag(xref,true)).  % XREF
:- module(mpred_justify, []).
:- set_module(class(library)).
:- endif.

%:- use_module(mpred_kb_ops).
%:- use_module(library(util_varnames)).
%:- use_module(library(no_repeats)).

:- include('mpred_header.pi').

%:- endif.

%   File   : pfcjust.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: predicates for accessing Pfc justifications.
%   Status: more or less working.
%   Bugs:

%  *** predicates for exploring supports of a fact *****

justification(F,J):- supporters_list(F,J).

justifications(F,Js):- bagof_nr(J,justification(F,J),Js).

:- set_prolog_flag(expect_pfc_file,never).

mpred_why(M:Conseq,Ante):- atom(M),!,M:mpred_why_2(Conseq,Ante).

mpred_why(Conseq,Ante):- mpred_why_2(Conseq,Ante).

mpred_why_2(Conseq,Ante):- var(Conseq),!,mpred_children(Ante,Conseq).
mpred_why_2(Conseq,Ante):- justifications(Conseq,Ante).


mpred_info(O):-call_u(mpred_info0(O)).

mpred_info0(O):-
 with_output_to(user_error,
 ((dmsg_pretty("======================================================================="),  
  quietly(call_with_inference_limit(ignore(on_xf_cont(deterministically_must(mpred_why_1(O)))),4000,_)),
  dmsg_pretty("======================================================================="),
  must_maplist(mp_printAll(O),
  [   mpred_db_type(O,v),  
      +(mpred_child(O,v)),
      % mpred_fact(O),
      mpred_axiom(O),
      well_founded(O),
      mpred_supported(local,O),
      mpred_supported(cycles,O),
      mpred_assumption(O),
      call(listing(O)),
      get_mpred_is_tracing(O)]),
 dmsg_pretty("=======================================================================")))).

mp_printAll(S,+(O)):- subst(O,v,V,CALL),CALL\==O,!,
  subst(O,S,s,NAME),safe_functor(O,F,_),!,
  nl,flush_output, fmt("=================="),wdmsg_pretty(NAME),wdmsg_pretty("---"),flush_output,!,
  doall(((flush_output,call_u(CALL),flush_output)*->fmt9(V);(fail=F))),nl,fmt("=================="),nl,flush_output.
mp_printAll(S,call(O)):- !,
  subst(O,S,s,NAME),
  nl,flush_output,fmt("=================="),wdmsg_pretty(NAME),wdmsg_pretty("---"),flush_output,!,
  doall(((flush_output,deterministically_must(call_u(O)),flush_output)*->true;wdmsg_pretty(false=NAME))),fmt("=================="),nl,flush_output.
mp_printAll(S,(O)):- subst(O,v,V,CALL),CALL\==O,!,
  subst(O,S,s,NAME),safe_functor(O,F,_),
  nl,flush_output, fmt("=================="),wdmsg_pretty(NAME),wdmsg_pretty("---"),flush_output,!,
  doall(((flush_output,deterministically_must(call_u(CALL)),flush_output)*->fmt9(V);(fail=F))),nl,fmt("=================="),nl,flush_output.
mp_printAll(S,(O)):-  !,  safe_functor(O,F,A),mp_nnvv(S,O,F,A),flush_output.
mp_nnvv(_,(O),F,1):- !, doall(((flush_output,deterministically_must(call_u(O)),flush_output)*->wdmsg_pretty(+F);wdmsg_pretty(-F))).
mp_nnvv(S,(O),_,_):- !, subst(O,S,s,NAME), !,
  doall(((flush_output,deterministically_must(call_u(O)),flush_output)*->wdmsg_pretty(-NAME);wdmsg_pretty(+NAME))).






%%  mpred_basis_list(+P,-L)
%
%  is true iff L is a list of "base" facts which, taken
%  together, allows us to deduce P.  A mpred "based on" list fact is an axiom (a fact
%  added by the user or a raw Prolog fact (i.e. one w/o any support))
%  or an assumption.
%
mpred_basis_list(F,[F]):- (mpred_axiom(F) ; mpred_assumption(F)),!.

mpred_basis_list(F,L):-
  % i.e. (reduce 'append (map 'mpred_basis_list (justification f)))
  justification(F,Js),
  bases_union(Js,L).


%%  bases_union(+L1,+L2).
%
%  is true if list L2 represents the union of all of the
%  facts on which some conclusion in list L1 is based.
%
bases_union([],[]).
bases_union([X|Rest],L):-
  mpred_basis_list(X,Bx),
  bases_union(Rest,Br),
  mpred_union(Bx,Br,L).

%mpred_axiom(F):- !, % Like OLD TODO
%  mpred_get_support(F,(_,ax)).
mpred_axiom(F):-
  mpred_get_support(F,UU),
  is_user_reason(UU),!.

%% mpred_assumption(P)
%
%  an mpred_assumption is a failed goal, i.e. were assuming that our failure to
%  prove P is a proof of not(P)
%
mpred_assumption(P):- !, % Like OLD TODO
  nonvar(P), mpred_unnegate(P,_).
mpred_assumption(P):- nonvar(P), 
  mpred_unnegate(P,N), 
 % fail,
  % added prohibited_check
  (current_prolog_flag(explicitly_prohibited_check,false) -> true ; \+ mpred_axiom(~ N)).


:- set_prolog_flag(explicitly_prohibited_check,false).

%% mpred_assumptions( +X, +AsSet) is semidet.
%
% true if AsSet is a set of assumptions which underly X.
%
mpred_assumptions(X,[X]):- mpred_assumption(X).
mpred_assumptions(X,[]):- mpred_axiom(X).
mpred_assumptions(X,L):-
  justification(X,Js),
  do_assumpts(Js,L).


%% do_assumpts(+Set1,?Set2) is semidet.
%
% Assumptions Secondary Helper.
%
do_assumpts([],[]).
do_assumpts([X|Rest],L):-
  mpred_assumptions(X,Bx),
  do_assumpts(Rest,Br),
  mpred_union(Bx,Br,L).


%  mpred_proofTree(P,T) the proof tree for P is T where a proof tree is
%  of the form
%
%      [P , J1, J2, ;;; Jn]         each Ji is an independent P justifier.
%           ^                         and has the form of
%           [J11, J12,... J1n]      a list of proof trees.


%% mpred_child(+P,?Q) is semidet.
%
% is true iff P is an immediate justifier for Q.
%
mpred_child(P,Q):- is_list(P),!,maplist(mpred_child,P,Q).
mpred_child(P,Q):-
  mpred_get_support(Q,(P,_)).
mpred_child(P,Q):-
  mpred_get_support(Q,(_,Trig)),
  mpred_db_type(Trig,trigger(_TT)),
  mpred_child(P,Trig).


%% mpred_children(+P, ?L) is semidet.
%
% PFC Children.
%
mpred_children(P,L):- bagof_nr(C,mpred_child(P,C),L).



%% mpred_descendant(+P, ?Q) is semidet.
%
% mpred_descendant(P,Q) is true iff P is a justifier for Q.
%
mpred_descendant(P,Q):-
   mpred_descendant1(P,Q,[]).


%% mpred_descendant1(+P, ?Q, ?Seen) is semidet.
%
% PFC Descendant Secondary Helper.
%
mpred_descendant1(P,Q,Seen):-
  mpred_child(X,Q),
  (\+ member(X,Seen)),
  (P=X ; mpred_descendant1(P,X,[X|Seen])).


%% mpred_descendants(+P, ?L) is semidet.
%
% PFC Descendants.
%
mpred_descendants(P,L):-
  bagof_nr(Q,mpred_descendant1(P,Q,[]),L).

:- meta_predicate(bagof_nr(?,^,*)).
bagof_nr(T,G,B):- no_repeats(B,(bagof(T,G,B))).
:- meta_predicate(bagof_PFC(?,^,-)).
bagof_PFC(T,G,B):- (bagof_nr(T,G,B) *-> true; B=[]).


:- meta_predicate(sanity_check(0,0)).
sanity_check(When,Must):- notrace(catch((When,Must),_,fail)),!.
sanity_check(When,Must):- show_call(When),!,must_or_rtrace(Must),!.

%
%  predicates for manipulating support relationships
%

notify_if_neg_trigger(spft(P,Fact,Trigger)):- 
  (Trigger= nt(F,Condition,Action) ->
    (mpred_trace_msg('~N~n\tAdding NEG mpred_do_fcnt via support~n\t\ttrigger: ~p~n\t\tcond: ~p~n\t\taction: ~p~n\t from: ~p~N',
      [F,Condition,Action,mpred_add_support_fast(P,(Fact,Trigger))]));true).

%  mpred_add_support(+Fact,+Support)
mpred_add_support(P,(Fact,Trigger)):-
  MSPFT = spft(P,Fact,Trigger),
   fix_mp(mpred_add_support,MSPFT,M,SPFT),
   M:notify_if_neg_trigger(SPFT),
  M:(clause_asserted_u(SPFT)-> true; sanity_check(assertz_mu(SPFT),clause_asserted_u(SPFT))).

%  mpred_add_support_fast(+Fact,+Support)
mpred_add_support_fast(P,(Fact,Trigger)):-
  must_or_rtrace(( MSPFT = spft(P,Fact,Trigger),      
      % copy_term(MSPFT,SPFTC),
       fix_mp(mpred_add_support3,MSPFT,M,SPFT),
   M:notify_if_neg_trigger(SPFT),
   
   M:sanity_check(M:assertz_mu(SPFT),call(M:clause_asserted(SPFT))))).


                                                                
:- meta_predicate(mpred_get_support(*,-)).

mpred_get_support((H:-B),(Fact,Trigger)):- 
   lookup_u(spft((H <- B),_,_),Ref),
   clause(spft(HH<-BB,Fact,Trigger),true,Ref),
   clause_ref_module(Ref),   
   H=@=HH,B=@=BB.
mpred_get_support(P,(Fact,Trigger)):-
  lookup_spft(P,Fact,Trigger).


mpred_get_support_why(P,FT):-
  (mpred_get_support_perfect(P,FT)*->true;
   (mpred_get_support_deeper(P,FT))).

mpred_get_support_perfect(P,(Fact,Trigger)):-
   lookup_spft_match_first(P,Fact,Trigger).

mpred_get_support_deeper((H:-B),(Fact,Trigger)):- (nonvar(H) -> ! ; true),
 lookup_u(spft((H <- B),_,_),Ref),
  clause(spft(HH<-BB,Fact,Trigger),true,Ref),
  H=@=HH,B=@=BB.

mpred_get_support_deeper(P,(Fact,Trigger)):-
    lookup_spft_match_deeper(P,Fact,Trigger).

lookup_spft_match(A,B,C):- copy_term(A,AA),lookup_spft(A,B,C),A=@=AA.

lookup_spft_match_deeper(H,Fact,Trigger):- 
  copy_term(H,HH),
  lookup_spft((H:- _B),Fact,Trigger),
  H=@=HH.

lookup_spft_match_first(A,B,C):- nonvar(A),!, 
  no_repeats(((lookup_spft_match(A,B,C);lookup_spft(A,B,C)))).

lookup_spft_match_first(A,B,C):- lookup_spft(A,B,C).


lookup_spft(A,B,C):- !, lookup_u(spft(A,B,C)).
% cutted above
/*
lookup_spft(A,B,C):- nonvar(A),!,lookup_spft_p(A,B,C).
lookup_spft(A,B,C):- var(B),!,lookup_spft_t(A,B,C).
lookup_spft(A,B,C):- lookup_spft_f(A,B,C).

lookup_spft_p(A,B,C):- with_some_vars_locked(A,lookup_u(spft(A,B,C))).
% TODO UNCOMMENT MAYBE IF NEEDED lookup_spft_p(A,B,C):- full_transform(lookup,A,AA),!,A\=@=AA,!,show_mpred_success(baseKB:spft(AA,B,C)).

lookup_spft_f(A,B,C):- with_some_vars_locked(B,lookup_u(spft(A,B,C))).
% TODO UNCOMMENT MAYBE IF NEEDED lookup_spft_f(A,B,C):- full_transform(lookup,B,BB),!,B\=@=BB,!,show_mpred_success(baseKB:spft(A,BB,C)).

lookup_spft_t(A,B,C):- lookup_u(spft(A,B,C)).
*/
/*
%  TODO MAYBE
mpred_get_support(F,J):-
  full_transform(mpred_get_support,F,FF),!,
  F\==FF,mpred_get_support(FF,J).
*/

mpred_rem_support_if_exists(P,(Fact,Trigger)):-
  lookup_spft(P,Fact,Trigger),
  mpred_retract_i_or_warn(spft(P,Fact,Trigger)).


mpred_rem_support(P,(Fact,Trigger)):-
  closest_u(spft(P,Fact,Trigger),spft(P,FactO,TriggerO)),
  mpred_retract_i_or_warn_1(spft(P,FactO,TriggerO)).
mpred_rem_support(P,S):-
  mpred_retract_i_or_warn(spft(P,Fact,Trigger)),
  ignore((Fact,Trigger)=S).



closest_u(Was,WasO):-clause_asserted_u(Was),!,Was=WasO.
closest_u(Was,WasO):-lookup_u(Was),!,Was=WasO,!.
closest_u(Was,WasO):-lookup_u(WasO),ignore(Was=WasO),!.
closest_u(H,HH):- ref(_) = Result,closest_uu(H,H,HH,Result),ref(Ref)= Result,
  (H==HH -> true ; nonvar(Ref)),!.

closest_uu(H,P,PP):- copy_term(H+P,HH+PP),
      ((lookup_u(HH)*-> (=@=(P,PP)->(!,HH=H);(fail));(!,fail));(true)).
closest_uu(H,P,PP,Result):-
      sanity(Result=@=ref(Ref)),
      (copy_term(H+P,HH+PP),
      ((lookup_u(HH,Ref)*-> (=@=(P,PP)->(!,HH=H);
          (nb_setarg(1,Result,Ref),fail));(!,fail));((clause(HH,B,Ref),must_ex(B))))).

/*
*/

mpred_collect_supports(Tripples):-
  bagof_PFC(Tripple, mpred_support_relation(Tripple), Tripples).

mpred_support_relation((P,F,T)):- lookup_spft(P,F,T).

mpred_make_supports((P,S1,S2)):-
  mpred_add_support(P,(S1,S2)),
  (mpred_ain_object(P); true),
  !.


pp_why:-mpred_why.

mpred_why:-
  call(t_l:whybuffer(P,_)),
  mpred_why_1(P).

pp_why(A):-mpred_why_1(A).

clear_proofs:- retractall(t_l:whybuffer(_P,_Js)),color_line(cyan,1).


:- thread_local(t_l:shown_why/1).

% see pfc_why

:- export(with_no_english/1).
:- meta_predicate(with_no_english(*)).
with_no_english(Goal):- setup_call_cleanup(flag('english', Was, 0),Goal,flag('english', _, Was )).

mpred_why(P):- clear_proofs,!,with_no_english((must(mpred_why_1(P)))).

mpred_why_1(M:P):-  atom(M),!,call_from_module(M,mpred_why_1(P)).
mpred_why_1(NX):- number(NX),!, trace, pfcWhy0(NX),!.
mpred_why_1(P):- is_list(P), !, maplist(mpred_why_1, P).
mpred_why_1(P):- ((callable(P), ((must_ex((mpred_why_justs(P))))))) *-> true ; mpred_why_1_fallback(P).

mpred_why_1_fallback(NX):-  
  (number(NX)-> true ; clear_proofs),
  trace,
  pfcWhy0(NX),!.
mpred_why_1_fallback(P):- mpred_why_sub(P).

% mpred_why_1(N):- number(N),!, call(t_l:whybuffer(P,Js)), mpred_handle_why_command(N,P,Js).

mpred_why_justs(P):- mpred_why_justs_1a(P)*->true;forall(mpred_why_justs_1b(P),true).
  
mpred_why_justs_1a(P) :-    
  color_line(green,2),!,
  findall(Js,((no_repeats(P-Js,(justifications(P,Js))),
    must((color_line(yellow,1),
      ignore(pfcShowJustifications(P,Js)))))),Count),
  (Count==[]-> format("~N No justifications for ~p. ~n~n",[P]) ; true),
  color_line(green,2).

mpred_why_justs_1b(P) :- term_variables(P,VarsPC), 
  ((call_u_no_bc(P),mpred_why_justs_1a(P))*-> 
  (term_variables(P,VarsAC),(VarsPC==VarsAC->!;true));
   mpred_why_justs_1a(P)).

/*
mpred_why_justs_2(P) :-    
  color_line(green,2),!,
  findall(Js,((no_repeats(P-Js,deterministically_must(justifications(P,Js))),
    must((color_line(yellow,1),
      ignore(pfcShowJustifications(P,Js)))))),Count),
  (Count==[]-> format("~N No justifications for ~p. ~n~n",[P]) ; true),
  color_line(green,2).
*/
/*

mpred_why_1(P):- loop_check(quietly_ex((must_ex(mpred_why_try_each(P)),color_line(green,2))),true).

% user:mpred_why_1((user:prolog_exception_hook(A, B, C, D) :- exception_hook(A, B, C, D))).
% mpred_why_1((prolog_exception_hook(A, B, C, D) :- exception_hook(A, B, C, D))).

mpred_why_try_each(MN):- strip_module(MN,_,N),number(N),!,pfcWhy0(N),!.

mpred_why_try_each(ain(H)):-!,mpred_why_try_each(H).
mpred_why_try_each(call_u(H)):-!,mpred_why_try_each(H).
mpred_why_try_each(clause(H,B)):-!,mpred_why_try_each(H:-B).
mpred_why_try_each(clause(H,B,_)):-!,mpred_why_try_each(H:-B).
mpred_why_try_each(clause_u(P)):-!,mpred_why_try_each(P).
mpred_why_try_each(clause_u(H,B)):-!,mpred_why_try_each(H:-B).
mpred_why_try_each(clause_u(H,B,_)):-!,mpred_why_try_each(H:-B).

mpred_why_try_each(P):- once((retractall(t_l:whybuffer(P,_)),color_line(green,2),
    show_current_source_location,format("~NJustifications for ~p:",[P]))),
    fail.

mpred_why_try_each(P):- mpred_why_try_each_0(P),!.
mpred_why_try_each(P):- mpred_why_sub(P),!.
mpred_why_try_each(M:P :- B):- atom(M),call_from_module(M,mpred_why_try_each_0(P:-B)),!.
mpred_why_try_each(M:P):- atom(M),call_from_module(M,mpred_why_try_each_0(P)),!.
mpred_why_try_each(P :- B):- is_true(B),!,mpred_why_try_each(P ).
mpred_why_try_each(M:H):- strip_module(H,Ctx,P),P==H,Ctx==M,!,mpred_why_try_each(H).
mpred_why_try_each(_):- format("~N No justifications. ~n").

mpred_why_try_each_0(P):- findall(Js,mpred_why_try_each_1(P,Js),Count),Count\==[],!.
mpred_why_try_each_0(\+ P):- mpred_why_try_each_0(~P)*->true;(call_u(\+ P),wdmsgl(why:- \+ P)),!.

mpred_why_try_each_1(P,Js):-
  ((no_repeats(P-Js,deterministically_must(justifications(P,Js))),
    ((color_line(yellow,1), pfcShowJustifications(P,Js))))).
mpred_why_try_each_1(\+ P,[MFL]):- !, find_mfl(P,MFL),ansi_format([fg(cyan)],"~N    ~q",[MFL]),fail.
mpred_why_try_each_1( P,[MFL]):-  find_mfl(P,MFL), \+ clause_asserted(t_l:shown_why(MFL)), ansi_format([fg(cyan)],"~N    ~q",[MFL]).

*/
pfcWhy0(N) :-
  number(N),
  !,
  t_l:whybuffer(P,Js),
  pfcWhyCommand0(N,P,Js).

pfcWhy0(P) :-
  justifications(P,Js),  
  assert(t_l:whybuffer(P,Js)),                     
  pfcWhyBrouse(P,Js).

pfcWhy1(P) :-
  justifications(P,Js),
  pfcWhyBrouse(P,Js).

pfcWhyBrouse(P,Js) :-    % non-interactive
  pfcShowJustifications(P,Js),source_file(_,_),!.

pfcWhyBrouse(P,Js) :- 
  pfcShowJustifications(P,Js),
  ttyflush,
  read_pending_chars(current_input,_,[]),!,
  ttyflush,
  % pfcAsk(' >> ',Answer),
  % read_pending_chars(current_input,[Answer|_],[]),!,  
  format('~N',[]),write('proof [q/h/u/?.?]: '),get_char(Answer),
  pfcWhyCommand0(Answer,P,Js).

pfcWhyCommand0(q,_,_) :- !.
pfcWhyCommand0(h,_,_) :- 
  !,
  format("~n
Justification Brouser Commands:
 q   quit.
 N   focus on Nth justification.
 N.M brouse step M of the Nth justification
 u   up a level
",
 []).

pfcWhyCommand0(N,_P,Js) :-
  float(N),
  !,
  pfcSelectJustificationNode(Js,N,Node),
  pfcWhy1(Node).

pfcWhyCommand0(u,_,_) :-
  % u=up
  !.

pfcWhyCommand0(N,_,_) :-
  integer(N),
  !,
  format("~n~w is a yet unimplemented command.",[N]),
  fail.

pfcWhyCommand0(X,_,_) :-
 format("~n~w is an unrecognized command, enter h. for help.",[X]),
 fail.

reset_shown_justs:- retractall(t_l:shown_why(_)),color_line(red,1).
  
pfcShowJustifications(P,Js) :-
  show_current_source_location,
  reset_shown_justs,
  color_line(yellow,1),
  format("~N~nJustifications for ~p:~n",[P]),  

  pfcShowJustification1(Js,1),!.

pfcShowJustification1([],_):-!.
pfcShowJustification1([J|Js],N) :- !,
  % show one justification and recurse.    
  reset_shown_justs,
  pfcShowSingleJust(N,step(1),J),!,
  N2 is N+1,  
  pfcShowJustification1(Js,N2).

pfcShowJustification1(J,N) :- 
  reset_shown_justs, % nl,
  pfcShowSingleJust(N,step(1),J),!.

incrStep(StepNo,Step):-arg(1,StepNo,Step),X is Step+1,nb_setarg(1,StepNo,X).

pfcShowSingleJust(JustNo,StepNo,C):- is_ftVar(C),!,incrStep(StepNo,Step),
  ansi_term:ansi_format([fg(cyan)],"~N    ~w.~w ~w ",[JustNo,Step,C]),!.
pfcShowSingleJust(_JustNo,_StepNo,[]):-!.
pfcShowSingleJust(JustNo,StepNo,(P,T)):-!, 
  pfcShowSingleJust(JustNo,StepNo,P),
  pfcShowSingleJust(JustNo,StepNo,T).
pfcShowSingleJust(JustNo,StepNo,(P,F,T)):-!, 
  pfcShowSingleJust1(JustNo,StepNo,P),
  pfcShowSingleJust(JustNo,StepNo,F),
  pfcShowSingleJust1(JustNo,StepNo,T).
pfcShowSingleJust(JustNo,StepNo,(P*->T)):-!, 
  pfcShowSingleJust1(JustNo,StepNo,P),format('      *-> ',[]),
  pfcShowSingleJust1(JustNo,StepNo,T).

pfcShowSingleJust(JustNo,StepNo,(P:-T)):-!, 
  pfcShowSingleJust1(JustNo,StepNo,P),format(':- ~p.',[T]).
 
pfcShowSingleJust(JustNo,StepNo,(P : -T)):-!, 
  pfcShowSingleJust1(JustNo,StepNo,P),format('      :- ',[]),
  pfcShowSingleJust(JustNo,StepNo,T).

pfcShowSingleJust(JustNo,StepNo,(P :- T) ):- !, 
  pfcShowSingleJust1(JustNo,StepNo,call(T)),  
  pfcShowSingleJust1(JustNo,StepNo,P).


pfcShowSingleJust(JustNo,StepNo,[P|T]):-!, 
  pfcShowSingleJust(JustNo,StepNo,P),
  pfcShowSingleJust(JustNo,StepNo,T).

pfcShowSingleJust(JustNo,StepNo,pt(P,Body)):- !, 
  pfcShowSingleJust1(JustNo,StepNo,pt(P)),  
  pfcShowSingleJust(JustNo,StepNo,Body).

pfcShowSingleJust(JustNo,StepNo,C):- 
 pfcShowSingleJust1(JustNo,StepNo,C).

fmt_cl(P):- \+ \+ (numbervars(P,126,_,[attvar(skip),singletons(true)]),write_term(P,[portray(true)])).

unwrap_litr(C,CCC+VS):- copy_term(C,CC,VS),
  numbervars(CC+VS,0,_),
  unwrap_litr0(CC,CCC),!.
unwrap_litr0(call(C),CC):-unwrap_litr0(C,CC).
unwrap_litr0(pt(C),CC):-unwrap_litr0(C,CC).
unwrap_litr0(body(C),CC):-unwrap_litr0(C,CC).
unwrap_litr0(head(C),CC):-unwrap_litr0(C,CC).
unwrap_litr0(C,C).

pfcShowSingleJust1(JustNo,StepNo,C):- unwrap_litr(C,CC),!,pfcShowSingleJust4(JustNo,StepNo,C,CC).
pfcShowSingleJust4(_,_,_,CC):- t_l:shown_why(C),C=@=CC,!.
pfcShowSingleJust4(JustNo,StepNo,C,CC):- assert(t_l:shown_why(CC)),!,
   incrStep(StepNo,Step),
   ansi_term:ansi_format([fg(cyan)],"~N    ~w.~w ~@ ",[JustNo,Step,fmt_cl(C)]),   
   pfcShowSingleJust_C(C),!.

pfcShowSingleJust_C(C):-is_file_ref(C),!.
pfcShowSingleJust_C(C):-find_mfl(C,MFL),assert(t_l:shown_why(MFL)),!,pfcShowSingleJust_MFL(MFL).
pfcShowSingleJust_C(_):-ansi_term:ansi_format([hfg(black)]," % [no_mfl] ",[]),!.

short_filename(F,FN):- atomic_list_concat([_,FN],'/pack/',F),!.
short_filename(F,FN):- atomic_list_concat([_,FN],swipl,F),!.
short_filename(F,FN):- F=FN,!.

pfcShowSingleJust_MFL(MFL):- MFL=mfl4(VarNameZ,_M,F,L),atom(F),short_filename(F,FN),!,varnames_load_context(VarNameZ),
   ansi_term:ansi_format([hfg(black)]," % [~w:~w] ",[FN,L]).
pfcShowSingleJust_MFL(MFL):- ansi_term:ansi_format([hfg(black)]," % [~w] ",[MFL]),!.

pfcAsk(Msg,Ans) :-
  format("~n~w",[Msg]),
  read(Ans).

pfcSelectJustificationNode(Js,Index,Step) :-
  JustNo is integer(Index),
  nth1(JustNo,Js,Justification),
  StepNo is 1+ integer(Index*10 - JustNo*10),
  nth1(StepNo,Justification,Step).







mpred_why_maybe(_,(F:-P)):-!,wdmsgl(F:-P),!.
mpred_why_maybe(F,P):-wdmsgl(F:-P),!.
mpred_why_maybe(_,P):-ignore(mpred_why_1(P)).

mpred_why_sub(P):- trace, loop_check(mpred_why_sub0(P),true).
mpred_why_sub0(P):- mpred_why_2(P,Why),!,wdmsg_pretty(:-mpred_why_1(P)),wdmsgl(mpred_why_maybe(P),Why).
mpred_why_sub0(P):-loop_check(mpred_why_sub_lc(P),trace_or_throw_ex(mpred_why_sub_lc(P)))-> \+ \+ call(t_l:whybuffer(_,_)),!.
mpred_why_sub_lc(P):- 
  justifications(P,Js),
  nb_setval('$last_printed',[]),
  clear_proofs,
  assertz(t_l:whybuffer(P,Js)),
  mpred_whyBrouse(P,Js).
  

mpred_why_sub_sub(P):-
  justifications(P,Js),
  clear_proofs,
  % retractall_u(t_l:whybuffer(_,_)),
  (nb_hasval('$last_printed',P)-> dmsg_pretty(hasVal(P)) ;
   ((
  assertz(t_l:whybuffer(P,Js)),
   nb_getval('$last_printed',LP),
   ((mpred_pp_db_justification1(LP,Js,1),fmt('~N~n',[])))))).

nb_pushval(Name,Value):-nb_current(Name,Before)->nb_setval(Name,[Value|Before]);nb_setval(Name,[Value]).
nb_peekval(Name,Value):-nb_current(Name,[Value|_Before]).
nb_hasval(Name,Value):-nb_current(Name,List),member(Value,List).
nb_popval(Name,Value):-nb_current(Name,[Value|Before])->nb_setval(Name,Before).

mpred_why1(P):-
  justifications(P,Js),
  mpred_whyBrouse(P,Js).

% non-interactive
mpred_whyBrouse(P,Js):-
   must_ex(quietly_ex(in_cmt((mpred_pp_db_justifications(P,Js))))), !.

% Interactive
mpred_whyBrouse(P,Js):-
  mpred_pp_db_justifications(P,Js),
  mpred_prompt_ask(' >> ',Answer),
  mpred_handle_why_command(Answer,P,Js).

mpred_handle_why_command(q,_,_):- !.
mpred_handle_why_command(h,_,_):-
  !,
  format("~N
Justification Brouser Commands:
 q   quit.
 N   focus on Nth justification.
 N.M brouse step M of the Nth justification
 user   up a level ~n",
  []).

mpred_handle_why_command(N,_ZP,Js):-
  float(N),
  !,
  mpred_select_justification_node(Js,N,Node),
  mpred_why1(Node).

mpred_handle_why_command(u,_,_):-
  % u=up
  !.

mpred_unhandled_command(N,_,_):-
  integer(N),
  !,
  format("~N~p is a yet unimplemented command.",[N]),
  fail.

mpred_unhandled_command(X,_,_):-
 format("~N~p is an unrecognized command, enter h. for help.",[X]),
 fail.

mpred_pp_db_justifications(P,Js):-
 show_current_source_location, 
 must_ex(quietly_ex(( format("~NJustifications for ~p:",[P]),
  mpred_pp_db_justification1('',Js,1)))).

mpred_pp_db_justification1(_Prefix,[],_).

mpred_pp_db_justification1(Prefix,[J|Js],N):-
  % show one justification and recurse.
  nl,  
  mpred_pp_db_justifications2(Prefix,J,N,1),
  reset_shown_justs,
  N2 is N+1,
  mpred_pp_db_justification1(Prefix,Js,N2).

mpred_pp_db_justifications2(_Prefix,[],_,_).

mpred_pp_db_justifications2(Prefix,[C|Rest],JustNo,StepNo):-
(nb_hasval('$last_printed',C)-> dmsg_pretty(chasVal(C)) ;
(
 (StepNo==1->fmt('~N~n',[]);true),
  format(string(LP),' ~w.~p.~p',[Prefix,JustNo,StepNo]),
  nb_pushval('$last_printed',LP),
  format("~N  ~w ~p",[LP,C]),
  ignore(loop_check(mpred_why_sub_sub(C))),
  StepNext is 1+StepNo,
  mpred_pp_db_justifications2(Prefix,Rest,JustNo,StepNext))).

mpred_prompt_ask(Info,Ans):-
  format("~N~p",[Info]),
  read(Ans).

mpred_select_justification_node(Js,Index,Step):-
  JustNo is integer(Index),
  nth1(JustNo,Js,Justification),
  StepNo is 1+ integer(Index*10 - JustNo*10),
  nth1(StepNo,Justification,Step).


%%  mpred_supported(+P) is semidet.
%
%  succeeds if P is "supported". What this means
%  depends on the TMS mode selected.
%
mpred_supported(P):-
  must_ex(get_tms_mode(P,Mode))->
  mpred_supported(Mode,P).

%%  mpred_supported(+TMS,+P) is semidet.
%
%  succeeds if P is "supported". What this means
%  depends on the TMS mode supplied.
%
mpred_supported(local,P):- !, mpred_get_support(P,_),!, not_rejected(P).
mpred_supported(cycles,P):-  !, well_founded(P),!, not_rejected(P).
mpred_supported(How,P):- ignore(How=unknown),not_rejected(P).

not_rejected(~P):- nonvar(P),  \+ mpred_get_support(P,_).
not_rejected(P):-  \+ mpred_get_support(~P,_).

%% well_founded(+Fact) is semidet.
%
% a fact is well founded if it is supported by the user
%  or by a set of facts and a rules, all of which are well founded.
%
well_founded(Fact):- each_E(well_founded_0,Fact,[_]).

well_founded_0(F,_):-
  % supported by user (axiom) or an "absent" fact (assumption).
  (mpred_axiom(F) ; mpred_assumption(F)),
  !.

well_founded_0(F,Descendants):-
  % first make sure we aren't in a loop.
  (\+ memberchk(F,Descendants)),
  % find a justification.
  supporters_list0(F,Supporters),!,
  % all of whose members are well founded.
  well_founded_list(Supporters,[F|Descendants]),
  !.

%%  well_founded_list(+List,-Decendants) is det.
%
% simply maps well_founded over the list.
%
well_founded_list([],_).
well_founded_list([X|Rest],L):-
  well_founded_0(X,L),
  well_founded_list(Rest,L).

%% supporters_list(+F,-ListofSupporters) is det.
%
% where ListOfSupports is a list of the
% supports for one justification for fact F -- i.e. a list of facts which,
% together allow one to deduce F.  One of the facts will typically be a rule.
% The supports for a user-defined fact are: [ax].
%
supporters_list(F,ListO):- no_repeats_cmp(same_sets,ListO,supporters_list_each(F,ListO)).

same_sets(X,Y):-
  flatten([X],FX),sort(FX,XS),
  flatten([Y],FY),sort(FY,YS),!,
  YS=@=XS.

supporters_list_each(F,ListO):-   
   supporters_list0(F,ListM),
   expand_supporters_list(ListM,ListM,ListO).

expand_supporters_list(_, [],[]):-!.
expand_supporters_list(Orig,[F|ListM],[F|NewListOO]):-
   supporters_list0(F,FList),
   list_difference_variant(FList,Orig,NewList),
   % NewList\==[],
   append(Orig,NewList,NewOrig),
   append(ListM,NewList,NewListM),!,
   expand_supporters_list(NewOrig,NewListM,ListO),
   append(ListO,NewList,NewListO),
   list_to_set_variant(NewListO,NewListOO).
expand_supporters_list(Orig,[F|ListM],[F|NewListO]):-
  expand_supporters_list(Orig,ListM,NewListO).


list_to_set_variant(List, Unique) :-
    list_unique_1(List, [], Unique),!.

list_unique_1([], _, []).
list_unique_1([X|Xs], So_far, Us) :-
    memberchk_variant(X, So_far),!,
    list_unique_1(Xs, So_far, Us).
list_unique_1([X|Xs], So_far, [X|Us]) :-
    list_unique_1(Xs, [X|So_far], Us).

% dif_variant(X,Y):- freeze(X,freeze(Y, X \=@= Y )).



%%	list_difference_variant(+List, -Subtract, -Rest)
%
%	Delete all elements of Subtract from List and unify the result
%	with Rest.  Element comparision is done using =@=/2.

list_difference_variant([],_,[]).
list_difference_variant([X|Xs],Ys,L) :-
	(   memberchk_variant(X,Ys)
	->  list_difference_variant(Xs,Ys,L)
	;   L = [X|T],
	    list_difference_variant(Xs,Ys,T)
	).


%%	memberchk_variant(+Val, +List)
%
%	Deterministic check of membership using =@= rather than
%	unification.

memberchk_variant(X, [Y|Ys]) :-
   (   X =@= Y
   ->  true
   ;   memberchk_variant(X, Ys)
   ).

:- module_transparent(supporters_list0/2).
supporters_list0(Var,[is_ftVar(Var)]):-is_ftVar(Var),!.
supporters_list0(F,OUT):-  
 pfc_with_quiet_vars_lock(supporters_list00(F,OUT)).

:- module_transparent(supporters_list00/2).
supporters_list00(F,OUT):- supporters_list1a(F,OUT) *-> true; supporters_list1b(F,OUT).

:- module_transparent(supporters_list1a/2).

supporters_list1a(F,[Fact|MoreFacts]):-
  mpred_get_support_why(F,(Fact,Trigger)),
  triggerSupports(Fact,Trigger,MoreFacts).
   

:- module_transparent(supporters_list1b/2).
supporters_list1b(Var,[is_ftVar(Var)]):- is_ftVar(Var),!.
supporters_list1b(U,[]):- axiomatic_supporter(U),!.
supporters_list1b((H:-B),[MFL]):- !, clause_match(H,B,Ref),find_hb_mfl(H,B,Ref,MFL).
supporters_list1b(\+ P, HOW):- !, supporters_list00(~ P,HOW),!.
supporters_list1b((H),[((H:-B))]):- may_cheat, clause_match(H,B,_Ref).

may_cheat:- fail.

uses_call_only(H):- predicate_property(H,foreign),!.
uses_call_only(H):- predicate_property(H,_), \+ predicate_property(H,interpreted),!.

clause_match(H,_B,uses_call_only(H)):- uses_call_only(H),!.
clause_match(H,B,Ref):- clause_asserted(H,B,Ref),!.
clause_match(H,B,Ref):- ((copy_term(H,HH),clause_u(H,B,Ref),H=@=HH)*->true;clause_u(H,B,Ref)), \+ reserved_body_helper(B).

find_mfl(C,MFL):- lookup_spft_match(C,MFL,ax).
find_mfl(C,MFL):- unwrap_litr0(C,UC) -> C\==UC -> find_mfl(UC,MFL).
find_mfl(C,MFL):- expand_to_hb(C,H,B),
   find_hb_mfl(H,B,_Ref,MFL)->true; (clause_match(H,B,Ref),find_hb_mfl(H,B,Ref,MFL)).

find_hb_mfl(_H,_B,Ref,mfl4(_VarNameZ,M,F,L)):- atomic(Ref),clause_property(Ref,line_count(L)),
 clause_property(Ref,file(F)),clause_property(Ref,module(M)). 
find_hb_mfl(H,B,_,mfl4(VarNameZ,M,F,L)):- lookup_spft_match_first( (H:-B),mfl4(VarNameZ,M,F,L),_),!.
find_hb_mfl(H,B,_Ref,mfl4(VarNameZ,M,F,L)):- lookup_spft_match_first(H,mfl4(VarNameZ,M,F,L),_),ground(B).
find_hb_mfl(H,_B,uses_call_only(H),MFL):- !,call_only_based_mfl(H,MFL).

/*


clause_match(H,_B,uses_call_only(H)):- uses_call_only(H),!.
clause_match(H,B,Ref):- clause_asserted(H,B,Ref),!.

clause_match(H,B,Ref):- no_repeats(Ref,((((copy_term(H,HH),clause_u(H,B,Ref),H=@=HH)*->true;clause_u(H,B,Ref)), \+ reserved_body_helper(B)))).

clause_match0(H,B,Ref):- no_repeats(Ref,clause_match1(H,B,Ref)).

clause_match1(H,B,Ref):- clause(H,B,Ref).
clause_match1(M:H,B,Ref):- !, (M:clause(H,B,Ref) ; clause_match2(H,B,Ref)).
clause_match1(H,B,Ref):- clause_match2(H,B,Ref).

clause_match2(H,B,Ref):- current_module(M),clause(M:H,B,Ref),(clause_property(Ref, module(MM))->MM==M;true).

find_mfl(C,MFL):-find_mfl0(C,MFL),compound(MFL),MFL=mfl4(VarNameZ,_,F,_),nonvar(F).
find_mfl0(C,MFL):- lookup_spft_match(C,MFL,ax).
% find_mfl0(mfl4(VarNameZ,M,F,L),mfl4(VarNameZ,M,F,L)):-!.
find_mfl0(C,MFL):-expand_to_hb(C,H,B),
   find_hb_mfl(H,B,_Ref,MFL)->true; (clause_match(H,B,Ref),find_hb_mfl(H,B,Ref,MFL)).
find_mfl0(C,MFL):-expand_to_hb(C,H,B),
   find_hb_mfl(H,B,_Ref,MFL)->true; (clause_match0(H,B,Ref),find_hb_mfl(H,B,Ref,MFL)).

*/
call_only_based_mfl(H,mfl4(_VarNameZ,M,F,L)):- 
  ignore(predicate_property(H,imported_from(M));predicate_property(H,module(M))),
  ignore(predicate_property(H,line_count(L))),
  ignore(source_file(M:H,F);predicate_property(H,file(F));(predicate_property(H,foreign),F=foreign)).

axiomatic_supporter(Var):-is_ftVar(Var),!,fail.
axiomatic_supporter(is_ftVar(_)).
axiomatic_supporter(clause_u(_)).
axiomatic_supporter(U):- is_file_ref(U),!.
axiomatic_supporter(ax):-!.

is_file_ref(A):-compound(A),A=mfl4(_VarNameZ,_,_,_).

triggerSupports(_,Var,[is_ftVar(Var)]):-is_ftVar(Var),!.
triggerSupports(_,U,[]):- axiomatic_supporter(U),!.
triggerSupports(FactIn,Trigger,OUT):-
  mpred_get_support(Trigger,(Fact,AnotherTrigger))*->
  (triggerSupports(Fact,AnotherTrigger,MoreFacts),OUT=[Fact|MoreFacts]);
  triggerSupports1(FactIn,Trigger,OUT).

triggerSupports1(_,X,[X]):- may_cheat.
/*
triggerSupports1(_,X,_):- mpred_db_type(X,trigger(_)),!,fail.
triggerSupports1(_,uWas(_),[]):-!.
triggerSupports1(_,U,[(U)]):- is_file_ref(U),!.
triggerSupports1(_,U,[uWas(U)]):- get_source_uu((U1,U2))->member(U12,[U1,U2]),U12=@=U.
triggerSupports1(_,X,[X]):- \+ mpred_db_type(X,trigger(_)).
*/


/*
:-module_transparent(mpred_ain/1).
:-module_transparent(mpred_aina/1).
:-module_transparent(mpred_ainz/1).
*/

% :- '$current_source_module'(M),forall(mpred_database_term(F,A,_),(abolish(pfc_lib:F/A),abolish(user:F/A),abolish(M:F/A))).
% :- initialization(ensure_abox_hybrid(baseKB)).


% % :- set_prolog_flag(mpred_pfc_file,true).
% local_testing

:- fixup_exports.

:- set_prolog_flag(expect_pfc_file,unknown).

