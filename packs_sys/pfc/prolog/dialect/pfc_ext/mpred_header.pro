% All modules are declared here so that this next lines dont have to be pasted into every file.
% Since this list will need at least 160 entries to cover the obj classes rooms and commands, 
% we add the modules here to not waste 160^2 lines of text and having to not 
% update 160+ files whenever a new module is used
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%

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


:- thread_local(t_l:infSkipFullExpand/0).
:- thread_local(t_l:deduceArgTypes/1).
:- thread_local(t_l:noDBaseMODs/1).
:- thread_local(t_l:side_effect_buffer/3).
:- thread_local(t_l:loading_mpred_file/2).
:- thread_local(t_l:consulting_sources/0).
% HOOKS

:- forall(member(M:F/A,[
el_assertions:el_holds/10, %el_assertions
el_assertions:el_holds/11, %el_assertions
el_assertions:el_holds/12, %el_assertions
el_assertions:el_holds/13, %el_assertions
el_assertions:el_holds/14, %el_assertions
el_assertions:el_holds/4, %el_assertions
el_assertions:el_holds/5, %el_assertions
el_assertions:el_holds/6, %el_assertions
el_assertions:el_holds/7, %el_assertions
el_assertions:el_holds/8, %el_assertions
el_assertions:el_holds/9, %el_assertions
el_assertions:el_holds_pred_impl/1, %el_assertions
% el_assertions:is_cyckb_t_pred/2, %el_assertions
lmcache:has_pfc_database_preds/1,
lmcache:after_mpred_load/0,
lmcache:loaded_external_kbs/1,
% baseKB:agent_call_command/2,
baseKB:decl_coerce/3,
baseKB:coerce_hook/3, 
baseKB:hook_mpred_listing/1,
baseKB:hook_one_minute_timer_tick/0,
baseKB:hook_one_second_timer_tick/0, 
baseKB:isa_pred_now_locked/0,
baseKB:loaded_file_world_time/3, 
baseKB:loaded_mpred_file/2,
baseKB:module_local_init/0,
baseKB:mpred_hook_rescan_files/0, 
baseKB:mpred_provide_read_attributes/3, 
baseKB:mpred_provide_setup/4, 
baseKB:mpred_provide_storage_clauses/3, 
baseKB:mpred_provide_storage_op/2, 
baseKB:mpred_provide_write_attributes/2, 
baseKB:mpred_skipped_module/1, 
baseKB:mud_test/2,
baseKB:never_reload_file/1, 
baseKB:pfcManageHybrids/0, 
baseKB:regression_test/0,
baseKB:feature_test/0,
baseKB:sanity_test/0,
baseKB:regression_test/1,
baseKB:feature_test/1,
baseKB:sanity_test/1,
baseKB:type_action_info/3,
% mpred_online:semweb_startup/0,
baseKB:use_ideep_swi/0,
baseKB:cycPred/2,
baseKB:isa/2,
baseKB:cycPlus2/2,

user:portray/1,
user:prolog_load_file/2, 
%user:prolog_clause_name/2,
%user:prolog_list_goal/1,
%user:prolog_predicate_name/2,
user:term_expansion/2,user:goal_expansion/2,system:term_expansion/2,system:goal_expansion/2]),
  (multifile(M:F/A),M:module_transparent(M:F/A),dynamic(M:F/A),discontiguous(M:F/A))). 

:- discontiguous(module_local_init/2).
% ================================================
% Thread Locals
% ================================================
% DYN KB
:- thread_local(t_l:repl_to_string/2).
:- thread_local(t_l:repl_writer/2).

:- thread_local(t_l:agenda_slow_op_do_prereqs/0).
:- thread_local(t_l:agenda_suspend_scans/0).
:- thread_local(t_l:agent_current_action/2).
:- thread_local(t_l:already_in_file_term_expansion/0).
:- thread_local(t_l:assert_op_override/1).
:- thread_local(t_l:caller_module/2).
:- thread_local(t_l:consulting_sources/0).
:- thread_local(t_l:current_pttp_db_oper/1).
:- thread_local(t_l:deduceArgTypes/1).
:- thread_local(t_l:disable_px /0).
:- thread_local(t_l:enable_src_loop_checking/0).
:- thread_local(t_l:in_dynamic_reader/1).
:- thread_local(t_l:in_prolog_source_code/0).
:- thread_local(t_l:infAssertedOnly/1).
:- thread_local(t_l:infForward/0).
:- thread_local(t_l:infInstanceOnly/1).
:- thread_local(t_l:infMustArgIsa/0).
:- thread_local(t_l:infSecondOrder/0).
:- thread_local(t_l:infSkipArgIsa/0).
:- thread_local(t_l:infSkipFullExpand/0).
:- thread_local(t_l:infSupertypeName/0).
:- thread_local(t_l:infThirdOrder/0).
:- thread_local(t_l:into_form_code/0).
:- thread_local(t_l:inVoProp/0).
:- thread_local(t_l:is_calling/0).
:- thread_local(t_l:loading_mpred_file/2).
:- thread_local(t_l:mpred_ain_loaded/0).
:- thread_local(t_l:mpred_loads_file/0).
:- thread_local(t_l:mpred_opcall/2).
:- thread_local(t_l:mpred_run_paused/0).
:- thread_local(t_l:no_arg_type_error_checking/0).
:- thread_local(t_l:no_kif_var_coroutines/1).
:- thread_local(t_l:noDBaseHOOKS/1).
:- thread_local(t_l:noDBaseMODs/1).
:- thread_local(t_l:noRandomValues/1).
:- thread_local(t_l:print_mode/1).
:- thread_local(t_l:side_effect_buffer/3).
:- thread_local(t_l:side_effect_ok/0).
:- thread_local(t_l:tracing80/0).
:- thread_local(t_l:use_side_effect_buffer/0).
:- thread_local(t_l:useAltPOS/0).
:- thread_local(t_l:useOnlyExternalDBs/0).
:- thread_local(t_l:usePlTalk/0).
:- thread_local(t_l:verify_side_effect_buffer/0).
:- thread_local(t_l:with_callMPred/1).

end_of_file.



:- multifile(baseKB:'$exported_op'/3). 
:- discontiguous baseKB:'$exported_op'/3. 
:- dynamic baseKB:'$exported_op'/3. 
%:- multifile(system:term_expansion/2).
%:- multifile(user:term_expansion/2).
:- multifile(system:goal_expansion/2).
:- multifile(user:goal_expansion/2).
:- multifile '$si$':'$was_imported_kb_content$'/2.
:- dynamic '$si$':'$was_imported_kb_content$'/2.
:- discontiguous('$si$':'$was_imported_kb_content$'/2).

:- multifile(baseKB:module_local_init/2).
:- dynamic(baseKB:module_local_init/2).
:- discontiguous(baseKB:module_local_init/2).
/*
:- ensure_loaded(library(error)).
:- ensure_loaded(library(backcomp)).
:- ensure_loaded(library(occurs)).
:- ensure_loaded(library(gensym)).
:- ensure_loaded(library(apply)).
:- ensure_loaded(library(memfile)).
:- ensure_loaded(library(terms)).
:- ensure_loaded(library(listing)).
:- ensure_loaded(library(codesio)).
*/


:- op(1100,fx,(shared_multifile)).

                   
/*        
assert_if_new_hh(G):- (catch(G,_,fail)->true;assert(G)).
:- prolog_load_context(module,M),
 once((M==baseKB ;
   ((assert_if_new_hh(baseKB:mtProlog(M)),on_x_log_cont(nop(add_import_module(baseKB,M,end))))))).
*/
%:- multifile(user_db:grant_openid_server/2).

:- multifile(baseKB:coerce_hook/3).
:- dynamic(baseKB:coerce_hook/3).

:- dynamic(lmcache:loaded_external_kbs/1).
:- volatile(lmcache:loaded_external_kbs/1).

:- multifile(system:goal_expansion/2).
:- multifile(user:goal_expansion/2).
:- multifile '$si$':'$was_imported_kb_content$'/2.
:- dynamic '$si$':'$was_imported_kb_content$'/2.
:- discontiguous('$si$':'$was_imported_kb_content$'/2).


:- multifile '$si$':'$was_imported_kb_content$'/2.
:- dynamic '$si$':'$was_imported_kb_content$'/2.
:- discontiguous('$si$':'$was_imported_kb_content$'/2).

:- multifile baseKB:startup_option/2. 
:- dynamic baseKB:startup_option/2. 
:- multifile baseKB:mpred_system_status/2.
:- dynamic baseKB:mpred_system_status/2.
:- multifile(t_l:disable_px/0).
:- thread_local(t_l:disable_px/0).

:- multifile(baseKB:module_local_init/2).
:- dynamic(baseKB:module_local_init/2).
:- discontiguous(baseKB:module_local_init/2).
:- multifile(baseKB:coerce_hook/3).
:- dynamic(baseKB:coerce_hook/3).
:- dynamic(lmcache:loaded_external_kbs/1).
:- volatile(lmcache:loaded_external_kbs/1).
:- multifile(baseKB:argsQuoted/1).
:- dynamic(baseKB:argsQuoted/1).
:- dynamic(baseKB:resolveConflict/1).
:- dynamic(baseKB:agent_call_command/2).
:- export(baseKB:agent_call_command/2).
:- system:import(baseKB:agent_call_command/2).
:- dynamic(baseKB:mpred_skipped_module/1).

:- multifile( baseKB:predicateConventionMt/2).
:- dynamic( baseKB:predicateConventionMt/2).



% :- ensure_loaded(library(logicmoo/util/logicmoo_util_catch)).
% :- ensure_loaded(library(logicmoo/util/logicmoo_util_first)).

:- multifile(baseKB:ignore_file_mpreds/1).
:- dynamic(baseKB:ignore_file_mpreds/1).


:- multifile(baseKB:mpred_is_impl_file/1).
:- dynamic(baseKB:mpred_is_impl_file/1).

/*
:- prolog_load_context(source,O),
   (baseKB:mpred_is_impl_file(O)->(debug,dmsg(throw(baseKB:mpred_is_impl_file(O))),dumpsT,break);
     asserta(baseKB:mpred_is_impl_file(O))).
*/

:- if(\+ current_predicate(lm_util:register_mpred_impl_file/1)).
lm_util:register_mpred_impl_file(F):- (current_prolog_flag(xref,true)->true;
   must((((
    (baseKB:ignore_file_mpreds(F)->true;assertz(baseKB:ignore_file_mpreds(F))))),
   initialization((
   (if_defined(baseKB:ignore_file_mpreds(F),fail)->true;assertz(baseKB:ignore_file_mpreds(F))),
   ((baseKB:mpred_is_impl_file(F))->true;assertz(baseKB:mpred_is_impl_file(F)))),restore)))).
:- endif.

:- prolog_load_context(source,F),lm_util:register_mpred_impl_file(F).
:- prolog_load_context(file,F),lm_util:register_mpred_impl_file(F).



:- style_check(-singleton).
:- set_prolog_flag(generate_debug_info, true).


:-        op(1150,fx,(was_dynamic)),
          op(1150,fx,(was_multifile)),
          op(1150,fx,(was_module_transparent)),
          op(1150,fx,(was_export)),
          op(1150,fx,(shared_multifile)).




:- prolog_load_context(module,M),
 once((M==baseKB;
   ((assert_if_new(baseKB:mtProlog(M)),
   nop(on_x_log_cont(add_import_module(baseKB,M,end))))))).

:-((current_prolog_flag(xref,true)->true;
    (   (prolog_load_context(source,F),
   initialization((
   (if_defined(baseKB:ignore_file_mpreds(F),fail)->true;assertz(baseKB:ignore_file_mpreds(F))),
   ((baseKB:mpred_is_impl_file(F))->true;assertz(baseKB:mpred_is_impl_file(F)))),now))))).


:- virtualize_source_file.

