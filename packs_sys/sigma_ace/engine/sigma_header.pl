% ===================================================================
% COMPLIER OPTIONS
% ===================================================================

:- style_check(-singleton).
:- style_check(-discontiguous).
:- was_style_check(-atom).
:- was_style_check(-string).

% ===================================================================
% DYNAMIC PREDICATES
% ===================================================================


:-dynamic(add_complete_search_args/4).
:-dynamic(add_context/3).
:-dynamic(add_proof_recording_args/4).
:-dynamic(allUnion/2).
:-dynamic(arg_meets_class_contraint/3).
:-dynamic(ask/3).
:-dynamic(atomical/1).
:-dynamic(autoload/0).
:-dynamic(buyErrand/3).
:-dynamic(calculateMostGoodsForCash/3).
:-dynamic(canonicalizeSigmaKBHTML/2).
:-dynamic(chars_to_term/3).
:-dynamic(cheapest/3).
:-dynamic(compileInstanceSubclass/1).
:-dynamic(context_dag/3).
:-dynamic(convertFactHeadWFS/5).
:-dynamic(convertRuleBodyWFS/6).
:-dynamic(convertRuleHeadWFS/6).
:-dynamic(crossref_to_proof/2).
:-dynamic(define_kb_proc/1).
:-dynamic(do_chars/1).
:-dynamic(draw_context_dag/4).
:-dynamic(eval_lr/2).
:-dynamic(event_to_chars/5).
:-dynamic(expanxd_term/2).
:-dynamic(file_get/3).
:-dynamic(file_read/3).
:-dynamic(fixExistentials/2).
:-dynamic(get_job_sent/3).
:-dynamic(get_job_sent_equal_fns/10).
:-dynamic(get_job_sent_holds_args/6).
:-dynamic(get_job_sent_holds_args/7).
:-dynamic(get_job_sent_holds_args_fn/7).
:-dynamic(get_job_sent_term/6).
:-dynamic(get_module/3).
:-dynamic(get_prompt/3).
:-dynamic(get_store/7).
:-dynamic(getAssertionClauses/7).
:-dynamic(getCash/3).
:-dynamic(getConjNF/2).
:-dynamic(getFirstErrand/3).
:-dynamic(getNegationForm/4).
:-dynamic(getTrade/3).
:-dynamic(getTruthCheckResults_ask/10).
:-dynamic(global_set/2).
:-dynamic(hardcoded/1).
:-dynamic(illarg/3).
:-dynamic(in_cache/4).
:-dynamic(inferGoalD/6).
:-dynamic(invoke_cmd/4).
:-dynamic(invokeQueryToBuffer/8).
:-dynamic(invokeQueryWithTime/8).
:-dynamic(is_a_var/1).
:-dynamic(is_instance_of/2).
:-dynamic(is_nth_domain_missing/3).
:-dynamic(is_nth_domain_of/3).
:-dynamic(is_subclass_of/2).
:-dynamic(kb_startup_start/1).
:-dynamic(le_pull/1).
:-dynamic(le_push/1).
:-dynamic(length/3).
:-dynamic(list/1).
:-dynamic(list_to_term/2).
:-dynamic(load_kb_file/1).
:-dynamic(mark_all_surface_to_uncanonicalized/2).
:-dynamic(notace/1).
:-dynamic(numberxvars/3).
:-dynamic(post_each_can_form/1).
:-dynamic(pttp/1).
:-dynamic(put_assoc/5).
:-dynamic(putDeducedNew/3).
:-dynamic(putOnWishlist/4).
:-dynamic(query/0).
:-dynamic(recanonicalizeSigmaKB/1).
:-dynamic(retract/5).
:-dynamic(retract_odbc/6).
:-dynamic(retract_pterm/6).
:-dynamic(save_kb_statuses/0).
:-dynamic(say/2).
:-dynamic(sayn/1).
:-dynamic(search/1).
:-dynamic(sellUneededExtra/2).
:-dynamic(sentence/3).
:-dynamic(sentenceToList/2).
:-dynamic(set_prompt/3).
:-dynamic(setof/2).
:-dynamic(sextof/3).
:-dynamic(sigma_file_type/2).
:-dynamic(sigma_inference/2).
:-dynamic(sigma_invoke_accept_surface/10).
:-dynamic(sigmaCache/10).
:-dynamic(sigmaCache/7).
:-dynamic(sigmaCache/8).
:-dynamic(slg_term_expansion/2).
:-dynamic(source_compile/10).
:-dynamic(source_to_p_file/1).
:-dynamic(subtractProp/3).
:-dynamic(surface_check_entire_kb_ctx/2).
:-dynamic(t_ado_cache/9).
:-dynamic(tell/5).
:-dynamic(test_batch_operation/6).
:-dynamic(tologic/2).
:-dynamic(write_clause_to_file/1).
:-dynamic(write_direct_l/1).
:-dynamic(write_sterm/2).
:-dynamic(xsbRequestStream/1).

:-multifile(add_complete_search_args/4).
:-multifile(add_context/3).
:-multifile(add_proof_recording_args/4).
:-multifile(allUnion/2).
:-multifile(arg_meets_class_contraint/3).
:-multifile(ask/3).
:-multifile(atomical/1).
:-multifile(autoload/0).
:-multifile(buyErrand/3).
:-multifile(calculateMostGoodsForCash/3).
:-multifile(canonicalizeSigmaKBHTML/2).
:-multifile(chars_to_term/3).
:-multifile(cheapest/3).
:-multifile(compileInstanceSubclass/1).
:-multifile(context_dag/3).
:-multifile(convertFactHeadWFS/5).
:-multifile(convertRuleBodyWFS/6).
:-multifile(convertRuleHeadWFS/6).
:-multifile(crossref_to_proof/2).
:-multifile(define_kb_proc/1).
:-multifile(do_chars/1).
:-multifile(draw_context_dag/4).
:-multifile(eval_lr/2).
:-multifile(event_to_chars/5).
:-multifile(expanxd_term/2).
:-multifile(file_get/3).
:-multifile(file_read/3).
:-multifile(fixExistentials/2).
:-multifile(get_job_sent/3).
:-multifile(get_job_sent_equal_fns/10).
:-multifile(get_job_sent_holds_args/6).
:-multifile(get_job_sent_holds_args/7).
:-multifile(get_job_sent_holds_args_fn/7).
:-multifile(get_job_sent_term/6).
:-multifile(get_module/3).
:-multifile(get_prompt/3).
:-multifile(get_store/7).
:-multifile(getAssertionClauses/7).
:-multifile(getCash/3).
:-multifile(getConjNF/2).
:-multifile(getFirstErrand/3).
:-multifile(getNegationForm/4).
:-multifile(getTrade/3).
:-multifile(getTruthCheckResults_ask/10).
:-multifile(global_set/2).
:-multifile(hardcoded/1).
:-multifile(illarg/3).
:-multifile(in_cache/4).
:-multifile(inferGoalD/6).
:-multifile(invoke_cmd/4).
:-multifile(invokeQueryToBuffer/8).
:-multifile(invokeQueryWithTime/8).
:-multifile(is_a_var/1).
:-multifile(is_instance_of/2).
:-multifile(is_nth_domain_missing/3).
:-multifile(is_nth_domain_of/3).
:-multifile(is_subclass_of/2).
:-multifile(kb_startup_start/1).
:-multifile(le_pull/1).
:-multifile(le_push/1).
:-multifile(length/3).
:-multifile(list/1).
:-multifile(list_to_term/2).
:-multifile(load_kb_file/1).
:-multifile(mark_all_surface_to_uncanonicalized/2).
:-multifile(notace/1).
:-multifile(numberxvars/3).
:-multifile(post_each_can_form/1).
:-multifile(pttp/1).
:-multifile(put_assoc/5).
:-multifile(putDeducedNew/3).
:-multifile(putOnWishlist/4).
:-multifile(query/0).
:-multifile(recanonicalizeSigmaKB/1).
:-multifile(retract/5).
:-multifile(retract_odbc/6).
:-multifile(retract_pterm/6).
:-multifile(save_kb_statuses/0).
:-multifile(say/2).
:-multifile(sayn/1).
:-multifile(search/1).
:-multifile(sellUneededExtra/2).
:-multifile(sentence/3).
:-multifile(sentenceToList/2).
:-multifile(set_prompt/3).
:-multifile(setof/2).
:-multifile(sextof/3).
:-multifile(sigma_file_type/2).
:-multifile(sigma_inference/2).
:-multifile(sigma_invoke_accept_surface/10).
:-multifile(sigmaCache/10).
:-multifile(sigmaCache/7).
:-multifile(sigmaCache/8).
:-multifile(slg_term_expansion/2).
:-multifile(source_compile/10).
:-multifile(source_to_p_file/1).
:-multifile(subtractProp/3).
:-multifile(surface_check_entire_kb_ctx/2).
:-multifile(t_ado_cache/9).
:-multifile(tell/5).
:-multifile(test_batch_operation/6).
:-multifile(tologic/2).
:-multifile(write_clause_to_file/1).
:-multifile(write_direct_l/1).
:-multifile(write_sterm/2).
:-multifile(xsbRequestStream/1).

:-was_indexed(sigmaCache(1)).
:-was_indexed(sigmaCache(1,1)).
:-was_indexed(sigmaCache(1,1,1)).
:-was_indexed(sigmaCache(1,1,1,1)).
:-was_indexed(sigmaCache(1,1,1,1,0)).
:-was_indexed(sigmaCache(1,0,1,1,1,0)).

:-dynamic(sigmaCache/1).
:-dynamic(sigmaCache/2).
:-dynamic(sigmaCache/3).
:-dynamic(sigmaCache/4).
:-dynamic(sigmaCache/5).
:-dynamic(sigmaCache/6).
:-dynamic(sigmaCache/9).


:- op(1200,fx,::-).         /* operator for integrity constraints */

%:- op(1200,xfx,<--).
%:- op(1150,fx,[(tabled),(prolog),(default)]).
:- op(900,xfx,<-).
:- op(400,fy,'~').


% User Agent
:-dynamic(always_hide_callback/1).
:-dynamic(always_show_callback/1).
:-dynamic('$SigmaOption'/3).
:-dynamic(client_socket/1).
:-dynamic(callback_pred/1).
:-dynamic(kb_make_status_start/1). 
:-dynamic(saved_note/4).
:-dynamic(act_mem/3).
:-dynamic(server_environment/1).

% Database
:-dynamic(tq_attempted_query/0).      
:-dynamic(title/1).      

% TQ System
:-dynamic(tq_missed_one_answer/0).      
:-dynamic(tq_least_one_answer/0).      
:-dynamic(t_answer_found/1).

:-dynamic(predicates_declared_inline_HL/2).
:-multifile(predicates_declared_inline_HL/2).

:-dynamic('surface-word'/2).
:-dynamic('surface-macro'/2).
:-dynamic('browser-only'/1).
:-dynamic('not-implemented'/1).
:-dynamic('surface-atom'/1).
:-dynamic('surface-single-arity'/1).
:-dynamic('surface-multiple-arity'/1).
:-dynamic('surface-instance'/2).
:-dynamic('surface-subclass'/2).
:-dynamic('surface-class'/1).
:-dynamic('surface-quantifier'/1).
:-multifile('surface-word'/2).
:-multifile('surface-macro'/2).
:-multifile('browser-only'/1).
:-multifile('not-implemented'/1).
:-multifile('surface-atom'/1).
:-multifile('surface-single-arity'/1).
:-multifile('surface-multiple-arity'/1).
:-multifile('surface-instance'/2).
:-multifile('surface-subclass'/2).
:-multifile('surface-class'/1).
:-multifile('surface-quantifier'/1).

%:-set_prolog_flag(unknown,fail).
 
% ===================================================================
% OPERATION PREDICATES
% ===================================================================
% Defaults
:-dynamic(getDefaultKB/1).
:-dynamic(get_default_query_context/1).
:-dynamic(get_default_assertion_context/1).
:-dynamic(version_tag/1).

:-dynamic(answer_found/1).
:-dynamic(sigma_K_scenario/2).
:-dynamic(telling_prolog/0).  % If set asserts clauses into prolog database
:-dynamic(telling_file/0).   % If set write assertions to file
:-dynamic(disp_debug/5).         %PREDICATE RESOLUTON
:-dynamic(contexts/2).            %CONTEXT STATES
:-dynamic(setting_table/2).
:-dynamic(tabling/1).
:-dynamic(tabled_t/1).
:-dynamic(tabled_f/1).
:-dynamic(answer_yes/0).
:-dynamic(already_asked/2).
:-dynamic(save_all/2).
:-dynamic(sigma_K_scenario/6).         %We keep a cache of forall consultations
:-dynamic(consultation_mode_on/0).
:-dynamic(resource_cache/2).
:-dynamic(debuggerize/0).

:-dynamic( le_ele/1).

% ===================================================================
% OPERATOR PRECEDANCE
% ===================================================================

% ===================================================================
% OPERATOR PRECEDANCE
% ===================================================================
:- op(500,xfx,#).
%:- op(500,xfy,:).
:- op(1000,xfy,'=>').
:- op(500,xfx,#).
:- op(500,xfx,'#').
%:- op(500,xfx,'@').

:- op(400,fy,not).    % negation
:- op(500,xfy,and).   % conjunction
:- op(600,xfy,or).   % disjunction
%:- op(500,xfy,:).
:- op(0,xfx, 'equal' ).
:- op(900,xfx,'<=').
:- op(900,xfx,'if').
:- op(1000,xfy,'=>').
:- op(400,fy,known).  % Found in Model
:- op(400,fy,possible).  % Not In Model
:- op(400,fy,next).  % Next time
:- op(400,fy,after).  % Next time
:- op(400,fy,then).  % Next time
:- op(650,xfy,=>).  % implication
:- op(700,xfy,<=>). % equivalence
:- op(400,fy,always).  % Necessity, Always
:- op(400,fy,possible).  % Possibly, Eventually
:- op(400,fy,necessary).  % Necessity


