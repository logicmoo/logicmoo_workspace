%:- set_prolog_flag(debug,true). 
%:- set_prolog_flag(report_error,true). 
%:- set_prolog_flag(verbose_load,true). 
%:- set_prolog_flag(debug_on_error,true). 
:- use_module(library(qsave)).

:- set_prolog_flag(check_memory,false).

:- set_prolog_flag(logicmoo_compiling,true).


:- dynamic(system:term_expansion/2).
:- multifile(system:term_expansion/2).
% find our leaker!
%system:term_expansion(X,_):- X==end_of_file,check_memory(X==end_of_file),fail.

:- ensure_loaded(run_mud_server).
/*
Warning: 14:43:55.898 sldnfdraw:draw_goal/2 is declared as meta_predicate draw_goal(:,+), but has no clauses
Warning: 14:43:55.911 phil:induce_hplp_par_func/9 is declared as meta_predicate induce_hplp_par_func(:,-,-,-,-,-,-,-,-), but has no clauses
Warning: 14:43:55.911 phil:setting_hplp/2 is declared as meta_predicate setting_hplp(:,-), but has no clauses
Warning: 14:43:55.912 phil:objective_hplp_func/8 is declared as meta_predicate objective_hplp_func(:,-,-,-,-,-,-,-), but has no clauses
Warning: 14:43:55.912 phil:induce_hplp_parameters/4 is declared as meta_predicate induce_hplp_parameters(:,-,-,-), but has no clauses
Warning: 14:43:55.912 phil:induce_hplp_par_func/5 is declared as meta_predicate induce_hplp_par_func(:,-,-,-,-), but has no clauses
Warning: 14:43:55.956 mu:memorize_edit/5 is declared as meta_predicate memorize_edit(+,3,?,?,?), but has no clauses
Warning: 14:43:55.956 mu:map_apply_findall/3 is declared as meta_predicate map_apply_findall(+,?,?), but has no clauses
Warning: 14:43:55.956 mu:apply_act/3 is declared as meta_predicate apply_act(+,?,?), but has no clauses
Warning: 14:43:55.957 mu:must_act/3 is declared as meta_predicate must_act(+,?,?), but has no clauses
*/
  :- meta_predicate aleph:abgen(*,*,*,0).
  :- meta_predicate aleph:abgen(*,*,0).
  :- meta_predicate aleph:abgen(*,0).
  :- meta_predicate aleph:add_eqs(*,*,*,*,*,*,0).
  :- meta_predicate aleph:add_eqs(*,*,*,*,*,0).
  :- meta_predicate aleph:add_new_lit(*,*,*,*,*,*,0).
  :- meta_predicate aleph:aleph_induce_theory(*,*,0).
  :- meta_predicate aleph:aleph_induce_theory(*,0).
  :- meta_predicate aleph:create_worker_pool(*,*,*,*,*,0).
  :- meta_predicate aleph:create_worker_pool(*,*,*,*,0).
  :- meta_predicate aleph:cwinduce(0).
  :- meta_predicate aleph:estimate_clauselength_distribution(*,*,*,*,0).
  :- meta_predicate aleph:estimate_clauselength_scores(*,*,*,*,*,0).
  :- meta_predicate aleph:evalfn(*,*,0).
  :- meta_predicate aleph:execute_equality(0).
  :- meta_predicate aleph:find_clause(*,*,0).
  :- meta_predicate aleph:find_clause(*,0).
  :- meta_predicate aleph:find_theory(*,*,0).
  :- meta_predicate aleph:find_theory1(*,0).
  :- meta_predicate aleph:flatten(*,*,*,*,0).
  :- meta_predicate aleph:flatten_atom(*,*,*,*,*,*,*,0).
  :- meta_predicate aleph:flatten_atoms(*,*,*,*,0).
  :- meta_predicate aleph:flatten_lits(*,*,*,*,*,*,*,0).
  :- meta_predicate aleph:gcws(*,*,*,*,0).
  :- meta_predicate aleph:gcws(0).
  :- meta_predicate aleph:gen_abduced_atoms(*,*,0).
  :- meta_predicate aleph:get_atoms(*,*,*,*,*,0).
  :- meta_predicate aleph:get_atoms1(*,*,*,*,*,0).
  :- meta_predicate aleph:get_besthyp(*,0).
  :- meta_predicate aleph:get_gain(*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,0).
  :- meta_predicate aleph:get_gains(*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,0).
  :- meta_predicate aleph:get_refine_gain(*,*,*,*,*,*,*,*,*,*,*,0).
  :- meta_predicate aleph:get_sibgain(*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,0).
  :- meta_predicate aleph:get_sibgains(*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,0).
  :- meta_predicate aleph:get_theory_gain(*,*,*,*,*,*,*,*,*,*,0).
  :- meta_predicate aleph:graphsearch(*,*,0).
  :- meta_predicate aleph:insert_eqs(*,*,*,*,0).
  :- meta_predicate aleph:reduce(*,*,0).
  :- meta_predicate aleph:reduce(*,0).
  :- meta_predicate aleph:rls_refine(*,*,*,0).
  :- meta_predicate aleph:rls_search(*,*,*,*,*,0).
  :- meta_predicate aleph:rls_thread(*,*,*,*,*,0).
  :- meta_predicate aleph:rsat(*,0).
  :- meta_predicate aleph:rsat(0).
  :- meta_predicate aleph:sample_clauses(*,*,0).
  :- meta_predicate aleph:sample_nclauses(*,*,*,0).
  :- meta_predicate aleph:sat(*,*,0).
  :- meta_predicate aleph:sat(*,0).
  :- meta_predicate aleph:search(*,*,0).
  :- meta_predicate aleph:sphyp(0).
  :- meta_predicate aleph:theory_move(*,*,*,0).
  :- meta_predicate aleph:theorysearch(*,*,0).
  :- meta_predicate aleph:time(0,*,*).
  :- meta_predicate aleph:time_loop(*,0,*).
  :- meta_predicate aleph:tsearch(*,*,0).
  :- meta_predicate aleph:work(*,*,0).
  :- meta_predicate aleph:worker(*,*,0).
  :- meta_predicate ape_utils:cpu_time(0,*).
  :- meta_predicate common_logic_compiler:map_each_subterm_compound(2,*,*).
  :- meta_predicate drs_to_coreace:conds_to_andlist(2,*,*).
  :- meta_predicate ec:abdemo_call(0).
  :- meta_predicate ec:ec_current_domain_bi(0).
  :- meta_predicate ec_loader:must_or_dumpst(0).
  :- meta_predicate ec_loader:only_dec_pl(0).
  :- meta_predicate ec_loader:only_lps(0).
  :- meta_predicate ec_lps_convert:'__aux_maplist/3_map_nonvars+2'(*,*,*,2).
  :- meta_predicate ec_lps_convert:map_nonvars(*,2,?,*).
  :- meta_predicate ec_nnf:hide_output(0).
  :- meta_predicate ec_nnf:if_dbg(0).
  :- meta_predicate ec_nnf:thmust(0).
  :- meta_predicate ec_reader:'__aux_maplist/2_ec_on_each_read+2'(*,1,?).
  :- meta_predicate ec_reader:'__aux_maplist/2_process_e_token_with_string+2'(*,1,?).
  :- meta_predicate explanator:expandE(0,*,*).
  :- meta_predicate explanator:explain_preconditions(*,*,*,0,*).
  :- meta_predicate explanator:generate_explanation_tree_relation(0).
  :- meta_predicate explanator:generate_explanation_tree_relation_(0).
  :- meta_predicate explanator:i(*,*,0,*).
  :- meta_predicate explanator:m(*,*,0,*).
  :- meta_predicate explanator:m(*,0,*).
  :- meta_predicate explanator:tree_node(0,*,-).
  :- meta_predicate explanator:w(0,*,0,*).
  :- meta_predicate explanator:w(0,0,*).
  :- meta_predicate get_ape_results:call_ape(0).
  :- meta_predicate grammar_words:try(0,*,*,*).
  :- meta_predicate grammar_words:word(*,0,*,*).
  :- meta_predicate grammar_words:word_initial(*,0,*,*).
  :- meta_predicate grammar_words:word_noninitial(*,0,*,*).
  :- meta_predicate grammar_words:words(*,0,*,*).
  :- meta_predicate grammar_words:words_initial(*,0,*,*).
  :- meta_predicate grammar_words:words_noninitial(*,0,*,*).
  :- meta_predicate icl_int:ex(0,*,*).
  :- meta_predicate icl_int:example_query(0).
  :- meta_predicate icl_int:explain(0).
  :- meta_predicate icl_int:explain(0,*).
  :- meta_predicate icl_int:explain(0,*,*).
  :- meta_predicate icl_int:prove(0,*,*,*,*,*,*,*).
  :- meta_predicate icl_int:prove1(0,*,*,*,*,*,*,*).
  :- meta_predicate icl_int:tprove(*,0,*,*,*,*,*,*,*).
  :- meta_predicate logicmoo_ocl_and_pddl:compile_problem(2,*).
  :- meta_predicate logicmoo_ocl_and_pddl:dess_ify(2,*,*,*,*,*,*).
  :- meta_predicate logicmoo_ocl_and_pddl:record_time(0,*).
  :- meta_predicate logicmoo_ocl_and_pddl:record_time(0,*,*).
  :- meta_predicate logicmoo_startup:enotrace(0).
  :- meta_predicate logicmoo_startup:with_abs_paths(1,?).
  :- meta_predicate lps_pddl_convert:map_pddl_list(1,*).
  :- meta_predicate lps_server_UI:any_call(0).
  :- meta_predicate mcintyre:take_a_sample(*,*,*,2,?).
  :- meta_predicate mpred_type_constraints:freeze_rev(0,?).
  :- meta_predicate mpred_type_constraints:lazy_1(0).
  :- meta_predicate mu:apply_to_goal(2,?,?).
  :- meta_predicate mu:findall_set2(?,0,*).
  :- meta_predicate mu:get_advstate(1,*).
  :- meta_predicate mu:map_tree_pred(2,?,?).
  :- meta_predicate mu:munl_call_hide(0).
  :- meta_predicate mu:notrace_state(0).
  :- meta_predicate mu:reframed_call5(4,?,?,?,?).
  :- meta_predicate parser_sharing:try_maybe_f(*,0,*).
  :- meta_predicate pfc:show_all_debug_pfc(0).
  :- meta_predicate rdf_describe:rdf_bounded_description(3,+,*,+,-).
  :- meta_predicate rdf_describe:rdf_include_labels(3,+,+).
  :- meta_predicate rsasak_forward_wa_star_h_add:replc_structure_vars(2,-).
  :- meta_predicate rsasak_forward_wa_star_h_add:replc_structure_vars1(2,-).
  :- meta_predicate rsasak_pddl_parser:dcgStructSetOpt(*,*,3,?,?).
  :- meta_predicate rsasak_pddl_parser:dcgStructSetOptTraced(*,*,3,?,?).
  :- meta_predicate rsasak_pddl_parser:effected_typed_list(3,*,?,*).
  :- meta_predicate rsasak_pddl_parser:function_typed_list(3,*,?,*).
  :- meta_predicate rsasak_pddl_parser:oneOrMore(3,*,?,?).
  :- meta_predicate rsasak_pddl_parser:typed_list(3,*,?,*).
  :- meta_predicate rsasak_pddl_parser:typed_list0(3,*,?,*).
  :- meta_predicate rsasak_pddl_parser:typed_list_as_list(3,*,?,?).
  :- meta_predicate rsasak_pddl_parser:typed_list_exact(3,*,?,*).
  :- meta_predicate rsasak_pddl_parser:typed_list_keys(3,*,?,*).
  :- meta_predicate rsasak_pddl_parser:zeroOrMore(3,*,?,?).
  :- meta_predicate smtp:do_send_mail(*,+,*,1,*).
  :- meta_predicate smtp:do_send_mail_cont(*,*,*,1,*,*).
  :- meta_predicate smtp:error_cleanup(*,0).
  :- meta_predicate states_explorer:my_ite(0,0,0).
  :- meta_predicate swish_data_source:range(*,:,0).
  :- meta_predicate swish_filesystems:catch_reply(0,?,0).
  :- meta_predicate talkdb:erase_when(2,?,?).
  :- meta_predicate talkdb:save_to_file(*,2,*).
  :- meta_predicate user:keep_user_module(0).
  :- meta_predicate user:only_runtime(0).
  :- meta_predicate verbnet_iface:is_reloading(0).
  :- meta_predicate xml_reader:error_catch(0,*,0).
  :- meta_predicate xml_reader:immediateCall(*,0).

  :- meta_predicate baseKB:term_to_info(*,0).
  :- meta_predicate baseKB:call_close_and_detatch(*,*,*,0).
  :- meta_predicate baseKB:freeze_safe(?,0).
  :- meta_predicate baseKB:in_call(*,0,*,0).
  :- meta_predicate baseKB:want_more_question(0).
  :- meta_predicate baseKB:agent_coerce_for(2,*,?,?,?).
  :- meta_predicate baseKB:run_mud_test_clause(:,0).
  :- meta_predicate baseKB:hooked_random_instance(*,*,0).
  :- meta_predicate baseKB:thread_signal_blocked(*,0).
  :- meta_predicate baseKB:telnet_repl_writer(*,*,*,0).
  :- meta_predicate baseKB:test_call0(0).
  :- meta_predicate baseKB:apply_cond(*,0).
  :- meta_predicate baseKB:now_try_game_dir(0).
  :- meta_predicate baseKB:string_to_info(*,0).
  :- meta_predicate baseKB:random_instance_no_throw0(*,*,0).
  :- meta_predicate baseKB:within_user(0).
  :- meta_predicate baseKB:matcher_to_data_args(3,*,*,?,*,?).
  :- meta_predicate baseKB:dcgParse213(//,//,//,*,*).
  :- meta_predicate baseKB:get_sorted_instances(?,*,3).
  :- meta_predicate baseKB:findall_set(?,0,*).
  :- meta_predicate baseKB:nonvar_must_be(*,0).
  :- meta_predicate baseKB:intersect(*,*,*,*,0,-).
  :- meta_predicate baseKB:punless(0,0).
  :- meta_predicate baseKB:add_game_dir(*,0).
  :- meta_predicate baseKB:cycword_sem(*,*,0).

% :- meta_predicate ec_reader:'__aux_wrapper_594d82f1742fe8b6586d0fcc675e4bd8258e4541'(0).
% :- meta_predicate mpred_type_constraints:'__aux_maplist/2_freeze_rev+1'(*,0).
% :- meta_predicate mpred_type_constraints:'__aux_wrapper_594d82f1742fe8b6586d0fcc675e4bd8258e4541'(0).
% :- meta_predicate swish_svgtree:'__aux_maplist/3_filtered_tree+2'(*,*,3,+).

%- set_prolog_flag(logicmoo_compiling,false).
% :- set_prolog_flag(check_memory,true),
%:- debug, qsave_logicmoo.

end_of_file.


% Restarting analysis ...

Warning: 14:58:23.417 logicmoo_util_bb_env:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.418 typ30f:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.418 nl_iface:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.419 aleph:search/3 is declared as meta_predicate search(?,?,0), but has no clauses
Warning: 14:58:23.419 aleph:aleph_induce_theory/2 is declared as meta_predicate aleph_induce_theory(?,0), but has no clauses
Warning: 14:58:23.419 aleph:add_eqs/6 is declared as meta_predicate add_eqs(?,?,?,?,?,0), but has no clauses
Warning: 14:58:23.419 aleph:sphyp/1 is declared as meta_predicate sphyp(0), but has no clauses
Warning: 14:58:23.419 aleph:get_sibgain/20 is declared as meta_predicate get_sibgain(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,0), but has no clauses
Warning: 14:58:23.419 aleph:sat/3 is declared as meta_predicate sat(?,?,0), but has no clauses
Warning: 14:58:23.419 aleph:rls_search/6 is declared as meta_predicate rls_search(?,?,?,?,?,0), but has no clauses
Warning: 14:58:23.419 aleph:create_worker_pool/5 is declared as meta_predicate create_worker_pool(?,?,?,?,0), but has no clauses
Warning: 14:58:23.419 aleph:gen_abduced_atoms/3 is declared as meta_predicate gen_abduced_atoms(?,?,0), but has no clauses
Warning: 14:58:23.419 aleph:graphsearch/3 is declared as meta_predicate graphsearch(?,?,0), but has no clauses
Warning: 14:58:23.420 aleph:aleph_induce_theory/3 is declared as meta_predicate aleph_induce_theory(?,?,0), but has no clauses
Warning: 14:58:23.420 aleph:abgen/4 is declared as meta_predicate abgen(?,?,?,0), but has no clauses
Warning: 14:58:23.420 aleph:tsearch/3 is declared as meta_predicate tsearch(?,?,0), but has no clauses
Warning: 14:58:23.420 aleph:get_gains/16 is declared as meta_predicate get_gains(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,0), but has no clauses
Warning: 14:58:23.420 aleph:add_eqs/7 is declared as meta_predicate add_eqs(?,?,?,?,?,?,0), but has no clauses
Warning: 14:58:23.420 aleph:rsat/2 is declared as meta_predicate rsat(?,0), but has no clauses
Warning: 14:58:23.420 aleph:theorysearch/3 is declared as meta_predicate theorysearch(?,?,0), but has no clauses
Warning: 14:58:23.420 aleph:get_sibgains/16 is declared as meta_predicate get_sibgains(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,0), but has no clauses
Warning: 14:58:23.420 aleph:gcws/1 is declared as meta_predicate gcws(0), but has no clauses
Warning: 14:58:23.420 aleph:find_theory1/2 is declared as meta_predicate find_theory1(?,0), but has no clauses
Warning: 14:58:23.420 aleph:rsat/1 is declared as meta_predicate rsat(0), but has no clauses
Warning: 14:58:23.420 aleph:evalfn/3 is declared as meta_predicate evalfn(?,?,0), but has no clauses
Warning: 14:58:23.421 aleph:find_clause/3 is declared as meta_predicate find_clause(?,?,0), but has no clauses
Warning: 14:58:23.421 aleph:create_worker_pool/6 is declared as meta_predicate create_worker_pool(?,?,?,?,?,0), but has no clauses
Warning: 14:58:23.421 aleph:add_new_lit/7 is declared as meta_predicate add_new_lit(?,?,?,?,?,?,0), but has no clauses
Warning: 14:58:23.421 aleph:sample_nclauses/4 is declared as meta_predicate sample_nclauses(?,?,?,0), but has no clauses
Warning: 14:58:23.421 aleph:abgen/3 is declared as meta_predicate abgen(?,?,0), but has no clauses
Warning: 14:58:23.421 aleph:get_besthyp/2 is declared as meta_predicate get_besthyp(?,0), but has no clauses
Warning: 14:58:23.421 aleph:estimate_clauselength_distribution/5 is declared as meta_predicate estimate_clauselength_distribution(?,?,?,?,0), but has no clauses
Warning: 14:58:23.421 aleph:rls_thread/6 is declared as meta_predicate rls_thread(?,?,?,?,?,0), but has no clauses
Warning: 14:58:23.421 aleph:find_theory/3 is declared as meta_predicate find_theory(?,?,0), but has no clauses
Warning: 14:58:23.421 aleph:cwinduce/1 is declared as meta_predicate cwinduce(0), but has no clauses
Warning: 14:58:23.421 aleph:sample_clauses/3 is declared as meta_predicate sample_clauses(?,?,0), but has no clauses
Warning: 14:58:23.421 aleph:insert_eqs/5 is declared as meta_predicate insert_eqs(?,?,?,?,0), but has no clauses
Warning: 14:58:23.422 aleph:flatten_atoms/5 is declared as meta_predicate flatten_atoms(?,?,?,?,0), but has no clauses
Warning: 14:58:23.422 aleph:work/3 is declared as meta_predicate work(?,?,0), but has no clauses
Warning: 14:58:23.422 aleph:get_theory_gain/11 is declared as meta_predicate get_theory_gain(?,?,?,?,?,?,?,?,?,?,0), but has no clauses
Warning: 14:58:23.422 aleph:rls_refine/4 is declared as meta_predicate rls_refine(?,?,?,0), but has no clauses
Warning: 14:58:23.422 aleph:estimate_clauselength_scores/6 is declared as meta_predicate estimate_clauselength_scores(?,?,?,?,?,0), but has no clauses
Warning: 14:58:23.422 aleph:get_gain/17 is declared as meta_predicate get_gain(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,0), but has no clauses
Warning: 14:58:23.422 aleph:flatten/5 is declared as meta_predicate flatten(?,?,?,?,0), but has no clauses
Warning: 14:58:23.422 aleph:get_atoms/6 is declared as meta_predicate get_atoms(?,?,?,?,?,0), but has no clauses
Warning: 14:58:23.422 aleph:flatten_atom/8 is declared as meta_predicate flatten_atom(?,?,?,?,?,?,?,0), but has no clauses
Warning: 14:58:23.423 aleph:get_refine_gain/12 is declared as meta_predicate get_refine_gain(?,?,?,?,?,?,?,?,?,?,?,0), but has no clauses
Warning: 14:58:23.423 aleph:theory_move/4 is declared as meta_predicate theory_move(?,?,?,0), but has no clauses
Warning: 14:58:23.423 aleph:time_loop/3 is declared as meta_predicate time_loop(?,0,?), but has no clauses
Warning: 14:58:23.423 aleph:get_atoms1/6 is declared as meta_predicate get_atoms1(?,?,?,?,?,0), but has no clauses
Warning: 14:58:23.423 aleph:time/3 is declared as meta_predicate time(0,?,?), but has no clauses
Warning: 14:58:23.423 aleph:reduce/3 is declared as meta_predicate reduce(?,?,0), but has no clauses
Warning: 14:58:23.423 aleph:flatten_lits/8 is declared as meta_predicate flatten_lits(?,?,?,?,?,?,?,0), but has no clauses
Warning: 14:58:23.423 aleph:gcws/5 is declared as meta_predicate gcws(?,?,?,?,0), but has no clauses
Warning: 14:58:23.423 aleph:find_clause/2 is declared as meta_predicate find_clause(?,0), but has no clauses
Warning: 14:58:23.423 aleph:abgen/2 is declared as meta_predicate abgen(?,0), but has no clauses
Warning: 14:58:23.423 aleph:worker/3 is declared as meta_predicate worker(?,?,0), but has no clauses
Warning: 14:58:23.424 aleph:execute_equality/1 is declared as meta_predicate execute_equality(0), but has no clauses
Warning: 14:58:23.424 aleph:sat/2 is declared as meta_predicate sat(?,0), but has no clauses
Warning: 14:58:23.424 aleph:reduce/2 is declared as meta_predicate reduce(?,0), but has no clauses
Warning: 14:58:23.424 get_ape_results:call_ape/1 is declared as meta_predicate call_ape(0), but has no clauses
Warning: 14:58:23.424 get_ape_results:try_maybe_f/3 is declared as meta_predicate try_maybe_f(?,0,?), but has no clauses
Warning: 14:58:23.424 places:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.425 add_game_dir/2 is declared as meta_predicate add_game_dir(?,0), but has no clauses
Warning: 14:58:23.425 string_to_info/2 is declared as meta_predicate string_to_info(?,0), but has no clauses
Warning: 14:58:23.425 telnet_repl_writer/4 is declared as meta_predicate telnet_repl_writer(?,?,?,0), but has no clauses
Warning: 14:58:23.426 in_call/4 is declared as meta_predicate in_call(?,0,?,0), but has no clauses
Warning: 14:58:23.426 is_reloading/1 is declared as meta_predicate is_reloading(0), but has no clauses
Warning: 14:58:23.426 effected_typed_list/4 is declared as meta_predicate effected_typed_list(3,?,?,?), but has no clauses
Warning: 14:58:23.426 only_dec_pl/1 is declared as meta_predicate only_dec_pl(0), but has no clauses
Warning: 14:58:23.426 test_call0/1 is declared as meta_predicate test_call0(0), but has no clauses
Warning: 14:58:23.426 typed_list/4 is declared as meta_predicate typed_list(3,?,?,?), but has no clauses
Warning: 14:58:23.427 cycword_sem/3 is declared as meta_predicate cycword_sem(?,?,0), but has no clauses
Warning: 14:58:23.427 findall_set/3 is declared as meta_predicate findall_set(?,0,?), but has no clauses
Warning: 14:58:23.427 zeroOrMore/4 is declared as meta_predicate zeroOrMore(3,?,?,?), but has no clauses
Warning: 14:58:23.427 typed_list_as_list/4 is declared as meta_predicate typed_list_as_list(3,?,?,?), but has no clauses
Warning: 14:58:23.427 erase_when/3 is declared as meta_predicate erase_when(2,?,?), but has no clauses
Warning: 14:58:23.428 typed_list_keys/4 is declared as meta_predicate typed_list_keys(3,?,?,?), but has no clauses
Warning: 14:58:23.428 within_user/1 is declared as meta_predicate within_user(0), but has no clauses
Warning: 14:58:23.428 freeze_rev/2 is declared as meta_predicate freeze_rev(0,?), but has no clauses
Warning: 14:58:23.428 now_try_game_dir/1 is declared as meta_predicate now_try_game_dir(0), but has no clauses
Warning: 14:58:23.428 term_to_info/2 is declared as meta_predicate term_to_info(?,0), but has no clauses
Warning: 14:58:23.428 try_maybe_f/3 is declared as meta_predicate try_maybe_f(?,0,?), but has no clauses
Warning: 14:58:23.428 hide_output/1 is declared as meta_predicate hide_output(0), but has no clauses
Warning: 14:58:23.429 tprove/9 is declared as meta_predicate tprove(?,0,?,?,?,?,?,?,?), but has no clauses
Warning: 14:58:23.429 explain/3 is declared as meta_predicate explain(0,?,?), but has no clauses
Warning: 14:58:23.429 prove1/8 is declared as meta_predicate prove1(0,?,?,?,?,?,?,?), but has no clauses
Warning: 14:58:23.429 example_query/1 is declared as meta_predicate example_query(0), but has no clauses
Warning: 14:58:23.429 replc_structure_vars1/2 is declared as meta_predicate replc_structure_vars1(2,-), but has no clauses
Warning: 14:58:23.429 dcgParse213/5 is declared as meta_predicate dcgParse213(//,//,//,?,?), but has no clauses
Warning: 14:58:23.430 get_sorted_instances/3 is declared as meta_predicate get_sorted_instances(?,?,3), but has no clauses
Warning: 14:58:23.430 abdemo_call/1 is declared as meta_predicate abdemo_call(0), but has no clauses
Warning: 14:58:23.430 dcgStructSetOpt/5 is declared as meta_predicate dcgStructSetOpt(?,?,3,?,?), but has no clauses
Warning: 14:58:23.430 save_to_file/3 is declared as meta_predicate save_to_file(?,2,?), but has no clauses
Warning: 14:58:23.431 must_or_dumpst/1 is declared as meta_predicate must_or_dumpst(0), but has no clauses
Warning: 14:58:23.431 call_close_and_detatch/4 is declared as meta_predicate call_close_and_detatch(?,?,?,0), but has no clauses
Warning: 14:58:23.431 map_nonvars/4 is declared as meta_predicate map_nonvars(?,2,?,?), but has no clauses
Warning: 14:58:23.431 immediateCall/2 is declared as meta_predicate immediateCall(?,0), but has no clauses
Warning: 14:58:23.432 error_catch/3 is declared as meta_predicate error_catch(0,?,0), but has no clauses
Warning: 14:58:23.432 ex/3 is declared as meta_predicate ex(0,?,?), but has no clauses
Warning: 14:58:23.432 prove/8 is declared as meta_predicate prove(0,?,?,?,?,?,?,?), but has no clauses
Warning: 14:58:23.432 replc_structure_vars/2 is declared as meta_predicate replc_structure_vars(2,-), but has no clauses
Warning: 14:58:23.432 only_lps/1 is declared as meta_predicate only_lps(0), but has no clauses
Warning: 14:58:23.432 oneOrMore/4 is declared as meta_predicate oneOrMore(3,?,?,?), but has no clauses
Warning: 14:58:23.432 typed_list_exact/4 is declared as meta_predicate typed_list_exact(3,?,?,?), but has no clauses
Warning: 14:58:23.433 thmust/1 is declared as meta_predicate thmust(0), but has no clauses
Warning: 14:58:23.433 dcgStructSetOptTraced/5 is declared as meta_predicate dcgStructSetOptTraced(?,?,3,?,?), but has no clauses
Warning: 14:58:23.433 freeze_safe/2 is declared as meta_predicate freeze_safe(?,0), but has no clauses
Warning: 14:58:23.433 with_abs_paths/2 is declared as meta_predicate with_abs_paths(1,?), but has no clauses
Warning: 14:58:23.433 apply_cond/2 is declared as meta_predicate apply_cond(?,0), but has no clauses
Warning: 14:58:23.434 map_each_subterm_compound/3 is declared as meta_predicate map_each_subterm_compound(2,?,?), but has no clauses
Warning: 14:58:23.434 enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.434 lazy_1/1 is declared as meta_predicate lazy_1(0), but has no clauses
Warning: 14:58:23.434 map_pddl_list/2 is declared as meta_predicate map_pddl_list(1,?), but has no clauses
Warning: 14:58:23.434 want_more_question/1 is declared as meta_predicate want_more_question(0), but has no clauses
Warning: 14:58:23.435 function_typed_list/4 is declared as meta_predicate function_typed_list(3,?,?,?), but has no clauses
Warning: 14:58:23.435 typed_list0/4 is declared as meta_predicate typed_list0(3,?,?,?), but has no clauses
Warning: 14:58:23.435 matcher_to_data_args/6 is declared as meta_predicate matcher_to_data_args(3,?,?,?,?,?), but has no clauses
Warning: 14:58:23.435 run_mud_test_clause/2 is declared as meta_predicate run_mud_test_clause(:,0), but has no clauses
Warning: 14:58:23.435 if_dbg/1 is declared as meta_predicate if_dbg(0), but has no clauses
Warning: 14:58:23.436 instant_prolog_docs:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.437 d0c5:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.437 mpred_agenda:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.437 common_logic_utils:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.438 virtualize_source:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.438 mpred_type_wff:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.438 string:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.438 logicmoo_rsasak:typed_list0/4 is declared as meta_predicate typed_list0(3,?,?,?), but has no clauses
Warning: 14:58:23.438 logicmoo_rsasak:typed_list_as_list/4 is declared as meta_predicate typed_list_as_list(3,?,?,?), but has no clauses
Warning: 14:58:23.438 logicmoo_rsasak:dcgStructSetOpt/5 is declared as meta_predicate dcgStructSetOpt(?,?,3,?,?), but has no clauses
Warning: 14:58:23.438 logicmoo_rsasak:replc_structure_vars/2 is declared as meta_predicate replc_structure_vars(2,-), but has no clauses
Warning: 14:58:23.438 logicmoo_rsasak:effected_typed_list/4 is declared as meta_predicate effected_typed_list(3,?,?,?), but has no clauses
Warning: 14:58:23.439 logicmoo_rsasak:typed_list_exact/4 is declared as meta_predicate typed_list_exact(3,?,?,?), but has no clauses
Warning: 14:58:23.439 logicmoo_rsasak:oneOrMore/4 is declared as meta_predicate oneOrMore(3,?,?,?), but has no clauses
Warning: 14:58:23.439 logicmoo_rsasak:typed_list_keys/4 is declared as meta_predicate typed_list_keys(3,?,?,?), but has no clauses
Warning: 14:58:23.439 logicmoo_rsasak:function_typed_list/4 is declared as meta_predicate function_typed_list(3,?,?,?), but has no clauses
Warning: 14:58:23.439 logicmoo_rsasak:zeroOrMore/4 is declared as meta_predicate zeroOrMore(3,?,?,?), but has no clauses
Warning: 14:58:23.439 logicmoo_rsasak:replc_structure_vars1/2 is declared as meta_predicate replc_structure_vars1(2,-), but has no clauses
Warning: 14:58:23.439 logicmoo_rsasak:typed_list/4 is declared as meta_predicate typed_list(3,?,?,?), but has no clauses
Warning: 14:58:23.439 logicmoo_rsasak:dcgStructSetOptTraced/5 is declared as meta_predicate dcgStructSetOptTraced(?,?,3,?,?), but has no clauses
Warning: 14:58:23.440 dcg_meta:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.440 grammar_functionwords:word/4 is declared as meta_predicate word(?,0,?,?), but has no clauses
Warning: 14:58:23.440 grammar_functionwords:word_noninitial/4 is declared as meta_predicate word_noninitial(?,0,?,?), but has no clauses
Warning: 14:58:23.440 grammar_functionwords:word_initial/4 is declared as meta_predicate word_initial(?,0,?,?), but has no clauses
Warning: 14:58:23.440 grammar_functionwords:try/4 is declared as meta_predicate try(0,?,?,?), but has no clauses
Warning: 14:58:23.440 grammar_functionwords:words/4 is declared as meta_predicate words(?,0,?,?), but has no clauses
Warning: 14:58:23.440 grammar_functionwords:words_initial/4 is declared as meta_predicate words_initial(?,0,?,?), but has no clauses
Warning: 14:58:23.440 grammar_functionwords:words_noninitial/4 is declared as meta_predicate words_noninitial(?,0,?,?), but has no clauses
Warning: 14:58:23.441 lps_server_UI:any_call/1 is declared as meta_predicate any_call(0), but has no clauses
Warning: 14:58:23.441 no_repeats:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.441 clex_iface:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.441 rsasak_pddl_parser:oneOrMore/4 is declared as meta_predicate oneOrMore(3,?,?,?), but has no clauses
Warning: 14:58:23.441 rsasak_pddl_parser:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.441 rsasak_pddl_parser:zeroOrMore/4 is declared as meta_predicate zeroOrMore(3,?,?,?), but has no clauses
Warning: 14:58:23.442 rsasak_pddl_parser:typed_list/4 is declared as meta_predicate typed_list(3,?,?,?), but has no clauses
Warning: 14:58:23.442 rsasak_pddl_parser:typed_list_as_list/4 is declared as meta_predicate typed_list_as_list(3,?,?,?), but has no clauses
Warning: 14:58:23.442 rsasak_pddl_parser:dcgStructSetOpt/5 is declared as meta_predicate dcgStructSetOpt(?,?,3,?,?), but has no clauses
Warning: 14:58:23.442 rsasak_pddl_parser:typed_list_exact/4 is declared as meta_predicate typed_list_exact(3,?,?,?), but has no clauses
Warning: 14:58:23.442 rsasak_pddl_parser:function_typed_list/4 is declared as meta_predicate function_typed_list(3,?,?,?), but has no clauses
Warning: 14:58:23.442 rsasak_pddl_parser:dcgStructSetOptTraced/5 is declared as meta_predicate dcgStructSetOptTraced(?,?,3,?,?), but has no clauses
Warning: 14:58:23.442 rsasak_pddl_parser:typed_list0/4 is declared as meta_predicate typed_list0(3,?,?,?), but has no clauses
Warning: 14:58:23.442 rsasak_pddl_parser:effected_typed_list/4 is declared as meta_predicate effected_typed_list(3,?,?,?), but has no clauses
Warning: 14:58:23.442 rsasak_pddl_parser:typed_list_keys/4 is declared as meta_predicate typed_list_keys(3,?,?,?), but has no clauses
Warning: 14:58:23.442 tagbody:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.443 fn:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.443 ape_utils:word_noninitial/4 is declared as meta_predicate word_noninitial(?,0,?,?), but has no clauses
Warning: 14:58:23.443 ape_utils:word/4 is declared as meta_predicate word(?,0,?,?), but has no clauses
Warning: 14:58:23.443 ape_utils:cpu_time/2 is declared as meta_predicate cpu_time(0,?), but has no clauses
Warning: 14:58:23.444 ape_utils:try/4 is declared as meta_predicate try(0,?,?,?), but has no clauses
Warning: 14:58:23.444 ape_utils:words/4 is declared as meta_predicate words(?,0,?,?), but has no clauses
Warning: 14:58:23.444 ape_utils:words_initial/4 is declared as meta_predicate words_initial(?,0,?,?), but has no clauses
Warning: 14:58:23.444 ape_utils:word_initial/4 is declared as meta_predicate word_initial(?,0,?,?), but has no clauses
Warning: 14:58:23.444 ape_utils:words_noninitial/4 is declared as meta_predicate words_noninitial(?,0,?,?), but has no clauses
Warning: 14:58:23.444 thread1ng:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.444 ec_lps_convert:'__aux_maplist/3_map_nonvars+2'/4 is declared as meta_predicate __aux_maplist/3_map_nonvars+2(?,?,?,2), but has no clauses
Warning: 14:58:23.444 ec_lps_convert:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.445 ec_lps_convert:map_nonvars/4 is declared as meta_predicate map_nonvars(?,2,?,?), but has no clauses
Warning: 14:58:23.445 ec_lps_convert:only_dec_pl/1 is declared as meta_predicate only_dec_pl(0), but has no clauses
Warning: 14:58:23.445 ec_lps_convert:must_or_dumpst/1 is declared as meta_predicate must_or_dumpst(0), but has no clauses
Warning: 14:58:23.445 ec_lps_convert:only_lps/1 is declared as meta_predicate only_lps(0), but has no clauses
Warning: 14:58:23.445 mud_http_hmud:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.445 hook_database:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.446 swish_data_source:range/3 is declared as meta_predicate range(?,:,0), but has no clauses
Warning: 14:58:23.446 talkdb:save_to_file/3 is declared as meta_predicate save_to_file(?,2,?), but has no clauses
Warning: 14:58:23.446 talkdb:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.446 talkdb:erase_when/3 is declared as meta_predicate erase_when(2,?,?), but has no clauses
Warning: 14:58:23.446 logicmoo_util_bb_frame:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.467 enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.467 grammar_contentwords:word_noninitial/4 is declared as meta_predicate word_noninitial(?,0,?,?), but has no clauses
Warning: 14:58:23.467 grammar_contentwords:word/4 is declared as meta_predicate word(?,0,?,?), but has no clauses
Warning: 14:58:23.468 grammar_contentwords:try/4 is declared as meta_predicate try(0,?,?,?), but has no clauses
Warning: 14:58:23.468 grammar_contentwords:words/4 is declared as meta_predicate words(?,0,?,?), but has no clauses
Warning: 14:58:23.468 grammar_contentwords:words_initial/4 is declared as meta_predicate words_initial(?,0,?,?), but has no clauses
Warning: 14:58:23.468 grammar_contentwords:word_initial/4 is declared as meta_predicate word_initial(?,0,?,?), but has no clauses
Warning: 14:58:23.468 grammar_contentwords:words_noninitial/4 is declared as meta_predicate words_noninitial(?,0,?,?), but has no clauses
Warning: 14:58:23.468 print:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.469 pretty_clauses:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.469 rsasak_forward_wa_star_h_add:oneOrMore/4 is declared as meta_predicate oneOrMore(3,?,?,?), but has no clauses
Warning: 14:58:23.469 rsasak_forward_wa_star_h_add:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.469 rsasak_forward_wa_star_h_add:zeroOrMore/4 is declared as meta_predicate zeroOrMore(3,?,?,?), but has no clauses
Warning: 14:58:23.469 rsasak_forward_wa_star_h_add:typed_list/4 is declared as meta_predicate typed_list(3,?,?,?), but has no clauses
Warning: 14:58:23.469 rsasak_forward_wa_star_h_add:typed_list_as_list/4 is declared as meta_predicate typed_list_as_list(3,?,?,?), but has no clauses
Warning: 14:58:23.469 rsasak_forward_wa_star_h_add:dcgStructSetOpt/5 is declared as meta_predicate dcgStructSetOpt(?,?,3,?,?), but has no clauses
Warning: 14:58:23.469 rsasak_forward_wa_star_h_add:typed_list_exact/4 is declared as meta_predicate typed_list_exact(3,?,?,?), but has no clauses
Warning: 14:58:23.470 rsasak_forward_wa_star_h_add:function_typed_list/4 is declared as meta_predicate function_typed_list(3,?,?,?), but has no clauses
Warning: 14:58:23.470 rsasak_forward_wa_star_h_add:replc_structure_vars1/2 is declared as meta_predicate replc_structure_vars1(2,-), but has no clauses
Warning: 14:58:23.470 rsasak_forward_wa_star_h_add:dcgStructSetOptTraced/5 is declared as meta_predicate dcgStructSetOptTraced(?,?,3,?,?), but has no clauses
Warning: 14:58:23.470 rsasak_forward_wa_star_h_add:typed_list0/4 is declared as meta_predicate typed_list0(3,?,?,?), but has no clauses
Warning: 14:58:23.470 rsasak_forward_wa_star_h_add:replc_structure_vars/2 is declared as meta_predicate replc_structure_vars(2,-), but has no clauses
Warning: 14:58:23.470 rsasak_forward_wa_star_h_add:effected_typed_list/4 is declared as meta_predicate effected_typed_list(3,?,?,?), but has no clauses
Warning: 14:58:23.470 rsasak_forward_wa_star_h_add:typed_list_keys/4 is declared as meta_predicate typed_list_keys(3,?,?,?), but has no clauses
Warning: 14:58:23.470 parser_tokenize:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.470 parser_tokenize:try_maybe_f/3 is declared as meta_predicate try_maybe_f(?,0,?), but has no clauses
Warning: 14:58:23.470 loadfile:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.471 logicmoo_utils:with_abs_paths/2 is declared as meta_predicate with_abs_paths(1,?), but has no clauses
Warning: 14:58:23.471 logicmoo_utils:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.471 tests:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.472 dumpst:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.472 dumpst:with_abs_paths/2 is declared as meta_predicate with_abs_paths(1,?), but has no clauses
Warning: 14:58:23.473 streams:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.473 macr:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.474 nb_set_term:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.475 os7r33M:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.475 swish_filesystems:catch_reply/3 is declared as meta_predicate catch_reply(0,?,0), but has no clauses
Warning: 14:58:23.477 parser_all:try_maybe_f/3 is declared as meta_predicate try_maybe_f(?,0,?), but has no clauses
Warning: 14:58:23.479 parser_all:with_abs_paths/2 is declared as meta_predicate with_abs_paths(1,?), but has no clauses
Warning: 14:58:23.479 parser_all:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.479 parser_all:call_ape/1 is declared as meta_predicate call_ape(0), but has no clauses
Warning: 14:58:23.480 logicmoo_mud:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.480 env:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.481 s3xpr:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.482 ace_to_drs:cpu_time/2 is declared as meta_predicate cpu_time(0,?), but has no clauses
Warning: 14:58:23.482 logicmoo_i_cyc_rewriting:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.482 mcintyre:take_a_sample/5 is declared as meta_predicate take_a_sample(?,?,?,2,?), but has no clauses
Warning: 14:58:23.483 in1t:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.483 arglists:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.484 grammar:word_noninitial/4 is declared as meta_predicate word_noninitial(?,0,?,?), but has no clauses
Warning: 14:58:23.484 grammar:word/4 is declared as meta_predicate word(?,0,?,?), but has no clauses
Warning: 14:58:23.484 grammar:cpu_time/2 is declared as meta_predicate cpu_time(0,?), but has no clauses
Warning: 14:58:23.484 grammar:try/4 is declared as meta_predicate try(0,?,?,?), but has no clauses
Warning: 14:58:23.484 grammar:words/4 is declared as meta_predicate words(?,0,?,?), but has no clauses
Warning: 14:58:23.485 grammar:words_initial/4 is declared as meta_predicate words_initial(?,0,?,?), but has no clauses
Warning: 14:58:23.485 grammar:word_initial/4 is declared as meta_predicate word_initial(?,0,?,?), but has no clauses
Warning: 14:58:23.485 grammar:words_noninitial/4 is declared as meta_predicate words_noninitial(?,0,?,?), but has no clauses
Warning: 14:58:23.485 mpred_storage:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.485 mu:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.486 mu:findall_set2/3 is declared as meta_predicate findall_set2(?,0,?), but has no clauses
Warning: 14:58:23.486 mu:try_maybe_f/3 is declared as meta_predicate try_maybe_f(?,0,?), but has no clauses
Warning: 14:58:23.486 mu:munl_call_hide/1 is declared as meta_predicate munl_call_hide(0), but has no clauses
Warning: 14:58:23.486 mu:get_advstate/2 is declared as meta_predicate get_advstate(1,?), but has no clauses
Warning: 14:58:23.486 mu:apply_to_goal/3 is declared as meta_predicate apply_to_goal(2,?,?), but has no clauses
Warning: 14:58:23.487 mu:reframed_call5/5 is declared as meta_predicate reframed_call5(4,?,?,?,?), but has no clauses
Warning: 14:58:23.487 mu:map_tree_pred/3 is declared as meta_predicate map_tree_pred(2,?,?), but has no clauses
Warning: 14:58:23.487 mu:notrace_state/1 is declared as meta_predicate notrace_state(0), but has no clauses
Warning: 14:58:23.487 portray_vars:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.488 package:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.488 typcheck:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.488 symbol:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.489 pathname:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.489 logicmoo_util_filesystem:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.489 smtp:do_send_mail_cont/6 is declared as meta_predicate do_send_mail_cont(?,?,?,1,?,?), but has no clauses
Warning: 14:58:23.489 smtp:do_send_mail/5 is declared as meta_predicate do_send_mail(?,+,?,1,?), but has no clauses
Warning: 14:58:23.489 smtp:error_cleanup/2 is declared as meta_predicate error_cleanup(?,0), but has no clauses
Warning: 14:58:23.490 common_logic_loader:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.491 parser_chat80:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.491 parser_chat80:try_maybe_f/3 is declared as meta_predicate try_maybe_f(?,0,?), but has no clauses
Warning: 14:58:23.492 xml_reader:error_catch/3 is declared as meta_predicate error_catch(0,?,0), but has no clauses
Warning: 14:58:23.492 xml_reader:immediateCall/2 is declared as meta_predicate immediateCall(?,0), but has no clauses
Warning: 14:58:23.492 xml_reader:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.493 rdf_describe:rdf_include_labels/3 is declared as meta_predicate rdf_include_labels(3,+,+), but has no clauses
Warning: 14:58:23.493 rdf_describe:rdf_bounded_description/5 is declared as meta_predicate rdf_bounded_description(3,+,?,+,-), but has no clauses
Warning: 14:58:23.493 parser_pldata:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.493 explanator:w/4 is declared as meta_predicate w(0,?,0,?), but has no clauses
Warning: 14:58:23.493 explanator:m/4 is declared as meta_predicate m(?,?,0,?), but has no clauses
Warning: 14:58:23.493 explanator:w/3 is declared as meta_predicate w(0,0,?), but has no clauses
Warning: 14:58:23.494 explanator:expandE/3 is declared as meta_predicate expandE(0,?,?), but has no clauses
Warning: 14:58:23.494 explanator:m/3 is declared as meta_predicate m(?,0,?), but has no clauses
Warning: 14:58:23.494 explanator:generate_explanation_tree_relation_/1 is declared as meta_predicate generate_explanation_tree_relation_(0), but has no clauses
Warning: 14:58:23.494 explanator:generate_explanation_tree_relation/1 is declared as meta_predicate generate_explanation_tree_relation(0), but has no clauses
Warning: 14:58:23.494 explanator:tree_node/3 is declared as meta_predicate tree_node(0,?,-), but has no clauses
Warning: 14:58:23.494 explanator:i/4 is declared as meta_predicate i(?,?,0,?), but has no clauses
Warning: 14:58:23.494 explanator:explain_preconditions/5 is declared as meta_predicate explain_preconditions(?,?,?,0,?), but has no clauses
Warning: 14:58:23.494 common_logic_snark:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.495 '8ball':enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.495 logicmoo_ocl_and_pddl:record_time/3 is declared as meta_predicate record_time(0,?,?), but has no clauses
Warning: 14:58:23.495 logicmoo_ocl_and_pddl:compile_problem/2 is declared as meta_predicate compile_problem(2,?), but has no clauses
Warning: 14:58:23.495 logicmoo_ocl_and_pddl:record_time/2 is declared as meta_predicate record_time(0,?), but has no clauses
Warning: 14:58:23.495 logicmoo_ocl_and_pddl:dess_ify/7 is declared as meta_predicate dess_ify(2,?,?,?,?,?,?), but has no clauses
Warning: 14:58:23.496 states_explorer:my_ite/3 is declared as meta_predicate my_ite(0,0,0), but has no clauses
Warning: 14:58:23.496 eggdrop:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.496 decls:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.496 butterfly:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.497 parser_e2c:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.497 body:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.498 enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.498 multimodal_dcg:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.499 pfc:show_all_debug_pfc/1 is declared as meta_predicate show_all_debug_pfc(0), but has no clauses
Warning: 14:58:23.499 common_logic_boxlog:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.499 logicmoo_ocl:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.520 predicate_inheritance:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.520 logicmoo_util_strings:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.520 clause_attvars:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.520 errs:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.521 c0ndif:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.521 ec_common:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.521 ec_common:only_dec_pl/1 is declared as meta_predicate only_dec_pl(0), but has no clauses
Warning: 14:58:23.522 ec_common:must_or_dumpst/1 is declared as meta_predicate must_or_dumpst(0), but has no clauses
Warning: 14:58:23.522 ec_common:only_lps/1 is declared as meta_predicate only_lps(0), but has no clauses
Warning: 14:58:23.522 mth:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.523 subclause_expansion:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.523 mpred_type_constraints:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.523 mpred_type_constraints:freeze_rev/2 is declared as meta_predicate freeze_rev(0,?), but has no clauses
Warning: 14:58:23.523 mpred_type_constraints:lazy_1/1 is declared as meta_predicate lazy_1(0), but has no clauses
Warning: 14:58:23.524 aray:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.524 v4rZ:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.525 ec_common_swi:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.525 ec_loader:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.526 ec_loader:hide_output/1 is declared as meta_predicate hide_output(0), but has no clauses
Warning: 14:58:23.526 ec_loader:only_dec_pl/1 is declared as meta_predicate only_dec_pl(0), but has no clauses
Warning: 14:58:23.526 ec_loader:only_lps/1 is declared as meta_predicate only_lps(0), but has no clauses
Warning: 14:58:23.526 ec_loader:if_dbg/1 is declared as meta_predicate if_dbg(0), but has no clauses
Warning: 14:58:23.526 ec_loader:thmust/1 is declared as meta_predicate thmust(0), but has no clauses
Warning: 14:58:23.527 ec_loader:must_or_dumpst/1 is declared as meta_predicate must_or_dumpst(0), but has no clauses
Warning: 14:58:23.527 ec:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.527 ec:only_dec_pl/1 is declared as meta_predicate only_dec_pl(0), but has no clauses
Warning: 14:58:23.527 ec:only_lps/1 is declared as meta_predicate only_lps(0), but has no clauses
Warning: 14:58:23.528 ec:ec_current_domain_bi/1 is declared as meta_predicate ec_current_domain_bi(0), but has no clauses
Warning: 14:58:23.528 ec:abdemo_call/1 is declared as meta_predicate abdemo_call(0), but has no clauses
Warning: 14:58:23.528 ec:must_or_dumpst/1 is declared as meta_predicate must_or_dumpst(0), but has no clauses
Warning: 14:58:23.528 util_varnames:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.529 r3d3rz:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.529 rtrace:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.529 ec_reader:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.529 ec_reader:'__aux_maplist/2_process_e_token_with_string+2'/3 is declared as meta_predicate __aux_maplist/2_process_e_token_with_string+2(?,1,?), but has no clauses
Warning: 14:58:23.530 ec_reader:'__aux_maplist/2_ec_on_each_read+2'/3 is declared as meta_predicate __aux_maplist/2_ec_on_each_read+2(?,1,?), but has no clauses
Warning: 14:58:23.530 comp:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.530 callp:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.531 mpred_type_naming:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.531 plkb0988_iface:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.531 plkb0988_iface:with_abs_paths/2 is declared as meta_predicate with_abs_paths(1,?), but has no clauses
Warning: 14:58:23.531 attvar_serializer:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.532 cl0z3rs:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.532 enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.533 with_abs_paths/2 is declared as meta_predicate with_abs_paths(1,?), but has no clauses
Warning: 14:58:23.534 soops:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.534 common_logic_modalization:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.534 ec_nnf:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.534 ec_nnf:if_dbg/1 is declared as meta_predicate if_dbg(0), but has no clauses
Warning: 14:58:23.535 ec_nnf:thmust/1 is declared as meta_predicate thmust(0), but has no clauses
Warning: 14:58:23.535 ec_nnf:hide_output/1 is declared as meta_predicate hide_output(0), but has no clauses
Warning: 14:58:23.535 c0nz:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.536 cl:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.536 eq4l1y:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.536 logicmoo_cg:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.538 kp4rms:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.538 swish_app:take_a_sample/5 is declared as meta_predicate take_a_sample(?,?,?,2,?), but has no clauses
Warning: 14:58:23.538 lps_pddl_convert:oneOrMore/4 is declared as meta_predicate oneOrMore(3,?,?,?), but has no clauses
Warning: 14:58:23.538 lps_pddl_convert:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.539 lps_pddl_convert:zeroOrMore/4 is declared as meta_predicate zeroOrMore(3,?,?,?), but has no clauses
Warning: 14:58:23.539 lps_pddl_convert:map_pddl_list/2 is declared as meta_predicate map_pddl_list(1,?), but has no clauses
Warning: 14:58:23.539 lps_pddl_convert:typed_list/4 is declared as meta_predicate typed_list(3,?,?,?), but has no clauses
Warning: 14:58:23.539 lps_pddl_convert:typed_list_as_list/4 is declared as meta_predicate typed_list_as_list(3,?,?,?), but has no clauses
Warning: 14:58:23.539 lps_pddl_convert:dcgStructSetOpt/5 is declared as meta_predicate dcgStructSetOpt(?,?,3,?,?), but has no clauses
Warning: 14:58:23.539 lps_pddl_convert:map_nonvars/4 is declared as meta_predicate map_nonvars(?,2,?,?), but has no clauses
Warning: 14:58:23.539 lps_pddl_convert:typed_list_exact/4 is declared as meta_predicate typed_list_exact(3,?,?,?), but has no clauses
Warning: 14:58:23.539 lps_pddl_convert:function_typed_list/4 is declared as meta_predicate function_typed_list(3,?,?,?), but has no clauses
Warning: 14:58:23.539 lps_pddl_convert:dcgStructSetOptTraced/5 is declared as meta_predicate dcgStructSetOptTraced(?,?,3,?,?), but has no clauses
Warning: 14:58:23.539 lps_pddl_convert:typed_list0/4 is declared as meta_predicate typed_list0(3,?,?,?), but has no clauses
Warning: 14:58:23.540 lps_pddl_convert:effected_typed_list/4 is declared as meta_predicate effected_typed_list(3,?,?,?), but has no clauses
Warning: 14:58:23.540 lps_pddl_convert:typed_list_keys/4 is declared as meta_predicate typed_list_keys(3,?,?,?), but has no clauses
Warning: 14:58:23.540 readtables:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.541 icl_int:prove1/8 is declared as meta_predicate prove1(0,?,?,?,?,?,?,?), but has no clauses
Warning: 14:58:23.541 icl_int:tprove/9 is declared as meta_predicate tprove(?,0,?,?,?,?,?,?,?), but has no clauses
Warning: 14:58:23.541 icl_int:example_query/1 is declared as meta_predicate example_query(0), but has no clauses
Warning: 14:58:23.541 icl_int:explain/3 is declared as meta_predicate explain(0,?,?), but has no clauses
Warning: 14:58:23.541 icl_int:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.541 icl_int:ex/3 is declared as meta_predicate ex(0,?,?), but has no clauses
Warning: 14:58:23.541 icl_int:prove/8 is declared as meta_predicate prove(0,?,?,?,?,?,?,?), but has no clauses
Warning: 14:58:23.541 pfc_test:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.542 s3q:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.542 logicmoo_lib:with_abs_paths/2 is declared as meta_predicate with_abs_paths(1,?), but has no clauses
Warning: 14:58:23.542 logicmoo_lib:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.543 mpred_stubs_file_module:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.543 parser_ape:call_ape/1 is declared as meta_predicate call_ape(0), but has no clauses
Warning: 14:58:23.543 parser_ape:cpu_time/2 is declared as meta_predicate cpu_time(0,?), but has no clauses
Warning: 14:58:23.543 parser_ape:try_maybe_f/3 is declared as meta_predicate try_maybe_f(?,0,?), but has no clauses
Warning: 14:58:23.543 locally_each:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.543 common_logic_reordering:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.543 mizepro:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.544 clstructs:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.544 mpred_hooks:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.545 add_game_dir/2 is declared as meta_predicate add_game_dir(?,0), but has no clauses
Warning: 14:58:23.545 string_to_info/2 is declared as meta_predicate string_to_info(?,0), but has no clauses
Warning: 14:58:23.545 telnet_repl_writer/4 is declared as meta_predicate telnet_repl_writer(?,?,?,0), but has no clauses
Warning: 14:58:23.546 in_call/4 is declared as meta_predicate in_call(?,0,?,0), but has no clauses
Warning: 14:58:23.546 is_reloading/1 is declared as meta_predicate is_reloading(0), but has no clauses
Warning: 14:58:23.546 effected_typed_list/4 is declared as meta_predicate effected_typed_list(3,?,?,?), but has no clauses
Warning: 14:58:23.546 only_dec_pl/1 is declared as meta_predicate only_dec_pl(0), but has no clauses
Warning: 14:58:23.546 typed_list/4 is declared as meta_predicate typed_list(3,?,?,?), but has no clauses
Warning: 14:58:23.547 cycword_sem/3 is declared as meta_predicate cycword_sem(?,?,0), but has no clauses
Warning: 14:58:23.547 findall_set/3 is declared as meta_predicate findall_set(?,0,?), but has no clauses
Warning: 14:58:23.547 zeroOrMore/4 is declared as meta_predicate zeroOrMore(3,?,?,?), but has no clauses
Warning: 14:58:23.547 typed_list_as_list/4 is declared as meta_predicate typed_list_as_list(3,?,?,?), but has no clauses
Warning: 14:58:23.548 erase_when/3 is declared as meta_predicate erase_when(2,?,?), but has no clauses
Warning: 14:58:23.548 typed_list_keys/4 is declared as meta_predicate typed_list_keys(3,?,?,?), but has no clauses
Warning: 14:58:23.548 within_user/1 is declared as meta_predicate within_user(0), but has no clauses
Warning: 14:58:23.548 freeze_rev/2 is declared as meta_predicate freeze_rev(0,?), but has no clauses
Warning: 14:58:23.548 now_try_game_dir/1 is declared as meta_predicate now_try_game_dir(0), but has no clauses
Warning: 14:58:23.549 term_to_info/2 is declared as meta_predicate term_to_info(?,0), but has no clauses
Warning: 14:58:23.549 try_maybe_f/3 is declared as meta_predicate try_maybe_f(?,0,?), but has no clauses
Warning: 14:58:23.549 hide_output/1 is declared as meta_predicate hide_output(0), but has no clauses
Warning: 14:58:23.549 tprove/9 is declared as meta_predicate tprove(?,0,?,?,?,?,?,?,?), but has no clauses
Warning: 14:58:23.549 explain/3 is declared as meta_predicate explain(0,?,?), but has no clauses
Warning: 14:58:23.549 prove1/8 is declared as meta_predicate prove1(0,?,?,?,?,?,?,?), but has no clauses
Warning: 14:58:23.550 example_query/1 is declared as meta_predicate example_query(0), but has no clauses
Warning: 14:58:23.550 replc_structure_vars1/2 is declared as meta_predicate replc_structure_vars1(2,-), but has no clauses
Warning: 14:58:23.550 dcgParse213/5 is declared as meta_predicate dcgParse213(//,//,//,?,?), but has no clauses
Warning: 14:58:23.550 get_sorted_instances/3 is declared as meta_predicate get_sorted_instances(?,?,3), but has no clauses
Warning: 14:58:23.550 abdemo_call/1 is declared as meta_predicate abdemo_call(0), but has no clauses
Warning: 14:58:23.551 dcgStructSetOpt/5 is declared as meta_predicate dcgStructSetOpt(?,?,3,?,?), but has no clauses
Warning: 14:58:23.551 save_to_file/3 is declared as meta_predicate save_to_file(?,2,?), but has no clauses
Warning: 14:58:23.551 reframed_call5/5 is declared as meta_predicate reframed_call5(4,?,?,?,?), but has no clauses
Warning: 14:58:23.551 must_or_dumpst/1 is declared as meta_predicate must_or_dumpst(0), but has no clauses
Warning: 14:58:23.552 call_close_and_detatch/4 is declared as meta_predicate call_close_and_detatch(?,?,?,0), but has no clauses
Warning: 14:58:23.552 map_nonvars/4 is declared as meta_predicate map_nonvars(?,2,?,?), but has no clauses
Warning: 14:58:23.552 immediateCall/2 is declared as meta_predicate immediateCall(?,0), but has no clauses
Warning: 14:58:23.552 error_catch/3 is declared as meta_predicate error_catch(0,?,0), but has no clauses
Warning: 14:58:23.553 ex/3 is declared as meta_predicate ex(0,?,?), but has no clauses
Warning: 14:58:23.553 prove/8 is declared as meta_predicate prove(0,?,?,?,?,?,?,?), but has no clauses
Warning: 14:58:23.553 replc_structure_vars/2 is declared as meta_predicate replc_structure_vars(2,-), but has no clauses
Warning: 14:58:23.553 only_lps/1 is declared as meta_predicate only_lps(0), but has no clauses
Warning: 14:58:23.553 oneOrMore/4 is declared as meta_predicate oneOrMore(3,?,?,?), but has no clauses
Warning: 14:58:23.553 typed_list_exact/4 is declared as meta_predicate typed_list_exact(3,?,?,?), but has no clauses
Warning: 14:58:23.554 thmust/1 is declared as meta_predicate thmust(0), but has no clauses
Warning: 14:58:23.554 dcgStructSetOptTraced/5 is declared as meta_predicate dcgStructSetOptTraced(?,?,3,?,?), but has no clauses
Warning: 14:58:23.554 freeze_safe/2 is declared as meta_predicate freeze_safe(?,0), but has no clauses
Warning: 14:58:23.554 with_abs_paths/2 is declared as meta_predicate with_abs_paths(1,?), but has no clauses
Warning: 14:58:23.554 apply_cond/2 is declared as meta_predicate apply_cond(?,0), but has no clauses
Warning: 14:58:23.575 map_each_subterm_compound/3 is declared as meta_predicate map_each_subterm_compound(2,?,?), but has no clauses
Warning: 14:58:23.575 enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.576 lazy_1/1 is declared as meta_predicate lazy_1(0), but has no clauses
Warning: 14:58:23.576 map_pddl_list/2 is declared as meta_predicate map_pddl_list(1,?), but has no clauses
Warning: 14:58:23.576 want_more_question/1 is declared as meta_predicate want_more_question(0), but has no clauses
Warning: 14:58:23.576 munl_call_hide/1 is declared as meta_predicate munl_call_hide(0), but has no clauses
Warning: 14:58:23.577 function_typed_list/4 is declared as meta_predicate function_typed_list(3,?,?,?), but has no clauses
Warning: 14:58:23.577 typed_list0/4 is declared as meta_predicate typed_list0(3,?,?,?), but has no clauses
Warning: 14:58:23.577 matcher_to_data_args/6 is declared as meta_predicate matcher_to_data_args(3,?,?,?,?,?), but has no clauses
Warning: 14:58:23.577 if_dbg/1 is declared as meta_predicate if_dbg(0), but has no clauses
Warning: 14:58:23.578 parser_sharing:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.578 parser_sharing:try_maybe_f/3 is declared as meta_predicate try_maybe_f(?,0,?), but has no clauses
Warning: 14:58:23.578 pfc_mod:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.579 pfc_mod:with_abs_paths/2 is declared as meta_predicate with_abs_paths(1,?), but has no clauses
Warning: 14:58:23.580 call_reorder:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.580 enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.580 grammar_words:word_noninitial/4 is declared as meta_predicate word_noninitial(?,0,?,?), but has no clauses
Warning: 14:58:23.580 grammar_words:word/4 is declared as meta_predicate word(?,0,?,?), but has no clauses
Warning: 14:58:23.581 grammar_words:try/4 is declared as meta_predicate try(0,?,?,?), but has no clauses
Warning: 14:58:23.581 grammar_words:words/4 is declared as meta_predicate words(?,0,?,?), but has no clauses
Warning: 14:58:23.581 grammar_words:words_initial/4 is declared as meta_predicate words_initial(?,0,?,?), but has no clauses
Warning: 14:58:23.581 grammar_words:word_initial/4 is declared as meta_predicate word_initial(?,0,?,?), but has no clauses
Warning: 14:58:23.581 grammar_words:words_noninitial/4 is declared as meta_predicate words_noninitial(?,0,?,?), but has no clauses
Warning: 14:58:23.581 gr0v:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.582 logicmoo_nlu:try_maybe_f/3 is declared as meta_predicate try_maybe_f(?,0,?), but has no clauses
Warning: 14:58:23.583 '0pts':enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.584 logicmoo_util_terms:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.584 hash7s:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.584 verbnet_iface:error_catch/3 is declared as meta_predicate error_catch(0,?,0), but has no clauses
Warning: 14:58:23.584 verbnet_iface:immediateCall/2 is declared as meta_predicate immediateCall(?,0), but has no clauses
Warning: 14:58:23.584 verbnet_iface:is_reloading/1 is declared as meta_predicate is_reloading(0), but has no clauses
Warning: 14:58:23.585 verbnet_iface:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.585 common_logic_sanity:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.585 logicmoo_planner:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.586 logicmoo_util_structs:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.586 bq:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.587 enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.587 tprove/9 is declared as meta_predicate tprove(?,0,?,?,?,?,?,?,?), but has no clauses
Warning: 14:58:23.587 explain/3 is declared as meta_predicate explain(0,?,?), but has no clauses
Warning: 14:58:23.587 prove1/8 is declared as meta_predicate prove1(0,?,?,?,?,?,?,?), but has no clauses
Warning: 14:58:23.587 example_query/1 is declared as meta_predicate example_query(0), but has no clauses
Warning: 14:58:23.588 ex/3 is declared as meta_predicate ex(0,?,?), but has no clauses
Warning: 14:58:23.588 prove/8 is declared as meta_predicate prove(0,?,?,?,?,?,?,?), but has no clauses
Warning: 14:58:23.588 w/4 is declared as meta_predicate w(0,?,0,?), but has no clauses
Warning: 14:58:23.588 m/4 is declared as meta_predicate m(?,?,0,?), but has no clauses
Warning: 14:58:23.588 w/3 is declared as meta_predicate w(0,0,?), but has no clauses
Warning: 14:58:23.588 keep_user_module/1 is declared as meta_predicate keep_user_module(0), but has no clauses
Warning: 14:58:23.589 only_runtime/1 is declared as meta_predicate only_runtime(0), but has no clauses
Warning: 14:58:23.589 m/3 is declared as meta_predicate m(?,0,?), but has no clauses
Warning: 14:58:23.589 dasm:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.589 funcall:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.590 logicmoo_ec:map_pddl_list/2 is declared as meta_predicate map_pddl_list(1,?), but has no clauses
Warning: 14:58:23.590 logicmoo_ec:map_nonvars/4 is declared as meta_predicate map_nonvars(?,2,?,?), but has no clauses
Warning: 14:58:23.590 logicmoo_ec:abdemo_call/1 is declared as meta_predicate abdemo_call(0), but has no clauses
Warning: 14:58:23.590 cplint_r:take_a_sample/5 is declared as meta_predicate take_a_sample(?,?,?,2,?), but has no clauses
Warning: 14:58:23.590 common_logic_kb_hooks:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.591 mpred_type_args:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.591 drs_to_coreace:conds_to_andlist/3 is declared as meta_predicate conds_to_andlist(2,?,?), but has no clauses
Warning: 14:58:23.591 api_json:rdf_include_labels/3 is declared as meta_predicate rdf_include_labels(3,+,+), but has no clauses
Warning: 14:58:23.591 api_json:rdf_bounded_description/5 is declared as meta_predicate rdf_bounded_description(3,+,?,+,-), but has no clauses
Warning: 14:58:23.592 logicmoo_util_filestreams:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.592 logicmoo_swilib:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.592 logicmoo_startup:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.592 logicmoo_startup:with_abs_paths/2 is declared as meta_predicate with_abs_paths(1,?), but has no clauses
Warning: 14:58:23.593 evil:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.595 socksrv:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.595 pnames:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.595 rdf_json:rdf_include_labels/3 is declared as meta_predicate rdf_include_labels(3,+,+), but has no clauses
Warning: 14:58:23.596 rdf_json:rdf_bounded_description/5 is declared as meta_predicate rdf_bounded_description(3,+,?,+,-), but has no clauses
Warning: 14:58:23.596 mpred_type_isa:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.596 genr:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.596 common_logic_compiler:map_each_subterm_compound/3 is declared as meta_predicate map_each_subterm_compound(2,?,?), but has no clauses
Warning: 14:58:23.597 common_logic_compiler:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.597 dcg_must:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.597 snark_theorist:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses
Warning: 14:58:23.598 block:enotrace/1 is declared as meta_predicate enotrace(0), but has no clauses

