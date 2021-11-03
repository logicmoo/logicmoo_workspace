% load.pl : Load Chat-80, for Quintus Prolog

/*
 _________________________________________________________________________
|       Copyright (C) 1982                                                |
|                                                                         |
|       David Warren,                                                     |
|               SRI International, 333 Ravenswood Ave., Menlo Park,       |
|               California 94025, USA;                                    |
|                                                                         |
|       Fernando Pereira,                                                 |
|               Dept. of Architecture, University of Edinburgh,           |
|               20 Chambers St., Edinburgh EH1 1JZ, Scotland              |
|                                                                         |
|       This program may Be used, copied, altered or included in other    |
|       programs only for academic purposes and provided that the         |
|       authorship of the initial program is aknowledged.                 |
|       Use for commercial purposes without the previous written          |
|       agreement of the authors is forbidden.                            |
|_________________________________________________________________________|

*/


:- ensure_loaded(xgrun).	% XG runtimes

:- ensure_loaded(xgproc).
:- ensure_loaded(library(logicmoo_common)).

:- multifile( rel_spatial/3 ).
:- discontiguous( rel_spatial/3 ).
:- dynamic( rel_spatial/3 ).


:- multifile( value_units/2 ).
:- multifile( ti/2 ).
:- discontiguous( value_units/2 ).
:- discontiguous( ti/2 ).
:- dynamic( value_units/2 ).
:- dynamic( ti/2 ).



:- discontiguous( trans_spatial/5 ).
:- discontiguous( direct_spatial/5 ).
:- discontiguous( done_by_rel/5 ).
:- multifile( trans_spatial/5 ).
:- multifile( direct_spatial/5 ).
:- multifile( done_by_rel/5 ).
:- dynamic( trans_spatial/5 ).
:- dynamic( direct_spatial/5 ).
:- dynamic( done_by_rel/5 ).

multifile_dynamic_discontiguous(P):- multifile(P),dynamic(P),discontiguous(P).

:- multifile_dynamic_discontiguous(count_pred/4).
:- multifile_dynamic_discontiguous(measure_pred/4).
:- multifile_dynamic_discontiguous(position_pred/4).
:- multifile_dynamic_discontiguous(specific_pred/4).
:- multifile_dynamic_discontiguous(trans_pred/4).
:- multifile_dynamic_discontiguous(path_pred/4).
:- multifile_dynamic_discontiguous(path_pred_linkage/5).
:- multifile_dynamic_discontiguous(symmetric_pred/4).

:- multifile_dynamic_discontiguous(ti_subclass/2).
:- multifile_dynamic_discontiguous(ti/2).

:- multifile_dynamic_discontiguous(chat80/2).
:- multifile_dynamic_discontiguous(clex_verb80/4).
:- multifile_dynamic_discontiguous(unit_format/2).
:- multifile_dynamic_discontiguous(talkdb_talk_db/6).

:- multifile_dynamic_discontiguous(type_conversion/2).
:- multifile_dynamic_discontiguous(verb_form_db/5).
:- multifile_dynamic_discontiguous(verb_type_db/3).
:- multifile_dynamic_discontiguous(name_template_lf0/2).

:- multifile_dynamic_discontiguous(adjunction_LF/4).
:- multifile_dynamic_discontiguous(intrans_LF/6).
:- multifile_dynamic_discontiguous(thing_LF/6).
:- multifile_dynamic_discontiguous(trans_LF/9).


:- multifile_dynamic_discontiguous(thing_LF_access/6).


:-multifile_dynamic_discontiguous(ed/3).
:-multifile_dynamic_discontiguous(ed/2).
:-multifile_dynamic_discontiguous(chat80_test_q1/1).
:-multifile_dynamic_discontiguous(chat80_test_q2/1).
:-multifile_dynamic_discontiguous(ed1/1).
:-multifile_dynamic_discontiguous(chat_80_ed/3).
:-multifile_dynamic_discontiguous(results80/2).
:-multifile_dynamic_discontiguous(test_chat80/1).
:-multifile_dynamic_discontiguous(t_to_w2/2).
:-multifile_dynamic_discontiguous(from_wordlist_atoms/2).
:-multifile_dynamic_discontiguous(w2_to_t/2).
:-multifile_dynamic_discontiguous(process4/4).
:-multifile_dynamic_discontiguous(into_combines/2).
:-multifile_dynamic_discontiguous(into_lexical_segs/2).
:-multifile_dynamic_discontiguous(into_chat80_segs0/2).
:-multifile_dynamic_discontiguous(runtime/1).
:-multifile_dynamic_discontiguous(eng_to_logic/2).
:-multifile_dynamic_discontiguous(sent_to_prelogic/2).
:-multifile_dynamic_discontiguous(simplify80/2).
:-multifile_dynamic_discontiguous(check_words/2).
:-multifile_dynamic_discontiguous(advance80/3).
:-multifile_dynamic_discontiguous(runtime_entry/1).
:-multifile_dynamic_discontiguous(uses80/2).
:-multifile_dynamic_discontiguous(chars80/2).
:-multifile_dynamic_discontiguous(print_test/1).
:-multifile_dynamic_discontiguous(end80/1).
:-multifile_dynamic_discontiguous(doing80/2).
:-multifile_dynamic_discontiguous(out80/1).
:-multifile_dynamic_discontiguous(ask80/2).
:-multifile_dynamic_discontiguous(trace_chat80/1).
:-multifile_dynamic_discontiguous(report0/5).
:-multifile_dynamic_discontiguous(chat80_test/1).
:-multifile_dynamic_discontiguous(close_answer/2).
:-multifile_dynamic_discontiguous(report/5).
:-multifile_dynamic_discontiguous(deref/3).
:-multifile_dynamic_discontiguous(=+ / 2).
:-multifile_dynamic_discontiguous(check_word0/2).
:-multifile_dynamic_discontiguous(inv_map/4).
:-multifile_dynamic_discontiguous(check_word/2).
:-multifile_dynamic_discontiguous(inv_map_enter/4).
:-multifile_dynamic_discontiguous(drop_eq/5).
:-multifile_dynamic_discontiguous(process/2).
:-multifile_dynamic_discontiguous(quote_amp/1).
:-multifile_dynamic_discontiguous(revand/3).
:-multifile_dynamic_discontiguous(inv_map_list/5).
:-multifile_dynamic_discontiguous(exquant/5).
:-multifile_dynamic_discontiguous(simplify_not/2).
:-multifile_dynamic_discontiguous(irev/4).
:-multifile_dynamic_discontiguous(unequalise/2).
:-multifile_dynamic_discontiguous(clausify_simplify80/2).
:-multifile_dynamic_discontiguous(simplify80/3).
:-multifile_dynamic_discontiguous(reduce1/2).
:-multifile_dynamic_discontiguous(report_item/2).
:-multifile_dynamic_discontiguous(=: / 2).
:-multifile_dynamic_discontiguous(control80/1).
:-multifile_dynamic_discontiguous(show_results/3).
:-multifile_dynamic_discontiguous(answer80/2).
:-multifile_dynamic_discontiguous(~= / 2).
:-multifile_dynamic_discontiguous(rtest_chats/1).
:-multifile_dynamic_discontiguous(report_item0/2).
:-multifile_dynamic_discontiguous(show_format/1).
:-multifile_dynamic_discontiguous(rtest_chat/1).
:-multifile_dynamic_discontiguous(test_chat80/2).
:-multifile_dynamic_discontiguous(baseKB_test_chat80_mpred/4).
:-multifile_dynamic_discontiguous(demo/2).
:-multifile_dynamic_discontiguous(test_chat80/4).
:-multifile_dynamic_discontiguous(quote80/1).
:-multifile_dynamic_discontiguous(hi80/1).
:-multifile_dynamic_discontiguous(process5/5).
:-multifile_dynamic_discontiguous(demo/1).
:-multifile_dynamic_discontiguous(check_answer/4).
:-multifile_dynamic_discontiguous(inform1/1).
:-multifile_dynamic_discontiguous(inform/1).
:-multifile_dynamic_discontiguous(aggregate80/3).
:-multifile_dynamic_discontiguous(u_total/2).
:-multifile_dynamic_discontiguous(u_maxs/2).
:-multifile_dynamic_discontiguous(u_mins/2).
:-multifile_dynamic_discontiguous(i_maxs/2).
:-multifile_dynamic_discontiguous(dimensioned/1).
:-multifile_dynamic_discontiguous(i_mins/2).
:-multifile_dynamic_discontiguous(i_total/2).
:-multifile_dynamic_discontiguous(u_aggr/3).
:-multifile_dynamic_discontiguous(i_aggr/3).
:-multifile_dynamic_discontiguous(card/2).
:-multifile_dynamic_discontiguous(one_of/2).
:-multifile_dynamic_discontiguous(ratio/3).
:-multifile_dynamic_discontiguous(i_maxs0/5).
:-multifile_dynamic_discontiguous(i_mins0/5).
:-multifile_dynamic_discontiguous(u_maxs0/5).
:-multifile_dynamic_discontiguous(u_mins0/5).
:-multifile_dynamic_discontiguous(u_sum/3).
:-multifile_dynamic_discontiguous(u_lt/2).
:-multifile_dynamic_discontiguous(as_is_old/1).
:-multifile_dynamic_discontiguous(print_tree80/1).
:-multifile_dynamic_discontiguous(pt_old/2).
:-multifile_dynamic_discontiguous(pl_old/2).
:-multifile_dynamic_discontiguous(digit/1).
:-multifile_dynamic_discontiguous(digits/3).
:-multifile_dynamic_discontiguous(read_in/1).
:-multifile_dynamic_discontiguous(alphanum/2).
:-multifile_dynamic_discontiguous(lc/2).
:-multifile_dynamic_discontiguous(word1/3).
:-multifile_dynamic_discontiguous(readrest/2).
:-multifile_dynamic_discontiguous(initread/1).
:-multifile_dynamic_discontiguous(alphanums/3).
:-multifile_dynamic_discontiguous(blanks/2).
:-multifile_dynamic_discontiguous(words/3).
:-multifile_dynamic_discontiguous(nd_costs/5).
:-multifile_dynamic_discontiguous(nd_costs/4).
:-multifile_dynamic_discontiguous(nd_costs/3).
:-multifile_dynamic_discontiguous(nd_costs/6).
:-multifile_dynamic_discontiguous('$mode'/2).
:-multifile_dynamic_discontiguous(answer802/2).
:-multifile_dynamic_discontiguous(reply/1).
:-multifile_dynamic_discontiguous(header80/1).
:-multifile_dynamic_discontiguous(seto/3).
:-multifile_dynamic_discontiguous(complex/1).
:-multifile_dynamic_discontiguous(write_tree/1).
:-multifile_dynamic_discontiguous(holds_truthvalue/2).
:-multifile_dynamic_discontiguous(othervars/3).
:-multifile_dynamic_discontiguous(wt/2).
:-multifile_dynamic_discontiguous(decomp/3).
:-multifile_dynamic_discontiguous(replies/1).
:-multifile_dynamic_discontiguous(respond/1).
:-multifile_dynamic_discontiguous(satisfy80/4).
:-multifile_dynamic_discontiguous(answer80/1).
:-multifile_dynamic_discontiguous(exceptionto/1).
:-multifile_dynamic_discontiguous(numberof/3).
:-multifile_dynamic_discontiguous(exception80/1).
:-multifile_dynamic_discontiguous(pickargs/3).
:-multifile_dynamic_discontiguous(pick/2).
:-multifile_dynamic_discontiguous(quant_op/4).
:-multifile_dynamic_discontiguous(strong0/1).
:-multifile_dynamic_discontiguous(weak_det/1).
:-multifile_dynamic_discontiguous(weak/1).
:-multifile_dynamic_discontiguous(some_word/1).
:-multifile_dynamic_discontiguous(all_word/1).
:-multifile_dynamic_discontiguous(chain_apply/3).
:-multifile_dynamic_discontiguous(close_tree/2).
:-multifile_dynamic_discontiguous(clausify80/2).
:-multifile_dynamic_discontiguous(pre_apply/8).
:-multifile_dynamic_discontiguous(set_var/4).
:-multifile_dynamic_discontiguous(split_quants/6).
:-multifile_dynamic_discontiguous(clausify80_qa/4).
:-multifile_dynamic_discontiguous(strong/1).
:-multifile_dynamic_discontiguous(head_vars/5).
:-multifile_dynamic_discontiguous(quantify/4).
:-multifile_dynamic_discontiguous(value80/3).
:-multifile_dynamic_discontiguous(governs_lex/2).
:-multifile_dynamic_discontiguous(complete_aggr/7).
:-multifile_dynamic_discontiguous(conj_apply/4).
:-multifile_dynamic_discontiguous(setifiable/1).
:-multifile_dynamic_discontiguous(bubble/3).
:-multifile_dynamic_discontiguous(lower/3).
:-multifile_dynamic_discontiguous(index_vars/3).
:-multifile_dynamic_discontiguous(indices/4).
:-multifile_dynamic_discontiguous(setify/5).
:-multifile_dynamic_discontiguous(but_last0/4).
:-multifile_dynamic_discontiguous(but_last/3).
:-multifile_dynamic_discontiguous(index_det/2).
:-multifile_dynamic_discontiguous(det_apply/3).
:-multifile_dynamic_discontiguous(set_vars/4).
:-multifile_dynamic_discontiguous(chain_apply0/3).
:-multifile_dynamic_discontiguous(apply80/7).
:-multifile_dynamic_discontiguous(apply0/6).
:-multifile_dynamic_discontiguous(open_quant/5).
:-multifile_dynamic_discontiguous(extract_var/3).
:-multifile_dynamic_discontiguous(meta_apply/6).
:-multifile_dynamic_discontiguous(unit_det/1).
:-multifile_dynamic_discontiguous(apply_set/5).
:-multifile_dynamic_discontiguous(sort_quants/3).
:-multifile_dynamic_discontiguous(pipe/6).
:-multifile_dynamic_discontiguous(quant_op/5).
:-multifile_dynamic_discontiguous(strip_types/2).
:-multifile_dynamic_discontiguous(quantify_args/3).
:-multifile_dynamic_discontiguous(op_apply/3).
:-multifile_dynamic_discontiguous(compare_dets/6).
:-multifile_dynamic_discontiguous(i_s/4).
:-multifile_dynamic_discontiguous(i_np_mods/8).
:-multifile_dynamic_discontiguous(i_np_head0/8).
:-multifile_dynamic_discontiguous(i_np/8).
:-multifile_dynamic_discontiguous(i_verb/8).
:-multifile_dynamic_discontiguous(i_np_rest/10).
:-multifile_dynamic_discontiguous(i_verb_args/9).
:-multifile_dynamic_discontiguous(i_np_head/10).
:-multifile_dynamic_discontiguous(i_adj/8).
:-multifile_dynamic_discontiguous(i_np_mod/11).
:-multifile_dynamic_discontiguous(i_sentence/2).
:-multifile_dynamic_discontiguous(i_rel/8).
:-multifile_dynamic_discontiguous(i_adjs/8).
:-multifile_dynamic_discontiguous(i_bind/10).
:-multifile_dynamic_discontiguous(noun_template/5).
:-multifile_dynamic_discontiguous(slot_match/5).
:-multifile_dynamic_discontiguous(conversion/5).
:-multifile_dynamic_discontiguous(i_adjoin/6).
:-multifile_dynamic_discontiguous(measure_op/4).
:-multifile_dynamic_discontiguous(i_np_modify/6).
:-multifile_dynamic_discontiguous(i_noun/4).
:-multifile_dynamic_discontiguous(nominal_slot/3).
:-multifile_dynamic_discontiguous(i_verb_mods/7).
:-multifile_dynamic_discontiguous(i_voids/3).
:-multifile_dynamic_discontiguous(index_args/5).
:-multifile_dynamic_discontiguous(in_slot/6).
:-multifile_dynamic_discontiguous(deepen_case/2).
:-multifile_dynamic_discontiguous(held_arg/6).
:-multifile_dynamic_discontiguous(index_slot/3).
:-multifile_dynamic_discontiguous(is_my_index/1).
:-multifile_dynamic_discontiguous(verb_template/5).
:-multifile_dynamic_discontiguous(nominal_kind/1).
:-multifile_dynamic_discontiguous(i_pred/6).
:-multifile_dynamic_discontiguous(indexable_arg/1).
:-multifile_dynamic_discontiguous(have_pred/4).
:-multifile_dynamic_discontiguous(i_subj/7).
:-multifile_dynamic_discontiguous(i_measure/5).
:-multifile_dynamic_discontiguous(reshape_pred/6).
:-multifile_dynamic_discontiguous(active_passive_subjcase/2).
:-multifile_dynamic_discontiguous(verb_kind/6).
:-multifile_dynamic_discontiguous(i_sup_op/2).
:-multifile_dynamic_discontiguous(meta_head/1).
:-multifile_dynamic_discontiguous(op_inverse/3).
:-multifile_dynamic_discontiguous(i_neg/2).
:-multifile_dynamic_discontiguous(i_adj/9).
:-multifile_dynamic_discontiguous(verb_slot/9).
:-multifile_dynamic_discontiguous(fill_verb/9).
:-multifile_dynamic_discontiguous(thing_LF_access/6).
:-multifile_dynamic_discontiguous(intrans_LF/6).
:-multifile_dynamic_discontiguous(adjunction_LF/4).
:-multifile_dynamic_discontiguous(name_template_lf0/2).
:-multifile_dynamic_discontiguous(verb_type_db/3).
:-multifile_dynamic_discontiguous(verb_form_db/5).
:-multifile_dynamic_discontiguous(type_conversion/2).
:-multifile_dynamic_discontiguous(talkdb_talk_db/6).
:-multifile_dynamic_discontiguous(unit_format/2).
:-multifile_dynamic_discontiguous(clex_verb80/4).
:-multifile_dynamic_discontiguous(ti_subclass/2).
:-multifile_dynamic_discontiguous(symmetric_pred/4).
:-multifile_dynamic_discontiguous(path_pred_linkage/5).
:-multifile_dynamic_discontiguous(path_pred/4).
:-multifile_dynamic_discontiguous(trans_pred/4).
:-multifile_dynamic_discontiguous(specific_pred/4).
:-multifile_dynamic_discontiguous(position_pred/4).
:-multifile_dynamic_discontiguous(measure_pred/4).
:-multifile_dynamic_discontiguous(count_pred/4).
:-multifile_dynamic_discontiguous(multifile_dynamic_discontiguous/1).
:-multifile_dynamic_discontiguous(done_by_rel/5).
:-multifile_dynamic_discontiguous(direct_spatial/5).
:-multifile_dynamic_discontiguous(trans_spatial/5).
:-multifile_dynamic_discontiguous(ti/2).
:-multifile_dynamic_discontiguous(value_units/2).
:-multifile_dynamic_discontiguous(rel_spatial/3).
:-multifile_dynamic_discontiguous(phraseXG/5).
:-multifile_dynamic_discontiguous(virtual/3).
:-multifile_dynamic_discontiguous(gap/1).
:-multifile_dynamic_discontiguous(terminal/5).
:-multifile_dynamic_discontiguous(xg_listing/1).
:-multifile_dynamic_discontiguous(conc_gx/3).
:-multifile_dynamic_discontiguous(unwind/3).
:-multifile_dynamic_discontiguous(islist/1).
:-multifile_dynamic_discontiguous(and/3).
:-multifile_dynamic_discontiguous(tag/6).
:-multifile_dynamic_discontiguous(expandlist/6).
:-multifile_dynamic_discontiguous(expandor/6).
:-multifile_dynamic_discontiguous(expandrhs/6).
:-multifile_dynamic_discontiguous(virtualrule/1).
:-multifile_dynamic_discontiguous(case/4).
:-multifile_dynamic_discontiguous(front/3).
:-multifile_dynamic_discontiguous(xg_flatten/3).
:-multifile_dynamic_discontiguous(xg_flatten0/3).
:-multifile_dynamic_discontiguous(expandlhs/6).
:-multifile_dynamic_discontiguous(retractrule/2).
:-multifile_dynamic_discontiguous(retractrules/1).
:-multifile_dynamic_discontiguous(usurping/2).
:-multifile_dynamic_discontiguous(xg_complete/1).
:-multifile_dynamic_discontiguous(xg_assertz/1).
:-multifile_dynamic_discontiguous(xg_process/2).
:-multifile_dynamic_discontiguous(consume_xg/2).
:-multifile_dynamic_discontiguous(tidy_consume/2).
:-multifile_dynamic_discontiguous(consume0/2).
:-multifile_dynamic_discontiguous(load_minus_xg_file/2).
:-multifile_dynamic_discontiguous(load_plus_xg_file/2).
:-multifile_dynamic_discontiguous(load_plus_xg_file/1).
:-multifile_dynamic_discontiguous(chat80_term_expansion_now/2).
:-multifile_dynamic_discontiguous(chat80_term_expansion/2).
:-multifile_dynamic_discontiguous(xg_process_te_clone/3).
:-multifile_dynamic_discontiguous(xg_process_te_clone5/5).
:-multifile_dynamic_discontiguous(is_file_ext/1).
:-multifile_dynamic_discontiguous(new_pred/4).
:-multifile_dynamic_discontiguous(new_pred/2).
:-multifile_dynamic_discontiguous(new_pred/1).
:-multifile_dynamic_discontiguous(maybe_share_mp/1).
:-multifile_dynamic_discontiguous(abolish_xg/1).
:-multifile_dynamic_discontiguous(count_pred/4).
:-multifile_dynamic_discontiguous(trans_direct/4).
:-multifile_dynamic_discontiguous(trans_pred/4).
:-multifile_dynamic_discontiguous(trans_rel_cache_create/2).
:-multifile_dynamic_discontiguous(trans_rel_rl/4).
:-multifile_dynamic_discontiguous(trans_rel_lr/4).
:-multifile_dynamic_discontiguous(trans_rel_nc/4).
:-multifile_dynamic_discontiguous(trans_rel/4).
:-multifile_dynamic_discontiguous(add_ss/4).
:-multifile_dynamic_discontiguous(symmetric_direct/4).
:-multifile_dynamic_discontiguous(symmetric_pred/4).
:-multifile_dynamic_discontiguous(maybe_less_specific/2).
:-multifile_dynamic_discontiguous(type_begins_thru_ends/5).
:-multifile_dynamic_discontiguous(less_specific/2).
:-multifile_dynamic_discontiguous(node_pairs_indirect/3).
:-multifile_dynamic_discontiguous(node_pairs_direct/3).
:-multifile_dynamic_discontiguous(last_node/2).
:-multifile_dynamic_discontiguous(first_node/2).
:-multifile_dynamic_discontiguous(path_pred_linkage/5).
:-multifile_dynamic_discontiguous(path_pred/4).
:-multifile_dynamic_discontiguous(intrans_LF/6).
:-multifile_dynamic_discontiguous(agentitive_trans/3).
:-multifile_dynamic_discontiguous(ti/2).
:-multifile_dynamic_discontiguous(ratio/4).
:-multifile_dynamic_discontiguous(exceeds0/2).
:-multifile_dynamic_discontiguous(freeze_until/2).
:-multifile_dynamic_discontiguous(exceeds/2).
:-multifile_dynamic_discontiguous(measure_pred/4).
:-multifile_dynamic_discontiguous(database80/1).
:-multifile_dynamic_discontiguous(database801/1).
:-multifile_dynamic_discontiguous(comparator_LF/5).
:-multifile_dynamic_discontiguous(adj_sign_db/2).
%:-multifile_dynamic_discontiguous(units_db/2).
:-multifile_dynamic_discontiguous(aggr_adj/4).
:-multifile_dynamic_discontiguous(restriction_LF/4).
:-multifile_dynamic_discontiguous(trans_LF/6).
:-multifile_dynamic_discontiguous(clex_verb80/4).
:-multifile_dynamic_discontiguous(verb_form_db/5).
:-multifile_dynamic_discontiguous(talkdb_talk_db/6).
:-multifile_dynamic_discontiguous(use_lexicon_80/1).
:-multifile_dynamic_discontiguous(type_conversion/2).
:-multifile_dynamic_discontiguous(btype_conversion/2).
:-multifile_dynamic_discontiguous(bfeature_path/3).
:-multifile_dynamic_discontiguous(feature_path1/3).
:-multifile_dynamic_discontiguous(thing/1).
:-multifile_dynamic_discontiguous(meta_noun_LF/8).
:-multifile_dynamic_discontiguous(aggr_noun/4).
:-multifile_dynamic_discontiguous(name_template_LF/2).
:-multifile_dynamic_discontiguous(adjunction_LF/4).
:-multifile_dynamic_discontiguous(synonymous_thing/2).
:-multifile_dynamic_discontiguous(property_LF/9).
:-multifile_dynamic_discontiguous(ordering_pred/4).
:-multifile_dynamic_discontiguous(symmetric_verb/2).
:-multifile_dynamic_discontiguous(verb_type_db/3).
:-multifile_dynamic_discontiguous(measure_LF/4).
:-multifile_dynamic_discontiguous(unit_format/2).
:-multifile_dynamic_discontiguous(attribute_LF/6).
:-multifile_dynamic_discontiguous(ttribute_LF/6).
:-multifile_dynamic_discontiguous(name_template_lf0/2).
:-multifile_dynamic_discontiguous(thing_LF_access/6).
:-multifile_dynamic_discontiguous(trans_LF/9).
:-multifile_dynamic_discontiguous(c_r_l_l_s_cap_m/8).
:-multifile_dynamic_discontiguous(not_where/1).
:-multifile_dynamic_discontiguous(measure_pred/4).
:-multifile_dynamic_discontiguous(specific_pred/4).
:-multifile_dynamic_discontiguous(add_border/2).
:-multifile_dynamic_discontiguous(country_contains_thing/2).
:-multifile_dynamic_discontiguous(region_contains_country/2).
:-multifile_dynamic_discontiguous(continent_contains_region/2).
:-multifile_dynamic_discontiguous(geo_contains/2).
:-multifile_dynamic_discontiguous(trans_direct/4).
:-multifile_dynamic_discontiguous(continent/1).
:-multifile_dynamic_discontiguous(city_country_popu/3).
:-multifile_dynamic_discontiguous(madeup_city_country_popu/3).
:-multifile_dynamic_discontiguous(count_pred/4).
:-multifile_dynamic_discontiguous(river_flows/2).
:-multifile_dynamic_discontiguous(path_nodes/4).
:-multifile_dynamic_discontiguous(type_specific_bte/5).
:-multifile_dynamic_discontiguous(circle_latitude/2).
:-multifile_dynamic_discontiguous(position_pred/4).
:-multifile_dynamic_discontiguous(ti/2).
:-multifile_dynamic_discontiguous(agentitive_trans_80/3).
:-multifile_dynamic_discontiguous(ti_subclass/2).
:-multifile_dynamic_discontiguous(unique_of_obj/8).
:-multifile_dynamic_discontiguous(type_measure_pred/4).
:-multifile_dynamic_discontiguous(like_type/3).
:-multifile_dynamic_discontiguous(place_lex/1).
:-multifile_dynamic_discontiguous(agentitive_symmetric_type/2).
:-multifile_dynamic_discontiguous(sup_adv_lex/1).
:-multifile_dynamic_discontiguous(comp_adv_lex/1).
:-multifile_dynamic_discontiguous(sup_adj_db/3).
:-multifile_dynamic_discontiguous(sup_adj_lex/2).
:-multifile_dynamic_discontiguous(comp_adj_db/3).
:-multifile_dynamic_discontiguous(comp_adj_lex/2).
:-multifile_dynamic_discontiguous(noun_plu_db/3).
:-multifile_dynamic_discontiguous(which_var/3).
:-multifile_dynamic_discontiguous(noun_plu_lex/2).
:-multifile_dynamic_discontiguous(hide_plur_root_noun/3).
:-multifile_dynamic_discontiguous(noun_sing_plu_lex/1).
:-multifile_dynamic_discontiguous(noun_form_lex/3).
:-multifile_dynamic_discontiguous(noun_form_wlex0/4).
:-multifile_dynamic_discontiguous(noun_form_wlex/4).
:-multifile_dynamic_discontiguous(loc_pred_lex/3).
:-multifile_dynamic_discontiguous(adverb_lex/1).
:-multifile_dynamic_discontiguous(adj_db/3).
:-multifile_dynamic_discontiguous(adj_lex/2).
:-multifile_dynamic_discontiguous(verb_type_db/3).
:-multifile_dynamic_discontiguous(verb_type_lex/2).
:-multifile_dynamic_discontiguous(verb_form_db/5).
:-multifile_dynamic_discontiguous(avoided_verb/1).
:-multifile_dynamic_discontiguous(verb_form_lex/4).
:-multifile_dynamic_discontiguous(verb_form_aux/4).
:-multifile_dynamic_discontiguous(verb_form_wlex/5).
:-multifile_dynamic_discontiguous(correct_root/2).
:-multifile_dynamic_discontiguous(try_lex/1).
:-multifile_dynamic_discontiguous(warn_when/2).
:-multifile_dynamic_discontiguous(show_tries_except/3).
:-multifile_dynamic_discontiguous(try_lex/3).
:-multifile_dynamic_discontiguous(available_lexicon/1).
:-multifile_dynamic_discontiguous(first_lexicon/1).
:-multifile_dynamic_discontiguous(how_many_lex/1).
:-multifile_dynamic_discontiguous(ctx_pron_lex/3).
:-multifile_dynamic_discontiguous(tr_number/2).
:-multifile_dynamic_discontiguous(terminator_lex/2).
:-multifile_dynamic_discontiguous(root_form/1).
:-multifile_dynamic_discontiguous(rel_pron_lex/2).
:-multifile_dynamic_discontiguous(quantifier_pron_lex/3).
:-multifile_dynamic_discontiguous(prep_db/2).
:-multifile_dynamic_discontiguous(prep_lex/1).
:-multifile_dynamic_discontiguous(poss_pron_lex/4).
:-multifile_dynamic_discontiguous(pers_pron_lex/5).
:-multifile_dynamic_discontiguous(number_lex/3).
:-multifile_dynamic_discontiguous(name_LF/1).
:-multifile_dynamic_discontiguous(wh_pron_lex/2).
:-multifile_dynamic_discontiguous(wh_art_lex/4).
:-multifile_dynamic_discontiguous(det_lex/4).
:-multifile_dynamic_discontiguous(conj_lex/1).
:-multifile_dynamic_discontiguous('`'/1).
:-multifile_dynamic_discontiguous(word/1).
:-multifile_dynamic_discontiguous(chk_word/1).
:-multifile_dynamic_discontiguous(ag_number/2).
:-multifile_dynamic_discontiguous(is_my_index/1).
:-multifile_dynamic_discontiguous(indexable_arg/1).
:-multifile_dynamic_discontiguous(index_args/5).
:-multifile_dynamic_discontiguous(index_slot/3).
:-multifile_dynamic_discontiguous(deepen_case/2).
:-multifile_dynamic_discontiguous(verb_kind/6).
:-multifile_dynamic_discontiguous(verb_template/5).
:-multifile_dynamic_discontiguous(noun_template/5).
:-multifile_dynamic_discontiguous(op_inverse/3).
:-multifile_dynamic_discontiguous(measure_op/4).
:-multifile_dynamic_discontiguous(conversion/5).
:-multifile_dynamic_discontiguous(i_sup_op/2).
:-multifile_dynamic_discontiguous(nominal_kind/1).
:-multifile_dynamic_discontiguous(i_verb_mods/7).
:-multifile_dynamic_discontiguous(i_measure/5).
:-multifile_dynamic_discontiguous(i_adjoin/6).
:-multifile_dynamic_discontiguous(i_pred/6).
:-multifile_dynamic_discontiguous(verb_slot/9).
:-multifile_dynamic_discontiguous(fill_verb/9).
:-multifile_dynamic_discontiguous(i_verb_args/9).
:-multifile_dynamic_discontiguous(i_subj/7).
:-multifile_dynamic_discontiguous(i_neg/2).
:-multifile_dynamic_discontiguous(meta_head/1).
:-multifile_dynamic_discontiguous(have_pred/4).
:-multifile_dynamic_discontiguous(reshape_pred/6).
:-multifile_dynamic_discontiguous(i_verb/8).
:-multifile_dynamic_discontiguous(i_s/4).
:-multifile_dynamic_discontiguous(i_adj/9).
:-multifile_dynamic_discontiguous(i_adj/8).
:-multifile_dynamic_discontiguous(i_adjs/8).
:-multifile_dynamic_discontiguous(slot_match/5).
:-multifile_dynamic_discontiguous(in_slot/6).
:-multifile_dynamic_discontiguous(i_np_modify/6).
:-multifile_dynamic_discontiguous(i_bind/10).
:-multifile_dynamic_discontiguous(i_noun/4).
:-multifile_dynamic_discontiguous(i_np_mod/11).
:-multifile_dynamic_discontiguous(i_rel/8).
:-multifile_dynamic_discontiguous(i_voids/3).
:-multifile_dynamic_discontiguous(i_np_mods/8).
:-multifile_dynamic_discontiguous(i_np_head0/8).
:-multifile_dynamic_discontiguous(held_arg/6).
:-multifile_dynamic_discontiguous(i_np_rest/10).
:-multifile_dynamic_discontiguous(i_np_head/10).
:-multifile_dynamic_discontiguous(i_np/8).
:-multifile_dynamic_discontiguous(i_sentence/2).
:-multifile_dynamic_discontiguous(ditrans_lex80/13).
:-multifile_dynamic_discontiguous(standard_adj_db/4).
:-multifile_dynamic_discontiguous(adv_template_db/4).
:-multifile_dynamic_discontiguous(conj_apply/4).
:-multifile_dynamic_discontiguous(bubble/3).
:-multifile_dynamic_discontiguous(op_apply/3).
:-multifile_dynamic_discontiguous(setifiable/1).
:-multifile_dynamic_discontiguous(lower/3).
:-multifile_dynamic_discontiguous(weak_det/1).
:-multifile_dynamic_discontiguous(weak/1).
:-multifile_dynamic_discontiguous(strong0/1).
:-multifile_dynamic_discontiguous(strong/1).
:-multifile_dynamic_discontiguous(governs_lex/2).
:-multifile_dynamic_discontiguous(apply_set/5).
:-multifile_dynamic_discontiguous(some_word/1).
:-multifile_dynamic_discontiguous(all_word/1).
:-multifile_dynamic_discontiguous(value80/3).
:-multifile_dynamic_discontiguous(quant_op/4).
:-multifile_dynamic_discontiguous(quant_op/5).
:-multifile_dynamic_discontiguous(apply0/6).
:-multifile_dynamic_discontiguous(apply80/7).
:-multifile_dynamic_discontiguous(det_apply/3).
:-multifile_dynamic_discontiguous(unit_det/1).
:-multifile_dynamic_discontiguous(index_det/2).
:-multifile_dynamic_discontiguous(open_quant/5).
:-multifile_dynamic_discontiguous(compare_dets/6).
:-multifile_dynamic_discontiguous(split_quants/6).
:-multifile_dynamic_discontiguous(sort_quants/3).
:-multifile_dynamic_discontiguous(set_var/4).
:-multifile_dynamic_discontiguous(set_vars/4).
:-multifile_dynamic_discontiguous(complete_aggr/7).
:-multifile_dynamic_discontiguous(index_vars/3).
:-multifile_dynamic_discontiguous(pipe/6).
:-multifile_dynamic_discontiguous(setify/5).
:-multifile_dynamic_discontiguous(indices/4).
:-multifile_dynamic_discontiguous(meta_apply/6).
:-multifile_dynamic_discontiguous(close_tree/2).
:-multifile_dynamic_discontiguous(but_last0/4).
:-multifile_dynamic_discontiguous(but_last/3).
:-multifile_dynamic_discontiguous(pre_apply/8).
:-multifile_dynamic_discontiguous(quantify_args/3).
:-multifile_dynamic_discontiguous(chain_apply0/3).
:-multifile_dynamic_discontiguous(chain_apply/3).
:-multifile_dynamic_discontiguous(extract_var/3).
:-multifile_dynamic_discontiguous(strip_types/2).
:-multifile_dynamic_discontiguous(head_vars/5).
:-multifile_dynamic_discontiguous(quantify/4).
:-multifile_dynamic_discontiguous(clausify80_qa/4).
:-multifile_dynamic_discontiguous(clausify80/2).
:-multifile_dynamic_discontiguous(pick/2).
:-multifile_dynamic_discontiguous(pickargs/3).
:-multifile_dynamic_discontiguous(exception80/1).
:-multifile_dynamic_discontiguous(exceptionto/1).
:-multifile_dynamic_discontiguous(satisfy80/4).
:-multifile_dynamic_discontiguous(numberof/3).
:-multifile_dynamic_discontiguous('$pldoc'/4).
:-multifile_dynamic_discontiguous('$mode'/2).
:-multifile_dynamic_discontiguous(reply/1).
:-multifile_dynamic_discontiguous(replies/1).
:-multifile_dynamic_discontiguous(holds_truthvalue/2).
:-multifile_dynamic_discontiguous(seto/3).
:-multifile_dynamic_discontiguous(answer802/2).
:-multifile_dynamic_discontiguous(answer80/1).
:-multifile_dynamic_discontiguous(respond/1).
:-multifile_dynamic_discontiguous(complex/1).
:-multifile_dynamic_discontiguous(othervars/3).
:-multifile_dynamic_discontiguous(decomp/3).
:-multifile_dynamic_discontiguous(header80/1).
:-multifile_dynamic_discontiguous(wt/2).
:-multifile_dynamic_discontiguous(write_tree/1).
:-multifile_dynamic_discontiguous(nd_costs/6).
:-multifile_dynamic_discontiguous(nd_costs/5).
:-multifile_dynamic_discontiguous(nd_costs/3).
:-multifile_dynamic_discontiguous(nd_costs/4).
:-multifile_dynamic_discontiguous(nd_costs/7).
:-multifile_dynamic_discontiguous(lc/2).
:-multifile_dynamic_discontiguous(digit/1).
:-multifile_dynamic_discontiguous(blanks/2).
:-multifile_dynamic_discontiguous(digits/3).
:-multifile_dynamic_discontiguous(alphanum/2).
:-multifile_dynamic_discontiguous(alphanums/3).
:-multifile_dynamic_discontiguous(word1/3).
:-multifile_dynamic_discontiguous(words/3).
:-multifile_dynamic_discontiguous(readrest/2).
:-multifile_dynamic_discontiguous(initread/1).
:-multifile_dynamic_discontiguous(read_in/1).
:-multifile_dynamic_discontiguous(as_is_old/1).
:-multifile_dynamic_discontiguous(pl_old/2).
:-multifile_dynamic_discontiguous(pt_old/2).
:-multifile_dynamic_discontiguous(print_tree80/1).
:-multifile_dynamic_discontiguous(card/2).
:-multifile_dynamic_discontiguous(ratio/3).
:-multifile_dynamic_discontiguous(one_of/2).
:-multifile_dynamic_discontiguous(dimensioned/1).
:-multifile_dynamic_discontiguous(u_lt/2).
:-multifile_dynamic_discontiguous(u_mins0/5).
:-multifile_dynamic_discontiguous(u_mins/2).
:-multifile_dynamic_discontiguous(u_maxs0/5).
:-multifile_dynamic_discontiguous(u_maxs/2).
:-multifile_dynamic_discontiguous(u_sum/3).
:-multifile_dynamic_discontiguous(u_total/2).
:-multifile_dynamic_discontiguous(i_mins0/5).
:-multifile_dynamic_discontiguous(i_mins/2).
:-multifile_dynamic_discontiguous(i_maxs0/5).
:-multifile_dynamic_discontiguous(i_maxs/2).
:-multifile_dynamic_discontiguous(i_total/2).
:-multifile_dynamic_discontiguous(u_aggr/3).
:-multifile_dynamic_discontiguous(i_aggr/3).
:-multifile_dynamic_discontiguous(aggregate80/3).
:-multifile_dynamic_discontiguous(=: / 2).
:-multifile_dynamic_discontiguous(=+ / 2).
:-multifile_dynamic_discontiguous(~= / 2).
:-multifile_dynamic_discontiguous(check_word0/2).
:-multifile_dynamic_discontiguous(check_word/2).
:-multifile_dynamic_discontiguous(check_words/2).
:-multifile_dynamic_discontiguous(irev/4).
:-multifile_dynamic_discontiguous(exquant/5).
:-multifile_dynamic_discontiguous(deref/3).
:-multifile_dynamic_discontiguous(drop_eq/5).
:-multifile_dynamic_discontiguous(inv_map_list/5).
:-multifile_dynamic_discontiguous(inv_map/4).
:-multifile_dynamic_discontiguous(inv_map_enter/4).
:-multifile_dynamic_discontiguous(unequalise/2).
:-multifile_dynamic_discontiguous(revand/3).
:-multifile_dynamic_discontiguous(simplify_not/2).
:-multifile_dynamic_discontiguous(simplify80/3).
:-multifile_dynamic_discontiguous(simplify80/2).
:-multifile_dynamic_discontiguous(clausify_simplify80/2).
:-multifile_dynamic_discontiguous(reduce1/2).
:-multifile_dynamic_discontiguous(sent_to_prelogic/2).
:-multifile_dynamic_discontiguous(quote_amp/1).
:-multifile_dynamic_discontiguous(quote80/1).
:-multifile_dynamic_discontiguous(runtime/1).
:-multifile_dynamic_discontiguous(report_item0/2).
:-multifile_dynamic_discontiguous(report_item/2).
:-multifile_dynamic_discontiguous(report0/5).
:-multifile_dynamic_discontiguous(report/5).
:-multifile_dynamic_discontiguous(results80/2).
:-multifile_dynamic_discontiguous(process4/4).
:-multifile_dynamic_discontiguous(from_wordlist_atoms/2).
:-multifile_dynamic_discontiguous(t_to_w2/2).
:-multifile_dynamic_discontiguous(w2_to_t/2).
:-multifile_dynamic_discontiguous(into_combines/2).
:-multifile_dynamic_discontiguous(into_chat80_segs0/2).
:-multifile_dynamic_discontiguous(into_lexical_segs/2).
:-multifile_dynamic_discontiguous(eng_to_logic/2).
:-multifile_dynamic_discontiguous(process/2).
:-multifile_dynamic_discontiguous(process5/5).
:-multifile_dynamic_discontiguous(trace_chat80/1).
:-multifile_dynamic_discontiguous(control80/1).
:-multifile_dynamic_discontiguous(chat80_test/1).
:-multifile_dynamic_discontiguous(end80/1).
:-multifile_dynamic_discontiguous(chars80/2).
:-multifile_dynamic_discontiguous(uses80/2).
:-multifile_dynamic_discontiguous(advance80/3).
:-multifile_dynamic_discontiguous(out80/1).
:-multifile_dynamic_discontiguous(doing80/2).
:-multifile_dynamic_discontiguous(print_test/1).
:-multifile_dynamic_discontiguous(ask80/2).
:-multifile_dynamic_discontiguous(hi80/1).
:-multifile_dynamic_discontiguous(runtime_entry/1).
:-multifile_dynamic_discontiguous(close_answer/2).
:-multifile_dynamic_discontiguous(check_answer/4).
:-multifile_dynamic_discontiguous(answer80/2).
:-multifile_dynamic_discontiguous(show_format/1).
:-multifile_dynamic_discontiguous(show_results/3).
:-multifile_dynamic_discontiguous(rtest_chat/1).
:-multifile_dynamic_discontiguous(rtest_chats/1).
:-multifile_dynamic_discontiguous(baseKB_test_chat80_mpred/4).
:-multifile_dynamic_discontiguous(test_chat80/4).
:-multifile_dynamic_discontiguous(test_chat80/2).
:-multifile_dynamic_discontiguous(test_chat80/1).
:-multifile_dynamic_discontiguous(inform1/1).
:-multifile_dynamic_discontiguous(inform/1).
:-multifile_dynamic_discontiguous(demo/2).
:-multifile_dynamic_discontiguous(demo/1).
:-multifile_dynamic_discontiguous(chat80/2).
:-multifile_dynamic_discontiguous(ed1/1).
:-multifile_dynamic_discontiguous(ed/3).
:-multifile_dynamic_discontiguous(ed/2).
:-multifile_dynamic_discontiguous(chat80_test_q2/1).
:-multifile_dynamic_discontiguous(chat80_test_q1/1).
:-multifile_dynamic_discontiguous(chat_80_ed/3).


% :- expects_dialect(pfc).
:-op(600,xfy,--).

:- ensure_loaded(templa).	% semantic dictionary


%:- ensure_loaded(newg).		% clone + lex
:-  load_plus_xg_file('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/ext/chat80/original/clone.xg').
:-  load_plus_xg_file('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/ext/chat80/original/lex.xg').


:- ensure_loaded(geography/load_kb).
%:- ensure_loaded(mud_status/load_kb).


%:- ensure_loaded(clotab).	% attachment tables
:- ensure_loaded(newdict).	% syntactic dictionary
:- ensure_loaded(slots).	% fits arguments into predicates
:- ensure_loaded(scopes).	% quantification and scoping


:- use_module(qplan).	% query planning
:- ensure_loaded(talkr).	% query evaluation
:- ensure_loaded(ndtabl).	% relation info.
:- ensure_loaded(readin).	% sentence input
:- ensure_loaded(ptree).	% print trees
:- ensure_loaded(aggreg).	% aggregation operators

:- ensure_loaded(newtop).	% top level

:- fixup_exports.

list_chat80_preds:- 
 forall((source_file(P,Y),atom(Y),atom_contains(Y,'chat80'),atom_contains(Y,'original'),
   compound(P)),(compound_name_arity(P,F,A),format('~q.~n',[:-multifile_dynamic_discontiguous(F/A)]))).

