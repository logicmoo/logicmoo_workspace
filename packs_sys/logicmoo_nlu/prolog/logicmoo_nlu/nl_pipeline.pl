% ===================================================================
% File 'nl_pipeline.pl'
% Purpose: English to KIF conversions from SWI-Prolog
% This implementation is incomplete
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'nl_pipeline.pl' 1.0.0
% Revision:  $Revision: 1.3 $
% Revised At:   $Date: 2002/06/06 15:43:15 $
% ===================================================================

:- module(nl_pipeline, [pipeline_file_loaded/0]).


% end_of_file.
% :- ensure_loaded(library(logicmoo_nlu/nl_pipeline)).


/*
% From /usr/lib/swi-prolog/library/apply_macros.pl:389
:- must(system:retract(((goal_expansion(GoalIn, PosIn, GoalOut, PosOut) :-
    apply_macros:expand_apply(GoalIn, PosIn, GoalOut, PosOut))))).
% From /usr/lib/swi-prolog/library/apply_macros.pl:386
:- must(system:retract(((goal_expansion(GoalIn, GoalOut) :-
    apply_macros:(\+ current_prolog_flag(xref, true),
    expand_apply(GoalIn, GoalOut)))))).
*/
:- use_module(library(apply_macros)).
:- (abolish(apply_macros:expand_apply, 4), asserta((apply_macros:expand_apply(_In, _, _, _):- !, fail))).
:- (abolish(apply_macros:expand_apply, 2), asserta((apply_macros:expand_apply(_In, _):- !, fail))).

:- use_module(library(pfc_lib)).
%:- '$set_typein_module'(baseKB).
%:- '$set_source_module'(baseKB).
:- use_module(library(logicmoo_utils)).
:- use_module(parser_sharing).
:- use_module(parser_tokenize).
:- use_module(parser_pipeline).

:- ignore(( Z = ('/'), user:current_op(X, Y, Z), maybe_display(:-(op(X, Y, Z))), nl, fail)).
:- ignore((Z = (':'), user:current_op(X, Y, Z), maybe_display(:-(op(X, Y, Z))), nl, fail)).
:- ignore((Z = ('-'), user:current_op(X, Y, Z), maybe_display(:-(op(X, Y, Z))), nl, fail)).
:- dmsg(nl_pipeline_start).

% ==============================================================================

:- volatile(t_l:disable_px/0).
:- thread_local(t_l:disable_px/0).
:- retractall(t_l:disable_px).
:- asserta(t_l:disable_px).

:- shared_parser_data(baseKB:type_action_info/3).
:- shared_parser_data(baseKB:agent_call_command/2).
:- shared_parser_data(baseKB:partOfSpeech/3).
:- shared_parser_data(baseKB:determinerStrings/2).

:- shared_parser_data(baseKB:mud_test/2).
:- multifile(baseKB:sanity_test/0).
:- shared_parser_data(baseKB:sanity_test/0).
:- multifile(baseKB:regression_test/0).
:- shared_parser_data(baseKB:regression_test/0).
:- multifile(baseKB:feature_test/0).
:- shared_parser_data(baseKB:feature_test/0).
:- shared_parser_data(baseKB:sanity_test/1).
:- shared_parser_data(baseKB:regression_test/1).
:- shared_parser_data(baseKB:feature_test/1).
:- shared_parser_data(clex_iface:clex_noun/5).

:- shared_parser_data(talkdb:talk_db/2).
:- shared_parser_data(talkdb:talk_db/3).
:- shared_parser_data(talkdb:talk_db/6).



% ================================================================================================
:- if(load_parser_interface(parser_e2c)).
% ================================================================================================

:- install_converter(parser_e2c, e2c_parse(+merged_lexical_segs, -clause_e2c)).
%:- install_converter(parser_e2c:e2c(+acetext, -lf_e2c)).
%:- install_converter(parser_e2c, e2c_clausify(+lf_e2c, -clause_e2c)).
:- install_converter(parser_e2c, e2c_reply(+clause_e2c, -reply_e2c)).

%:- debug.

:- endif.

% ================================================================================================
%:- include(parser_ape).
:- if(load_parser_interface(parser_ape)).
%:- pfc_lib:load_parser_interface('AceRules/engine/run_testcases').
% ================================================================================================
:- use_module(ape(parser/ace_to_drs)).
:- use_module(ape(get_ape_results)).
:- user:import(get_ape_results:ace_to_pkif/2).
:- system:import(get_ape_results:ace_to_pkif/2).


system:my_aceparagraph_to_drs(AceText, Sentences, SyntaxTrees, UnresolvedDrsCopy, Drs, Messages):-
  lower_first_word(AceText,AceTextL), 
   ace_to_drs:aceparagraph_to_drs(AceTextL, on, off, 1, Sentences, SyntaxTrees, UnresolvedDrsCopy, Drs, Messages, _).

%:- install_converter(parser_tokenize:into_acetext(+input, -acetext)).
:- install_converter(parser_tokenize:into_acetext(+text80, -acetext)).
:- install_converter(parser_tokenize:into_text80(+input, -text80)).
:- install_converter(parser_tokenize:into_text80(+acetext, -text80)).
%:- install_converter(parser_tokenize:tokens_to_acetext(+(tokens), -acetext)).
%:- install_converter(tokenizer:tokenize(+input, -(tokens))).
%:- install_converter(get_ape_results:ace_to_pkif(+acetext, -aceKif(p_kif))).
%:- install_converter(ace_to_drs:call_tokenizer(+acetext, +(guess, on), -sentences:set, -sentencesToParse)).
%:- install_converter(ace_to_drs:paragraphs_to_drs(+sentences:list, +(guess, on), +(catch, off), +(startID, 1), -sentences, -syntaxTrees, -drs0, -messages, -time)).
%:- install_converter(ace_to_drs:call_parser(+sentences:list, +(startID, 1), -syntaxtrees, -drs0:reversed_set)).
:- install_converter(system:my_aceparagraph_to_drs(+acetext, -sentences_set, -syntaxTrees, -unresolvedDrs, -drs0, -messages)).
%:- install_converter(ace_to_drs:acetext_to_drs(+acetext, -sentences_set, -syntaxTrees, -drs0, -messages)).
%:- install_converter(tokens_to_sentences:tokens_to_sentences(+(tokens), -sentences:set)).
%:- install_converter(tokens_to_sentences:tokens_to_paragraphs(+(tokens), -sentences:set)).

:- install_converter(system:(duplicate_term(+unresolvedDrs, -drs1))).
:- install_converter(drs_fol_pnf:drs_pnf(+drs1, -fol1)).
:- install_converter(drs_fol_pnf:drs_fol(+drs1, -pnf1)).
:- install_converter(drs_fol_pnf:drs_pnf(+drs0, -fol)).
:- install_converter(drs_fol_pnf:drs_fol(+drs0, -pnf)).

:- install_converter(get_ape_results:fol_to_pkif(+pnf, -aceKif(p_kif))).
:- install_converter(get_ape_results:fol_to_pkif(+fol, -aceKif(f_kif))).
:- install_converter(get_ape_results:fol_to_pkif(+drs0, -aceKif(d_kif))).
:- install_converter(get_ape_results:fol_to_pkif(+sdrs, -aceKif(s_kif))).

:- install_converter(drs_to_ace:drs_to_ace(+drs0, -paraphrase_set)).
:- install_converter(drs_to_ace:drslist_to_ace(+drs_set, -paraphrase_set)).
:- install_converter(drs_to_drslist:drs_to_drslist(+drs0, -drs_set)).
:- install_converter(drs_to_sdrs:drs_to_sdrs(+drs, -sdrs)).
:- endif.


% ================================================================================================
% CHAT80:  acetext, text_no_punct, pos_sents_pre, parsed80, simplify80, qplan
:-  if(load_parser_interface(parser_chat80)).
% ================================================================================================

:- export(pa_domain/2).
pa_domain(Var, List):-freeze(Var, member(Var, List)).

:- export(was_punct/1).
was_punct(Remove):-
  pa_domain(WRemove, [(, ), (.), (?), (!)]),
   (pa_domain(Remove, [w(_, punc), w(WRemove, _)]);Remove=WRemove).

remove_punctuation(W2, NP):- is_list(W2), was_punct(Remove), delete(W2, Remove, W3), W2 \=@= W3, !, remove_punctuation(W3, NP).
remove_punctuation(W2, NP):- is_list(W2), !, maplist(remove_punctuation, W2, NP).
remove_punctuation(W2, NP):- atom(W2), member(P, [(, ), (.), (?), (!)]), (atom_concat(NP, P, W2);atom_concat(P, NP, W2)), !.
remove_punctuation(W2, NP):- string(W2), member(P, [(, ), (.), (?), (!)]), (string_concat(NP, P, W2);string_concat(P, NP, W2)), !.
remove_punctuation(W2, W2).

%:- install_converter(nl_pipeline:remove_punctuation(+acetext, -acetext_no_punct)).

%:- install_converter(parser_chat80:words_to_segs(+acetext_no_punct, -pos_sents_pre)).
% :- install_converter(parser_chat80:into_text80(+(tokens), -text80)).

%:- install_converter(parser_lexical:into_lexical_segs(+text80, -e2c_lexical_segs)).
:- use_module(library(logicmoo_nlu/parser_penn_trees)).

:- install_converter(parser_penn_trees:text_to_best_tree(+text80, -e2c_syntaxTree)).
:- install_converter(parser_penn_trees:tree_to_lexical_segs(+e2c_syntaxTree, -e2c_lexical_segs)).

:- install_converter(parser_penn_trees:tree_to_lexical_segs(+ape_penn_syntaxTree, -ape_penn_segs)).
:- install_converter(parser_penn_trees:ape_to_penn_tree(+syntaxTrees, -ape_penn_syntaxTree)).
:- install_converter(parser_penn_trees:smerge_segs(ape, +e2c_lexical_segs, +ape_penn_segs, -merged_lexical_segs)).


:- parser_pipeline:asserta((
 default_pipeline_opts([lf, clause, combined_info, simplify80, results80, clause_e2c, 
   reply_e2c, ape_penn_syntaxTree, ape_penn_segs, merged_lexical_segs]))).

% TEMP DISABLE :- install_converter(parser_chat80:smerge_segs(+charniak_segs, +corenlp_segs,-e2c_segs)).
:- install_converter(parser_chat80:sent_to_parsed(+merged_lexical_segs, -parsed80)).
:- install_converter(parser_chat80:i_sentence(+parsed80, -sent80)).
:- install_converter(parser_chat80:clausify_simplify80(+sent80, -clausify80)).
%:- install_converter(parser_chat80:simplify80(+clausify80, -simplify80)).
%:- install_converter(parser_chat80:qplan(+clausify80, -qplan80)).
:- install_converter(parser_chat80:results80(+clausify80, -results80)).

:-asserta((type(SET):- call_u(tSet(SET)))).




:- endif.

% ================================================================================================
% TODO - grovel the API
:-  if(load_parser_interface(parser_charniak)).
% ================================================================================================
%:- install_converter(parser_charniak:text_to_charniak(+acetext, -charniak)).
%:- install_converter(parser_charniak:charniak_segs_to_segs(+charniak_segs,-charniak_info,-charniak_segs)).
%:- install_converter(parser_charniak:charniak_segs_to_sentences(+charniak_segs,-charniak_info,-charniak_segs)).
% TEMP DISABLE :- install_converter(parser_charniak:text_to_charniak_segs(+text80, -charniak_segs)).

:- endif.

% ================================================================================================
load_parser_stanford:-  load_parser_interface(parser_stanford).
:- if(load_parser_stanford).
% ================================================================================================

%:- install_converter(parser_stanford:text_to_corenlp(+acetext, -corenlp)).
% TEMP DISABLE :- install_converter(parser_corenlp:text_to_corenlp_segs(+text80, -corenlp_segs)).
:- endif.

% ================================================================================================
% TODO - grovel the API
:- if(load_parser_interface(parser_lexical)).
% ================================================================================================

%:- install_converter(parser_lexical:lex_winfo(+e2c_segs, -e2c_lexical_segs)).
:- endif.

% ================================================================================================
% English2CycL:
:-  if((fail, load_parser_interface(parser_e2c))). % TODO confirm CHAT80 runs without E2C
% ================================================================================================


%:- debug.

:- endif.


% ================================================================================================
:-  if(load_parser_interface(parser_candc)).
% ================================================================================================

%:- debug.
%:- break.

:- endif.


% ================================================================================================
%:-  load_parser_interface(parser_chart89).
% ================================================================================================

% ================================================================================================
:-  if((false, load_parser_interface(parser_e2c))).
% ================================================================================================

eng_to_bratko(Sentence, LF, Type, Clause, FreeVars) :-
   show_call(bratko_parse(Sentence, LF, Type)),
   show_call(bratko_clausify(LF, Clause, FreeVars)), !.


:- install_converter(nl_pipeline, eng_to_bratko(+(tokens), -lf, -type, -clause, +(freevars))).
:- install_converter(parser_bratko, bratko_parse(+(tokens), -lf, -type)).
:- install_converter(parser_bratko, bratko_clausify(+lf, -clause, -(freevars))).
:- install_converter(parser_bratko, bratko_reply(+type, +(freevars), +clause, -reply)).

%:- debug.

:- endif.





% :- get_pos_tagger(I), jpl_set(I, is_DEBUG, '@'(false)).


:- reexport(library('logicmoo/common_logic/common_logic_snark.pl')).


%% with_el_holds_enabled_4_nl( :Goal) is semidet.
%
% Using El Holds Enabled.
%
:- meta_predicate(with_el_holds_enabled_4_nl(0)).
with_el_holds_enabled_4_nl(Goal):-locally_hide(el_holds_DISABLED_KB, Goal).


:- dynamic is_cyckb_t_pred/2.
:- dynamic is_cyckb_t_pred_rename/2.

do_el_assertions :- 
   dmsg("Scanning el_assertions.pl for programatic definations (This may take 10-30 seconds)"),
%:- ain(cyckb_t(A, _, _) ==> is_cyckb_t_pred(A, 2)).
   with_el_holds_enabled_4_nl(gripe_time(10, forall(cyckb_t(A, _, _) , assert_if_new(is_cyckb_t_pred(A, 2))))),
%:- ain(cyckb_t(A, _, _, _ ) ==> is_cyckb_t_pred(A, 3)).
   with_el_holds_enabled_4_nl(gripe_time(2, forall(cyckb_t(A, _, _, _) , assert_if_new(is_cyckb_t_pred(A, 3))))),
%:- ain(cyckb_t(A, _, _, _, _ ) ==> is_cyckb_t_pred(A, 4)).
   with_el_holds_enabled_4_nl(gripe_time(2, forall(cyckb_t(A, _, _, _ , _ ) , assert_if_new(is_cyckb_t_pred(A, 4))))),
%:- ain(cyckb_t(A, _, _, _, _, _ ) ==> is_cyckb_t_pred(A, 5)).
   with_el_holds_enabled_4_nl(gripe_time(2, forall(cyckb_t(A, _, _, _ , _, _ ) , assert_if_new(is_cyckb_t_pred(A, 5))))),

  dmsg("Implementing programatic definations (This shoiuld take less than 2 seconds)"),
% :- ain((is_cyckb_t_pred(F, A) ==> {functor(H, F, A), H=..[F|ARGS], KB=..[cyckb_t, F|ARGS], assert_if_new((H:-KB))})).
  gripe_time(2, forall(is_cyckb_t_pred(F, A) , ignore((atom(F), functor(H, F, A), H=..[F|ARGS],
    KB=..[cyckb_t, F|ARGS],
       on_x_log_cont(assert_if_new((H:- \+ (t_l:el_holds_DISABLED_KB), KB))))))).

:- if(prolog_load_context(reloading,false)).
:- do_el_assertions.
:- endif.
% ================================================================================================



% ================================================================================================
% TODO Not yet started
:-  nop(load_parser_interface(parser_CURT)).
% ================================================================================================

% ================================================================================================
% TODO - grovel the API
:-  load_parser_interface(parser_regulus).
% ================================================================================================

% ================================================================================================
% TODO - grovel the API
:-  load_parser_interface(parser_SUPPLE).
% ================================================================================================

% ================================================================================================
% TODO - grovel the API
:-  load_parser_interface(parser_jpaine).
% ================================================================================================

% ================================================================================================
% TODO - grovel the API
:-  load_parser_interface(parser_SIRIDUS).
% ================================================================================================


% ================================================================================================
% TODO - grovel the API
:-  load_parser_interface(parser_ProNTo).
% ================================================================================================

:- ensure_loaded(parser_pldata).

% ================================================================================================
%:-  load_parser_interface(parser_fwd).
% ================================================================================================

% :- dmsg(nl_pipeline_complete).


baseKB:sanity_test:- run_pipeline(input='A person who loves all animals is loved by someone.', [aceKif(p_kif)=_], O), show_kvs(O).
baseKB:sanity_test:- run_pipeline(input='All persons are happy.', [aceKif(p_kif)=_], O), wdmsg(O).
baseKB:regression_test:- run_pipeline('What are the oceans that border african countries and that border asian countries ?').
baseKB:regression_test:- run_pipeline('What is the ocean that border african countries and that border asian countries?', [qplan=_], O), wdmsg(O).
baseKB:regression_test:- run_pipeline(input='what countries are there in europe ?', [qplan=_], O), show_kvs(O).
baseKB:regression_test:- must_test_80(Tokens, _, _), run_pipeline([(tokens)=Tokens], [qplan=_], O), show_kvs(O).
baseKB:regression_test_TODO:- run_pipeline(input='A person who loves all animals is loved by someone.', [aceKif(p_kif)=_], O), show_kvs(O).
animals_test:- must_det_l((ace_to_pkif('A person who loves all animals is loved by someone.', X), kif_to_boxlog(X, BOX), portray_clause(user_error, (fol:-BOX)))).
baseKB:regression_test:- animals_test.

:- import(get_ape_results:ace_to_pkif/2).
:- baseKB:import(get_ape_results:rename_vars/2).

% som3how this next directive changes  -/1 op?
% :- animals_test.
:- op(300, fx, (-)).


baseKB:regression_test:- gripe_time(5, test_chat80_sanity).


% :- must(retract(t_l:disable_px)).

:- ensure_loaded(library(apply_macros)).


% set  -/1 op
:- op(200, fy, (-)).
:- must((current_op(P, FXY, (-)), ((arg(_, v(fy, fx), FXY), P =< 300)))).

:- ignore((Z = ('`'), user:current_op(X, Y, Z), dmsg(call((writeq(:-(op(X, Y, Z))), nl, fail))))).
% :- halt(666).

baseKB:sanity_test:- call(make),call(run_pipeline("The Norwegian dude lives happily in the first house.")).
baseKB:sanity_test:- run_pipeline("what countries are there in europe ?").
baseKB:regression_test:- run_pipeline("What countries are there in north_america ?").
baseKB:feature_test:- run_pipeline("What countries are there in europe ?").
baseKB:feature_test:- run_pipeline("What countries are there in north america ?").

/*
baseKB:feature_test(must_test_80):-
  forall(must_test_80(U, R, O),
    (ignore(\+ \+ process_run_diff(report, U, R, O)),
     ignore(\+ \+ (run_pipeline([input=U], [results80=_], OL), show_kvs(OL))))).
*/

baseKB:list_tests:- dmsg(call((
   listing(feature_test),
   listing(chat80/3),
   listing(chat80/1),
   listing(chat80/2),
   listing(test_e2c/1),
   listing(test_e2c/2),
   listing(regression_test),
   listing(sanity_test),
   !))).


%:- must_test_80.
%:- test_chat80_regressions.
:- dynamic(pipeline_file_loaded/0).

pipeline_file_loading:- 
 % show_pipeline,
 % baseKB:list_tests,
  (pipeline_file_loaded->Loaded=true;Loaded=false),
  assert_if_new(pipeline_file_loaded),!,
  nop(Loaded=false-> break ; true).

:- use_module(library(editline)).
:- add_history((call(make),call(pipeline_file_loading))).
:- findnsols(6,X,(chat80(X),add_history((call(make),call(run_pipeline(X))))),_).

:- fixup_exports.

% :- pipeline_file_loading.
:- initialization(pipeline_file_loading, after_load).

%:- break.

