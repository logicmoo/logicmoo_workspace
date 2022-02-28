
:- module(mkultra,[]).

% use_module(library('../ext/mkultra/Assets/logicmoo_mkultra')).
:- use_module(library(logicmoo_utils)).

pi_to_p(F/A,P):- functor(P,F,A),!.
pi_to_p(F//A,P):- A2 is A + 2, functor(P,F,A2).
assume_todo(FA):- pi_to_p(FA,P), TODO=assume_todo(P), asserta((P:- !, log(warn(TODO)),throw(TODO))).
assume_done(FA):- pi_to_p(FA,P), TODO=assume_done(P), asserta((P:- !, log(warn(TODO)))).
assume_dyn_fail(FA):- pi_to_p(FA,P), TODO=assume_dyn_fail(P), asserta((P:- !, log(warn(TODO)),fail)).
assume_dyn_succeed(FA):- pi_to_p(FA,P), TODO=assume_dyn_succeed(P), asserta((P:- !, log(warn(TODO)),!)).

:- if( \+ prolog_load_context(reloading,false)).
:- prolog_load_context(directory,D),asserta(mkultra_dir(D)).
:- endif.

load_mkultra:- mkultra_dir(D),atom_concat(D,'/*/*.prolog',F), expand_file_name(F,List),
  reverse(List,Rev),
  maplist(load_unity_prolog_file,Rev).

print_load_unity_prolog_file(F):- format('~N :- ~q.~n',[load_unity_prolog_file(F)]).

print_load_mkultra:- mkultra_dir(D),atom_concat(D,'/*/*.prolog',F), expand_file_name(F,List),
  reverse(List,Rev),
  maplist(print_load_unity_prolog_file,Rev).
%:- print_load_mkultra.

process_kind_hierarchy:- log(todo(process_kind_hierarchy)).

:- discontiguous valid_property_value/2.
:- discontiguous utterance/3.
:- discontiguous test/2.
:- discontiguous test/1.
%:- discontiguous switch_to_task/1.
:- discontiguous s/7.
%:- discontiguous player_question/3.
%:- discontiguous np/7.
:- discontiguous normalize_task/2.
%:- discontiguous maintenance_goal/1.
:- discontiguous load_special_csv_row/2.
%:- discontiguous know_about_object/1.
%:- discontiguous inverted_sentence/6.
:- discontiguous default_strategy/2.
:- discontiguous def_der/2.
%:- discontiguous conversation_idle_task/2.
%:- discontiguous beat_monolog/3.
%:- discontiguous adjectival_property/1.
:- multifile valid_property_value/2.
:- multifile utterance/3.
:- multifile unique_answer/2.
:- multifile test_options/2.
:- multifile test/2.
:- multifile test/1.
:- multifile strategy/2.
:- multifile step_completed/1.
:- multifile step_completed/0.
:- multifile start_task/3.
:- multifile standard_concern/2.
:- multifile self_achieving/1.
:- multifile retract_on_restart/2.
:- multifile reduction_clause/2.
:- multifile on_enter_state/3.
:- multifile normalize_task/2.
:- multifile load_special_csv_row/2.
:- multifile initialize_prop/2.
:- multifile incompatible_cl/2.
:- multifile ignore_undeclared_task/2.
:- multifile default_strategy/2.
:- multifile be/2.
:- multifile aux_have/4.
:- multifile aux_do/4.
:- multifile aux_be/4.

:- ensure_loaded(unity_prolog).

%:- assume_dyn_succeed(switch_to_task/1).
%:- assume_dyn_succeed(player_question/3).
%:- assume_dyn_succeed(np/7).
%:- assume_dyn_succeed(member/4).
%:- assume_dyn_succeed(maintenance_goal/1).
%:- assume_dyn_succeed(know_about_object/1).
%:- assume_dyn_succeed(inverted_sentence/6).
%:- assume_dyn_succeed(conversation_idle_task/2).
%:- assume_dyn_succeed(beat_monolog/3).
%:- assume_dyn_succeed(adjectival_property/1).
%:- assume_dyn_succeed(member/4).
%:- assume_dyn_succeed(can/1).
%:- assume_dyn_succeed(>-->/2).
/*
:- assume_dyn_succeed(manner/2).
:- assume_dyn_succeed(leaf_kind/1).
:- assume_dyn_succeed(kind_noun/4).
:- assume_dyn_succeed(kind/1).
:- assume_dyn_succeed(inverse_relation/2).
:- assume_dyn_succeed(implies_relation/2).
:- assume_dyn_succeed(genitive_form_of_relation/4).
:- assume_dyn_succeed(dialog_task_advances_current_beat/1).
:- assume_dyn_succeed(contradictory_pair/2).
:- assume_dyn_succeed(character/1).
:- assume_dyn_succeed(begin_concern/1).
:- assume_dyn_succeed(begin_child_concern/3).
*/
%:- load_mkultra.
:- load_unity_prolog_file('Prolog/prolog_primitives.prolog').
:- load_unity_prolog_file('Sims/indexicals.prolog').
:- load_unity_prolog_file('Utilities/tell.prolog').
:- load_unity_prolog_file('Utilities/general.prolog').
:- load_unity_prolog_file('Utilities/unity_stuff.prolog').
:- load_unity_prolog_file('Utilities/startup.prolog').
:- load_unity_prolog_file('Ontology/type_check.prolog').
:- load_unity_prolog_file('NL/interface.prolog').


:- load_unity_prolog_file('Utilities/why.prolog').
:- load_unity_prolog_file('Utilities/truth_value.prolog').
:- load_unity_prolog_file('Utilities/test_rig.prolog').
:- load_unity_prolog_file('Utilities/graphviz.prolog').
:- load_unity_prolog_file('Utilities/freeze_tests.prolog').
:- load_unity_prolog_file('Utilities/episodic_memory.prolog').
:- load_unity_prolog_file('Utilities/data_structures.prolog').
:- load_unity_prolog_file('Sims/utilities.prolog').
:- load_unity_prolog_file('Sims/events.prolog').
:- load_unity_prolog_file('Sims/delayed_operations.prolog').
:- load_unity_prolog_file('Sims/actions.prolog').
:- load_unity_prolog_file('Sims/action_selection.prolog').
:- load_unity_prolog_file('Script/radio.prolog').
%:- load_unity_prolog_file('Scripting/quips.prolog').
:- load_unity_prolog_file('Scripting/quip_manager.prolog').
:- load_unity_prolog_file('Scripting/beat_task_crossrefs.prolog').
:- load_unity_prolog_file('Scripting/beat_manager.prolog').
:- load_unity_prolog_file('Problem solver/task_reduction.prolog').
:- load_unity_prolog_file('Problem solver/switch_to_task.prolog').
:- load_unity_prolog_file('Problem solver/ps_tests.prolog').
:- load_unity_prolog_file('Problem solver/problem_solver.prolog').
%:- load_unity_prolog_file('Problem solver/problem_solver_orig.prolog').
:- load_unity_prolog_file('Problem solver/metastrategies.prolog').
:- load_unity_prolog_file('Problem solver/invoke_continuation.prolog').
:- load_unity_prolog_file('Problem solver/integrity_checks.prolog').
:- load_unity_prolog_file('Problem solver/general_strategies.prolog').
:- load_unity_prolog_file('Problem solver/everyday_life.prolog').
:- load_unity_prolog_file('Problem solver/driver_code.prolog').
:- load_unity_prolog_file('Problem solver/debugger.prolog').
:- load_unity_prolog_file('Problem solver/crash_log.prolog').
:- load_unity_prolog_file('Ontology/tort.prolog').
:- load_unity_prolog_file('Concerns/script.prolog').
:- load_unity_prolog_file('Ontology/physical_object.prolog').
:- load_unity_prolog_file('Ontology/person.prolog').
:- load_unity_prolog_file('Ontology/knowledge_representation.prolog').
:- load_unity_prolog_file('Ontology/kinds.prolog').
:- load_unity_prolog_file('Ontology/integrity_checks.prolog').
:- load_unity_prolog_file('Ontology/hypno.prolog').
:- load_unity_prolog_file('Ontology/food_drink.prolog').
:- load_unity_prolog_file('Ontology/csv_loading.prolog').
:- load_unity_prolog_file('Ontology/container.prolog').
:- load_unity_prolog_file('Norms/ethics.prolog').
:- load_unity_prolog_file('Norms/dputils.prolog').
:- load_unity_prolog_file('Norms/dprolog.prolog').
:- load_unity_prolog_file('NL/vp_tests.prolog').
:- load_unity_prolog_file('NL/vp.prolog').
:- load_unity_prolog_file('NL/preterminals.prolog').
:- load_unity_prolog_file('NL/pp.prolog').
:- load_unity_prolog_file('NL/player_help.prolog').
:- load_unity_prolog_file('NL/pdebug.prolog').
:- load_unity_prolog_file('NL/np_tests.prolog').
:- load_unity_prolog_file('NL/np.prolog').
:- load_unity_prolog_file('NL/lf.prolog').
:- load_unity_prolog_file('NL/discourse.prolog').
:- load_unity_prolog_file('NL/contractions.prolog').
:- load_unity_prolog_file('NL/context_menu.prolog').
:- load_unity_prolog_file('NL/base_grammar_test.prolog').
:- load_unity_prolog_file('NL/base_grammar.prolog').
:- load_unity_prolog_file('NL/auxverbs.prolog').
:- load_unity_prolog_file('NL/_aux.prolog').
:- load_unity_prolog_file('Conversation/strategies.prolog').
:- load_unity_prolog_file('Conversation/questions.prolog').
:- load_unity_prolog_file('Conversation/normalization.prolog').
:- load_unity_prolog_file('Conversation/misc_responses.prolog').
:- load_unity_prolog_file('Conversation/introductions.prolog').
:- load_unity_prolog_file('Conversation/imperatives.prolog').
:- load_unity_prolog_file('Conversation/description.prolog').
:- load_unity_prolog_file('Conversation/assertions.prolog').
:- load_unity_prolog_file('Concerns/social_interaction.prolog').
:- load_unity_prolog_file('Concerns/roles.prolog').
:- load_unity_prolog_file('Concerns/player_interaction.prolog').
:- load_unity_prolog_file('Concerns/patrol.prolog').
:- load_unity_prolog_file('Concerns/need_satisfaction.prolog').
:- load_unity_prolog_file('Concerns/need_satifaction.prolog').
:- load_unity_prolog_file('Concerns/event_grammar.prolog').
:- load_unity_prolog_file('Concerns/conversation.prolog').
:- load_unity_prolog_file('Concerns/concerns.prolog').
:- load_unity_prolog_file('Concerns/command_line.prolog').
:- load_unity_prolog_file('Concerns/be_polite.prolog').
:- load_unity_prolog_file('Concerns/affect_manager.prolog').
%:- load_unity_prolog_file('NL/grammar_exclaim.prolog').

:- load_unity_prolog_file('NL/lexicon.prolog').
:- load_unity_prolog_file('Characters/Kavi.prolog').
:- load_unity_prolog_file('Characters/captive.prolog').
:- load_unity_prolog_file('Characters/pc.prolog').

:- load_unity_prolog_file('NL/grammar.prolog').
:- load_unity_prolog_file('Script/demo_level.prolog').

:- add_history(module(mkultra)).

gen_all:- between(1,6,L),length(S,L),utterance(X,S,[]),print_tree_with_final(S=X,'.\n\n'),fail.
:- add_history(gen_all).
:- fixup_exports.