
:- module(mkultra,[]).

:- use_module(library(logicmoo_utils)).

:- ensure_loaded(unity_prolog).

load_mkultra:- expand_file_name('*/*.prolog',List),
  reverse(List,Rev),
  maplist(load_unity_prolog_file,Rev).

process_kind_hierarchy:- wdmsg(todo(process_kind_hierarchy)).

:- discontiguous valid_property_value/2.
:- discontiguous utterance/3.
:- discontiguous test/2.
:- discontiguous test/1.
:- discontiguous switch_to_task/1.
:- discontiguous s/7.
:- discontiguous player_question/3.
:- discontiguous np/7.
:- discontiguous normalize_task/2.
:- discontiguous maintenance_goal/1.
:- discontiguous load_special_csv_row/2.
:- discontiguous know_about_object/1.
:- discontiguous inverted_sentence/6.
:- discontiguous default_strategy/2.
:- discontiguous def_der/2.
:- discontiguous conversation_idle_task/2.
:- discontiguous beat_monolog/3.
:- discontiguous adjectival_property/1.

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
:- multifile incompatible/2.
:- multifile ignore_undeclared_task/2.
:- multifile default_strategy/2.
:- multifile be/2.
:- multifile aux_have/4.
:- multifile aux_do/4.
:- multifile aux_be/4.


:- load_mkultra.

