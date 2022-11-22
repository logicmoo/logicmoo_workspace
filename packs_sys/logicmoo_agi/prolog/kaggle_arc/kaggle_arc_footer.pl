:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.


:- enable_arc_expansion.

%:- set_prolog_flag(verbose_load,true).  
%:- set_prolog_flag(verbose_autoload,true).

%:- learn_shapes.
:- ensure_loaded(kaggle_arc_utils).
:- ensure_loaded(kaggle_arc_ui_ansi).
:- ensure_loaded(kaggle_arc_deepening).
:- ensure_loaded(kaggle_arc_typecheck).
:- ensure_loaded(kaggle_arc_interpreter).
:- ensure_loaded(kaggle_arc_test_favs).
:- ensure_loaded(kaggle_arc_test_loader).
:- ensure_loaded(kaggle_arc_domaintypes).
:- ensure_loaded(kaggle_arc_test_iface).
:- ensure_loaded(kaggle_arc_explaination).
:- ensure_loaded(kaggle_arc_howdiff).
:- ensure_loaded(kaggle_arc_imageproc).
:- ensure_loaded(kaggle_arc_physics).
:- ensure_loaded(kaggle_arc_db).
:- ensure_loaded(kaggle_arc_heuristics).
:- ensure_loaded(kaggle_arc_intruder).
:- ensure_loaded(kaggle_arc_test_cache).
:- ensure_loaded(kaggle_arc_individuation).


:- ensure_loaded(kaggle_arc_object).
:- ensure_loaded(kaggle_arc_boards).
:- ensure_loaded(kaggle_arc_learning).
:- ensure_loaded(kaggle_arc_imagens).
:- ensure_loaded(kaggle_arc_recognise).
:- ensure_loaded(kaggle_arc_uniqueness).
:- ensure_loaded(kaggle_arc_ui_html).
:- ensure_loaded(kaggle_arc_test_easy).
:- ensure_loaded(kaggle_arc_test_old).
:- set_prolog_flag(verbose_load,false).
:- set_prolog_flag(verbose_autoload,false).

:- fixup_module_exports_now.
:- fixup_exports.
