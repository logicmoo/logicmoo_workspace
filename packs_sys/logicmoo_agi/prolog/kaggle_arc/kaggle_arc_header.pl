
 
:- discontiguous(is_changeable_param/1).
:- multifile(is_changeable_param/1).
:- dynamic(is_changeable_param/1).

:- discontiguous(fav/1).
:- multifile(fav/1).
:- dynamic(fav/1).

:- discontiguous(fav/2).
:- multifile(fav/2).
:- dynamic(fav/2).

:- discontiguous(ping_indiv_grid/1).
:- multifile(ping_indiv_grid/1).

:- discontiguous(make_shape/2).
:- dynamic(make_shape/2).

:- discontiguous(decl_sf/1).
:- discontiguous(decl_gf/1).


:- multifile(is_fti_step/1).
:- discontiguous(is_fti_step/1).

:- multifile(is_fti_stepr/1).
:- discontiguous(is_fti_stepr/1).

:- discontiguous(in_shape_lib/2).
:- multifile(in_shape_lib/2).
:- dynamic(in_shape_lib/2).

:- dynamic((ap/1,apv/2)).
:- dynamic(cmem/3).
:- dynamic(omem/3).
:- dynamic(cmemo/3).
:- dynamic(grid_nums/1).
:- dynamic(grid_nums/2).

:- multifile(prolog:make_hook/2).
:- dynamic(prolog:make_hook/2).
:- discontiguous(prolog:make_hook/2).

:- discontiguous(muarc:clear_all_caches/0).

:- multifile(fav/2).
:- discontiguous(fav/2).
:- dynamic(fav/2).
:- export(fav/2).


:- multifile(easy_sol/1).
:- discontiguous(easy_sol/1).
:- dynamic(easy_sol/1).
:- export(easy_sol/1).

:- multifile(user:portray/1).
:- dynamic(user:portray/1).
:- discontiguous(user:portray/1).

:- dynamic(kaggle_arc/4).
:- discontiguous(kaggle_arc/4).
:- multifile(kaggle_arc/4).
:- export(kaggle_arc/4).

:- dynamic(quick_test_menu/1).
:- discontiguous(quick_test_menu/1).
:- multifile(quick_test_menu/1).
:- export(quick_test_menu/1).

:- dynamic(arc_test_property/4).
:- discontiguous(arc_test_property/4).
:- multifile(arc_test_property/4).

:- dynamic(individuation_macros/2).
:- discontiguous(individuation_macros/2).
:- multifile(individuation_macros/2).
:- dynamic(individuator/2).
:- discontiguous(individuator/2).
:- multifile(individuator/2).

:- system:use_module(library(quasi_quotations)).
:- system:use_module(library(hashtable)).
:- system:use_module(library(gensym)).
:- system:use_module(library(sort)).
:- system:use_module(library(writef)).
:- system:use_module(library(rbtrees)).
:- system:use_module(library(dicts)).
:- system:use_module(library(shell)).
:- system:use_module(library(edinburgh)).
%:- system:use_module(library(lists)).
:- system:use_module(library(statistics)).
:- system:use_module(library(nb_set)).
:- system:use_module(library(assoc)).
:- system:use_module(library(pairs)).

/*
:- use_module(library(prolog_colour)).
:- use_module(library(prolog_xref)).
:- consult('/usr/lib/swi-prolog/library/prolog_xref'). % into prolog_xref 0.00 sec, 0 clauses
:- consult('/usr/lib/swi-prolog/library/pldoc/doc_html'). % into pldoc_html 0.00 sec, 0 clauses
:- consult('/usr/lib/swi-prolog/library/prolog_colour'). % into prolog_colour 0.00 sec, 0 clauses
:- consult('/usr/lib/swi-prolog/xpce/prolog/lib/pce'). % into pce 0.11 sec, 0 clauses
:- consult('/usr/lib/swi-prolog/xpce/prolog/boot/pce_expand'). %  into pce_expand 0.00 sec, 2 clauses
%  ../lib/swi_compatibility'). %  into pce_compatibility_layer 0.00 sec, 15 clauses
:- consult('/usr/lib/swi-prolog/xpce/prolog/boot/pce_pl'). %  into pce_host 0.00 sec, 2 clauses
:- consult('/usr/lib/swi-prolog/xpce/prolog/lib/swi_compatibility'). %  into pce_compatibility_layer 0.00 sec, 0 clauses
:- consult('/usr/lib/swi-prolog/xpce/prolog/boot/pce_principal'). %  into pce_principal 0.03 sec, 20 clauses
:- consult('/usr/lib/swi-prolog/xpce/prolog/boot/pce_error'). %  into pce_error 0.00 sec, 4 clauses
:- consult('/usr/lib/swi-prolog/xpce/prolog/boot/pce_global'). %  into pce_global 0.02 sec, 4 clauses
:- consult('/usr/lib/swi-prolog/xpce/prolog/boot/pce_expansion'). %  into pce_expansion 0.02 sec, 30 clauses
:- consult('/usr/lib/swi-prolog/xpce/prolog/boot/pce_realise'). %  into pce_realise 0.01 sec, 22 clauses
:- consult('/usr/lib/swi-prolog/xpce/prolog/boot/pce_goal_expansion'). %  into pce_goal_expansion 0.00 sec, 6 clauses
:- consult('/usr/lib/swi-prolog/xpce/prolog/boot/pce_autoload'). %  into pce_autoload 0.02 sec, 6 clauses
:- consult('/usr/lib/swi-prolog/xpce/prolog/boot/pce_editor'). %  into editor_buttons 0.02 sec, 2 clauses
:- consult('/usr/lib/swi-prolog/xpce/prolog/boot/pce_keybinding'). %  into pce_keybinding 0.02 sec, 10 clauses
:- consult('/usr/lib/swi-prolog/xpce/prolog/boot/pce_portray'). %  into pce_portray 0.00 sec, 6 clauses
:- consult('/usr/lib/swi-prolog/xpce/prolog/lib/english/pce_messages'). %  into pce_messages 0.00 sec, 2 clauses
:- consult('/usr/lib/swi-prolog/library/prolog_xref'). % into prolog_xref 0.00 sec, 0 clauses
:- consult('/usr/lib/swi-prolog/library/pldoc/doc_html'). % into pldoc_html 0.00 sec, 0 clauses
:- consult('/usr/lib/swi-prolog/library/prolog_colour'). % into prolog_colour 0.00 sec, 0 clauses
:- consult('/usr/lib/swi-prolog/xpce/prolog/lib/pce'). % into pce 0.13 sec, 0 clauses
:- autoload_all.
%:- use_module(library(use_pce_compatibility_layer)).
:- set_prolog_flag(xpce,false).
:- set_prolog_flag(gui,false).
*/

:- system:use_module(library(logicmoo_common)).
:- system:use_module(library(prolog_trace)).
%:- redefine_system_predicate(prolog_trace:trace/2),abolish(prolog_trace:trace/2),asserta(prolog_trace:trace(_,_)).
:- system:use_module(library(prolog_clause)).
:- system:use_module(library(prolog_source)).
 %library(trace/clause) 
%:- autoload_all.
:- system:use_module(library(gvar_globals_api)).
:- system:use_module(library(dictoo_lib)).

:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.


