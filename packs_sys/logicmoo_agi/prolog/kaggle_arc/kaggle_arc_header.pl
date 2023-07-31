
 
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

:- multifile(is_fti_step/1).
:- discontiguous(is_fti_step/1).

:- multifile(is_fti_stepr/1).
:- discontiguous(is_fti_stepr/1).

:- discontiguous(in_shape_lib/2).
:- multifile(in_shape_lib/2).
:- dynamic(in_shape_lib/2).

:- dynamic((ap/1,apv/2)).
:- dynamic(cmem/3).
:- dynamic(cmemo/3).
:- dynamic(grid_nums/1).
:- dynamic(grid_nums/2).

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
:- system:use_module(library(edinburgh)).
%:- system:use_module(library(lists)).
:- system:use_module(library(statistics)).
:- system:use_module(library(nb_set)).
:- system:use_module(library(assoc)).
:- system:use_module(library(pairs)).
:- system:use_module(library(logicmoo_common)).
:- system:use_module(library(prolog_trace)).
:- system:use_module(library(prolog_clause)).
:- system:use_module(library(prolog_source)).
 %library(trace/clause) 
%:- autoload_all.
:- system:use_module(library(gvar_globals_api)).
:- system:use_module(library(dictoo_lib)).

:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.

