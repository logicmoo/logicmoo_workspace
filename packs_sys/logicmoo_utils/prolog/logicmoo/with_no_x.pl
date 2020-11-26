/* Part of LogicMOO Base Logicmoo Debug Tools
% ===================================================================
% File 'with_no_x.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'with_no_x.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_with_assertions.pl
:- module(with_no_x,[ with_no_x/1]).
:- meta_predicate with_no_x(0).

:- thread_local(tlbugger:show_must_go_on/1).
% WAS OFF  :- system:use_module(library(gui_tracer)).

%% with_no_x( :Goal) is nondet.
%
% Using No X.
%
with_no_x(G):- getenv('DISPLAY',DISP),!,call_cleanup((unsetenv('DISPLAY'),with_no_x(G)),setenv('DISPLAY',DISP)).
with_no_x(G):- current_prolog_flag(gui,true),!,call_cleanup((set_prolog_flag(gui,false),with_no_x(G)),set_prolog_flag(gui,true)).
with_no_x(G):- current_prolog_flag(gui_tracer,true),!,call_cleanup((set_prolog_flag(gui,false),with_no_x(G)),set_prolog_flag(gui,true)).
with_no_x(G):- locally_each(tlbugger:show_must_go_on,call(G)).



