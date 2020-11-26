:- module(cgt,[load_cge/0,load_cgt/0]).
:- use_module(library(cgt/cge/swi_apeal)).

cgt_data(F/A):- multifile(F/A), dynamic(F/A), discontiguous(F/A).

:- cgt_data('<<'/2).
:- cgt_data(c/3).
:- cgt_data(concept_type/5).
:- cgt_data(description/3).
:- cgt_data(g/3).
:- cgt_data(l/3).
:- cgt_data(p/4).
:- cgt_data(relation_type/5).


/* COPYRIGHT ************************************************************

Conceptual Graph Tools (CGT) - a partial implementation of Sowa's CS Theory
Copyright (C) 1990 Miguel Alexandre Wermelinger

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

************************************************************************/

/* AUTHOR(S) ************************************************************

Michel Wermelinger
Dept. de Informatica, Univ. Nova de Lisboa, Quinta da Torre
P - 2825 Monte da Caparica, PORTUGAL
Phone: (+351) (1) 295 44 64 ext. 1360  Internet: mw@fct.unl.pt

************************************************************************/

/* GENERALITIES *********************************************************
 
File Name	: QXP.PL
Creation Date	: 90/11/19 	By: mw
Abbreviations	: mw - Michel Wermelinger 
Description	: Main file; for Quintus Prolog
 
************************************************************************/

/* HISTORY **************************************************************

1.01	92/04/24  mw	created load_cgt/0 and load_cge/0 so that you only
				load what you want
			added comments
1.02	92/05/05  mw	the 'lexicon' file is now loaded by portugues/0

************************************************************************/

/* CONTENTS *************************************************************

not/1			the not operator (Quintus doesn't have it)
\=/2			the different operator (Quintus doesn't have it)
succ/2			the successor relation for integers

load_cgt/0		loads the Conceptual Graph Tools
load_cge/0		loads the Conceptual Graph Editor
load_get/0		loads the Graph Editor and Tools
portugues/0		loads the Portuguese semantic interpreter

get_back/0		returns to the Quintus X Prolog top level shell
snapshot/1		creates a widget to make screen dumps

************************************************************************/

/* IMPORTANT NOTES ******************************************************

If you don't have X-Prolog please comment out all predicates where the
widget/2 infix operator occurs.

If you're not using CGE please edit files 'misc.pl', 'sem_int.pl' and
'gramaux.pl'.

************************************************************************/


%%% Some miscellaneous stuff

:- use_module(library(logicmoo_common)).

/*
:- op(900, fy, not).

not(X) :- X, !, fail.
not(X).

:- op(700, xfx, \=).

X \= Y :- not X = Y.

*/
succ(X, Y) :-
    nonvar(X), !, Y is X + 1.
succ(X, Y) :-
    X is Y - 1.

%:- unknown(X, fail).		% calls to undefined predicates simply fail

%:- leash([call,redo]).		% leash call and redo ports only
%:- style_check(single_var).	% check for single occurrences of variables

%%% Load Portuguese semantic interpreter
%%%
%%% load_cgt/0 (or load_get/0) must be called before portugues/0
%%%
%%% there will be some synax errors when compiling the 'syntax' file;
%%% please ignore them. I won't change the 'syntax' file as I'm not its author.

portugues :- 
    no_style_check(single_var), % don't check for single occurrences of vars
    compile(syntax),		% compile the file with the Portuguese syntax
    [lexicon, sem_int], 	% load lexicon and the semantic interpreter
    style_check(single_var).	% enable checking again

%%% Widget to make screen dumps

/*snapshot(X) :- shell widget snapshot(ID), ID wproc window(X).

shell widget snapshot(S) :-
	S= transientShell / [
		backgroundPixmap(0), width(100), height(100)].
*/

%%% Load the Conceptual Graph Tools

load_dir_file(Dir,[F]):-!, load_dir_file(Dir,F).
load_dir_file(Dir,F):- F\==[],!, reconsult(library(Dir/F)).
load_dir_file(_,_).


load_cgt :-
   maplist(load_dir_file(cgt),[
    [can_ops], 		% canonical formation rules
    [type_ops],		% operations on the type hierarchy
    [log_ops],		% propositional inference rules
    [misc], 		% DB management, referent expansions & contractions
    [gen_lin],		% generates the linear notation
    [rec_lin],		% reads the linear notation
    [gramaux],		% auxillary grammar rules (tokeniser)
    [list],		% list and set operations
    []]),
    start_cgp(canon).	% load the backup database

%%% Load the Conceptual Graph Editor

load_cge :-
   % load_set(xgraph),		% load the graph widget
   % language(L, [[unlp,wdl]|L]),% CGE is written in UNL Prolog and WDL
   maplist(load_dir_file('cgt/cge'), [

    [wdl_ext],			% extensions to the Widget Description Language
    [cge_actions], 		% actions performed by the editor
    [cge_widgets],		% CGE's Window gadgets (editor's visual look)
    [dialog],			% widgets for several kinds of dialog boxes
    ['choice'],		% widgets for choice dialogs
    []]),
    %xt_display(D, D), 
    %xt_fetch_server_fonts(D),	% to display greek letters
    shell widget qxp_shell(G), 	% open a new top-level shell
    G = prolog,
    recorda(qxp_goal, G, _)	% remember the shell to make get_back possible
  %  !, G.			% start at the new shell
   .

%%% get_back acts as an abort for CGE: it returns control to the top-level shell

get_back :- recorded(qxp_goal, G, _), !, call(G).
get_back :- shell widget qxp_shell(G), recorda(qxp_goal, G, _), !, G.

%%% Load the whole GET system

load_get :- load_cgt, load_cge.


:- dynamic(defined/3).


:- load_cgt.
% :- load_cge.


