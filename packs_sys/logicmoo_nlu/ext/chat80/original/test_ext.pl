% load.pl : Load Chat-80, for Quintus Prolog

/*
 _________________________________________________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or ensure_loadedd in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_________________________________________________________________________|

*/

%:- module(baseKB,[]).

:- use_module(library(statistics)).

%:- autoload_all.
:- use_module(library(logicmoo_common)).
:- use_module(library(logicmoo_nlu)).
:- if(\+ prolog_load_context(reloading, true)).
:- use_module(library(logicmoo_clif)).
:- endif.
%:- xlisting(lock_predicate/1).
%:- autoload_all.

:- module(parser_chat80).
:- '$set_source_module'(parser_chat80).


% text_drs_eval(Evaluation, Id, Text, DRS, LHSs, Timestamp, Author, Comment).
:- ensure_loaded(lang_model).


%:- autoload_all.
:- use_module(library(logicmoo_common)).
%:- xlisting(lock_predicate/1).
%:- autoload_all.


%:- module(baseKB).

%:- include(load).


:- fixup_exports.

