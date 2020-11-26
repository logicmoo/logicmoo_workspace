/* <module> logicmoo_plarkc - special module hooks into the logicmoo engine allow
%   clif syntax to be recocogized via our CycL/KIF handlers 
% 
% Logicmoo Project: A LarKC Server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
:- module(logicmoo_pdt,[load_pdt/0,ensure_guitracer/0]).

% :- absolute_file_name(swi(xpce/prolog/lib),X), assert_if_new(user:library_directory(X)).
% ==============================================
% [Required] Load the Logicmoo Common Utils
% ==============================================
:- ensure_loaded(library(logicmoo_common)).

ensure_guitracer:-!.
ensure_guitracer:- % break,
 absolute_file_name(swi(xpce/prolog/lib),X), assert_if_new(user:library_directory(X)), 
 user:use_module(library(pce_prolog_xref)),
 user:use_module(library(emacs_extend)),
 user:use_module(library(trace/gui)),
 user:use_module(library(pce)),
 user:use_module(library(gui_tracer)),
 reload_library_index.


%% load_pdt is det.
%
%  Calls user:consult(library('logicmoo/pdt_server/socketProcessXXX.tmp.pl')) 
%  and imports/reexprts the preds
%
load_pdt:-
 % ensure_guitracer, 
 % user:use_module('/opt/logicmoo_workspace/lib/swipl/xpce/prolog/lib/gui_tracer.pl'),
 %break,
 abolish(start_pdt/0),
 abolish(start_pdt/1),
 user:consult(library('logicmoo/pdt_server/socketProcessXXX.tmp.pl')),
 import(user:start_pdt/0),
 import(user:start_pdt/1),
 export(start_pdt/0),export(start_pdt/1).


:- if(app_argv('--pdt')).
%:- break.
:- load_pdt. %during_boot()
:- endif.


