:- module(logicmoo_clif,[
 ]).

:- 
 op(1199,fx,('==>')), 
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),  
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'), 
 op(600,yfx,'&'), 
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'),
 op(300,fx,'-').

:- if(use_module(library(logicmoo_utils))). :- endif.
%:- in_lm_ws(use_module(library(logicmoo_webui))).
%:- webui_load_swish_and_clio.


:- reexport(library('logicmoo/common_logic/common_logic_utils.pl')).

:- reexport(library(sigma_ace)).

/** <module> MODULE LOGICMOO CLIF / logicmoo_plarkc
Logicmoo CLIF - Base Libraries that extend Prolog to support Dynamic Epistemic Logic (DEL) with Constraints.
This special module hooks into the logicmoo engine allow clif syntax to be recocogized via our CycL/KIF handlers 
 
 Logicmoo Projects LarKC Server written in Prolog
 Dec 13, 2035
@author Douglas R. Miles
@license LGPL
*/


:- nop('$set_source_module'( baseKB)).

:- ensure_loaded(library(logicmoo_lib)).


:- pfc_lib:use_module(library(pfc_lib)).
%:- set_fileAssertMt(baseKB).

%:- set_defaultAssertMt(baseKB).

:- set_prolog_flag_until_eof(retry_undefine,false).

:- user:use_module(library(logicmoo_common)).

:- if((exists_source(library(wam_cl/sreader)), \+ current_module(wmclrt))).
:- use_module(library(wam_cl/sreader)).
:- endif.


:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.
:- prolog_load_context(directory,Dir),
   DirFor = library,
   before_boot((( \+ user:file_search_path(DirFor,Dir)) ->asserta(user:file_search_path(DirFor,Dir));true)),!.

:- nop('$set_source_module'( baseKB)).

:- asserta_new(user:file_search_path(logicmoo,library('logicmoo/.'))).
:- prolog_load_context(directory,Dir), asserta_new(user:file_search_path(logicmoo,Dir)).
% :- asserta_new(user:file_search_path(logicmoo,library('.'))).

% :- add_library_search_path('./logicmoo/common_logic/',[ 'common_*.pl']).


:- reexport(library('logicmoo/common_logic/common_logic_boxlog.pl')).
:- reexport(library('logicmoo/common_logic/common_logic_modal.pl')).
:- reexport(library('logicmoo/common_logic/common_logic_exists.pl')).
%:- consult(library('logicmoo/common_logic/common_logic_exists.pl')).
:- reexport(library('logicmoo/common_logic/common_logic_compiler.pl')). 
:- reexport(library('logicmoo/common_logic/common_logic_kb_hooks.pl')).
:- reexport(library('logicmoo/common_logic/common_logic_loader.pl')).
:- reexport(library('logicmoo/common_logic/common_logic_theorist.pl')).
:- reexport(library('logicmoo/common_logic/common_logic_reordering.pl')).
:- reexport(library('logicmoo/common_logic/common_logic_snark.pl')). 
:- reexport(library('logicmoo/common_logic/common_logic_sanity.pl')).

% 

really_load_clif_file(Found, Options):-
  dmsg(really_load_clif_file(Found, Options)),
  fail.

maybe_load_clif_file(Found, Options):- 
  atom(Found),exists_file(Found),!,
  file_name_extension(_,Ext,Found),
  memberchk(Ext,['.clif','.ikl','.kif','.lbase']),!,
  really_load_clif_file(Found, Options).
  
maybe_load_clif_file(Spec, Options):- 
  notrace(absolute_file_name(Spec,Found,[extensions(['.clif','.ikl','.kif',
  %'.lisp',
  '.lbase']),access(read),expand(true),solutions(all)])),
  exists_file(Found),!,
  really_load_clif_file(Found, Options).

:- baseKB:ensure_loaded(baseKB:library('logicmoo/common_logic/common_logic_clif.pfc')).

%:- kif_compile.

%:-system:use_module(library(make)).

:- add_history(use_module(library(logicmoo_clif))).

:- use_module(library(logicmoo_cg)).
:- use_module(library(logicmoo_ec)).
:- use_module(library(logicmoo_nlu)).

:- add_history(qsave_bin(clif)).

:- dynamic user:prolog_load_file/2.
:- multifile user:prolog_load_file/2.
%:- use_module(library(logicmoo_common)).
user:prolog_load_file(Spec, Options):- maybe_load_clif_file(Spec, Options),!.

:- fixup_exports.

:- if(qsave_bin(clif)).
% :- break.
:- endif.


