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

/** <module> logicmoo_plarkc - special module hooks into the logicmoo engine allow
   clif syntax to be recocogized via our CycL/KIF handlers 
 
 Logicmoo Project: A LarKC Server written in Prolog
 Maintainer: Douglas Miles
 Dec 13, 2035

 ?- ensure_loaded(library(logicmoo_clif)).

:- set_prolog_flag(verbose_autoload,true).
*/

:- '$set_source_module'(baseKB).

:- ensure_loaded(library(logicmoo_lib)).


:- pfc_lib:use_module(library(pfc_lib)).
:- set_fileAssertMt(baseKB).

%:- set_defaultAssertMt(baseKB).

:- set_prolog_flag_until_eof(retry_undefine,false).

:- user:use_module(library(logicmoo_common)).

:- if((exists_source(library(wam_cl/sreader)))).
:- use_module(library(wam_cl/sreader)).
:- endif.

:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.
:- prolog_load_context(directory,Dir),
   DirFor = library,
   before_boot((( \+ user:file_search_path(DirFor,Dir)) ->asserta(user:file_search_path(DirFor,Dir));true)),!.

:- '$set_source_module'(baseKB).

:- asserta_new(user:file_search_path(logicmoo,library('logicmoo/.'))).
:- prolog_load_context(directory,Dir), asserta_new(user:file_search_path(logicmoo,Dir)).
% :- asserta_new(user:file_search_path(logicmoo,library('.'))).

% :- add_library_search_path('./logicmoo/common_logic/',[ 'common_*.pl']).


:- reexport(library('logicmoo/common_logic/common_logic_utils.pl')).
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

:- fixup_exports.

:- dynamic user:prolog_load_file/2.
:- multifile user:prolog_load_file/2.
%:- use_module(library(logicmoo_common)).
user:prolog_load_file(Spec, Options):- maybe_load_clif_file(Spec, Options),!.


:- baseKB:ensure_loaded(baseKB:library('logicmoo/common_logic/common_logic_clif.pfc')).

:- kif_compile.




   