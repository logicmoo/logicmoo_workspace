/*   
  LogicMOO Base FOL/PFC Setup
% Dec 13, 2035
% Douglas Miles

*/
:- if(('$current_source_module'(SM),'context_module'(M),'$current_typein_module'(CM),asserta(baseKB:'wusing_pfc'(M,CM,SM,pfc_toplevel)))).
:- endif.
:- module(pfc_toplevel,[use_pfc/0]).
:- use_module(library(logicmoo_utils_all)).
% :- abolish(use_pfc/0).
% :- prolog_load_context(file,File),unload_file(File).
% :- asserta(use_pfc).

use_pfc.


:- if(\+ current_prolog_flag(lm_no_autoload,_)).
:- set_prolog_flag(lm_no_autoload,false).
:- wdmsg_pretty("WARNING: PFC_AUTOLOAD").
:- endif.

:- if(\+ current_prolog_flag(lm_pfc_lean,_)).
:- set_prolog_flag(lm_pfc_lean,false).
:- wdmsg_pretty("WARNING: PFC_NOT_LEAN").
:- endif.

:- reexport(pfc_lib).     
:- set_prolog_flag(mpred_te,true).
%:- set_prolog_flag(verbose_load,true).
%:- set_prolog_flag(debug_on_error,true).
%:- set_prolog_flag(report_error,true).
%:- set_prolog_flag(access_level,system).

:- retract(baseKB:'wusing_pfc'(M,CM,SM,pfc_toplevel)),
   assert(baseKB:'using_pfc'(M,CM,SM,pfc_toplevel)),
   (M==SM -> 
     (ensure_abox(SM),nop((M:ain(genlMt(SM,baseKB)))));
     wdmsg_pretty(baseKB:'lusing_pfc'(M,CM,SM,pfc_toplevel))).

