/*   
  LogicMOO Base FOL/PFC Setup
% Dec 13, 2035
% Douglas Miles

*/
:- if((\+ current_prolog_flag(xref,true),
  dynamic(baseKB:'using_pfc'/6),
  '$current_source_module'(SM),context_module(CM),'$current_typein_module'(TM),
  (Info='using_pfc'(+,SM,TM,CM,_,pfc_mod)), 
  % writeln(Info),
  asserta(baseKB:Info))).
:- endif.
:- module(pfc_mod, [using_pfc_mod/0,
      op(1199,fx,('==>')),
      op(1190,xfx,('::::')),
      op(1180,xfx,('==>')),
      op(1170,xfx,('<==>')),
      op(1160,xfx,('<-')),
      %op(1150,xfx,('=>')),
      %op(1140,xfx,('<=')),
      %op(1130,xfx,('<=>')),
      %op(300,fx,('-')),
      %op(600,yfx,('&')),
      %op(600,yfx,('v')),
      %op(350,xfx,('xor')),
      op(300,fx,('~'))]).
:- set_module(class(library)).
:- reexport(library(logicmoo_utils)).

:- dynamic(baseKB:'using_pfc'/4).
:- dynamic(lmcache:pfc_mod_filename/1).
:- volatile(lmcache:pfc_mod_filename/1).
:- lmcache:pfc_mod_filename(File) -> true ;
   prolog_load_context(file,File),
   asserta(lmcache:pfc_mod_filename(File)).

:- create_prolog_flag(dmsg_level,never,[type(term),keep(true)]).
:- use_module(library(pfc_lib)).


:- multifile(user:prolog_load_file/2).
:- dynamic(user:prolog_load_file/2).      
%! prolog_load_file( ?ModuleSpec, ?Options) is semidet.
%
% Hook To [user:prolog_load_file/2] For PFC Modules
% Prolog Load File.
%
user:prolog_load_file(ModuleSpec, _Options):-  fail,
 once((
  \+ current_prolog_flag(xref,true),
  strip_module(ModuleSpec,_Module,Spec),
  catch(exists_source(Spec, Path),error(_,_),fail),atomic(Path),
  lmcache:pfc_mod_filename(Path),
  '$current_source_module'(SM),
  '$current_typein_module'(TM),
  context_module(CM),
  prolog_load_context(file,File),
  add_pfc_to_module(+,SM,TM,CM,File,pfc_mod))),
  fail.


%! using_pfc_mod is det.
% 
%  This is seen by XREF 
%
using_pfc_mod.

:- baseKB:'using_pfc'(Mod,SM,TM,CM,File,Why) -> 
   add_pfc_to_module(Mod,SM,TM,CM,File,Why); true.

