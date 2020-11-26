:- module(logicmoo_cg,[cg_reader_tests/0,cg_demo/0]).
:- set_module(class(library)).
cginput:is_module.
:- use_module(library(logicmoo_common)).
%:- abolish(cgpro:cg_isa/2).
%:- multifile(baseKB:cg_isa/2).
%:- dynamic(baseKB:cg_isa/2).
%:- kb_global(baseKB:cg_isa/2).
%:- baseKB:export(baseKB:cg_isa/2).
%:- import(baseKB:cg_isa/2).



:- ensure_loaded(cgp_lib/cgp_api).
:- ensure_loaded(cgp_lib/cgp_translator).
:- ensure_loaded(cgp_lib/cgp_operations).
:- ensure_loaded(cgp_lib/cgp_reader).
:- ensure_loaded(cgp_lib/cgp_common_logic).
:- ensure_loaded(cgt/qxp).
%:- cginput:ensure_loaded('cgp_lib/cgp_fwd.pfc').

%:- break.
% :- kb_shared(cg_isa/2).

:- initialization(module(logicmoo_cg),main).




