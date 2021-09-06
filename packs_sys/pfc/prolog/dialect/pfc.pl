:- if(set_prolog_flag(loading_pfc_dialect,true)). :- endif.
:- module(pfc_dialect, []).
:- set_prolog_flag(loading_pfc_dialect,true).
:- '$set_source_module'(pfc_dialect).
:- ensure_loaded(pfc_dia).

:- abolish(pfc:setup_dialect/0).
:- system:module_transparent(pfc:setup_dialect/0). 
pfc:setup_dialect:- pfc_expects_dialect(pfc).

:- prolog_load_context(file,File),retractall(system:'$load_context_module'(File, _, _)),unload_file(File).

:- set_prolog_flag(loading_pfc_dialect,false).


