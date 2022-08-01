%:- module(wmclrt,[load_wamcl_runtime/0]).

:- set_prolog_flag(wamcl_modules,false).

:- create_prolog_flag(lisp_repl_goal,prolog,[keep(true),type(term)]).
:- include('./wam_cl/header').
%:- ensure_loaded(wamcl).

%:- ensure_loaded(utests).

load_wamcl_runtime.

