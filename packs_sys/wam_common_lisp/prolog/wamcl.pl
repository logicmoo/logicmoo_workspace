:- module(wamcl,[wamcl/0]).

:- create_prolog_flag(lisp_repl_goal,repl,[keep(true),type(term)]).
:- ensure_loaded(library(wam_cl/repl)).
:- ensure_loaded(library(wamcl_runtime)).

system:wamcl:- lisp.
    

    


