:- module(wamcl,[wamcl/0]).

:- set_prolog_flag(wamcl_modules,false).

:- create_prolog_flag(lisp_repl_goal,repl,[keep(true),type(term)]).
:- ensure_loaded((wamclrt)).
%:- ensure_loaded((wam_cl/repl)).
%:- initialization((do_wamcl_inits),now).
%:- initialization((do_wamcl_inits),restore).

system:wamcl:- lisp.
    

    


