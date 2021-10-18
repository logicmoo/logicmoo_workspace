
:- ensure_loaded(logicmoo_module_aiml_toplevel).

% :- ensure_loaded(logicmoo_module_aiml_testing).

:- ignore((current_prolog_flag(argv,Y),alicebot(Y))).

:- initialization(main_loop).

