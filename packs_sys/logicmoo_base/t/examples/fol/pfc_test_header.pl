
:- ensure_loaded(library(logicmoo_utils)).

:- if(gethostname(ubuntu)).

:- else.

% :- load_files(logicmoo_repl, [if(not_loaded),qcompile(auto)]).
:- endif.

:- with_umt(baseKB,baseKB:ensure_mpred_file_loaded(logicmoo(snark/'common_logic_clif.pfc'))).

