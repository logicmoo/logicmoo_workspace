% #!/usr/bin/env swipl

:- module(myMicrotheory,[]).

:- ensure_loaded(library(pfc)).

:- begin_pfc.


:- mpred_trace_exec.

%:- ain(clif(exists(P,tPerson(P)))).

end_of_file.

:- set_defaultAssertMt(myMicrotheory).

:- kb_shared(genlMt/2).

t(genlMt,myMicrotheory,baseKB).

genlMt(myMicrotheory,baseKB).

:- mpred_trace_exec.

:-ain(clif(exists(P,tPerson(P)))).

% ?- pp_facts.


