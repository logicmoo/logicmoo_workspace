:- module(ccreader, [ run_reader/3, ask/2 ]).

/** <module> Essentially, direct style reader monad.

*/
:- use_module(library(delimcc), [p_reset/3, p_shift/2]).

:- set_prolog_flag(generate_debug_info, false).

ask(Pr,X) :- p_shift(Pr,X).

:- meta_predicate run_reader(+,0,?).

%% run_reader(+Pr:prompt, +P:pred, X:A) is det.
%  Run P in an context that allows read/2 to be used to access X.
run_reader(Prompt, Goal, X) :-
   p_reset(Prompt, Goal, Status),
   cont_reader(Status, Prompt, X).

cont_reader(done,_,_).
cont_reader(susp(X,Cont), Prompt, X) :- run_reader(Prompt, Cont, X).

