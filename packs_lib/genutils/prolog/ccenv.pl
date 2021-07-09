:- module(ccenv, [ run_env/1, env_new/2, env_get/2, env_set/2, env_app/2, env_upd/3 ]).
/** <module> Delimited context providing environment with mutable name-value map */

:- use_module(library(data/env), [init_env//0, get_key//2, set_key//2, upd_key//3]).
:- use_module(library(ccstate), [run_state/4, app/2]).

:- meta_predicate run_env(0), env_app(+,2).

%% run_env(+P:pred) is det.
%  Run P inside a run_state/4 with the prompt set to =|env|=, providing
%  an environment containing mutable key-value mappings.
run_env(Goal) :-
   init_env(_,S),
   run_state(env, Goal, S, _).

env_new(R,X) :- app(env, ins_key(R,X)).
env_get(R,X) :- app(env, get_key(R,X)).
env_set(R,X) :- app(env, set_key(R,X)).
env_app(R,P) :- app(env, upd_key(R,X,Y)), call(P,X,Y).
env_upd(R,X,Y) :- app(env, upd_key(R,X,Y)).

