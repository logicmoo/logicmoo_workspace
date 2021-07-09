:- module(ccstate, [ run_state_handler//3, run_state//1, run_state//2 , run_nb_state//1
                   , set/1, set/2, get/1, get/2, app/1, app/2, upd/2
                   ]).

/** <module> Stateful computation as an effect using delimited control

   This module provides two kinds of stateful computation, one which undoes
   state changes on backtracking (run_state//{1,2,3}) and another which preserves
   state changes on backtracking (run_nb_state//1).

   On top this are built two execution contexts which provide mutable
   references (run_ref/1) and a mutable environment (run_env/1).
*/
:- use_module(library(delimcc), [p_reset/3, p_shift/2]).

:- set_prolog_flag(generate_debug_info, false).

% stateful operators
:- meta_predicate app(2), app(+,2).
app(Pr,P)  :- p_shift(Pr, app(P)).
get(Pr,S)  :- p_shift(Pr, get(S)).
set(Pr,S)  :- p_shift(Pr, set(S)).

app(P)     :- app(state, P).
get(S)     :- get(state, S).
set(S)     :- set(state, S).
upd(S1,S2) :- app(upd(S1,S2)).

upd(S1,S2,S1,S2).

% ------- stateful computation reified as DCG ----------
:- meta_predicate run_state_handler(+,3,0,?,?), run_state(0,?,?), run_state(+,0,?,?),
                  run_nb_state(0,+,-), run_nb_state(+,0,+,-).

%% run_state_handler(+Pr:prompt(R), +H:pred(+R,S,S), +G:pred, S1:S, S2:S) is det.
%
%  Run P in an context where handler H is available process requests with
%  state threaded through DCG style.
run_state_handler(Pr,H,G) --> {p_reset(Pr,G,Stat)}, cont_sh(Stat,Pr,H).

cont_sh(susp(Req,Cont),Pr,H) --> call(H,Req), run_state_handler(Pr,H,Cont).
cont_sh(done,_,_) --> [].

%% run_state(+Pr:prompt, +P:pred, S1:S, S2:S) is det.
%% run_state(+P:pred, S1:S, S2:S) is det.
%
%  Run P in an context that allows set/1 and get/1 to be used to
%  to handle a mutable state, initially S1. The final state is unified
%  with S2. run_state/3 uses the prompt =|state|=.
%  State changes are undone on backtracking. =|run_state(Pr,G,S1,S2)|= 
%  is equivalent to =|run_state_handler(Pr,handle,Goal,S1,S2)|=.
run_state(Goal) --> run_state(state, Goal).
run_state(Prompt, Goal) -->
   {p_reset(Prompt, Goal, Status)},
   cont_state(Status, Prompt).

cont_state(done,_) --> [].
cont_state(susp(R,Cont), Prompt) --> handle(R), run_state(Prompt, Cont).

handle(get(S),S,S).
handle(set(S),_,S).
handle(app(P),S1,S2) :- call(P,S1,S2).

%% run_nb_state(+Pr:prompt, +P:pred, +S1:S, -S2:S) is det.
%% run_nb_state(+P:pred, +S1:S, -S2:S) is det.
%
%  Run P in a context where get/1 and set/1 manipulate a mutable state,
%  similar to run_state/3, but state changes are not undone on backtracking.
%  Note that, to ensure preservation of state on backtracking, set/1 saves a
%  copy of the given term, not the term itself. Implementation uses nb_getval/2
%  and nb_setval/2 with a dynamically generated key. run_nb_state/3 uses
%  prompt =|state|=.
%
%  Note that using this can be quite expensive if the state is large due to
%  the copying that occurs whenever it is changed.
run_nb_state(Goal) --> run_nb_state(state, Goal).
run_nb_state(Prompt, Goal, S1, S2) :-
   gensym(nbs,Key),
   setup_call_cleanup( nb_setval(Key, S1),
                       (run_nb(Prompt, Goal, Key), nb_getval(Key, S2)),
                       nb_delete(Key)).

run_nb(Prompt, Goal, Key) :-
   p_reset(Prompt, Goal, Status),
   cont_nb_state(Status, Prompt, Key).

cont_nb_state(done, _, _).
cont_nb_state(susp(R,Cont), Prompt, Key) :- handle_nb(R,Key), run_nb(Prompt, Cont, Key).

handle_nb(get(S),Key) :- nb_getval(Key,S).
handle_nb(set(S),Key) :- nb_setval(Key,S).
handle_nb(app(P),Key) :- nb_getval(Key,S1), call(P,S1,S2), nb_setval(Key,S2).
