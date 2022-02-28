
:- module(lps_corner,[golps/1,golps/3]).

%:- system:use_module(library('../engine/db.P')).
:- module_transparent(golps/1).
:- module_transparent(golps/3).

golps(Out):- golps(T,DFA, []),!,(Out=T->true;Out=DFA).
golps(T,DFAgraph,Options) :-
  ignore((
    \+ member(cycle_hook(_, _, _), Options),
    \+ member(background(_), Options),
    (   catch(lps_server_UI:lps_user_is_super, _, fail)
    ->  true
    ;   \+ member(timeout(_), Options)
    ))),
    visualizer:gojson(_File, [dc, silent|Options], [], T, DFAgraph).


:- current_predicate(swish:is_a_module/0) -> true ; asserta(swish:is_a_module).
:- user:ensure_loaded(library(dialect/lps)).

:- interpreter:use_module(library('../engine/interpreter.P')).
:- visualizer:use_module(library('../utils/visualizer.P')).
:- lps_term_expander:ensure_loaded(library('../swish/term_expander')).   

:- use_module(library(logicmoo_utils)).
:- user:ensure_loaded(library(dialect/lps)).
:- notrace(lps_repl:ensure_loaded(library(lps_corner))).
:- notrace(lps_repl:ensure_loaded(library(logicmoo_planner))).

:- notrace(ec_loader:ensure_loaded(library(ec_planner/ec_planner_dmiles))).


:- if(current_module(swish)).
:- user:ensure_loaded('../swish/user_module_file').
:- else.
%:- user:ensure_loaded('../swish/user_module_file').
%:- user:ensure_loaded('../swish/user_module_repl').
:- endif.

