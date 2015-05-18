:- module(rtchecks, []).

% A wrapper for the Ciao Runtime-Checks library
:- use_module(library(compound_expand)).
:- reexport(library(swi/rtchecks_lib)).
:- expects_dialect(ciao).
:- use_package(rtchecks).
:- use_module(rtchecks(rtchecks_tr)).
:- set_prolog_flag(runtime_checks, no).

% To allow usage of rtchecks in normal SWI programs:

goal_expansion(Goal0, Goal) :-
    '$set_source_module'(M, M),
    (   Goal0 == end_of_file
    ->  ignore(rtchecks_goal_tr(Goal0, _, M)),
	Goal = true
    ;   rtchecks_goal_tr(Goal0, Goal, M)
    ).

term_expansion((:- include(library(nativeprops))),
	       (:- use_module(library(assertions/native_props)))) :- !.
term_expansion((:- module(M, L)), (:- module(M, L))
	      ) :- !,
    rtchecks_sentence_tr(0, _, M, []), !.
term_expansion(Term0, Term) :-
    '$set_source_module'(M, M),
    rtchecks_sentence_tr(Term0, Term1, M, []),
    ciao:call_eof_goal_hook(Term0, Term1, Term),
    !.
