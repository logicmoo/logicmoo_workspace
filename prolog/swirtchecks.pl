:- module(swirtchecks, []).

% A wrapper for the Ciao Runtime-Checks library
:- use_module(library(compound_expand)).

:- expects_dialect(ciao).

user:file_search_path(rtchecks, library(rtchecks)).

:- use_package(rtchecks).
:- reexport(rtchecks(rtchecks_rt)).
:- reexport(rtchecks(rtchecks_utils)).

% To allow usage of rtchecks in normal SWI programs:

goal_expansion(Goal0, Goal) :-
    '$set_source_module'(M, M),
    rtchecks_tr:rtchecks_goal_tr(Goal0, Goal, M),
    !.

term_expansion(Term0, Term) :-
    '$set_source_module'(M, M),
    rtchecks_tr:rtchecks_sentence_tr(Term0, Term, M, []),
    !.
