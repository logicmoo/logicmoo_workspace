:- module(assertions, []).

:- use_module(library(compound_expand)).
:- reexport(library(assertions_op)).
:- reexport(library(assertions/assrt_lib)).

:- multifile user:file_search_path/2.

user:file_search_path(assertions, library(assertions)).

% assrt_lib:nodirective_error_hook(Assr) :-
%     throw(error(context_error(nodirective, Assr), _)).

term_expansion((:- Decl), term_position(_, _, _, _, [DPos]), Records, RPos) :-
    assertion_records(Decl, DPos, Records, RPos).
