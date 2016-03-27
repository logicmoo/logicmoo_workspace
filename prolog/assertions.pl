:- module(assertions, []).

:- use_module(library(compound_expand)).
:- reexport(library(assertions_op)).
:- reexport(library(assrt_lib)).

:- multifile user:file_search_path/2.

term_expansion((:- Decl), term_position(_, _, _, _, [DPos]), Records, RPos) :-
    assertion_records(Decl, DPos, Records, RPos).

:- true pred assertion_db/13 is deprecated.
