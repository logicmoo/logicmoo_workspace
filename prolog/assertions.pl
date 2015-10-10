:- module(assertions, []).

:- use_module(xlibrary(compound_expand)).
:- reexport(assertions(assertions_op)).
:- reexport(assertions(assrt_lib)).

:- multifile user:file_search_path/2.

term_expansion((:- Decl), term_position(_, _, _, _, [DPos]), Records, RPos) :-
    assertion_records(Decl, DPos, Records, RPos).
