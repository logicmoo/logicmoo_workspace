:- module(assertions, []).

%% Do not change the order of the next sentences unless you are sure:

:- multifile user:file_search_path/2.

:- use_module(library(compound_expand)).
:- reexport(library(assertions_op)).
:- reexport(library(assrt_lib)).

term_expansion_decl(Decl, PPos, Records, RPos) :-
    nonvar(PPos),
    PPos = parentheses_term_position(_, _, Pos), !,
    term_expansion_decl(Decl, Pos, Records, RPos).
term_expansion_decl(Decl, term_position(_, _, _, _, [DPos]), Records, RPos) :-
    assertion_records(Decl, DPos, Records, RPos).

term_expansion((:- Decl), DPos, Records, RPos) :-
    term_expansion_decl(Decl, DPos, Records, RPos).

:- use_module(library(basicprops)).

:- deprecated assertion_db/13.

:- regtype assrt_type/1.
