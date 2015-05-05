:- module(gcover, [gcover/2, covered_db/6, reset_cover/0, reset_cover/1]).

:- use_module(library(ontrace)).

:- meta_predicate gcover(0,+).

gcover(Goal, OptL0 ) :-
    select_option(tag(Tag), OptL0, OptL, user),
    ontrace(Goal, gcover_port(Tag), OptL).

:- dynamic covered_db/6.

gcover_port(Tag, Port, _Frame, _PC, _ParentL, Loc, continue) :-
    record_cover(Loc, Port, Tag).

loc_file_range(file_term_position(File, TermPos), File, Fr, To) :-
    arg(1, TermPos, Fr),
    arg(2, TermPos, To).

record_cover(Loc, Port, Tag) :-
    loc_file_range(Loc, File, Fr, To),
    ( retract(covered_db(Fr, To, File, Port, Tag, Count1))
    ->succ(Count1, Count)
    ; Count=1
    ),
    assertz(covered_db(Fr, To, File, Port, Tag, Count)).

reset_cover :- reset_cover(_).

reset_cover(Tag) :-
    retractall(covered_db(_, _, Tag, _, _, _)).
