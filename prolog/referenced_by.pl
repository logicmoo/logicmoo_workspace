:- module(referenced_by, [referenced_by/3]).

:- use_module(library(maplist_dcg)).

referenced_by(L) --> maplist_dcg(referenced_by_one, L).

referenced_by_one(Loc/CI) -->
    ['\t'], Loc, CI,
    [nl].
