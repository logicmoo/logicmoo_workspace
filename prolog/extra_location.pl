:- module(extra_location, [loc_declaration/4,
			   loc_dynamic/4,
			   extra_location/4]).

:- multifile
    loc_declaration/4.
:- discontiguous
    loc_declaration/4.

:- dynamic
     loc_dynamic/4.

extra_location(Head, M, Decl, From) :- loc_declaration(Head, M, Decl, From).
extra_location(Head, M, Decl, From) :- loc_dynamic(    Head, M, Decl, From).
