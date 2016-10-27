:- module(dynamic_locations,
	  [dynamic_locations/1]).

:- use_module(library(location_utils)).
:- use_module(library(extra_codewalk)).

dynamic_locations(OptionL) :-
    extra_walk_code([source(false), on_etrace(collect_dynamic_locations(M))|OptionL], M, _).

:- public collect_dynamic_locations/4.
collect_dynamic_locations(M, MGoal, _, From) :-
    record_location_dynamic(MGoal, M, From).
