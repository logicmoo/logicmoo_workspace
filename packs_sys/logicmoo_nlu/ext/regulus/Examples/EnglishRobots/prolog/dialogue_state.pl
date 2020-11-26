
:- module(dialogue_state,
	[empty_dialogue_state/1,

	 get_from_state/3,
	 set_in_state/4]
    ).

%======================================================================

:- use_module(library(assoc)).

%======================================================================

component_name(objects).
component_name(language).

%======================================================================

empty_dialogue_state(State) :-
	empty_assoc(State).

get_from_state(ComponentName, State, Component) :-
	check_component_name(ComponentName),
	get_assoc(ComponentName, State, Component),
	!.

set_in_state(ComponentName, InState, Component, OutState) :-
	check_component_name(ComponentName),
	put_assoc(ComponentName, InState, Component, OutState),
	!.

check_component_name(ComponentName) :-
	component_name(ComponentName),
	!.
check_component_name(ComponentName) :-
	format('~N*** Error: unknown component name in state: ~w~n', [ComponentName]),
	fail.



	    