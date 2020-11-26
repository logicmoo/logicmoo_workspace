
:- module(output_manager,
	[abstract_action_to_action/2]
    ).

%======================================================================

:- use_module(library(lists)).

:- use_module('$REGULUS/PrologLib/utilities').

%======================================================================

% OUTPUT MANAGEMENT: ABSTRACT ACTION TO CONCRETE ACTION

abstract_action_to_action(say(AbsResponse), tts(String)) :-
	perform_output_generation(AbsResponse, String),
	!.

perform_output_generation(AbsAction, OutputString) :-
	generation_grammar(AbsAction, OutputWords, []),
	join_with_spaces(OutputWords, OutputAtom),
	atom_chars(OutputAtom, OutputString),
	!.
perform_output_generation(_AbsAction, _OutputString) :-
	format('~N~nError in output manager.~n', []),
	fail.

generation_grammar(no) --> ['no'].
generation_grammar(unable_to_interpret) --> ['sorry that doesn\'t make sense'].
generation_grammar(ambiguous) --> ['sorry, that\'s ambiguous'].
generation_grammar(reverted) --> ['ok, undone'].

generation_grammar(dev(Device, Location, _OnOff, Intensity)) -->
	generation_grammar(device(Device)),
	['in'],
	generation_grammar(location(Location)),
	['is'],
	generation_grammar(intensity(Intensity)).

generation_grammar(device(light)) --> ['the light'].
generation_grammar(device(fan)) --> ['the fan'].

generation_grammar(location(kitchen)) --> ['the kitchen'].
generation_grammar(location(living_room)) --> ['the living room'].

generation_grammar(intensity(0)) --> ['off'].
generation_grammar(intensity(100)) --> ['on'].
generation_grammar(intensity(Other)) -->
	{ number(Other), 0 < Other, Other < 100 },
	['dimmed'].
