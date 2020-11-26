
:- module(output_manager,
	[abstract_action_to_action/2]
    ).

%======================================================================

:- use_module(library(lists)).

:- use_module('$REGULUS/PrologLib/utilities').

%======================================================================

% OUTPUT MANAGEMENT: ABSTRACT ACTION TO CONCRETE ACTION

abstract_action_to_action(say(Language, AbstractResponse), say_string(String)) :-
	perform_output_generation(Language, AbstractResponse, String),
	!.

perform_output_generation(Language, AbstractAbstractAction, OutputString) :-
	generation_grammar(Language, AbstractAbstractAction, OutputWords, []),
	join_with_spaces(OutputWords, OutputAtom),
	atom_codes(OutputAtom, OutputString),
	!.
perform_output_generation(_Language, _AbstractAbstractAction, _OutputString) :-
	format('~N~nUnable to formulate concrete response.~n', []),
	fail.

generation_grammar(english, no) --> ['no'].
generation_grammar(french, no) --> ['non'].

generation_grammar(english, unable_to_interpret) --> ['sorry that doesn\'t make sense'].
generation_grammar(french, unable_to_interpret) --> ['Je ne comprende pas'].

generation_grammar(english, ambiguous) --> ['sorry, that\'s ambiguous'].
generation_grammar(french, ambiguous) --> ['C\'est ambigue'].

generation_grammar(english, device(Device, Location, _OnOff, Intensity)) -->
	generation_grammar(english, device(Device)),
	['in'],
	generation_grammar(english, location(Location)),
	['is'],
	generation_grammar(english, intensity(Intensity)).
generation_grammar(french, device(Device, Location, _OnOff, Intensity)) -->
	generation_grammar(french, device(Device, Gender)),
	['dans'],
	generation_grammar(french, location(Location)),
	['est'],
	generation_grammar(french, intensity(Intensity, Gender)).

generation_grammar(english, device(light)) --> ['the light'].
generation_grammar(french, device(light, fem)) --> ['la lampe'].
generation_grammar(english, device(fan)) --> ['the fan'].
generation_grammar(french, device(fan, masc)) --> ['le ventilateur'].

generation_grammar(english, location(kitchen)) --> ['the kitchen'].
generation_grammar(french, location(kitchen)) --> ['la cuisine'].
generation_grammar(english, location(living_room)) --> ['the living room'].
generation_grammar(french, location(living_room)) --> ['le salon'].

generation_grammar(english, intensity(0)) --> ['off'].
generation_grammar(english, intensity(100)) --> ['on'].
generation_grammar(english, intensity(Other)) -->
	{ number(Other), 0 < Other, Other < 100 },
	['dimmed'].
generation_grammar(french, intensity(0, masc)) --> ['éteint'].
generation_grammar(french, intensity(0, fem)) --> ['éteinte'].
generation_grammar(french, intensity(100, masc)) --> ['allumé'].
generation_grammar(french, intensity(100, fem)) --> ['allumée'].
generation_grammar(french, intensity(Other, masc)) -->
	{ number(Other), 0 < Other, Other < 100 },
	['baissé'].
generation_grammar(french, intensity(Other, fem)) -->
	{ number(Other), 0 < Other, Other < 100 },
	['baissée'].
