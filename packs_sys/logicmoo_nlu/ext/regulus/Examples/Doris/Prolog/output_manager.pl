
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(output_manager,
	[abstract_action_to_action/2]
    ).

%======================================================================

:- use_module('$REGULUS/Prolog/generate').

:- use_module(library(lists)).

:- use_module('$REGULUS/PrologLib/utilities').

%======================================================================

% OUTPUT MANAGEMENT: ABSTRACT ACTION TO CONCRETE ACTION

abstract_action_to_action(say(AbsResponse, Tense), tts(Atom)) :-
	perform_output_generation(AbsResponse, Tense, Atom),
	!.

perform_output_generation(AbsAction, Tense, OutputAtom) :-
	generation_grammar(AbsAction, Tense, OutputWords, []),
	fix_orthography(OutputWords, OutputWords1),
	join_with_spaces(OutputWords1, OutputAtom),
	!.
perform_output_generation(_AbsAction, _Tense, OutputAtom) :-
	OutputAtom = 'sorry, i can\'t produce any output',
	!.
perform_output_generation(_AbsAction, _Tense, _OutputAtom) :-
	format('~N~nError in output manager.~n', []),
	fail.

generation_grammar(no, _Tense) --> ['no'].
generation_grammar(yes, _Tense) --> ['yes'].
generation_grammar(unable_to_interpret, _Tense) --> ['sorry that doesn\'t make sense'].
generation_grammar(ambiguous, _Tense) --> ['sorry, that\'s ambiguous'].
generation_grammar(i_dont_understand, _Tense) --> ['sorry, I don\'t understand'].
