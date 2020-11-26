:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(dynamic_lexicon_runtime,
	  [init_dynamic_lexicon_runtime/0,
	   assert_dynamic_lex_entry/1
	  ]).

:- use_module('$REGULUS/Prolog/regulus_utilities').

%---------------------------------------------------------------

init_dynamic_lexicon_runtime.

assert_dynamic_lex_entry(Entry) :-
	format2error('~N*** Error: no support for dynamic lexicon. Unable to assert dynamic lexicon entry ~w~n',
		     [Entry]),
	fail.
