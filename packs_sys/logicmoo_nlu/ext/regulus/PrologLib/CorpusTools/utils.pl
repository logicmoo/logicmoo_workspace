:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%---------------------------------------------------------------

:- module(corpus_utils,
	  [remove_spaces_and_punctuation/2
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

remove_spaces_and_punctuation([], []).
remove_spaces_and_punctuation([F | R], R1) :-
	space_or_punctuation(F),
	!,
	remove_spaces_and_punctuation(R, R1).
remove_spaces_and_punctuation([F | R], [F | R1]) :-
	!,
	remove_spaces_and_punctuation(R, R1).

space_or_punctuation(' ').
% Try keeping punctuation
space_or_punctuation('.').
%space_or_punctuation(',').
%space_or_punctuation('-').
%space_or_punctuation(';').
%space_or_punctuation(':').
space_or_punctuation('!').
space_or_punctuation('?').
%space_or_punctuation('(').
%space_or_punctuation(')').
%space_or_punctuation('"').
%space_or_punctuation('\'').
