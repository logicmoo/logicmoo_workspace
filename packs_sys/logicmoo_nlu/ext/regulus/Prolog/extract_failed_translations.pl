
:- module(extract_failed_translations,
	  [extract_failed_translations/3]
	 ).

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/PrologLib/utilities').
:- use_module(library(lists)).

%----------------------------------------------------------------------

extract_failed_translations(InFile, OutFile, PrologOrText) :-
	absolute_file_name(InFile, AbsInFile),
	absolute_file_name(OutFile, AbsOutFile),

	prolog_file_to_list(AbsInFile, InList),
	length(InList, NInList),
	format('~N--- Read file (~d records): ~w~n', [NInList, AbsInFile]),

	extract_failed_translations1(InList, OutList),
	length(OutList, NOutList),

	open(AbsOutFile, write, S),
	print_failed_translations(OutList, S, PrologOrText),
	close(S),
	format('~N--- Written file (~d records): ~w~n', [NOutList, AbsOutFile]).
extract_failed_translations(InFile, OutFile, PrologOrText) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [extract_failed_translations(InFile, OutFile, PrologOrText)]),
	fail.

extract_failed_translations1([], []).
extract_failed_translations1([F | R], [Source | R1]) :-
	extract_failed_translation(F, Source),
	!,
	extract_failed_translations1(R, R1).
extract_failed_translations1([_F | R], R1) :-
	!,
	extract_failed_translations1(R, R1).

extract_failed_translation(Record, RealSource) :-
	Record = translation(Source, _Target, _Info, Judgement),
	member(Judgement, [error]),
	(   Source = RealSource+_Context ->
	    true
	;
	    otherwise ->
	    RealSource = Source
	),
	!.

print_failed_translations([], _S, _PrologOrText).
print_failed_translations([F | R], S, PrologOrText) :-
	print_failed_translation(F, S, PrologOrText),
	!,
	print_failed_translations(R, S, PrologOrText).

print_failed_translation(SourceAtom, S, text) :-
	format(S, '~N~w~n', [SourceAtom]),
	!.
print_failed_translation(SourceAtom, S, prolog) :-
	format(S, '~N~q.~n', [sent(SourceAtom)]),
	!.
