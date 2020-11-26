
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(summarise_make_trace,
	  [summarise_make_trace_from_command_line/1,
	   summarise_make_trace/1,
	   test/1]
	 ).

:- use_module('$REGULUS/PrologLib/utilities').

%:- use_module(library(system)).
:- use_module(library(lists)).

%--------------------------------------------------------------------

test(0) :-
	summarise_make_trace('$CALLSLT/Fre/scripts/make_traces/tmp.txt').
test(1) :-
	summarise_make_trace('$CALLSLT/Fre/scripts/make_traces/2012_08_09.txt').
test(2) :-
	summarise_make_trace('$CALLSLT/Int/scripts/make_trace.txt').

%--------------------------------------------------------------------

summarise_make_trace_from_command_line([File]) :-
	!,
	summarise_make_trace(File).
summarise_make_trace_from_command_line(_Args) :-
	format('~N~nUsage: sicstus -l $REGULUS/Prolog/summarise_make_trace.pl -a <File>~2n', []).

summarise_make_trace(File) :-
	safe_absolute_file_name(File, AbsFile),
	format('~N~nSummarising make trace file "~w"~n~n', [AbsFile]),
	open(AbsFile, read, S),
	empty_assoc_generic(AssocIn),
	summarise_make_trace_stream(S, 0, null_context, AssocIn-AssocOut),
	print_summary_statistics(AssocOut),
	close(S).

%--------------------------------------------------------------------

summarise_make_trace_stream(S, IIn, ContextIn, AssocIn-AssocOut) :-
	read_line(S, Line),
	%format('~NRead line: "~s"~n', [Line]),
	INext1 is IIn + 1,
	(   Line = end_of_file ->
	    AssocIn = AssocOut
	;
	    otherwise ->
	    summarise_make_trace_stream1(Line, S, INext1-INext2, ContextIn-ContextNext, AssocIn-AssocNext),
	    !,
	    summarise_make_trace_stream(S, INext2, ContextNext, AssocNext-AssocOut)
	).

summarise_make_trace_stream1(Line, S, IIn-IOut, ContextIn-ContextOut, AssocIn-AssocOut) :-
	possibly_change_context(Line, ContextIn-ContextOut),
	(   error_line(Line, ContextOut, ErrorType, NLines) ->
	    inc_assoc_generic(AssocIn, ErrorType, AssocOut),
	    format('~N~w (line ~d)~n~s~n', [ErrorType, IIn, Line]),
	    print_n_lines(S, NLines),
	    IOut is IIn + NLines
	;
	    otherwise ->
	    IOut = IIn,
	    AssocOut = AssocIn
	),
	!.
summarise_make_trace_stream1(Line, S, I, Context, Assoc) :-
	format('~N*** INTERNAL ERROR: BAD CALL: ~w~n',
	       [summarise_make_trace_stream1(Line, S, I, Context, Assoc)]),
	fail.

%--------------------------------------------------------------------

print_n_lines(_S, I) :-
	I =< 0,
	!.
print_n_lines(S, I) :-
	read_line(S, Line),
	format('~N~s~n', [Line]),
	I1 is I - 1,
	!,
	print_n_lines(S, I1).

%--------------------------------------------------------------------

print_summary_statistics(AssocOut) :-
	assoc_generic_to_list(AssocOut, List),
	(   List = [] ->
	    format('~N~nNO ERRORS~n', [])
	;
	    otherwise ->
	    format('~N~nERROR SUMMARY:~n~n', []),
	    print_summary_statistics1(List)
	).

print_summary_statistics1([]).
print_summary_statistics1([Type-N | R]) :-
	format('~N~d~6|~w~n', [N, Type]),
	!,
	print_summary_statistics1(R).

%--------------------------------------------------------------------

possibly_change_context(Line, ContextIn-ContextOut) :-
	context_pattern(Pattern, ContextIn-ContextOut),
	match_line_to_pattern(Pattern, Line),
	format('~N~nNew context: ~w~n', [ContextOut]),
	!.
possibly_change_context(_Line, ContextIn-ContextIn).

%--------------------------------------------------------------------

error_line(Line, Context, ErrorType, NLines) :-
	error_pattern(Pattern, Context, ErrorType, NLines),
	match_line_to_pattern(Pattern, Line),
	!.

%--------------------------------------------------------------------

match_line_to_pattern([], _Line).
match_line_to_pattern([not(F) | R], Line) :-
	!,
	\+ is_contiguous_sublist(F, Line),
	match_line_to_pattern(R, Line).
match_line_to_pattern([F | R], Line) :-
	append(F, Rest, Line),
	!,
	match_line_to_pattern(R, Rest).
match_line_to_pattern(Pattern, [_FChar | RChars]) :-
	!,
	match_line_to_pattern(Pattern, RChars).

%--------------------------------------------------------------------

% context_pattern(Pattern, ContextIn-ContextOut)

context_pattern(["make[1]: Entering directory", "Int/scripts"], _Any-interlingua).
context_pattern(["make[1]: Leaving directory"], interlingua-no_context).
context_pattern(["make corpora"], _Any-corpora).
context_pattern(["make specialised_grammar"], _Any-specialised_grammar).
context_pattern(["make recognition_generation_grammar"], _Any-recognition_generation_grammar).
context_pattern(["make generation"], _Any-generation).
context_pattern(["make text_test"], _Any-text_test).
context_pattern(["make fre_fre"], _Any-fre_fre).
context_pattern(["make cache_dialogue_info"], _Any-cache_dialogue_info).
context_pattern(["make corpora_with_lesson_info"], _Any-corpora_with_lesson_info).
context_pattern(["make nuance_grammar"], _Any-nuance_grammar).
context_pattern(["make recogniser"], _Any-recogniser).
%context_pattern([], -).

%--------------------------------------------------------------------

% error_pattern(Pattern, Context, ErrorType, NLines)

error_pattern(["*** Parsing failed"], _Any, 'Parsing failed', 0).
error_pattern(["*** REGULUS COMPILATION ERROR"], specialised_grammar, 'Regulus compilation error in specialised_grammar', 1).
error_pattern(["*** REGULUS COMPILATION ERROR"], recognition_generation_grammar, 'Regulus compilation error in recognition_generation_grammar', 1).
error_pattern(["*** REGULUS COMPILATION ERROR"], generation, 'Regulus compilation error in generation', 1).
error_pattern(["*** REGULUS COMPILATION ERROR"], nuance_grammar, 'Regulus compilation error in nuance_grammar', 1).
error_pattern(["*** REGULUS COMPILATION ERROR"], cache_dialogue_info, 'Regulus compilation error in cache_dialogue_info', 1).

error_pattern(["System error"], interlingua, 'System error in interlingua', 2).
error_pattern(["System error"], corpora, 'System error in corpora', 2).
error_pattern(["System error"], specialised_grammar, 'System error in specialised_grammar', 2).
error_pattern(["System error"], recognition_generation_grammar, 'System error in recognition_generation_grammar', 2).
error_pattern(["System error"], generation, 'System error in generation', 2).
error_pattern(["System error"], text_test, 'System error in text_test', 2).
error_pattern(["System error"], fre_fre, 'System error in fre_fre', 2).
error_pattern(["System error"], cache_dialogue_info, 'System error in cache_dialogue_info', 2).
error_pattern(["System error"], corpora_with_lesson_info, 'System error in corpora_with_lesson_info', 2).
error_pattern(["System error"], nuance_grammar, 'System error in nuance_grammar', 2).
error_pattern(["System error"], recogniser, 'System error in recogniser', 2).

error_pattern(["Syntax error"], interlingua, 'Syntax error in interlingua', 6).
error_pattern(["Syntax error"], corpora, 'Syntax error in corpora', 6).
error_pattern(["Syntax error"], specialised_grammar, 'Syntax error in specialised_grammar', 6).
error_pattern(["Syntax error"], recognition_generation_grammar, 'Syntax error in recognition_generation_grammar', 6).
error_pattern(["Syntax error"], generation, 'Syntax error in generation', 6).
error_pattern(["Syntax error"], text_test, 'Syntax error in text_test', 6).
error_pattern(["Syntax error"], fre_fre, 'Syntax error in fre_fre', 6).
error_pattern(["Syntax error"], cache_dialogue_info, 'Syntax error in cache_dialogue_info', 6).
error_pattern(["Syntax error"], corpora_with_lesson_info, 'Syntax error in corpora_with_lesson_info', 6).
error_pattern(["Syntax error"], nuance_grammar, 'Syntax error in nuance_grammar', 6).
error_pattern(["Syntax error"], recogniser, 'Syntax error in recogniser', 6).

error_pattern(["*** Error", not("Vocabulary item contains spaces")], interlingua, 'Error in interlingua', 0).
error_pattern(["*** Error"], corpora, 'Error in corpora', 1).
error_pattern(["*** Error"], specialised_grammar, 'Error in specialised_grammar', 1).
error_pattern(["*** Error"], recognition_generation_grammar, 'Error in recognition_generation_grammar', 1).
error_pattern(["*** Error"], generation, 'Error in generation', 1).
error_pattern(["*** Error"], text_test, 'Error in text_test', 1).
error_pattern(["*** Error"], fre_fre, 'Error in fre_fre', 1).
error_pattern(["*** Error", not("needs to be the name of an existing file")],
	      cache_dialogue_info, 'Error in cache_dialogue_info', 1).
error_pattern(["*** Error"], corpora_with_lesson_info, 'Error in corpora_with_lesson_info', 1).
error_pattern(["*** Error"], nuance_grammar, 'Error in nuance_grammar', 1).
error_pattern(["*** Error"], recogniser, 'Error in recogniser', 1).

error_pattern(["*** Warning"], corpora, 'Warning in corpora', 1).
error_pattern(["*** Warning", not("is never used")], specialised_grammar, 'Warning in specialised_grammar', 1).
error_pattern(["*** Warning", not("is never used")], recognition_generation_grammar, 'Warning in recognition_generation_grammar', 1).
error_pattern(["*** Warning", not("is never used")], generation, 'Warning in generation', 1).
error_pattern(["*** Warning"], text_test, 'Warning in text_test', 1).
error_pattern(["*** Warning"], fre_fre, 'Warning in fre_fre', 1).
error_pattern(["*** Warning", not("bad recorded wavfile record")], cache_dialogue_info, 'Warning in cache_dialogue_info', 1).
error_pattern(["*** Warning"], corpora_with_lesson_info, 'Warning in corpora_with_lesson_info', 1).
error_pattern(["*** Warning", not("is never used")], nuance_grammar, 'Warning in nuance_grammar', 1).
error_pattern(["*** Warning"], recogniser, 'Warning in recogniser', 1).
