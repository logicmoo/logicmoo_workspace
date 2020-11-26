
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(nl_tool_parse,
	  [nl_tool_parse_as_described_by_config_file/2,
	   start_nl_tool_process_as_described_by_config_file/0,
	   kill_nl_tool_process/0,
	   nl_tool_process_is_running/0,

	   debug_nl_tool_parse_on/0,
	   debug_nl_tool_parse_off/0
	  ]
	 ).

:- use_module('$REGULUS/Prolog/recognition').
:- use_module('$REGULUS/Prolog/regulus_utilities').

:- use_module('$REGULUS/PrologLib/batchrec_tools').
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(system)).
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).
:- use_module(library(lists)).
'SICSTUS3/4'( ( :- use_module(library(system)) ),
	      ( :- use_module(library(system3), [shell/1, shell/2, working_directory/2] ) ) ).
'SICSTUS3/4'( ( :- use_module(library(charsio)) ), ( :- use_module('$REGULUS/PrologLib/compatibility_charsio') ) ).

%------------------------------------------------------------------------------------

:- dynamic nl_tool_process_is_running/0.
:- dynamic nl_tool_in_stream/1, nl_tool_out_stream/1.
:- dynamic debug_nl_tool_parse/0.

%------------------------------------------------------------------------------------

debug_nl_tool_parse_on :-
	retractall(debug_nl_tool_parse),
	assertz(debug_nl_tool_parse),
	!.

debug_nl_tool_parse_off :-
	retractall(debug_nl_tool_parse),
	!.

%------------------------------------------------------------------------------------

nl_tool_parse_as_described_by_config_file(SentAtom, LF) :-
	on_exception(Exception,
		     nl_tool_parse_as_described_by_config_file_main(SentAtom, LF),
		     inform_about_error_in_nl_tool_parse(SentAtom, Exception)
		    ).

inform_about_error_in_nl_tool_parse(SentAtom, Exception) :-
	format('~N~nNL tool parse on input "~w" aborted due to error:~n~n', [SentAtom]),
	print_message(error, Exception),
	format('~n', []),
	fail.

nl_tool_parse_as_described_by_config_file_main(SentAtom, LF) :-
	(   nl_tool_process_is_running ->
	    true
	;
	    otherwise ->
	    start_nl_tool_process_as_described_by_config_file
	),
	get_nl_tool_in_and_out_streams(SIn, SOut),
	format(SIn, '~w~n', [SentAtom]),
	flush_output(SIn),
	%sleep(1.0),
	read_nl_tool_output_lines(SOut, LFLines),
	!,
	member(LFLine, LFLines),
	parse_nl_tool_output_line_to_regulus_form(LFLine, LF).

%------------------------------------------------------------------------------------
/*
$ nl-tool -package recogniser -grammar .MAIN
Ready

switch on the light in the kitchen
Number of Interpretations: 1

{<value ((utterance_type command) (action switch) (onoff on) (device light) (location kitchen))>}

foobar
Number of Interpretations: 0

*/

read_nl_tool_output_lines(S, LFLines) :-
	read_lines_until_number_of_interpretations(S, NumberOfInterpretations),
	read_n_lf_lines(S, NumberOfInterpretations, LFLines),
	!.
read_nl_tool_output_lines(_S, _LFLines) :-
	format2error('~N*** Unable to read nl-tool output~n', []),
	fail.

read_lines_until_number_of_interpretations(S, NumberOfInterpretations) :-
	read_line_stripping_final_13_if_necessary(S, Line),
	%format('~N--- Read line: ~s~n', [Line]),
	(   Line = end_of_file ->
	    fail
	;
	    append("Number of Interpretations: ", NumChars, Line) ->
	    (   safe_number_codes(NumberOfInterpretations, NumChars) ->
		true
	    ;
		format2error('~N*** Error: unable to get number of interpretations from: "~s"~n', [Line]),
		fail
	    )
	;
	    otherwise ->
	    read_lines_until_number_of_interpretations(S, NumberOfInterpretations)
	),
	!.
read_lines_until_number_of_interpretations(_S, _NumberOfInterpretations) :-
	format2error('~N*** Error: unable to find number of interpretations~n', []),
	fail.

read_n_lf_lines(_S, 0, []) :-
	!.
read_n_lf_lines(S, I, Result) :-
	I > 0,
	read_line_stripping_final_13_if_necessary(S, Line),
	(   Line = end_of_file ->
	    fail
	;
	    append("{", _Rest, Line) ->
	    Result = [Line | Lines],
	    I1 is I - 1,
	    !,
	    read_n_lf_lines(S, I1, Lines)
	;
	    otherwise ->
	    read_n_lf_lines(S, I, Result)
	),
	!.

%------------------------------------------------------------------------------------

% Temporary def

parse_nl_tool_output_line_to_regulus_form(LFLine, LF) :-
	nl_tool_output_line(LF0, LFLine, []),
	convert_slot_notation(LF0, LF),
	!.
parse_nl_tool_output_line_to_regulus_form(LFLine, _LF) :-
	format2error('~N*** Error: unable to parse nl-tool output "~s"~n', [LFLine]),
	fail.

nl_tool_output_line(LF) -->
	"{",
	slot_value_list(LF),
	"}",
	!.

convert_slot_notation([], []) :-
	!.
convert_slot_notation([slot(Key, Val) | Slots], [Key=Val | Slots1]) :-
	!,
	convert_slot_notation(Slots, Slots1).
convert_slot_notation(LF, LF).

%------------------------------------------------------------------------------------

start_nl_tool_process_as_described_by_config_file :-
	%kill_nl_tool_process,
	retractall(nl_tool_process_is_running),
	get_package_and_grammar_from_config_file(Package, Grammar),
	format_to_atom('nl-tool -package ~w -grammar ~w', [Package, Grammar], Command),
	safe_exec(Command, [pipe(SIn), pipe(SOut), null], _PID),
	sleep(2),
	existing_nl_tool_processes(NLToolProcessesList),
	(   ( running_under_windows, NLToolProcessesList = [] ) ->
	    format('~N*** Error: appear to be on Windows, no running nl-tool process found after running command "~w"~n', [Command]),
	    fail
	;
	    otherwise ->
	    store_nl_tool_streams(SIn, SOut),
	    assertz(nl_tool_process_is_running),
	    format('~N--- Started nl-tool process using command "~w"~n', [Command])
	),
	!.
start_nl_tool_process_as_described_by_config_file :-
	format2error('~N*** Error: unable to start nl-tool process~n', []),
	fail.

%------------------------------------------------------------------------------------

kill_nl_tool_process :-
	(   running_under_windows ->
	    format('~N--- Appear to be on Windows, trying to kill existing nl-tool processes~n', []),
	    kill_nl_tool_process1
	;
	    otherwise ->
	    format2error('~N--- Warning: do not appear to be on Windows, so can\'t find and kill old nl-tool processes. ~N--- Clean them up by hand if necessary.', [])
	),
	!.
kill_nl_tool_process :-
	format2error('~N*** Error: bad call: ~w~n', [kill_nl_tool_process]),
	fail.

kill_nl_tool_process1 :-
	existing_nl_tool_processes(List),
	(   List \== [] ->
	    close_and_free_nl_tool_streams,
	    kill_any_existing_nl_tool_processes1(List),
	    sleep(2)
	;
	    true
	),
	retractall(nl_tool_process_is_running),
	!.

kill_any_existing_nl_tool_processes1([]).
kill_any_existing_nl_tool_processes1([F | R]) :-
	kill_nl_tool_process(F),
	!,
	kill_any_existing_nl_tool_processes1(R).

kill_nl_tool_process(rec_process(Type, PID)) :-
	format('~N--- Killing existing ~w process (PID = ~d)~n', [Type, PID]),
	kill_process(PID),
	!.
kill_nl_tool_process(Other) :-
	format2error('~N*** Error: bad call: ~w~n', [kill_nl_tool_process(Other)]),
	fail.

existing_nl_tool_processes(List) :-
	\+ running_under_windows,
	!,
	List = [].
existing_nl_tool_processes(List) :-
	get_windows_ps_info(PSInfo),
	existing_nl_tool_processes1(PSInfo, List),
	!.
existing_nl_tool_processes(List) :-
	format2error('~N*** Error: bad call: ~w~n', [existing_nl_tool_processes(List)]),
	fail.

existing_nl_tool_processes1([], []).
existing_nl_tool_processes1([F | R], [rec_process(Type, PID) | R1]) :-
	nl_tool_process_line(F, Type, PID),
	!,
	existing_nl_tool_processes1(R, R1).
existing_nl_tool_processes1([_F | R], R1) :-
	!,
	existing_nl_tool_processes1(R, R1).

nl_tool_process_line(PSLine, Type, PID) :-
	member(command=CommandAtom, PSLine),
	member(pid=PID, PSLine),
	atom_codes(CommandAtom, CommandChars),
	nl_tool_command_string(Type, String),
	is_contiguous_sublist(String, CommandChars),
	!.

nl_tool_command_string('nl-tool', "nl-tool.exe").
	
%------------------------------------------------------------------------------------

store_nl_tool_streams(SIn, SOut) :-
	close_and_free_nl_tool_streams,
	assertz(nl_tool_in_stream(SIn)),
	assertz(nl_tool_out_stream(SOut)).

close_and_free_nl_tool_streams :-
	close_and_free_nl_tool_in_stream,
	close_and_free_nl_tool_out_stream.

close_and_free_nl_tool_in_stream :-
	nl_tool_in_stream(S),
	retractall(nl_tool_in_stream(_)),
	safe_close(S),
	!.
close_and_free_nl_tool_in_stream.

close_and_free_nl_tool_out_stream :-
	nl_tool_out_stream(S),
	retractall(nl_tool_out_stream(_)),
	safe_close(S),
	!.
close_and_free_nl_tool_out_stream.

%------------------------------------------------------------------------------------

get_nl_tool_in_and_out_streams(SIn, SOut) :-
	nl_tool_in_stream(SIn),
	nl_tool_out_stream(SOut),
	!.
get_nl_tool_in_and_out_streams(SIn, SOut) :-
	format('~N*** Error: bad call: ~w~n',
	       [get_nl_tool_in_and_out_streams(SIn, SOut)]),
	fail.

%------------------------------------------------------------------------------------

get_package_and_grammar_from_config_file(Package, Grammar) :-
	get_rec_params(RecParams),
	get_package_from_rec_params(RecParams, Package0),
	safe_absolute_file_name(Package0, Package),
	member(grammar=Grammar, RecParams),
	atom(Grammar),
	!.
get_package_and_grammar_from_config_file(Package, Grammar) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [get_package_and_grammar_from_config_file(Package, Grammar)]),
	fail.

%------------------------------------------------------------------------------------

read_line_stripping_final_13_if_necessary(S, Line) :-
	read_line(S, Line0),
	(   debug_nl_tool_parse ->
	    format('~N--- Read line: ~s~n', [Line0])
	;
	    true
	),
	strip_final_13_if_necessary(Line0, Line).

strip_final_13_if_necessary([], []) :-
	!.
strip_final_13_if_necessary([13], []) :-
	!.
strip_final_13_if_necessary([F | R], [F | R1]) :-
	!,
	strip_final_13_if_necessary(R, R1).
strip_final_13_if_necessary(Other, Other).
