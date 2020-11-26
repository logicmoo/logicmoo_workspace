/*

Minimal file-based Regulus server. Invoke as follows:

sicstus -l $REGULUS/RegulusLanguageServer/Prolog/start_minimal_server.pl -a <CFGFile> <InputFile> <OutputFile> <Touchfile>

for example

sicstus -l minimal_server.pl -a $REGULUS/Examples/Toy1/scripts/toy1.cfg infile.txt outfile.txt touchfile.txt

The functionality is to enter a loop, where each cycle performs the following operations:

1. Wait until <Touchfile> exists.
2. Delete <Touchfile>.
3. Read the contents of <InputFile>. This should be a list of zero or more Regulus top-level commands,
   one command per line. For example:

LOAD
switch on the light
is the light switched on

4. Regulus is called on the material in <InputFile>. stdout is redirected to write to <OutputFile>.
   A new copy of <OutputFile> is produced for each cycle, i.e. the old output is overwritten.
5. If there were zero lines in <InputFile>, the loop terminates, otherwise the cycle repeats.

*/

:- ['$REGULUS/Prolog/load'].

:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module('$REGULUS/PrologLib/utilities').

start_minimal_regulus_language_server_from_command_line :-
	prolog_flag(argv, Args),
	start_minimal_regulus_language_server_from_command_line1(Args).

start_minimal_regulus_language_server_from_command_line1(Args) :-
	get_args_or_complain(Args, CFGFile, InputFile, OutputFile, Touchfile),
	minimal_regulus_language_server(CFGFile, InputFile, OutputFile, Touchfile).

get_args_or_complain(Args, AbsCFGFile, AbsInputFile, AbsOutputFile, AbsTouchfile) :-
	Args = [CFGFile, InputFile, OutputFile, Touchfile],
	atom(CFGFile),
	atom(InputFile),
	atom(OutputFile),
	atom(Touchfile),
	absolute_file_name(CFGFile, AbsCFGFile),
	absolute_file_name(InputFile, AbsInputFile),
	absolute_file_name(OutputFile, AbsOutputFile),
	absolute_file_name(Touchfile, AbsTouchfile),
	(   file_exists(AbsCFGFile) ->
	    true ;
	    
	    format('~N*** Error: unable to find config file ~w~n', [AbsCFGFile]),
	    fail
	),
	!.
get_args_or_complain(_Args, _CFGFile, _InputFile, _OutputFile, _Touchfile) :-
	format('~N~nUsage: sicstus -l \'$REGULUS/RegulusLanguageServer/Prolog/start_minimal_server.pl\' -a <CFGFile> <InputFile> <OutputFile> <Touchfile>~n', []),
	fail.

minimal_regulus_language_server(CFGFile, InputFile, OutputFile, Touchfile) :-
	format('~N--- Waiting for touchfile ~w... ', [Touchfile]),
	wait_for_touchfile(Touchfile),
	delete_file(Touchfile),
	format('found and deleted~n', []),
	(   file_exists(InputFile) ->
	    read_file_to_atom_list(InputFile, Atoms) ;

	    format('~N*** Error: unable to find input file ~w~n', [InputFile]),
	    fail
	),
	minimal_regulus_language_server1(Atoms, CFGFile, InputFile, OutputFile, Touchfile).

minimal_regulus_language_server1(Atoms, _CFGFile, _InputFile, _OutputFile, _Touchfile) :-
	Atoms = [],
	!.
minimal_regulus_language_server1(Atoms, CFGFile, InputFile, OutputFile, Touchfile) :-
	format('~N--- Processing input: ~q~n', [Atoms]),
	atom_list_to_string_list(Atoms, Strings),
	open(OutputFile, write, S),
	set_output(S),
	regulus_batch(CFGFile, Strings),
	set_output(user),
	close(S),
	format('~N--- Done, output written to ~w~n', [OutputFile]),
	!,
	minimal_regulus_language_server(CFGFile, InputFile, OutputFile, Touchfile).

wait_for_touchfile(Touchfile) :-
	file_exists(Touchfile),
	!.
wait_for_touchfile(Touchfile) :-
	sleep(0.25),
	!,
	wait_for_touchfile(Touchfile).

atom_list_to_string_list([], []).
atom_list_to_string_list([F | R], [F1 | R1]) :-
	atom_chars(F, F1),
	!,
	atom_list_to_string_list(R, R1).	

:- start_minimal_regulus_language_server_from_command_line.

:- halt.



