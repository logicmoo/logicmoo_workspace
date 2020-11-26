
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%---------------------------------------------------------------

:- module(orthography_process_text,
	  [init_orthography_process_text/1,
	   orthography_process_string/2,
	   orthography_process_file/3,
	   test_orthography_process_text/1
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/Prolog/generate').
:- use_module('$REGULUS/Prolog/regulus_utilities').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

test_orthography_process_text(0) :-
	%Sent = "tu te es inquiétés",
	Sent = "l avocat",
	test_orthography_process_sent(Sent),
	!.

test_orthography_process_text(ez_small) :-
	orthography_process_file('$ACCEPT/MT/Europarl/Generated/europarl_ez_transformed_small.txt',
				 '$REGULUS/PrologLib/CorpusTools/french_orthography.pl',
				 '$ACCEPT/MT/Europarl/Generated/europarl_ez_orthography_processed_small.txt').

%---------------------------------------------------------------

test_orthography_process_sent(Sent) :-
	init_orthography_process_text('$REGULUS/PrologLib/CorpusTools/french_orthography.pl'),
	format('~NIn   : ~s~n', [Sent]),

	orthography_process_string(Sent, Sent1),

	format('~NTrans: ~s~n', [Sent1]).

%---------------------------------------------------------------

init_orthography_process_text(File) :-
	tmp_regulus_file('compiled_orthography.pl', CompiledFile),

	safe_absolute_file_name(File, AbsFile),
	safe_absolute_file_name(CompiledFile, AbsCompiledFile),

	compile_orthography_file_or_files(AbsFile, AbsCompiledFile),
	format('~N--- Compiled orthography rule file ~w~n', [AbsFile]),

	safe_compile(orthography_rules, AbsCompiledFile),
	format('~N--- Loaded compiled orthography rule file ~w~n', [AbsCompiledFile]),
	!.

%---------------------------------------------------------------

orthography_process_file(InFile, OrthographyRulesFile, OutFile) :-
	absolute_file_name(InFile, AbsInFile),
	absolute_file_name(OutFile, AbsOutFile),

	init_orthography_process_text(OrthographyRulesFile),
	
	read_unicode_file_to_atom_list(AbsInFile, InList),
	length(InList, NIn),
	format('~N--- Read file (~d lines) ~w~n', [NIn, AbsInFile]),
	
	transform_sents_in_list(InList, OutList, 0, 0-NOut),

	write_out_transformed_list_to_file(OutList, AbsOutFile, NIn, NOut),
	!.

write_out_transformed_list_to_file(List, File, NIn, NOut) :-
	open(File, write, S, [encoding('UTF-8'), encoding_signature(true)]),
	write_out_transformed_list_to_stream(List, S),
	close(S),
	PC is (100.0 * NOut) / NIn,
	format('~N--- Written file (~d lines altered = ~1f%) ~w~n', [NOut, PC, File]),
	!.

write_out_transformed_list_to_stream([], _S).
write_out_transformed_list_to_stream([F | R], S) :-
	format(S, '~N~w~n', [F]),
	!,
	write_out_transformed_list_to_stream(R, S).

transform_sents_in_list([], [], _I, CIn-CIn).
transform_sents_in_list([F | R], Out, I, CIn-COut) :-
	orthography_process_string(F, F1),
	Out = [F1 | R1],
	(   F \== F1 ->
	    CNext is CIn + 1,
	    format('+', [])
	    %format('~N~w~n', [F1])
	;
	    otherwise ->
	    CNext = CIn,
	    format('-', [])
	),
	I1 is I + 1,
	(   0 is I1 mod 100 ->
	    format(' (~d)~n', [I1])
	;
	    otherwise ->
	    true
	),
	flush_output(user),
	!,
	transform_sents_in_list(R, R1, I1, CNext-COut).

%---------------------------------------------------------------

orthography_process_string(AtomIn, AtomOut) :-
	atom_codes(AtomIn, Str),
	fix_orthography_simple_on_string(Str, orthography_rules, Str1),
	initial_uppercase_string(Str1, Str2),
	atom_codes(AtomOut, Str2),
	!.
	