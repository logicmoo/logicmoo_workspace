:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(ebl_grammar_probs,
	  [sents_file_or_files_to_grammar_probs_data_file/3,
	   sents_file_to_grammar_probs_data_file/3]
      ).

/*

sents_file_to_grammar_probs_data_file(+InFile, +GrammarName, +OutFile)

1. InFile 

Annotated wavfiles file.

2. GrammarName

Prolog atom

3. OutFile

File suitable for use in PFSG training. Each line is of form

<GrammarName> <Transcription>

*/

:- use_module('$REGULUS/Prolog/regulus_utilities').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%--------------------------------------------------------------------------------------------

sents_file_to_grammar_probs_data_file(InFile, BaseGrammarName, OutFile) :-
	sents_file_or_files_to_grammar_probs_data_file([InFile], BaseGrammarName, OutFile).

sents_file_or_files_to_grammar_probs_data_file(InFiles, BaseGrammarName, OutFile) :-
	absolute_file_name(OutFile, AbsOutFile),
	open(AbsOutFile, write, SOut),

	sents_file_or_files_to_grammar_probs_data_stream(InFiles, BaseGrammarName, SOut, 0-Count),

	close(SOut),
	format('~N~n-- Grammar probs data file ~w written (~d records)~n', [AbsOutFile, Count]),
	!.
sents_file_or_files_to_grammar_probs_data_file(InFiles, BaseGrammarName, OutFile) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [sents_file_or_files_to_grammar_probs_data_file(InFiles, BaseGrammarName, OutFile)]),
	fail.

sents_file_or_files_to_grammar_probs_data_stream([], _BaseGrammarName, _SOut, CountIn-CountIn) :-
	!.
sents_file_or_files_to_grammar_probs_data_stream(File, BaseGrammarName, SOut, CountIn-CountOut) :-
	\+ is_list(File),
	!,
	sents_file_or_files_to_grammar_probs_data_stream([File], BaseGrammarName, SOut, CountIn-CountOut).
sents_file_or_files_to_grammar_probs_data_stream([F | R], BaseGrammarName, SOut, CountIn-CountOut) :-
	single_sents_file_to_grammar_probs_data_stream(F, BaseGrammarName, SOut, CountIn-CountNext),
	!,
	sents_file_or_files_to_grammar_probs_data_stream(R, BaseGrammarName, SOut, CountNext-CountOut).

single_sents_file_to_grammar_probs_data_stream(InFile, BaseGrammarName, SOut, CountIn-CountOut) :-
	absolute_file_name(InFile, AbsInFile),
	format('~N~n-- Reading sents file ~w~n', [AbsInFile]),
	open(AbsInFile, read, SIn),

	sents_file_to_grammar_probs_data_stream(SIn, BaseGrammarName, SOut, CountIn-CountOut),

	close(SIn),
	!.

sents_file_to_grammar_probs_data_stream(SIn, BaseGrammarName, SOut, CIn-COut) :-
	read(SIn, T),
	sents_file_to_grammar_probs_data_stream1(T, SIn, BaseGrammarName, SOut, CIn-COut).

sents_file_to_grammar_probs_data_stream1(end_of_file, _SIn, _BaseGrammarName, _SOut, CIn-CIn) :-
	!.
sents_file_to_grammar_probs_data_stream1(Record, SIn, BaseGrammarName, SOut, CIn-COut) :-
	fake_sent_record(Record),
	!,
	sents_file_to_grammar_probs_data_stream(SIn, BaseGrammarName, SOut, CIn-COut).
sents_file_to_grammar_probs_data_stream1(Record, SIn, BaseGrammarName, SOut, CIn-COut) :-
	functor(Record, sent, N),
	N >= 1,
	arg(1, Record, TranscriptionAtom),
	(   N = 1 ->
	    GrammarIDs = [default] ;
	    arg(2, Record, GrammarIDs)
	),
	sents_file_to_grammar_probs_data_stream2(GrammarIDs, TranscriptionAtom, BaseGrammarName, SOut, CIn-CNext),
	!,
	sents_file_to_grammar_probs_data_stream(SIn, BaseGrammarName, SOut, CNext-COut).
sents_file_to_grammar_probs_data_stream1(Record, SIn, BaseGrammarName, SOut, C) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [sents_file_to_grammar_probs_data_stream1(Record, SIn, BaseGrammarName, SOut, C)]),
	fail.

sents_file_to_grammar_probs_data_stream2([], _TranscriptionAtom, _BaseGrammarName, _SOut, CIn-CIn) :-
	!.
sents_file_to_grammar_probs_data_stream2([GrammarID | GrammarIDs], TranscriptionAtom, BaseGrammarName, SOut, CIn-COut) :-
	sents_file_to_grammar_probs_data_stream3(GrammarID, TranscriptionAtom, BaseGrammarName, SOut, CIn-CNext),
	!,
	sents_file_to_grammar_probs_data_stream2(GrammarIDs, TranscriptionAtom, BaseGrammarName, SOut, CNext-COut).

sents_file_to_grammar_probs_data_stream3(GrammarID, TranscriptionAtom, BaseGrammarName, SOut, CIn-COut) :-
	(   GrammarID = default ->
	    format(SOut, '~N~w ~w~n', [BaseGrammarName, TranscriptionAtom]) ;
	    format(SOut, '~N~w__~w ~w~n', [BaseGrammarName, GrammarID, TranscriptionAtom])
	),
	COut is CIn + 1,
	!.

