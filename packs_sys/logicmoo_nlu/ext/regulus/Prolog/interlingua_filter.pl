
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(interlingua_filter,
	[interlingua_filter_lf/1,
	 interlingua_filter_file/3,
	 interlingua_filter_file/4,
	 test/0
	]).

:- use_module('$REGULUS/Prolog/translate').
:- use_module('$REGULUS/Prolog/regulus_eval').
:- use_module('$REGULUS/Prolog/regulus_utilities').
 
:- use_module('$REGULUS/PrologLib/utilities').
:- use_module('$REGULUS/PrologLib/batchrec_tools').

:- use_module(library(lists)).

%---------------------------------------------------------------

test :-
	InFile = '$MED_SLT2/corpora/headache_random.txt',
	OutFile = '$MED_SLT2/corpora/headache_random_filtered.txt',
	Package = '$MED_SLT2/Eng/GeneratedFiles/recogniser',
	interlingua_filter_file(InFile, Package, OutFile).

%---------------------------------------------------------------

% Assumes translation is loaded, including an interlingua checking grammar.
% Succeeds if utterance produces well-formed interlingua.

interlingua_filter_lf(SourceLF) :-
	atom(SourceLF),
	SourceLF = 'NO_LF',
	!,
	fail.
interlingua_filter_lf(SourceLF) :-
	transfer_representation_to_source_discourse(SourceLF, SourceDiscourseLF),
	transfer_representation_to_interlingua(SourceDiscourseLF, Interlingua),
	check_interlingua_structure(Interlingua, InterlinguaSurfaceForm),
	\+ warning_form(InterlinguaSurfaceForm),
	!.

warning_form(Atom) :-
	atom_codes(Atom, String),
	append("WARNING", _, String).

%---------------------------------------------------------------

% InFile is a file of utterances in plain text form, one utterance per line.
%
% OutFile is the subset of utterances which pass interlingua filtering, same format.
%
% Package is the recognition package to use for parsing
%
% Constraints is a list which may optionally contain items of the following forms
%
%   max_length=MaxLength
%   grammar=Grammar

interlingua_filter_file(InFile, Package, OutFile) :-
	interlingua_filter_file(InFile, Package, [], OutFile).

interlingua_filter_file(InFile, Package, Constraints, OutFile) :-
	get_tmp_dir(TmpDir),
	interlingua_filter_file(InFile, Package, Constraints, TmpDir, OutFile).
	
interlingua_filter_file(InFile, Package, Constraints, TmpDir, OutFile) :-
	get_tmp_file('transcriptions_with_wavfiles.txt', TmpDir, TranscriptionsFile),
	get_tmp_file('raw_parsed_file.pl', TmpDir, RawParsedFile),
	get_tmp_file('discarded_transcriptions.txt', TmpDir, DiscardedFile),

	(   member(max_length=MaxLength, Constraints) ->
	    true
	;
	    MaxLength = 1000000
	),

	(   member(grammar=Grammar, Constraints) ->
	    true
	;
	    Grammar = '.MAIN'
	),
	add_dummy_wavfile_ids_in_utterance_file(InFile, MaxLength, TranscriptionsFile, DiscardedFile),
	
	BatchrecCall = do_text_batchrec(TranscriptionsFile, [package=Package, grammar=Grammar], RawParsedFile, TmpDir),
	format('~N--- Call to text batchrec: ~w~n', [BatchrecCall]),
	call(BatchrecCall),
	format('~N--- Done~n', []),
	
	interlingua_filter_raw_parsed_file(RawParsedFile, OutFile, DiscardedFile).

%---------------------------------------------------------------

add_dummy_wavfile_ids_in_utterance_file(InFile, MaxLength, TranscriptionsFile, DiscardedFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(TranscriptionsFile, AbsTranscriptionsFile),
	safe_absolute_file_name(DiscardedFile, AbsDiscardedFile),
	
	open(AbsInFile, read, SIn),
	open(AbsTranscriptionsFile, write, SOut),
	open(AbsDiscardedFile, write, SDiscard),

	format(SDiscard, '~N--- EXAMPLES DISCARDED BECAUSE #WORDS > ~d ---~n', [MaxLength]),
	
	format('~N--- Reading input text file ~w~n', [AbsInFile]),
	
	add_dummy_wavfile_ids_in_stream(SIn, MaxLength, 0-NIn, 0-NOut, SOut, SDiscard),

	close(SIn),
	close(SOut),
	close(SDiscard),

	NDiscarded is NIn - NOut,

	format('~N--- Read input text file (~d records) ~w~n', [NIn, AbsInFile]),
	format('~N--- Written tmp transcriptions file (~d records) ~w~n', [NOut, AbsTranscriptionsFile]),
	format('~N--- Written tmp discarded file (~d records) ~w~n', [NDiscarded, AbsDiscardedFile]),
	!.
add_dummy_wavfile_ids_in_utterance_file(InFile, MaxLength, TranscriptionsFile, DiscardedFile) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [add_dummy_wavfile_ids_in_utterance_file(InFile, MaxLength, TranscriptionsFile, DiscardedFile)]),
	fail.

add_dummy_wavfile_ids_in_stream(SIn, MaxLength, InSoFar-InFinal, OutSoFar-OutFinal, SOut, SDiscard) :-
	read_line(SIn, Line),
	(   Line = end_of_file ->
	    InSoFar = InFinal,
	    OutSoFar = OutFinal
	;
	    otherwise ->
	    InNext is InSoFar + 1,
	    split_string_into_words(Line, Words),
	    length(Words, NWords),
	    (   NWords > MaxLength ->
		OutNext = OutSoFar,
		format(SDiscard, '~N~s~n', [Line])
	    ;
		otherwise ->
		atom_codes(Term, Line),
		add_dummy_wavfile_ids_in_utt(Term, InSoFar, Term1),
		format(SOut, '~N~w~n', [Term1]),
		OutNext is OutSoFar + 1
	    ),
	    !,
	    add_dummy_wavfile_ids_in_stream(SIn, MaxLength, InNext-InFinal, OutNext-OutFinal, SOut, SDiscard)
	).

add_dummy_wavfile_ids_in_utt(Utt, I, UttWithWavfile) :-
	format_to_atom('c:/tmp/dummy~d ~w', [I, Utt], UttWithWavfile),
	!.
add_dummy_wavfile_ids_in_utt(Utt, I, UttWithWavfile) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [add_dummy_wavfile_ids_in_utt(Utt, I, UttWithWavfile)]),
	fail.

%---------------------------------------------------------------

interlingua_filter_raw_parsed_file(RawParsedFile, OutFile, DiscardedFile) :-
	safe_absolute_file_name(RawParsedFile, AbsRawParsedFile),
	safe_absolute_file_name(OutFile, AbsOutFile),
	safe_absolute_file_name(DiscardedFile, AbsDiscardedFile),

	open(AbsRawParsedFile, read, SIn),
	open(AbsOutFile, write, SOut),
	open(AbsDiscardedFile, append, SDiscard),

	format(SDiscard, '~N--- EXAMPLES DISCARDED BECAUSE THEY PRODUCE BAD INTERLINGUA ---~n', []),	
	
	format('~N--- Reading text batchrec output from ~w~n', [AbsRawParsedFile]),

	interlingua_filter_raw_parsed_stream(SIn, 0-NIn, 0-NOut, SOut, SDiscard),
	
	close(SIn),
	close(SOut),
	close(SDiscard),

	NDiscarded is NIn - NOut,

	format('~N--- Read text batchrec output (~d records)~n', [NIn]),
	format('~N--- Written filtered file (~d records) ~w~n', [NOut, AbsOutFile]),
	format('~N--- Appended ~d records to discard file ~w~n', [NDiscarded, AbsDiscardedFile]),
	!.
interlingua_filter_raw_parsed_file(RawParsedFile, OutFile, DiscardedFile) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [interlingua_filter_raw_parsed_file(RawParsedFile, OutFile, DiscardedFile)]),
	fail.

%---------------------------------------------------------------

interlingua_filter_raw_parsed_stream(SIn, NInIn-NInOut, NOutIn-NOutOut, SOut, SDiscard) :-
	read(SIn, Term),
	(   Term = end_of_file ->
	    NInIn = NInOut,
	    NOutIn = NOutOut
	;
	    otherwise ->
	    NInNext is NInIn + 1,
	    (   0 is NInNext mod 500 ->
		format('~d ', [NInNext]), flush_output(user)
	    ;
		true
	    ),
	    (   interlingua_filter_raw_batchrec_record(Term, Term1) ->
		format(SOut, '~N~w~n', [Term1]),
		NOutNext is NOutIn + 1
	    ;
		NOutNext = NOutIn,
		get_text_atom_from_batchrec_record(Term, Text),
		format(SDiscard, '~N~w~n', [Text])
	    ),
	    !,
	    interlingua_filter_raw_parsed_stream(SIn, NInNext-NInOut, NOutNext-NOutOut, SOut, SDiscard)
	).

%---------------------------------------------------------------

interlingua_filter_raw_batchrec_record(BatchrecRecord, Text) :-
	get_raw_lf_from_batchrec_record(BatchrecRecord, RawLF),
	postprocess_raw_lf(RawLF, LF),
	interlingua_filter_lf(LF),
	get_text_atom_from_batchrec_record(BatchrecRecord, Text),
	!.

%---------------------------------------------------------------

postprocess_raw_lf(RawLF, LF) :-
	(   user:regulus_config(lf_postproc_pred, PostProcPred) ->
	    true ;
	    PostProcPred = no_post_proc_pred
	),
	regulus_eval_speech(RawLF, LF, PostProcPred).

%---------------------------------------------------------------

/*

batchrec_item([
wavfile=<Wavfile>,
words=<Words>,
confidence=<ConfidenceScore>,
nl_value=<NLVal>
]). 

*/

get_raw_lf_from_batchrec_record(batchrec_item(List), RawLF) :-
	member(nl_value=RawLF, List),
	!.
get_raw_lf_from_batchrec_record(_BatchrecRecord, RawLF) :-
	RawLF = 'NO_LF'.

get_text_atom_from_batchrec_record(batchrec_item(List), Text) :-
	member(words=Words, List),
	join_with_spaces(Words, Text).

%---------------------------------------------------------------

get_tmp_dir(TmpDir) :-
	safe_absolute_file_name(tmp(''), TmpDir),
	!.
get_tmp_dir(TmpDir) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [get_tmp_dir(TmpDir)]),
	fail.

get_tmp_file(Base, Dir, AbsFile) :-
	format_to_atom('~w/~w', [Dir, Base], File),
	safe_absolute_file_name(File, AbsFile),
	!.
get_tmp_file(Base, Dir, AbsFile) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [get_tmp_file(Base, Dir, AbsFile)]),
	fail.


