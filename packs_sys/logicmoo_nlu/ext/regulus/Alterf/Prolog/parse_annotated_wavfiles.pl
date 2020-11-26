
:- module(parse_annotated_wavfiles,
	[filter_annotated_wavfiles/3,
	 filter_annotated_wavfiles/4,
	 annotated_wavfiles_to_sents_file/2,
	 annotated_wavfiles_to_sents_file/3,
	 sents_file_to_grammar_probs_data_file/3,
	 parse_annotated_wavfiles/5,
	 parse_annotated_wavfile_line/2,
	 split_annotated_wavfiles_into_training_and_test/4,
	 check_annotated_wavfiles/3]
      ).

%--------------------------------------------------------------------------------------------

:- use_module('$REGULUS/Alterf/Prolog/generic_numbers').
:- use_module('$REGULUS/Alterf/Prolog/classifier_utilities').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(random)).
:- use_module(library(system)).

%--------------------------------------------------------------------------------------------

/*

Routines for processing the test and training data.

filter_annotated_wavfiles(+InFile, +FilterFile, +OutFile, +Mode)

1. InFile 

File of training data in the format defined in classifier_trainer.pl

2. FilterFile 

File specifying what to filter out. A line in the filter file can be of one of following two form:

- ignore_examples_including_tags <Tags> 
  where <Tags> is a space-separated list of semantic atoms.

- ignore_examples_including_words <Words>
  where <Words> is a space-separated list of words

An example filter file is home/speech/rialist/checklist/corpora/filter_spec.

3. OutFile

File produced by filtering, in same format as InFile.

4. Mode.

One of [keep_ignores, discard_ignores]

If mode is 'keep_ignores', and the tags are not compatible with the filter spec, replace them with 'ignore'

If mode is 'discard_ignores', keep with tags unchanged iff compatible with the filter spec, else discard.

-----------------------------------------------------------------------------------------------

annotated_wavfiles_to_sents_file(+InFile, +OutFile, +Mode)

1. InFile 

Annotated wavfiles file.

2. OutFile

File of records in one of forms

sent(<Transcription>).      [Mode = prolog]
<Transcription>             [Mode = text]

that can be used for EBL training.

Form depends on value of Mode.

3. Mode

One of [prolog, text]

-----------------------------------------------------------------------------------------------

sents_file_to_grammar_probs_data_file(+InFile, +GrammarName, +OutFile)

1. InFile 

Annotated wavfiles file.

2. GrammarName

Prolog atom

3. OutFile

File suitable for use in PFSG training. Each line is of form

<GrammarName> <Transcription>

-----------------------------------------------------------------------------------------------

parse_annotated_wavfiles(+InFile, +WavsFile, +TranscriptionsFile, +SentsFile, +PrologFile)

Do pre-processing of annotated wavfiles file preparatory to batchrec and Alterf training/testing.

1. InFile 

Annotated wavfiles file.

2. WavsFile

File in format

<Wavfile>

needed for Nuance batchrec

3. TranscriptionsFile

File in format

<Wavfile> <Transcription>

needed for Nuance batchrec

4. SentsFile

File in format

sent(<TranscriptionAtom>, <SemAtomList>).

used by Alterf

5. PrologFile

File in format

wavfile_and_annotations(<Wavfile>, <SemAtomList>, <TranscriptionList>)

used by Alterf

-----------------------------------------------------------------------------------------------

parse_annotated_wavfile_line(+Line, -ParsedLine)

1. Line

Prolog string representing a line in an Alterf-tagged file.

2. ParsedLine

Term of form either

- batchrec_command(Command)
  with Command an atom

- annotated_wavfile_line(Wavfile, Annotations, Transcription)
  with
  - Wavfile an atom
  - Annotations a list of Alterf tags
  - Transcription a list of atoms representing words

-----------------------------------------------------------------------------------------------

split_annotated_wavfiles_into_training_and_test(+InFile, +TrainingFile, +TestFile, +ProportionTest)

1. InFile 

Annotated wavfiles file.

2. TrainingFile 

Annotated wavfiles file.

3. TestFile 

Annotated wavfiles file.

4. ProportionTest

Real number between 0 and 1. Proportion of data to put in TestFile.

-----------------------------------------------------------------------------------------------

check_annotated_wavfiles(+InFile, +OutFile, +ErrorFile)

Interactive routine for hand-checking contents of annotated wavfiles file. On each record,
the wavfile is played and the transcription shown to the human annotator, who has to
approve/disapprove. Correct records are sent to OutFile, incorrect to ErrorFile.

1. InFile 

Annotated wavfiles file.

2. OutFile 

Annotated wavfiles file.

3. ErrorFile 

Annotated wavfiles file.

*/



filter_annotated_wavfiles(InFile, FilterFile, OutFile) :-
	filter_annotated_wavfiles(InFile, FilterFile, OutFile, discard_ignores).

filter_annotated_wavfiles(InFile, FilterFile, OutFile, Mode) :-
	read_filter_file(FilterFile, FilterSpec),
	present_filter_spec(FilterSpec),

	absolute_file_name(InFile, AbsInFile),
	absolute_file_name(OutFile, AbsOutFile),
	format('~N~nFiltering annotated wavfiles file ~w~n', [AbsInFile]),

	open(InFile, read, SIn),
	open(OutFile, write, SOut),

	filter_annotated_wavfiles_stream(SIn, SOut, FilterSpec, Mode, 0-Count, 0-KeptCount),

	close(SIn),
	close(SOut),
	format('~NOutput written to ~w~n', [AbsOutFile]),
	format('~N~d records, of which ~d kept after filtering~n', [Count, KeptCount]),
	!.
filter_annotated_wavfiles(InFile, FilterFile, _OutFile, _Mode) :-
	format('~N*** Error: unable to filter ~w using filter spec file ~w~n', [InFile, FilterFile]),
	fail.

present_filter_spec(FilterSpec) :-
	format('~NFilter spec: ~w~n', [FilterSpec]).

read_filter_file(InFile, FilterSpec) :-
	absolute_file_name(InFile, AbsInFile),
	format('~N~nReading filtering spec from file ~w~n', [AbsInFile]),
	read_file_to_atom_list(AbsInFile, FilterFileAtomList),
	atom_list_to_filter_spec(FilterFileAtomList, FilterSpec),
	!.
read_filter_file(InFile, _FilterSpec) :-
	format('~N*** Error: unable to parse filter spec file ~w~n', [InFile]),
	fail.

atom_list_to_filter_spec(FilterFileAtomList, FilterSpec) :-
	atom_list_to_filter_spec1(FilterFileAtomList, FilterFileAtomList1),
	findall( BadTag, ( member(bad_tags(BadTags0), FilterFileAtomList1), member(BadTag, BadTags0) ), BadTags),
	findall( BadWord, ( member(bad_words(BadWords0), FilterFileAtomList1), member(BadWord, BadWords0) ), BadWords),
	FilterSpec = [bad_tags=BadTags, bad_words=BadWords].

atom_list_to_filter_spec1([], []).
atom_list_to_filter_spec1([F | R], [F1 | R1]) :-
	parse_filter_spec_line(F, F1),
	atom_list_to_filter_spec1(R, R1).

parse_filter_spec_line(Atom, Result) :-
	split_atom_into_words(Atom, Words),
	(   Words = [] ->
	    Result = null ;

	    Words = [ignore_examples_including_tags | BadTags] ->
	    Result = bad_tags(BadTags) ;

	    Words = [ignore_examples_including_words | BadWords] ->
	    Result = bad_words(BadWords)
	),
	!.
parse_filter_spec_line(Atom, _Result) :-
	format('~N*** Error: bad line in filter spec file: "~w"~n', [Atom]),
	fail.

filter_annotated_wavfiles_stream(SIn, SOut, FilterSpec, Mode, CountIn-CountOut, KeptCountIn-KeptCountOut) :-
	getline(SIn, Line),
	filter_annotated_wavfiles_stream1(Line, SIn, SOut, FilterSpec, Mode, CountIn-CountOut, KeptCountIn-KeptCountOut).

filter_annotated_wavfiles_stream1(Line, _SIn, _SOut, _FilterSpec, _Mode, CountIn-CountIn, KeptCountIn-KeptCountIn) :-
	Line = eof,
	!.
filter_annotated_wavfiles_stream1(Line, SIn, SOut, FilterSpec, Mode, CountIn-CountOut, KeptCountIn-KeptCountOut) :-
	atom_chars(LineAtom, Line),
	CountNext is CountIn + 1,
	(   parse_annotated_wavfile_line(Line, ParsedLine) ->
	    filter_annotated_wavfiles_stream2(ParsedLine, LineAtom, SOut, FilterSpec, Mode, KeptCountIn-KeptCountNext) ;
	    format('~N*** Error: unable to parse line ~w in annotated wavfile~n', [LineAtom]),
	    fail
	),
	!,
	filter_annotated_wavfiles_stream(SIn, SOut, FilterSpec, Mode, CountNext-CountOut, KeptCountNext-KeptCountOut).

filter_annotated_wavfiles_stream2(ParsedLine, _LineAtom, _SOut, _FilterSpec, _Mode, KeptCountIn-KeptCountIn) :-
	ParsedLine = null,
	!.
filter_annotated_wavfiles_stream2(ParsedLine, _LineAtom, SOut, _FilterSpec, _Mode, KeptCountIn-KeptCountOut) :-
	ParsedLine = batchrec_command(Command),
	format(SOut, '~N~w~n', [Command]),
	KeptCountOut is KeptCountIn + 1,
	!.
filter_annotated_wavfiles_stream2(ParsedLine, _LineAtom, SOut, FilterSpec, Mode, KeptCountIn-KeptCountOut) :-
	ParsedLine = annotated_wavfile_line(Wavfile, Annotations, Transcription),
	(   annotations_and_transcription_compatible_with_filter_spec(Annotations, Transcription, FilterSpec, Mode, Annotations1) ->

	    KeptCountOut is KeptCountIn + 1,
	    coerce_list_elements_to_atoms_if_necessary(Annotations1, Annotations2),
	    join_with_spaces(Annotations2, AnnotationsAtom),
	    join_with_spaces(Transcription, TranscriptionAtom),
	    format(SOut, '~N~w | ~w | ~w~n', [Wavfile, AnnotationsAtom, TranscriptionAtom]) ;
	    
	    KeptCountOut is KeptCountIn
	),
	!.
filter_annotated_wavfiles_stream2(ParsedLine, LineAtom, SOut, FilterSpec, Mode, KeptCounts) :-
	format('~N*** Error: bad call: ~w~n', [filter_annotated_wavfiles_stream2(ParsedLine, LineAtom, SOut, FilterSpec, Mode, KeptCounts)]),
	fail.

% If mode is 'keep_ignores', and the tags are compatible with the filter spec, keep them
annotations_and_transcription_compatible_with_filter_spec(Annotations, Transcription, FilterSpec, keep_ignores, Annotations) :-
	FilterSpec = [bad_tags=BadTags, bad_words=BadWords],
	\+ lists_share_atom(Annotations, BadTags),
	\+ lists_share_atom(Transcription, BadWords),
	!.
% If mode is 'keep_ignores', and the tags are not compatible with the filter spec, replace them with 'ignore'
annotations_and_transcription_compatible_with_filter_spec(_Annotations, _Transcription, _FilterSpec, keep_ignores, Annotations1) :-
	Annotations1 = [ignore],
	!.
% If mode is 'discard_ignores', keep with tags unchanged iff compatible with the filter spec, else discard.
annotations_and_transcription_compatible_with_filter_spec(Annotations, Transcription, FilterSpec, discard_ignores, Annotations) :-
	FilterSpec = [bad_tags=BadTags, bad_words=BadWords],
	!,
	\+ lists_share_atom(Annotations, BadTags),
	\+ lists_share_atom(Transcription, BadWords).
annotations_and_transcription_compatible_with_filter_spec(Annotations, Transcription, FilterSpec, Mode, Annotations1) :-
	format('~N*** Error: bad call: ~w~n', [annotations_and_transcription_compatible_with_filter_spec(Annotations, Transcription, FilterSpec, Mode, Annotations1)]),
	fail.

%--------------------------------------------------------------------------------------------

check_annotated_wavfiles(InFile, OutFile, ErrorFile) :-
	statistics(runtime, [StartTime, _]),

	absolute_file_name(InFile, AbsInFile),
	absolute_file_name(OutFile, AbsOutFile),
	absolute_file_name(ErrorFile, AbsErrorFile),
	format('~N~nChecking annotated wavfiles file ~w~n', [AbsInFile]),

	open(AbsInFile, read, SIn),

	check_annotated_wavfiles_stream(SIn, OutFile, ErrorFile, 0-Count, 0-KeptCount),

	close(SIn),
	format('~N~n--------------------------------------------------------------------~n', []),
	format('~N~nOutput written to ~w (correct) and ~w (incorrect)~n', [AbsOutFile, AbsErrorFile]),
	format('~N~d records, of which ~d kept after checking~n', [Count, KeptCount]),

	statistics(runtime, [EndTime, _]),
	% Time returned is in milliseconds.
	TimeTaken is ( float(EndTime) - float(StartTime) ) / 1000.0,
	(   Count > 0 ->
	    TimePerRecord is TimeTaken / Count ;
	    TimePerRecord = 0.0
	),
	format('~NElapsed time = ~1f seconds (~1f seconds/record)~n', [TimeTaken, TimePerRecord]),
	!.
check_annotated_wavfiles(InFile, OutFile, ErrorFile) :-
	format('~N*** Error: bad call: ~w~n', [check_annotated_wavfiles(InFile, OutFile, ErrorFile)]),
	fail.

check_annotated_wavfiles_stream(SIn, OutFile, ErrorFile, CountIn-CountOut, KeptCountIn-KeptCountOut) :-
	getline(SIn, Line),
	check_annotated_wavfiles_stream1(Line, SIn, OutFile, ErrorFile, CountIn-CountOut, KeptCountIn-KeptCountOut).

check_annotated_wavfiles_stream1(Line, _SIn, _OutFile, _ErrorFile, CountIn-CountIn, KeptCountIn-KeptCountIn) :-
	Line = eof,
	!.
check_annotated_wavfiles_stream1(Line, SIn, OutFile, ErrorFile, CountIn-CountOut, KeptCountIn-KeptCountOut) :-
	atom_chars(LineAtom, Line),
	CountNext is CountIn + 1,
	(   parse_annotated_wavfile_line(Line, ParsedLine) ->
	    check_annotated_wavfiles_stream2(ParsedLine, LineAtom, OutFile, ErrorFile, CountNext, KeptCountIn-KeptCountNext) ;
	    format('~N*** Error: unable to parse line ~w in annotated wavfile~n', [LineAtom]),
	    fail
	),
	!,
	check_annotated_wavfiles_stream(SIn, OutFile, ErrorFile, CountNext-CountOut, KeptCountNext-KeptCountOut).

check_annotated_wavfiles_stream2(ParsedLine, _LineAtom, _OutFile, _ErrorFile, _Count, KeptCountIn-KeptCountIn) :-
	ParsedLine = null,
	!.
check_annotated_wavfiles_stream2(ParsedLine, _LineAtom, OutFile, ErrorFile, Count, KeptCountIn-KeptCountOut) :-
	ParsedLine = annotated_wavfile_line(Wavfile, Annotations, Transcription),
	coerce_list_elements_to_atoms_if_necessary(Annotations, Annotations2),
	join_with_spaces(Annotations2, AnnotationsAtom),
	join_with_spaces(Transcription, TranscriptionAtom),

	(   wavfile_and_tags_confirmed(Count, Wavfile, AnnotationsAtom, TranscriptionAtom) ->

	    KeptCountOut is KeptCountIn + 1,
	    AppropriateFile = OutFile ;
	    
	    KeptCountOut is KeptCountIn,
	    AppropriateFile = ErrorFile
 	    
	),
	open(AppropriateFile, append, S),
	format(S, '~N~w | ~w | ~w~n', [Wavfile, AnnotationsAtom, TranscriptionAtom]),
	close(S),
	!.
check_annotated_wavfiles_stream2(ParsedLine, LineAtom, OutFile, ErrorFile, Count, KeptCounts) :-
	format('~N*** Error: bad call: ~w~n', [check_annotated_wavfiles_stream2(ParsedLine, LineAtom, OutFile, ErrorFile, Count, KeptCounts)]),
	fail.

wavfile_and_tags_confirmed(Count, Wavfile, Annotations, Transcription) :-
	absolute_file_name(Wavfile, Wavfile1),
	(   file_exists(Wavfile1) ->
	    true ;
	    format('~N*** Error: file does not exist: ~w~n', [Wavfile1]),
	    fail
	),	
	system_on_list([playwav, Wavfile1, '>', 'tmp_output.txt']),
	format('~N~n~n------------------------------------~n', []),
	format('~NRecord number ~d~n', [Count]),
	format('~NTranscription: ~w~n', [Transcription]),
	format('~N  Annotations: ~w~n', [Annotations]),
	format('~N~nOK? (y, n, r) ', []),
	getline(user, Line),
	atom_chars(Atom, Line),
	!,
	wavfile_and_tags_confirmed1(Atom, Count, Wavfile, Annotations, Transcription).
wavfile_and_tags_confirmed(Count, Wavfile, Annotations, Transcription) :-
	format('~N*** Error: bad call: ~w~n', [wavfile_and_tags_confirmed(Count, Wavfile, Annotations, Transcription)]),
	fail.

wavfile_and_tags_confirmed1(y, _Count, _Wavfile, _Annotations, _Transcription) :-
	!.
wavfile_and_tags_confirmed1(r, Count, Wavfile, Annotations, Transcription) :-
	!,
	wavfile_and_tags_confirmed(Count, Wavfile, Annotations, Transcription).
wavfile_and_tags_confirmed1(Other, Count, Wavfile, Annotations, Transcription) :-
	Other \== n,
	format('~NPlease answer "y" (yes), "n" (no), "r" (repeat)~n', []),
	!,
	wavfile_and_tags_confirmed(Count, Wavfile, Annotations, Transcription).
	
%--------------------------------------------------------------------------------------------

annotated_wavfiles_to_sents_file(InFile, OutFile) :-
	annotated_wavfiles_to_sents_file(InFile, OutFile, prolog).

annotated_wavfiles_to_sents_file(InFile, OutFile, Format) :-
	\+ member(Format, [prolog, text]),
	!,
	format('~N*** Error: bad call: ~w. Last argument must be "prolog" or "text".~n', [annotated_wavfiles_to_sents_file(InFile, OutFile, Format)]),
	fail.
annotated_wavfiles_to_sents_file(InFile, OutFile, Format) :-
	absolute_file_name(InFile, AbsInFile),
	absolute_file_name(OutFile, AbsOutFile),
	format('~N~nExtracting sents file from ~w~n', [AbsInFile]),

	open(InFile, read, SIn),
	open(OutFile, write, SOut),

	annotated_wavfiles_to_sents_file_stream(SIn, SOut, Format, 0-Count),

	close(SIn),
	close(SOut),
	format('~NOutput written to ~w (~d records)~n~n', [AbsOutFile, Count]).

annotated_wavfiles_to_sents_file_stream(SIn, SOut, Format, CountIn-CountOut) :-
	getline(SIn, Line),
	annotated_wavfiles_to_sents_file_stream1(Line, SIn, SOut, Format, CountIn-CountOut).

annotated_wavfiles_to_sents_file_stream1(Line, _SIn, _SOut, _Format, Count-Count) :-
	Line = eof,
	!.
annotated_wavfiles_to_sents_file_stream1(Line, SIn, SOut, Format, CountIn-CountOut) :-
	atom_chars(LineAtom, Line),
	(   parse_annotated_wavfile_line(Line, ParsedLine) ->
	    annotated_wavfiles_to_sents_file_stream2(ParsedLine, LineAtom, SOut, Format, CountIn-CountNext) ;
	    format('~N*** Error: unable to parse line ~w in annotated wavfile~n', [LineAtom]),
	    fail
	),
	!,
	annotated_wavfiles_to_sents_file_stream(SIn, SOut, Format, CountNext-CountOut).

annotated_wavfiles_to_sents_file_stream2(ParsedLine, _Line, _SOut, _Format, Count-Count) :-
	ParsedLine = null,
	!.
annotated_wavfiles_to_sents_file_stream2(ParsedLine, _Line, _SOut, _Format, Count-Count) :-
	ParsedLine = batchrec_command(_Command),
	!.
annotated_wavfiles_to_sents_file_stream2(ParsedLine, _Line, _SOut, _Format, CountIn-CountOut) :-
	ParsedLine = annotated_wavfile_line(_Wavfile, AnnotationsOS, Transcription),
	(   Transcription = [] ;
	    AnnotationsOS = [ignore]
	),
	CountOut is CountIn,
	!.
annotated_wavfiles_to_sents_file_stream2(ParsedLine, _Line, SOut, Format, CountIn-CountOut) :-
	ParsedLine = annotated_wavfile_line(_Wavfile, AnnotationsOS, Transcription),
	join_with_spaces(Transcription, TranscriptionAtom),
	(   Format = prolog ->
	    
	    alterf_tags_to_grammar_ids_if_possible(AnnotationsOS, GrammarIds),
	    format(SOut, '~N~q.~n', [sent(TranscriptionAtom, GrammarIds)]) ;
	    
	    format(SOut, '~N~w~n', [TranscriptionAtom])
	),
	CountOut is CountIn + 1,
	!.
annotated_wavfiles_to_sents_file_stream2(_ParsedLine, Line, _STraining, _STest, _ProportionTest) :-
	format('~N*** Error: unable to process line ~w in annotated wavfile~n', [Line]),
	fail.

alterf_tags_to_grammar_ids_if_possible(AlterfTags, GrammarIds) :-
	current_predicate(user:alterf_tags_to_grammar_id/2),
	findall(GrammarId, user:alterf_tags_to_grammar_id(AlterfTags, GrammarId), GrammarIds0),
	sort(GrammarIds0, GrammarIds),
	!.
alterf_tags_to_grammar_ids_if_possible(AlterfTags, AlterfTags).

%--------------------------------------------------------------------------------------------

sents_file_to_grammar_probs_data_file(InFile, BaseGrammarName, OutFile) :-
	absolute_file_name(InFile, AbsInFile),
	absolute_file_name(OutFile, AbsOutFile),
	format('~N~nConverting sents file ~w into grammar probs data file~n', [AbsInFile]),

	open(AbsInFile, read, SIn),
	open(AbsOutFile, write, SOut),

	sents_file_to_grammar_probs_data_stream(SIn, BaseGrammarName, SOut, 0-Count),

	close(SIn),
	close(SOut),
	format('~N~nGrammar probs data file ~w written (~d records)~n', [AbsOutFile, Count]),
	!.
sents_file_to_grammar_probs_data_file(InFile, BaseGrammarName, OutFile) :-
	format('~N*** Error: bad call: ~w~n',
	       [sents_file_to_grammar_probs_data_file(InFile, BaseGrammarName, OutFile)]),
	fail.

sents_file_to_grammar_probs_data_stream(SIn, BaseGrammarName, SOut, CIn-COut) :-
	read(SIn, T),
	sents_file_to_grammar_probs_data_stream1(T, SIn, BaseGrammarName, SOut, CIn-COut).

sents_file_to_grammar_probs_data_stream1(end_of_file, _SIn, _BaseGrammarName, _SOut, CIn-CIn) :-
	!.
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

%--------------------------------------------------------------------------------------------

split_annotated_wavfiles_into_training_and_test(InFile, TrainingFile, TestFile, ProportionTest) :-
	open(InFile, read, SIn),
	open(TrainingFile, write, STraining),
	open(TestFile, write, STest),

	split_annotated_wavfiles_into_training_and_test_stream(SIn, STraining, STest, ProportionTest),

	close(SIn),
	close(STraining),
	close(STest).

split_annotated_wavfiles_into_training_and_test_stream(SIn, STraining, STest, ProportionTest) :-
	getline(SIn, Line),
	split_annotated_wavfiles_into_training_and_test_stream1(Line, SIn, STraining, STest, ProportionTest).

split_annotated_wavfiles_into_training_and_test_stream1(Line, _SIn, _STraining, _STest, _ProportionTest) :-
	Line = eof,
	!.
split_annotated_wavfiles_into_training_and_test_stream1(Line, SIn, STraining, STest, ProportionTest) :-
	atom_chars(LineAtom, Line),
	(   parse_annotated_wavfile_line(Line, ParsedLine) ->
	    split_annotated_wavfiles_into_training_and_test_stream2(ParsedLine, LineAtom, STraining, STest, ProportionTest) ;
	    format('~N*** Error: unable to parse line ~w in annotated wavfile~n', [LineAtom]),
	    fail
	),
	!,
	split_annotated_wavfiles_into_training_and_test_stream(SIn, STraining, STest, ProportionTest).

split_annotated_wavfiles_into_training_and_test_stream2(ParsedLine, _Line, _STraining, _STest, _ProportionTest) :-
	ParsedLine = null,
	!.
split_annotated_wavfiles_into_training_and_test_stream2(ParsedLine, Line, STraining, STest, _ProportionTest) :-
	ParsedLine = batchrec_command(_Command),
	!,
	format(STraining, '~N~w~n', [Line]),
	format(STest, '~N~w~n', [Line]).
split_annotated_wavfiles_into_training_and_test_stream2(ParsedLine, Line, STraining, STest, ProportionTest) :-
	ParsedLine = annotated_wavfile_line(_Wavfile, _AnnotationsOS, _Transcription),
	!,
	random(RandomNumber),
	(   RandomNumber < ProportionTest ->
	    format(STest, '~N~w~n', [Line]) ;
	    format(STraining, '~N~w~n', [Line])
	).
split_annotated_wavfiles_into_training_and_test_stream2(_ParsedLine, Line, _STraining, _STest, _ProportionTest) :-
	format('~N*** Error: unable to classify line ~w in annotated wavfile~n', [Line]),
	fail.

%--------------------------------------------------------------------------------------------

parse_annotated_wavfiles(InFile, WavsFile, TranscriptionsFile, SentsFile, PrologFile) :-
	open(InFile, read, SIn),
	open(WavsFile, write, SOutWavs),
	open(TranscriptionsFile, write, SOutTranscriptions),
	open(SentsFile, write, SOutSents),
	open(PrologFile, write, SOutProlog),
	parse_annotated_wavfiles_stream([SIn, SOutWavs, SOutTranscriptions, SOutSents, SOutProlog], 0),
	close(SIn),
	close(SOutWavs),
	close(SOutTranscriptions),
	close(SOutSents),
	close(SOutProlog).

parse_annotated_wavfiles_stream(Streams, Counter) :-
	Streams = [SIn | _OutStreams],
	getline(SIn, Line),
	Counter1 is Counter + 1,
	Mod is Counter1 mod 500,
	(   Mod = 0 ->
	    format('~d ', [Counter1]), 
	    flush_output(user) ;
	    true
	),
	parse_annotated_wavfiles_stream1(Line, Streams, Counter1).

parse_annotated_wavfiles_stream1(Line, _Streams, _Counter) :-
	Line = eof,
	!.
parse_annotated_wavfiles_stream1(Line, [SIn | OutStreams], Counter) :-
	parse_annotated_wavfiles_line(Line, OutStreams),
	!,
	parse_annotated_wavfiles_stream([SIn | OutStreams], Counter).

%--------------------------------------------------------------------------------------------

parse_annotated_wavfiles_line(Line, OutStreams) :-
	parse_annotated_wavfile_line(Line, ParsedLine),
	parse_annotated_wavfiles_line1(ParsedLine, OutStreams),
	!.
parse_annotated_wavfiles_line(Line, OutStreams) :-
	format('N*** Error: bad call: ~q~n', [parse_annotated_wavfiles_line(Line, OutStreams)]),
	fail.

parse_annotated_wavfiles_line1(null, _OutStreams) :-
	!.
parse_annotated_wavfiles_line1(batchrec_command(BatchrecCommand), [SOutWavs, _SOutTranscriptions, _SOutSents, _SOutProlog]) :-
	format(SOutWavs, '~N~w~n', [BatchrecCommand]),
	!.
parse_annotated_wavfiles_line1(annotated_wavfile_line(Wavfile, AnnotationsOS, Transcription), OutStreams) :-
	OutStreams = [SOutWavs, SOutTranscriptions, SOutSents, SOutProlog],
	format(SOutWavs, '~N~w~n', [Wavfile]),
	(   Transcription = [] ->
	    format(SOutTranscriptions, '~N~w~n', [Wavfile]) ;
	    join_with_spaces(Transcription, TranscriptionAtom),
	    format(SOutTranscriptions, '~N~w ~w~n', [Wavfile, TranscriptionAtom]),
	    format(SOutSents, '~N~q.~n', [sent(TranscriptionAtom, AnnotationsOS)])
	),
	format(SOutProlog, '~N~q.~n', [wavfile_and_annotations(Wavfile, AnnotationsOS, Transcription)]),
	!.

%--------------------------------------------------------------------------------------------

parse_annotated_wavfile_line(Line, ParsedLine) :-
	Line = [],
	!,
	ParsedLine = null.
parse_annotated_wavfile_line(Line, ParsedLine) :-
	Line = [0'# | _Rest],
	!,
	ParsedLine = null.
parse_annotated_wavfile_line(Line, ParsedLine) :-
	split_string_into_words(Line, 0'#, PreAndPostCommentParts),
	(   PreAndPostCommentParts = [] ->
	    ParsedLine = null ;
	    PreAndPostCommentParts = [PreCommentPart | _Rest],
	    parse_annotated_wavfile_line1(PreCommentPart, ParsedLine)
	),
	!.
parse_annotated_wavfile_line(Line, _ParsedLine) :-
	format('N*** Error: bad line in call to parse_annotated_wavfile_line: "~s"~n', [Line]),
	fail.

parse_annotated_wavfile_line1(Atom, batchrec_command(Atom)) :-
	looks_like_batchrec_command(Atom),
	!.
parse_annotated_wavfile_line1(LineAtom, annotated_wavfile_line(AbsWavfile, AnnotationsOS, Transcription)) :-
	split_atom_into_words(LineAtom, 0'|, BarComponents),
	(   BarComponents = [WavfileAtom, AnnotationsAtom, TranscriptionAtom, _Comments] ->
	    split_atom_into_words(TranscriptionAtom, Transcription) ;
	    BarComponents = [WavfileAtom, AnnotationsAtom, TranscriptionAtom] ->
	    split_atom_into_words(TranscriptionAtom, Transcription) ;
	    BarComponents = [WavfileAtom, AnnotationsAtom] ->
	    Transcription = [] 
	),
	split_atom_into_words(WavfileAtom, [Wavfile]),
	absolute_file_name(Wavfile, AbsWavfile),
	split_atom_into_words(AnnotationsAtom, Annotations),
	coerce_list_elements_to_numbers_if_possible(Annotations, Annotations1),
	list_to_ord_set(Annotations1, AnnotationsOS),
	!.

%--------------------------------------------------------------------------------------------

looks_like_batchrec_command(BatchrecCommandAtom) :-
	atom(BatchrecCommandAtom),
	atom_chars(BatchrecCommandAtom, Chars),
	Chars = [0'* | _Rest],
	!.

lists_share_atom(L1, L2) :-
	member(X, L1),
	member(X, L2),
	!.
