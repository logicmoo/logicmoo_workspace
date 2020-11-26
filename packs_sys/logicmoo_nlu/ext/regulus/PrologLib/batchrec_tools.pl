
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%---------------------------------------------------------------

:- module(batchrec_tools,
	  [do_batchrec/6,
	   do_text_batchrec/4,
	   directory_to_rough_transcriptions_file/4,
	   add_rough_transcriptions_to_files_in_directory/4,
	   batchrec_transcribed_files_in_directory/3,
	   wavconvert_wavfiles_in_directory/4,
	   parse_transcriptions_file_to_list/2,
	   transcriptions_file_to_wavfiles_file/2,
	   transcriptions_file_to_sents_file/2,
	   compare_batchrec_trace_files/4,
	   compare_batchrec_trace_files/3,
	   add_transcriptions_to_prolog_batchrec_file/3,

	   slot_value_list/3
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/utilities').

%:- use_module(library(system)).
'SICSTUS3/4'( ( :- use_module(library(system)) ),
	      ( :- use_module(library(system3), [shell/1, shell/2, exec/3, environ/2, host_id/1, host_name/1, mktemp/2, system/1, system/2, working_directory/2] ) ) ).

'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).
:- use_module(library(lists)).
%:- use_module(library(assoc)).

%---------------------------------------------------------------

/*

do_batchrec(+Wavfiles, +Transcriptions, +RecParams, +BatchrecTraceFile, +PrologFile, +TmpDirectory)

Run Nuance batchrec and convert results into Prolog-readable form. 
See the Nuance documentation for 'batchrec'. Arguments:

   Wavfiles           The file containing the names of the wavfiles to batchrec (Prolog term)
   Transcriptions     The file containing transcriptions of the wavfiles to batchrec (Prolog term)
   RecParams          Nuance params to use (list of Prolog atoms)
   BatchrecTraceFile  File to write raw batchrec output to
   PrologFile         File to write Prolog-readable version of batchrec output to
   TmpDirectory       Directory to hold temporary files.
   
do_text_batchrec(+Transcriptions, +RecParams, +PrologFile, +TmpDirectory)

Run Nuance nl-tool and convert results into Prolog-readable form. 
See the Nuance documentation for 'nl-tool'. Arguments:

   Transcriptions     The file containing transcriptions of the sentences to run nl-tool on (Prolog term)
   RecParams          Nuance params to use (list of Prolog atoms). These will be irrelevant except for "-package"
   PrologFile         File to write Prolog-readable version of nl-tool output to
   TmpDirectory       Directory to hold temporary files.

The formats of items in the Prolog-readable files produced by both predicates are identical, as follows:

batchrec_item([
wavfile=<Wavfile>,
words=<Words>,
confidence=<ConfidenceScore>,
nl_value=<NLVal>
]). 

e.g.

batchrec_item([
wavfile='C:\\home\\speech\\Corpora\\wavfiles\\checklist\\bah_082902\\utt20.wav',
words=[show,me,the,small,waste,water,bag],
confidence=72,
nl_value=[[imp,[[tense,imperative],[subj,[[spec,pro],[head,you]]],[verb_type,ditrans],[verb,show],[indobj,[[spec,pro],[head,i]]],[obj,[[spec,the_sing],[adj,small],[nn,waste_water],[head,bag]]]]]]
]). %C:\home\speech\Corpora\wavfiles\checklist\bah_082902\utt20.wav

---------------------------------------------------------------

directory_to_rough_transcriptions_file(+Directory, +TranscriptionsFile, +RecParams, +TmpDir)

Finds all the wavfiles in a directory, and uses batchrec, with the parameters specified in RecParams,
to create a rough version of TranscriptionsFile.

   - Directory is the pathname to a directory, formatted as a Prolog atom

   - TranscriptionsFile is the pathname of the target file

   - RecParams is the recognition parameters, formatted as in do_batchrec/6 above

   - TmpDir is a temporary directory, as in do_batchrec/6 above

---------------------------------------------------------------

add_rough_transcriptions_to_files_in_directory(+Directory, +RecParams, +TmpDir, +AllOrWithoutTranscription)

Finds all the wavfiles in a directory, and uses batchrec, with the parameters specified in RecParams,
to add rough versions of transcriptions to all these files

   - Directory is the pathname to a directory, formatted as a Prolog atom

   - RecParams is the recognition parameters, formatted as in do_batchrec/6 above

   - TmpDir is a temporary directory, as in do_batchrec/6 above

   - AllOrWithoutTranscription is either 'all' (all wavfiles) or 'without_current_transcription' (only files with no current transcription)

---------------------------------------------------------------
   
wavconvert_wavfiles_in_directory(+Directory, +TmpDir, +Format, +Encoding)

Finds all the wavfiles in a directory, and uses wavconvert to change the format and encoding.

---------------------------------------------------------------

batchrec_transcribed_files_in_directory(+Directory, +RecParams, +TmpDir)

Extracts transcriptions from wavfiles in directory and then runs batchrec on them,
with the parameters specified in RecParams

   - Directory is the pathname to a directory, formatted as a Prolog atom

   - RecParams is the recognition parameters, formatted as in do_batchrec/6 above

   - TmpDir is a temporary directory, as in do_batchrec/6 above

---------------------------------------------------------------

parse_transcriptions_file_to_list(+TranscriptionsFile, -List)

    - TranscriptionsFile is a file of wavfiles and associated transcriptions in Nuance batchrec form, i.e.

    <Wavfile> <Transcription>

    - List is a list representation of the same file. Each element is of the form

    transcription(Wavfile, Transcription)

    where Wavfile is an atom and Transcription is a list of atoms.

---------------------------------------------------------------

transcriptions_file_to_sents_file(+TranscriptionsFile, +SentsFile)

    - TranscriptionsFile is a file of wavfiles and associated transcriptions in Nuance batchrec form, i.e.

    <Wavfile> <Transcription>

    - SentsFile is a file with just the transcriptions in "sents file" format, i.e.

    sent(Transcription)

    where Transcription is an atom.

%---------------------------------------------------------------

compare_batchrec_trace_files(+TraceFile1, +TraceFile2, +ComparisonFile, -McNemarResults)

    - TraceFile1 and TraceFile2 are Prolog-formatted batchrec trace files including transcriptions
    - ComparisonFile is Prolog-readable file giving a structured diff
    - McNemarResults is a list of the form [Good, Bad, Signif], where
      # Good is the number of examples where File1 gives a completely correct result and File2 doesn't.
      # Bad is the number of examples where File2 gives a completely correct result and File1 doesn't.
      # Significance is the significance of the difference according to the McNemar statistic,
        as one of the following: {not_significant, 0.05, 0.01, 0.001}. 

%---------------------------------------------------------------

compare_batchrec_trace_files(+TraceFile1, +TraceFile2, +ComparisonFile)

    As above, but discards the McNemar results.

*/

do_batchrec(Wavfiles0, Transcriptions0, RecParams, BatchrecTraceFile0, PrologFile0, TmpDirectory) :-
	
	(   consume_parameter_value_pair(RecParams, package=Package0, RecParams1) ->
	    true ;
	    format('N*** Error: bad call: ~q~n. Must specify package.~n', [do_batchrec(Wavfiles0, Transcriptions0, RecParams, PrologFile)]),
	    fail
	),

	(   consume_parameter_value_pair(RecParams1, grammar=GrammarName, RecParams2) ->
	    true ;
	    format('N*** Error: bad call: ~q~n. Must specify grammar.~n', [do_batchrec(Wavfiles0, Transcriptions0, RecParams, PrologFile)]),
	    fail
	),

	existing_absolute_file_name(Wavfiles0, Wavfiles),
	existing_absolute_file_name(Transcriptions0, Transcriptions),
	absolute_file_name(Package0, Package),
	safe_is_directory(Package),
	
	absolute_file_name(BatchrecTraceFile0, BatchrecTraceFile),
	absolute_file_name(PrologFile0, PrologFile),

	make_batchrec_tmp_file(abs_wavfile_transcriptions, TmpDirectory, AbsWavfileTranscriptions),
	make_batchrec_tmp_file(abs_wavfiles, TmpDirectory, AbsWavfile),
		
	make_wavfiles_absolute_in_transcriptions_file(Transcriptions, AbsWavfileTranscriptions),
	make_wavfiles_absolute_in_transcriptions_file(Wavfiles, AbsWavfile),

	parameter_list_to_string(RecParams2, ParameterValueString),
        (      environ('OS','Windows_NT') ->
               WriteToFile='2>'
	;
               WriteToFile='>&'
        ),
	join_with_spaces(['batchrec', '-package', Package, '-testset', AbsWavfile, '-transcriptions', AbsWavfileTranscriptions, '-grammar', GrammarName, 'rec.ConfidenceRejectionThreshold=0', ParameterValueString, WriteToFile, BatchrecTraceFile], CommandString),
	format('~N-- Starting batchrec run~n', []),
	format('~N  -- Executing command: ~w~n', [CommandString]),
	(   shell(CommandString) ->
	    format('~N-- Batchrec run finished normally~n', [])
	;
	    ( file_exists(BatchrecTraceFile), age_of_file(BatchrecTraceFile, Age), Age < 5 ) ->
	    format('~N*** Warning: call to shell/1 failed, but ~w exists, ~d seconds old... continuing~n', [BatchrecTraceFile, Age])
	;
	    otherwise -> 
	    format('~N*** Error: batchrec run failed to complete normally~n', []),
	    fail
	),
	batchrec_trace_to_prolog_form(BatchrecTraceFile, TmpDirectory, PrologFile).

%------------------------------------------------------------------------------------

do_text_batchrec(Transcriptions0, RecParams, PrologFile, TmpDirectory) :-
	
	(   consume_parameter_value_pair(RecParams, package=Package0, RecParams1) ->
	    true ;
	    format('N*** Error: bad call: ~q~n. Must specify package.~n', [do_text_batchrec(Transcriptions0, RecParams, PrologFile)]),
	    fail
	),

	(   consume_parameter_value_pair(RecParams1, grammar=GrammarName, _RecParams2) ->
	    true ;
	    format('N*** Error: bad call: ~q~n. Must specify grammar.~n', [do_text_batchrec(Transcriptions0, RecParams, PrologFile)]),
	    fail
	),

	make_batchrec_tmp_file(bare_transcriptions, TmpDirectory, BareTranscriptions),
	make_batchrec_tmp_file(nl_tool_output, TmpDirectory, NLToolOutput),
	make_batchrec_tmp_file(parsed_nl_tool_raw, TmpDirectory, ParsedNLToolOutputRaw),
	make_batchrec_tmp_file(parsed_nl_tool_output, TmpDirectory, ParsedNLToolOutput),

	existing_absolute_file_name(Transcriptions0, Transcriptions),
	absolute_file_name(Package0, Package),
	safe_is_directory(Package),

	remove_wavfiles_from_transcriptions_in_file(Transcriptions, BareTranscriptions),

	join_with_spaces(['nl-tool', '-package', Package, '-grammar', GrammarName, '<', BareTranscriptions, '>', NLToolOutput], CommandString),
	format('~N-- Starting nl-tool run~n', []),
	format('~N  -- Executing command: ~w~n', [CommandString]),
	shell(CommandString),
	format('~N-- nl-tool run finished~n', []),
	parse_nltool_output_file_to_file(NLToolOutput, ParsedNLToolOutputRaw, ParsedNLToolOutput),
	nlrefs_to_batchrec_compatible_prolog_form(Transcriptions, ParsedNLToolOutput, PrologFile).

%------------------------------------------------------------------------------------

directory_to_rough_transcriptions_file(Directory, TranscriptionsFile, RecParams, TmpDir) :-
	directory_to_rough_transcriptions_file(Directory, TranscriptionsFile, RecParams, TmpDir, all).

directory_to_rough_transcriptions_file(Directory, TranscriptionsFile, RecParams, TmpDir, AllOrWithoutCurrentTranscription) :-
	\+ member(AllOrWithoutCurrentTranscription, [all, without_current_transcription]),
	!,
	format('*** Error: bad call ~w: last arg in directory_to_rough_transcriptions_file/5 must be one of [all, without_current_transcription]',
	       [directory_to_rough_transcriptions_file(Directory, TranscriptionsFile, RecParams, TmpDir, AllOrWithoutCurrentTranscription)]),
	fail.
directory_to_rough_transcriptions_file(Directory, TranscriptionsFile, RecParams, TmpDir, AllOrWithoutCurrentTranscription) :-
	make_batchrec_tmp_file(tmp_wavfiles, TmpDir, WavfilesFile),
	make_batchrec_tmp_file(tmp_batchrec_trace, TmpDir, BatchrecTraceFile),
	make_batchrec_tmp_file(tmp_batchrec_trace_prolog, TmpDir, BatchrecPrologFile),

	format('~N-- Collecting wavfiles from directory ~w~n', [Directory]),
	wavfiles_in_directory(Directory, Wavfiles0),
	(   AllOrWithoutCurrentTranscription = without_current_transcription ->
	    findall(Wavfile,
		    (   member(Wavfile, Wavfiles0),
			\+ get_transcription_from_wavfile(Wavfile, _WavfileTranscription)
		    ),
		    Wavfiles)
	;
	    otherwise ->
	    Wavfiles = Wavfiles0
	),
	wavfile_list_to_wavfiles_file(Wavfiles, WavfilesFile),
	length(Wavfiles, NWavfiles),
	format('~N-- Performing batchrec on ~d wavfiles~n', [NWavfiles]),
	do_batchrec(WavfilesFile, WavfilesFile, RecParams, BatchrecTraceFile, BatchrecPrologFile, TmpDir),
	batchrec_output_file_to_rough_transcriptions_file(BatchrecPrologFile, TranscriptionsFile),
	!.

%------------------------------------------------------------------------------------

add_rough_transcriptions_to_files_in_directory(Directory, RecParams, TmpDir, AllOrWithoutCurrentTranscription) :-
	\+ member(AllOrWithoutCurrentTranscription, [all, without_current_transcription]),
	!,
	format('*** Error: bad call ~w: last arg in add_rough_transcriptions_to_files_in_directory/4 must be one of [all, without_current_transcription]',
	       [add_rough_transcriptions_to_files_in_directory(Directory, RecParams, TmpDir, AllOrWithoutCurrentTranscription)]),
	fail.
add_rough_transcriptions_to_files_in_directory(Directory, RecParams, TmpDir, AllOrWithoutCurrentTranscription) :-
	make_batchrec_tmp_file('tmp_rough_transcriptions.txt', TmpDir, RoughTranscriptionsFile),
	directory_to_rough_transcriptions_file(Directory, RoughTranscriptionsFile, RecParams, TmpDir, AllOrWithoutCurrentTranscription),
	parse_transcriptions_file_to_list(RoughTranscriptionsFile, TranscriptionsList),
	length(TranscriptionsList, NTranscriptions),
	format('~N-- Adding transcriptions to ~d wavfiles~n', [NTranscriptions]),
	add_transcriptions_to_wavfiles_in_transcriptions_list(TranscriptionsList, TmpDir, 0),
	!.

add_transcriptions_to_wavfiles_in_transcriptions_list([], _TmpDir, _N).
add_transcriptions_to_wavfiles_in_transcriptions_list([transcription(Wavfile, TranscriptionWords) | R], TmpDir, I) :-
	join_with_spaces(TranscriptionWords, TranscriptionAtom),
	make_batchrec_tmp_file('tmp.wav', TmpDir, TmpWavfile),
	wavconvert_wavfile_if_necessary(Wavfile, TmpWavfile, sphere, linear),
	(   add_transcription_to_wavfile(Wavfile, TranscriptionAtom, TmpWavfile) ->
	    copy_file(TmpWavfile, Wavfile)
	;
	    otherwise ->
	    format('~N*** Warning: unable to add transcription to wavfile ~w~n', [Wavfile])
	),
	format('.', []),
	(   ( 0 is I mod 100, I > 0 ) ->
	    
	    format(' (~d) ~n', [I]),
	    flush_output(user)
	;
	    true
	),
	I1 is I + 1,
	!,
	add_transcriptions_to_wavfiles_in_transcriptions_list(R, TmpDir, I1).

%------------------------------------------------------------------------------------

wavconvert_wavfiles_in_directory(Directory, TmpDir, Format, Encoding) :-
	make_batchrec_tmp_file('tmp.wav', TmpDir, TmpWavfile),
	wavfiles_in_directory(Directory, Wavfiles),
	wavconvert_wavfiles_in_list(Wavfiles, TmpWavfile, Format, Encoding).

wavconvert_wavfiles_in_list([], _TmpWavfile, _Format, _Encoding).
wavconvert_wavfiles_in_list([F | R], TmpWavfile, Format, Encoding) :-
	wavconvert_wavfile_if_necessary(F, TmpWavfile, Format, Encoding),
	!,
	wavconvert_wavfiles_in_list(R, TmpWavfile, Format, Encoding).

%------------------------------------------------------------------------------------

wavconvert_wavfile_if_necessary(Wavfile, _TmpWavfile, Format, Encoding) :-
	wavfile_format_and_encoding(Wavfile, Format0, Encoding0) ->
	[Format, Encoding] = [Format0, Encoding0],
	!.
wavconvert_wavfile_if_necessary(Wavfile, TmpWavfile, Format, Encoding) :-
	safe_absolute_file_name(Wavfile, AbsWavfile),
	safe_absolute_file_name(TmpWavfile, AbsTmpWavfile),
	wavconvert_file(AbsWavfile, AbsTmpWavfile, Encoding, Format),
	copy_file(TmpWavfile, Wavfile),
	!.
wavconvert_wavfile_if_necessary(Wavfile, _TmpWavfile, Format, Encoding) :-
	format('~N*** Error: unable to convert wavfile to ~w to ~w/~w~n',
	       [Wavfile, Format, Encoding]),
	fail.

%------------------------------------------------------------------------------------

batchrec_transcribed_files_in_directory(Directory, RecParams, TmpDir) :-
	make_batchrec_tmp_file('tmp_wavfiles.txt', TmpDir, WavfilesFile),
	make_batchrec_tmp_file('tmp_transcriptions.txt', TmpDir, TranscriptionsFile),
	make_batchrec_tmp_file('tmp_batchrec_trace.txt', TmpDir, BatchrecTraceFile),
	make_batchrec_tmp_file('tmp_batchrec_output.pl', TmpDir, PrologFile),
	directory_to_wavfiles_and_transcriptions_files(Directory, WavfilesFile, TranscriptionsFile),
	do_batchrec(WavfilesFile, TranscriptionsFile, RecParams, BatchrecTraceFile, PrologFile, TmpDir),
	format('~N--- Done. Prolog-readable batchrec output in ~w~n', [PrologFile]),
	!.

directory_to_wavfiles_and_transcriptions_files(Directory, WavfilesFile, TranscriptionsFile) :-
	wavfiles_in_directory(Directory, Wavfiles),
	wavfile_list_to_wavfiles_file(Wavfiles, WavfilesFile),
	transcribed_wavfile_list_to_transcriptions_file(Wavfiles, TranscriptionsFile),
	!.
	
%------------------------------------------------------------------------------------

compare_batchrec_trace_files(BatchrecTrace1, BatchrecTrace2, ComparisonFile) :-
	compare_batchrec_trace_files(BatchrecTrace1, BatchrecTrace2, ComparisonFile, _McNemarResults).

compare_batchrec_trace_files(BatchrecTrace1, BatchrecTrace2, ComparisonFile, McNemarResults) :-
	absolute_file_name(BatchrecTrace1, AbsBatchrecTrace1),
	absolute_file_name(BatchrecTrace2, AbsBatchrecTrace2),
	absolute_file_name(ComparisonFile, AbsComparisonFile),
	
	prolog_file_to_list(AbsBatchrecTrace1, List1),
	length(List1, N1),
	format('~N--- Read batchrec trace file (~d records) ~w~n',
	       [N1, AbsBatchrecTrace1]),
	
	prolog_file_to_list(AbsBatchrecTrace2, List2),
	length(List2, N2),
	format('~N--- Read batchrec trace file (~d records) ~w~n',
	       [N2, AbsBatchrecTrace2]),

	(   N1 = N2 ->
	    true ;
	    format('~N*** Warning: batchrec trace files have different numbers of records~n')
	),

	compare_batchrec_trace_lists(List1, List2, ComparisonList),
	length(ComparisonList, NDifferent),
	
	write_batchrec_comparison_file(ComparisonList, AbsComparisonFile),
	format('~N--- Written batchrec comparison file (~d records) ~w~n',
	       [NDifferent, AbsComparisonFile]),
	
	summarise_batchrec_comparison_list(ComparisonList, McNemarResults),
	!.

%------------------------------------------------------------------------------------

batchrec_trace_to_prolog_form(BatchrecTraceFile, TmpDirectory, PrologFile) :-
	make_batchrec_tmp_file(raw_prolog_batchrec, TmpDirectory, TmpFile),

	format('~N-- Creating raw Prolog batchrec file ~w... ', [TmpFile]),
	parse_batchrec_file1(BatchrecTraceFile, TmpFile, NRecords1),
	format('done (~d records)~n', [NRecords1]),

	format('~N-- Postprocessing Prolog batchrec file ~w... ', [PrologFile]),
	make_prolog_batchrec_file(TmpFile, PrologFile, NRecords2),
	format('done (~d records)~n', [NRecords2]).

%------------------------------------------------------------------------------------

parse_batchrec_file1(InFile, OutFile, NRecords) :-
	open(InFile, read, SIn),
	open(OutFile, write, SOut),
	%trace,
	parse_batchrec_stream(SIn, SOut, 0-NRecords),
	close(SIn),
	close(SOut).

parse_batchrec_stream(SIn, SOut, CounterIn-CounterOut) :-
	read_line(SIn, Line),
	CounterNext is CounterIn + 1,
	Mod is CounterNext mod 500,
	(   Mod = 0 ->
	    format('~d ', [CounterNext]), 
	    flush_output(user) ;
	    true
	),
	parse_batchrec_stream1(Line, SIn, SOut, CounterNext-CounterOut).

parse_batchrec_stream1(Line, _SIn, _SOut, Counter-Counter) :-
	Line = end_of_file,
	!.
parse_batchrec_stream1(Line, SIn, SOut, CounterIn-CounterOut) :-
	atom_codes(LineAtom, Line),
	parse_batchrec_line(LineAtom, SOut),
	!,
	parse_batchrec_stream(SIn, SOut, CounterIn-CounterOut).

parse_batchrec_line(LineAtom, SOut) :-
	atom_codes(LineAtom, Line),
	batchrec_line(Output, Line, []),
	!,
	(   functor(Output, discard, _) -> 
	    true ;  
	    Output = warning(Text) ->
	    format('~NBATCHREC WARNING: ~w~n', [Text]) ;
	    format(SOut, "~N~q.~n", [Output])
	).
% Conversion from atom to string added by DMC, 15-12-03; also
% "..." to '...' so we can read the format string in an error msg!!
parse_batchrec_line(LineAtom, _SOut) :-
	atom_codes(LineAtom, Line),
	format('~N*** Error: unable to process line ~s~n', [Line]).

%------------------------------------------------------------------------------------

make_prolog_batchrec_file(InFile, OutFile, NRecords) :-
	open(InFile, read, SIn),
	open(OutFile, write, SOut),
	make_prolog_batchrec_stream(SIn, SOut, '*start*', av_time_and_rt(0.0, 1.0), 0, 0-NRecords), 
	close(SIn),
	close(SOut).

make_prolog_batchrec_stream(SIn, SOut, CurrentWavfile, CurrentAvTimeAndTimesRT, Counter, NRecIn-NRecOut) :-
	read(SIn, Term),
	make_prolog_batchrec_stream1(Term, SIn, SOut, CurrentWavfile, CurrentAvTimeAndTimesRT, Counter, NRecIn-NRecOut).

%make_prolog_batchrec_stream1(end_of_file, _SIn, SOut, CurrentWavfile, _CurrentAvTimeAndTimesRT, _Counter, NRecIn-NRecIn) :-
%	(   CurrentWavfile = '*start*' ->
%	    true ;
%	    format(SOut, '~N]). %~w~n~n', [CurrentWavfile])
%	),
%	!.
make_prolog_batchrec_stream1(end_of_file, _SIn, _SOut, _CurrentWavfile, _CurrentAvTimeAndTimesRT, _Counter, NRecIn-NRecIn) :-
	!.
make_prolog_batchrec_stream1(Term, SIn, SOut, CurrentWavfile, CurrentAvTimeAndTimesRT, Counter, NRecIn-NRecOut) :-
	Counter1 is Counter + 1,
	Mod is Counter1 mod 500,
	(   Mod = 0 ->
	    format('~d ', [Counter1]), 
	    flush_output(user) ;
	    true
	),	
	!,
	do_prolog_batchrec_stream_line(Term, CurrentWavfile, NextWavfile, CurrentAvTimeAndTimesRT, NextAvTimeAndTimesRT, SOut, NRecIn-NRecNext),
	make_prolog_batchrec_stream(SIn, SOut, NextWavfile, NextAvTimeAndTimesRT, Counter1, NRecNext-NRecOut).
make_prolog_batchrec_stream1(Term, _SIn, _SOut, _CurrentWavfile, _CurrentAvTimeAndTimesRT, _Counter, _NRec) :-
	format("~N*** Error: unable to process term ~q~n", [Term]),
	fail.

do_prolog_batchrec_stream_line(wavfile(Wavfile), CurrentWavfile, NextWavfile, CurrentAvTimeAndTimesRT, CurrentAvTimeAndTimesRT, SOut, NRecIn-NRecOut) :-
	(   CurrentWavfile = '*start*' ->
	    true ;
	    format(SOut, '~N]). %~w~n~n', [CurrentWavfile])
	),
	format(SOut, '~Nbatchrec_item([~n', []),
	format(SOut, '~Nwavfile=~q', [Wavfile]),
	NextWavfile = Wavfile,
	NRecOut is NRecIn + 1,
	!.
do_prolog_batchrec_stream_line(transcription(Words), Wavfile, Wavfile, AvTimeAndTimesRT, AvTimeAndTimesRT, SOut, NRecIn-NRecIn) :-
	format(SOut, ',~ntranscription=~q', [Words]),
	!.
do_prolog_batchrec_stream_line(result_confidence_nlconf(_NBestPos, Words, Conf, NLConf, WordConfidence),
			       Wavfile, Wavfile, AvTimeAndTimesRT, AvTimeAndTimesRT, SOut, NRecIn-NRecIn) :-
	format(SOut, ',~nwords=~q', [Words]),
	format(SOut, ',~nconfidence=~q', [Conf]),
	format(SOut, ',~nnl_confidence=~q', [NLConf]),
	(   is_list(WordConfidence) ->
	    format(SOut, ',~nword_confidence=~q', [WordConfidence]) ;
	    true
	),
	!.
do_prolog_batchrec_stream_line(nl_result(_NBestPos,[slot(_, NLValue)]),
			       Wavfile, Wavfile, AvTimeAndTimesRT, AvTimeAndTimesRT, SOut, NRecIn-NRecIn) :-
	format(SOut, ',~nnl_value=~q', [NLValue]),
	!.
do_prolog_batchrec_stream_line(nl_result(_NBestPos,[]),
			       Wavfile, Wavfile, AvTimeAndTimesRT, AvTimeAndTimesRT, SOut, NRecIn-NRecIn) :-
	format(SOut, ',~nnl_value=~q', ['*no_nl_value*']),
	!.
do_prolog_batchrec_stream_line(Item, Wavfile, Wavfile, AvTimeAndTimesRT, AvTimeAndTimesRT, _SOut, NRecIn-NRecIn) :-
	functor(Item, rec_errors, _),
	!.
do_prolog_batchrec_stream_line(average_times_rt(AvAbsoluteTime, NextTimesRT),
			       Wavfile, Wavfile, _CurrentAvTimeAndTimesRT, NextAvTimeAndTimesRT, _SOut, NRecIn-NRecIn) :-
	NextAvTimeAndTimesRT = av_time_and_rt(AvAbsoluteTime, NextTimesRT),
	!.
do_prolog_batchrec_stream_line(rec_final_total(wer=Wer,w_num=WordNumber,ser=Ser,f_num=FileNumber),
			       Wavfile, Wavfile, AvTimeAndTimesRT, AvTimeAndTimesRT, SOut, NRecIn-NRecIn) :-
	AvTimeAndTimesRT = av_time_and_rt(AvAbsoluteTime, TimesRT),
	AvAbsoluteUttTime is AvAbsoluteTime / TimesRT,
	
	(   Wavfile = '*start*' ->
	    true ;
	    format(SOut, '~N]). %~w~n~n', [Wavfile])
	),
	format(SOut, '~N~n%Rec Final Total: ~2f% of ~d words (~2f% of ~d files), Av. utt. time = ~2f secs, ~3fxRT',
	       [Wer, WordNumber, Ser, FileNumber, AvAbsoluteUttTime, TimesRT]),
	format(SOut, '~N~q.~n~n',
	       [rec_stats(['WER'=Wer,
			   'TotalWords'=WordNumber,
			   'SER'=Ser,
			   'AvUttTime'=AvAbsoluteUttTime,
			   'xRT'=TimesRT])]),
	format('~N# Rec Final Total: ~2f% of ~d words (~2f% of ~d files), Av. utt. time = ~2f secs, ~3fxRT~n~n',
	       [Wer, WordNumber, Ser, FileNumber, AvAbsoluteUttTime, TimesRT]),
	!.
do_prolog_batchrec_stream_line(Term, CurrentWavfile, NextWavfile, CurrentAvTimeAndTimesRT, NextAvTimeAndTimesRT, SOut, NRec) :-
	format('~N*** Error: bad call: ~q~n', [do_prolog_batchrec_stream_line(Term, CurrentWavfile, NextWavfile, CurrentAvTimeAndTimesRT, NextAvTimeAndTimesRT, SOut, NRec)]),
	fail.

%------------------------------------------------------------------------------------

nlrefs_to_batchrec_compatible_prolog_form(Transcriptions, ParsedNLToolOutput, PrologFile) :-
	open(Transcriptions, read, STranscriptions),
	open(ParsedNLToolOutput, read, SNLToolOutput),
	open(PrologFile, write, SOut),

	nlrefs_to_batchrec_compatible_prolog_form_streams(STranscriptions, SNLToolOutput, SOut),

	close(STranscriptions),
	close(SNLToolOutput),
	close(SOut),
	!.
nlrefs_to_batchrec_compatible_prolog_form(Transcriptions, NLToolOutput, PrologFile) :-
	format('~N*** Error: bad call: ~w~n',
	       [nlrefs_to_batchrec_compatible_prolog_form(Transcriptions, NLToolOutput, PrologFile)]),
	fail.

nlrefs_to_batchrec_compatible_prolog_form_streams(STranscriptions, SNLToolOutput, SOut) :-
	read_line(STranscriptions, TranscriptionsLine),
	read(SNLToolOutput, NLValue),

	(   ( TranscriptionsLine = end_of_file, NLValue = end_of_file ) ->
	    true
	;
	    ( ( TranscriptionsLine = end_of_file, NLValue \== end_of_file ) ;
	      ( TranscriptionsLine \== end_of_file, NLValue = end_of_file )
	    ) ->
	    format('~N*** Error: transcriptions and nl-tool outputs of different lengths after parsing.~n', []),
	    fail
	;
	    atom_codes(Transcription, TranscriptionsLine),
	    parse_transcription_line(Transcription, TranscriptionItem),

	    nlref_and_value_to_batchrec_compatible_prolog(TranscriptionItem, NLValue, SOut),
	    !,
	    nlrefs_to_batchrec_compatible_prolog_form_streams(STranscriptions, SNLToolOutput, SOut)
	).

nlref_and_value_to_batchrec_compatible_prolog(TranscriptionItem, NLValue, S) :-
	TranscriptionItem = transcription(Wavfile, Words),
	(   NLValue = no_lf ->
	    %Confidence = 0 ;
	    Confidence = 100 ;
	    Confidence = 100
	),
	format(S, '~Nbatchrec_item([~n', []),
	format(S, '~Nwavfile=~q,', [Wavfile]),
	format(S, '~nwords=~q,', [Words]),
	format(S, '~nconfidence=~q', [Confidence]),
	(   NLValue = [slot(_, RealNLValue)] ->
	    format(S, ',~nnl_value=~q', [RealNLValue]) ;
	    true
	),
	format(S, '~N]).~n~n', []).

%------------------------------------------------------------------------------------

parse_transcriptions_file_to_list(Transcriptions, TranscriptionsList) :-
	read_file_to_atom_list(Transcriptions, TranscriptionsList0),
	parse_transcriptions_list(TranscriptionsList0, TranscriptionsList),
	!.
parse_transcriptions_file_to_list(Transcriptions, TranscriptionsList) :-
	format('~N*** Error: bad call: ~q~n', [parse_transcriptions_file_to_list(Transcriptions, TranscriptionsList)]),
	fail.

parse_transcriptions_list([], []).
parse_transcriptions_list([F | R], R1) :-
	null_transcription_item(F),
	!,
	parse_transcriptions_list(R, R1).
parse_transcriptions_list([F | R], [F1 | R1]) :-
	parse_transcription_line(F, F1),
	!,
	parse_transcriptions_list(R, R1).

parse_transcription_line(Atom, ParsedLine) :-
	atom_codes(Atom, Chars),
	split_off_pathname_from_chars(Chars, Wavfile0Chars, RestChars),
	atom_codes(Wavfile0, Wavfile0Chars),
	unescape_spaces_in_atom(Wavfile0, Wavfile1),
	split_string_into_words(RestChars, Transcription),
	wavfile_atom_to_absolute_wavfile_atom(Wavfile1, Wavfile),
	ParsedLine = transcription(Wavfile, Transcription),
	!.
parse_transcription_line(Atom, ParsedLine) :-
	format('~N*** Error: bad call: ~q~n', [parse_transcription_line(Atom, ParsedLine)]),
	fail.

%------------------------------------------------------------------------------------

split_off_pathname_from_chars([], [], []) :-
	!.
split_off_pathname_from_chars([0'\\, 0'  | R], [0'\\, 0'  | RWavfileChars], RestChars) :-
	!,
	split_off_pathname_from_chars(R, RWavfileChars, RestChars).
split_off_pathname_from_chars([F | R], [F | RWavfileChars], RestChars) :-
	\+ any_whitespace_char(F),
	!,
	split_off_pathname_from_chars(R, RWavfileChars, RestChars).
split_off_pathname_from_chars([F | RestChars], [], RestChars) :-
	any_whitespace_char(F),
	!.

%------------------------------------------------------------------------------------

parse_nltool_output_file_to_file(NLToolOutput, ParsedNLToolOutputRaw, ParsedNLToolOutput) :-
	parse_raw_nltool_output_file(NLToolOutput, ParsedNLToolOutputRaw),
	compact_nltool_output_file(ParsedNLToolOutputRaw, ParsedNLToolOutput),
	!.
parse_nltool_output_file_to_file(NLToolOutput, ParsedNLToolOutputRaw, ParsedNLToolOutput) :-
	format('~N*** Error: bad call: ~q~n',
	       [parse_nltool_output_file_to_file(NLToolOutput, ParsedNLToolOutputRaw, ParsedNLToolOutput)]),
	fail.	

parse_raw_nltool_output_file(NLToolOutput, ParsedNLToolOutputRaw) :-
	open(NLToolOutput, read, SIn),
	open(ParsedNLToolOutputRaw, write, SOut),

	parse_raw_nltool_output_stream(SIn, SOut),

	close(SIn),
	close(SOut).

parse_raw_nltool_output_stream(SIn, SOut) :-
	read_line(SIn, Line),
	(   Line = end_of_file ->
	    true
	;
	    otherwise ->
	    parse_nltool_output_line(Line, Value),
	    format(SOut, '~N~q.~n', [Value]),
	    !,
	    parse_raw_nltool_output_stream(SIn, SOut)
	).

parse_nltool_output_line(Chars, Value) :-
	nltool_output_line(Value, Chars, []),
	!.
parse_nltool_output_line(Chars, Value) :-
	format('~N*** Error: bad call: ~q~n', [parse_nltool_output_atom(Chars, Value)]),
	fail.

%------------------------------------------------------------------------------------

compact_nltool_output_file(ParsedNLToolOutputRaw, ParsedNLToolOutput) :-
	open(ParsedNLToolOutputRaw, read, SIn),
	open(ParsedNLToolOutput, write, SOut),

	compact_nltool_output_stream(SIn, SOut, waiting_for_number_of_interpretations),

	close(SIn),
	close(SOut).

compact_nltool_output_stream(SIn, SOut, State) :-
	read(SIn, Term),
	compact_nltool_output_stream1(Term, SIn, SOut, State).

compact_nltool_output_stream1(end_of_file, _SIn, _SOut, State) :-
	(   State = waiting_for_number_of_interpretations ->
	    true ;
	    format('~N*** Error: nl-tool output file apparently truncated.~n', []),
	    fail
	).
compact_nltool_output_stream1(Term, SIn, SOut, waiting_for_number_of_interpretations) :-
	Term = number_of_interpretations(0),
	format(SOut, '~N~q.~n', [no_lf]),
	!,
	compact_nltool_output_stream(SIn, SOut, waiting_for_number_of_interpretations).
compact_nltool_output_stream1(Term, SIn, SOut, waiting_for_number_of_interpretations) :-
	Term = number_of_interpretations(N),
	N > 0,
	!,
	compact_nltool_output_stream(SIn, SOut, waiting_for_value).
compact_nltool_output_stream1(Term, SIn, SOut, waiting_for_value) :-
	Term = value(Value),
	!,
	format(SOut, '~N~q.~n', [Value]),
	compact_nltool_output_stream(SIn, SOut, waiting_for_number_of_interpretations).
compact_nltool_output_stream1(_Term, SIn, SOut, State) :-
	!,
	compact_nltool_output_stream(SIn, SOut, State).

%---------------------------------------------------------------

remove_wavfiles_from_transcriptions_in_file(Transcriptions, BareTranscriptions) :-
	open(Transcriptions, read, SIn),
	open(BareTranscriptions, write, SOut),

	remove_wavfiles_from_transcriptions_stream(SIn, SOut),
	
	close(SIn),
	close(SOut),
	!.
remove_wavfiles_from_transcriptions_in_file(Transcriptions, BareTranscriptions) :-
	format('~N*** Error: bad call: ~q~n', [remove_wavfiles_from_transcriptions_in_file(Transcriptions, BareTranscriptions)]),
	fail.	

remove_wavfiles_from_transcriptions_stream(SIn, SOut) :-
	read_line(SIn, Line),
	(   Line = end_of_file ->
	    true
	;
	    otherwise ->
	    atom_codes(Term, Line),
	    (   null_transcription_item(Term) ->
		true
	    ;
		otherwise ->
		remove_wavfile_from_transcription_item(Term, Term1),
		format(SOut, '~N~w~n', [Term1])
	    ),
	    !,
	    remove_wavfiles_from_transcriptions_stream(SIn, SOut)
	).

remove_wavfile_from_transcription_item(Transcription, BareTranscription) :-
	split_atom_into_words(Transcription, [_Wavfile | Words]),
	join_with_spaces(Words, BareTranscription),
	!.
remove_wavfile_from_transcription_item(Transcription, BareTranscription) :-
	format('~N*** Error: bad call: ~q~n', [remove_wavfile_from_transcription_item(Transcription, BareTranscription)]),
	fail.

%---------------------------------------------------------------

transcriptions_file_to_sents_file(TranscriptionsFile, SentsFile) :-
	safe_absolute_file_name(TranscriptionsFile, AbsTranscriptionsFile),
	safe_absolute_file_name(SentsFile, AbsSentsFile),

	parse_transcriptions_file_to_list(AbsTranscriptionsFile, InList),
	length(InList, InN),
	format('~N--- Read transcriptions file (~d records): ~w~n', [InN, AbsTranscriptionsFile]),

	transcriptions_list_to_sents_list(InList, OutList),
	length(OutList, OutN),

	list_to_prolog_file(OutList, AbsSentsFile),
	format('~N--- Written sents file (~d records): ~w~n', [OutN, AbsSentsFile]),
	!.
transcriptions_file_to_sents_file(TranscriptionsFile, SentsFile) :-
	format('~N*** Error: bad call: ~q~n', [transcriptions_file_to_sents_file(TranscriptionsFile, SentsFile)]),
	fail.

transcriptions_list_to_sents_list([], []) :-
	!.
transcriptions_list_to_sents_list([F | R], [F1 | R1]) :-
	transcriptions_list_item_to_sents_list_item(F, F1),
	!,
	transcriptions_list_to_sents_list(R, R1).

transcriptions_list_item_to_sents_list_item(transcription(_Wavfile, TranscriptionWordList),
					    sent(TranscriptionAtom)) :-
	join_with_spaces(TranscriptionWordList, TranscriptionAtom),
	!.
transcriptions_list_item_to_sents_list_item(F, F1) :-
	format('~N*** Error: bad call: ~q~n', [transcriptions_list_item_to_sents_list_item(F, F1)]),
	fail.


%---------------------------------------------------------------

transcriptions_file_to_wavfiles_file(Transcriptions, Wavfiles) :-
	read_file_to_atom_list(Transcriptions, TranscriptionsList),
	transcriptions_list_to_wavfile_list(TranscriptionsList, WavfileList),
	write_atom_list_to_file(WavfileList, Wavfiles),
	!.
transcriptions_file_to_wavfiles_file(Transcriptions, Wavfiles) :-
	format('~N*** Error: bad call: ~q~n', [transcriptions_file_to_wavfiles_file(Transcriptions, Wavfiles)]),
	fail.	

transcriptions_list_to_wavfile_list([], []).
transcriptions_list_to_wavfile_list([F | R], R1) :-
	null_transcription_item(F),
	!,
	transcriptions_list_to_wavfile_list(R, R1).
transcriptions_list_to_wavfile_list([F | R], [F1 | R1]) :-
	transcription_item_to_wavfile(F, F1),
	!,
	transcriptions_list_to_wavfile_list(R, R1).

transcription_item_to_wavfile(Transcription, Wavfile) :-
	split_atom_into_words(Transcription, [Wavfile0 | _Words]),
	wavfile_atom_to_absolute_wavfile_atom(Wavfile0, Wavfile),
	!.
transcription_item_to_wavfile(Transcription, Wavfile) :-
	format('~N*** Error: bad call: ~q~n', [transcription_item_to_wavfile(Transcription, Wavfile)]),
	fail.

%---------------------------------------------------------------

make_wavfiles_absolute_in_transcriptions_file(Transcriptions, AbsWavfileTranscriptions) :-
	read_file_to_atom_list(Transcriptions, TranscriptionsList),
	make_wavfiles_absolute_in_transcriptions_list(TranscriptionsList, TranscriptionsList1),
	write_atom_list_to_file(TranscriptionsList1, AbsWavfileTranscriptions),
	!.
make_wavfiles_absolute_in_transcriptions_file(Transcriptions, AbsWavfileTranscriptions) :-
	format('~N*** Error: bad call: ~q~n', [make_wavfiles_absolute_in_transcriptions_file(Transcriptions, AbsWavfileTranscriptions)]),
	fail.

make_wavfiles_absolute_in_transcriptions_list([], []).
make_wavfiles_absolute_in_transcriptions_list([F | R], R1) :-
	null_transcription_item(F),
	!,
	make_wavfiles_absolute_in_transcriptions_list(R, R1).
make_wavfiles_absolute_in_transcriptions_list([F | R], [F1 | R1]) :-
	make_wavfiles_absolute_in_transcription_item(F, F1),
	!,
	make_wavfiles_absolute_in_transcriptions_list(R, R1).

make_wavfiles_absolute_in_transcription_item(Transcription, Transcription1) :-
	parse_transcription_line(Transcription, transcription(Wavfile, Words)),
	join_with_spaces(Words, WordsAtom),
	format_to_atom('"~w" ~w', [Wavfile, WordsAtom], Transcription1),
	!.
make_wavfiles_absolute_in_transcription_item(Transcription, Transcription1) :-
	format('~N*** Error: bad call: ~q~n', [make_wavfiles_absolute_in_transcription_item(Transcription, Transcription1)]),
	fail.

%---------------------------------------------------------------

wavfile_atom_to_absolute_wavfile_atom(Wavfile, AbsWavfile) :-
	remove_curly_brackets_from_atom(Wavfile, Wavfile1),
	absolute_file_name(Wavfile1, AbsWavfile0),
	escape_spaces_in_atom(AbsWavfile0, AbsWavfile),
	!.
wavfile_atom_to_absolute_wavfile_atom(Wavfile, AbsWavfile) :-
	format('~N*** Error: bad call: ~q~n', [wavfile_atom_to_absolute_wavfile_atom(Wavfile, AbsWavfile)]),
	fail.

remove_curly_brackets_from_atom(Atom, Atom1) :-
	atom_codes(Atom, Chars),
	remove_curly_brackets_from_string(Chars, Chars1),
	atom_codes(Atom1, Chars1),
	!.

remove_curly_brackets_from_string([], []) :-
	!.
remove_curly_brackets_from_string([F | R], R1) :-
	member(F, "{}"),
	!,
	remove_curly_brackets_from_string(R, R1).
remove_curly_brackets_from_string([F | R], [F | R1]) :-
	!,
	remove_curly_brackets_from_string(R, R1).

%---------------------------------------------------------------

parameter_list_to_string(ParameterList, ParameterString) :-
	parameter_list_to_string1(ParameterList, '', ParameterString),
	!.
parameter_list_to_string(ParameterList, ParameterString) :-
	format('~N*** Error: bad call: ~q~n', [parameter_list_to_string(ParameterList, ParameterString)]),
	fail.

parameter_list_to_string1([], ReturnParameterString, ReturnParameterString) :-
	!.
parameter_list_to_string1([Item | ParameterList], ParameterString, _ReturnParameterString) :-
	(   Item = (Parameter=Value) ->
	    append_atoms([Parameter, Value], 0'=, OneParameterString) ;
	    atomic(Item) ->
	    Item = OneParameterString
	),
	join_with_spaces([OneParameterString, ParameterString], NewParameterString),
	parameter_list_to_string1(ParameterList, NewParameterString, _ReturnParameterString).
	
%---------------------------------------------------------------

:- dynamic batchrec_trace_item/4.

compare_batchrec_trace_lists(List1, List2, ComparisonList) :-
	init_compare_batchrec_trace_lists,
	store_batchrec_trace_list(List1, id1),
	store_batchrec_trace_list(List2, id2),
	find_all_stored_wavfiles_in_compare_batchrec(Wavfiles),
	compare_stored_batchrec_traces(Wavfiles, id1, id2, ComparisonList0),
	sort(ComparisonList0, ComparisonList),
	!.
compare_batchrec_trace_lists(_List1, _List2, _ComparisonList) :-
	format('~N*** Error: bad call: ~w~n',
	       [compare_batchrec_trace_lists('...', '...', '...')]),
	fail.

init_compare_batchrec_trace_lists :-
	retractall(batchrec_trace_item(_, _, _, _)),
	!.

store_batchrec_trace_list([], _Id).
store_batchrec_trace_list([F | R], Id) :-
	store_batchrec_trace_item(F, Id),
	!,
	store_batchrec_trace_list(R, Id).

/*
batchrec_item([wavfile='c:/home/speech/speechtranslation/medslt2/corpora/coling2004/002-m-34/21-47-56-scylax-microsoft_sound_mapper/utt29.wav',words=[does,bright,light,cause,headaches],confidence=75,nl_confidence=75,nl_value=[[utterance_type,ynq],[cause,bright_light],[voice,active],[tense,present],[event,cause],[secondary_symptom,headache]],transcription=[does,bright,light,cause,headaches]]).
*/

store_batchrec_trace_item(rec_stats(_), _Id) :-
	!.
store_batchrec_trace_item(Item, Id) :-
	Item = batchrec_item(List),
	member(wavfile=Wavfile, List),
	(   member(transcription=Transcription, List) ->
	    true
	;
	    Transcription = ['*null*']
	),
	(   member(words=Words, List) ->
	    true
	;
	    Words = ['*null*']
	),
	assertz(batchrec_trace_item(Wavfile, Transcription, Words, Id)),
	!.
store_batchrec_trace_item(Item, Id) :-
	format('~N*** Error: bad call: ~w~n',
	       [store_batchrec_trace_item(Item, Id)]),
	fail.

find_all_stored_wavfiles_in_compare_batchrec(Wavfiles) :-
	findall(Wavfile,
		batchrec_trace_item(Wavfile, _Transcription, _Words, _Id),
		Wavfiles0),
	sort(Wavfiles0, Wavfiles),
	!.

compare_stored_batchrec_traces([], _Id1, _Id2, []).
compare_stored_batchrec_traces([F | R], Id1, Id2, Comparisons) :-
	(   compare_stored_batchrecs_for_wavfile(F, Id1, Id2, Comparison) ->
	    Comparisons = [Comparison | RestComparisons] ;
	    Comparisons = RestComparisons
	),
	!,
	compare_stored_batchrec_traces(R, Id1, Id2, RestComparisons).

compare_stored_batchrecs_for_wavfile(Wavfile, Id1, Id2, Comparison) :-
	batchrec_trace_item(Wavfile, Transcription, Words1, Id1),
	batchrec_trace_item(Wavfile, Transcription, Words2, Id2),
	(   Words1 = Words2 ->
	    fail ;
	    Words1 = Transcription ->
	    Result = first_ok ;
	    Words2 = Transcription ->
	    Result = second_ok ;
	    Result = neither_ok
	),
	Comparison = different_result(Result, Wavfile, Transcription, Words1, Words2),
	!.

summarise_batchrec_comparison_list(ComparisonList, McNemarResults) :-
	count_comparison_list_items(ComparisonList, first_ok, NFirstOK),
	count_comparison_list_items(ComparisonList, second_ok, NSecondOK),
	count_comparison_list_items(ComparisonList, neither_ok, NNeitherOK),
	mcnemar(NFirstOK, NSecondOK, Significance),
	McNemarResults = [NFirstOK, NSecondOK, Significance],
	format('~N---    #first_ok: ~d~n', [NFirstOK]),
	format('~N---   #second_ok: ~d~n', [NSecondOK]),
	format('~N---  #neither_ok: ~d~n', [NNeitherOK]),
	(   atom(Significance) ->
	    format('~N--- Significance: ~w~n', [Significance])
	;
	    number(Significance) ->
	    format('~N--- Significance: significant at P < ~3f~n', [Significance])
	),
	!.
summarise_batchrec_comparison_list(_ComparisonList, _McNemarResults) :-
	format('~N*** Error: bad call: ~w~n',
	       [summarise_batchrec_comparison_list('...', '...')]),
	fail.

count_comparison_list_items(List, Result, N) :-
	findall(x,
		member(different_result(Result, _, _, _Words1, _Words2), List),
		Xs),
	length(Xs, N),
	!.

%---------------------------------------------------------------

/*

The McNemar statistic is calculated as

X2 = (b - c)^2/(b + c).     (1)

The value X^2 can be viewed as a chi-squared statistic with 1 df. 

df 	P = 0.05 P = 0.01 P = 0.001
1       3.84 	 6.64 	  10.83

*/

mcnemar(B, C, Sig) :-
	B = 0,
	C = 0,
	Sig = no_differences,
	!.
mcnemar(B, C, Sig) :-
	Sum is B + C,
	Sum > 0,
	ChiSquare is ( ( B - C ) * ( B - C ) ) / ( B + C),
	chi_square_to_sig(ChiSquare, Sig),
	!.
mcnemar(B, C, Sig) :-
	format('~N*** Error: bad call: ~w', [mcnemar(B, C, Sig)]),
	fail.

chi_square_to_sig(ChiSquare, not_significant) :-
	ChiSquare < 3.84,
	!.
chi_square_to_sig(ChiSquare, 0.05) :-
	3.84 =< ChiSquare, ChiSquare < 6.64,
	!.
chi_square_to_sig(ChiSquare, 0.01) :-
	6.64 =< ChiSquare, ChiSquare < 10.83,
	!.
chi_square_to_sig(ChiSquare, 0.001) :-
	10.83 =< ChiSquare,
	!.

%---------------------------------------------------------------

write_batchrec_comparison_file(ComparisonList, ComparisonFile) :-
	%list_to_prolog_file(ComparisonList, ComparisonFile),
	open(ComparisonFile, write, S),
	write_batchrec_comparison_list(ComparisonList, S),
	close(S),
	!.

write_batchrec_comparison_list([], _S).
write_batchrec_comparison_list([F | R], S) :-
	write_batchrec_comparison_list_item(F, S),
	!,
	write_batchrec_comparison_list(R, S).

write_batchrec_comparison_list_item(Item, S) :-
	Item = different_result(Result, Wavfile, Transcription, Words1, Words2),
	format(S, '~N~ndifferent_result(~n', []),
	format(S, '~N    ~q,', [Result]),
	format(S, '~N    ~q,', [Wavfile]),
	format(S, '~N    ~q, %Transcription', [Transcription]),
	format(S, '~N    ~q, %Words1', [Words1]),
	format(S, '~N    ~q). %Words2', [Words2]),
	!.
write_batchrec_comparison_list_item(Item, S) :-
	format('~N*** Error: bad call: ~w~n',
	       [write_batchrec_comparison_list_item(Item, S)]),
	fail.

%---------------------------------------------------------------

wavfiles_in_directory(Directory, Wavfiles) :-
	absolute_file_name(Directory, AbsDirectory),
	safe_directory_files(AbsDirectory, Files),
	extract_wavfiles_from_list_and_add_directory(Files, AbsDirectory, Wavfiles),
	!.
wavfiles_in_directory(Directory, Wavfiles) :-
	format('~N*** Error: bad call: ~w~n',
	       [wavfiles_in_directory(Directory, Wavfiles)]),
	fail.

extract_wavfiles_from_list_and_add_directory([], _Directory, []).
extract_wavfiles_from_list_and_add_directory([F | R], Directory, [F1 | R1]) :-
	is_a_wavfile(F),
	append_atoms([Directory, F], 0'/, F1),
	!,
	extract_wavfiles_from_list_and_add_directory(R, Directory, R1).
extract_wavfiles_from_list_and_add_directory([_F | R], Directory, R1) :-
	!,
	extract_wavfiles_from_list_and_add_directory(R, Directory, R1).

is_a_wavfile(File) :-
	atom_codes(File, FileChars),
	append(_, ".wav", FileChars),
	!.

%---------------------------------------------------------------

transcribed_wavfile_list_to_transcriptions_file(Wavfiles, TranscriptionsFile) :-
	wavfile_list_to_wavfile_transcriptions_atom_list(Wavfiles, WavfileTranscriptions),
	absolute_file_name(TranscriptionsFile, AbsTranscriptionsFile),
	write_atom_list_to_file(WavfileTranscriptions, AbsTranscriptionsFile),
	!.

wavfile_list_to_wavfile_transcriptions_atom_list([], []).
wavfile_list_to_wavfile_transcriptions_atom_list([F | R], [F1 | R1]) :-
	wavfile_to_wavfile_transcription_atom(F, F1),
	!,
	wavfile_list_to_wavfile_transcriptions_atom_list(R, R1).

wavfile_to_wavfile_transcription_atom(Wavfile, WavfileTranscription) :-
	(   get_transcription_from_wavfile(Wavfile, Transcription) ->
	    join_with_spaces([Wavfile | Transcription], WavfileTranscription)
	;
	    otherwise ->
	    format('~N--- Warning: no transcription for ~w~n', [Wavfile]),
	    WavfileTranscription = Wavfile
	).

%---------------------------------------------------------------

wavfile_list_to_wavfiles_file(Wavfiles, WavfilesFile) :-
	escape_spaces_in_atoms_in_list(Wavfiles, Wavfiles1),
	%Wavfiles = Wavfiles1,
	absolute_file_name(WavfilesFile, AbsWavfilesFile),
	write_atom_list_to_file(Wavfiles1, AbsWavfilesFile),
	!.

escape_spaces_in_atoms_in_list([], []).
escape_spaces_in_atoms_in_list([F | R], [F1 | R1]) :-
	escape_spaces_in_atom(F, F1),
	!,
	escape_spaces_in_atoms_in_list(R, R1).

%---------------------------------------------------------------

batchrec_output_file_to_rough_transcriptions_file(BatchrecPrologFile, TranscriptionsFile) :-
	absolute_file_name(TranscriptionsFile, AbsTranscriptionsFile),
	prolog_file_to_list(BatchrecPrologFile, BatchrecList),
	batchrec_list_to_transcription_records(BatchrecList, TranscriptionsList),
	length(TranscriptionsList, NTranscriptionsList),
	write_atom_list_to_file(TranscriptionsList, AbsTranscriptionsFile),
	format('~N--- Written rough transcriptions file (~d records) ~w~n',
	       [NTranscriptionsList, AbsTranscriptionsFile]),
	!.
batchrec_output_file_to_rough_transcriptions_file(BatchrecPrologFile, TranscriptionsFile) :-
	format('~N*** Error: bad call: ~w~n',
	       [batchrec_output_file_to_rough_transcriptions_file(BatchrecPrologFile, TranscriptionsFile)]),
	fail.

batchrec_list_to_transcription_records([], []).
batchrec_list_to_transcription_records([F | R], [F1 | R1]) :-
	batchrec_list_to_transcription_record(F, F1),
	!,
	batchrec_list_to_transcription_records(R, R1).
batchrec_list_to_transcription_records([_F | R], R1) :-
	!,
	batchrec_list_to_transcription_records(R, R1).

batchrec_list_to_transcription_record(BatchrecRecord, _TransRecord) :-
	BatchrecRecord = rec_stats(_),
	!,
	fail.
batchrec_list_to_transcription_record(BatchrecRecord, TransRecord) :-
	BatchrecRecord = batchrec_item(List),
	member(wavfile=Wavfile, List),
	(   member(words=Words, List) ->
	    join_with_spaces([Wavfile | Words], TransRecord) ;
	    TransRecord = Wavfile
	),
	!.
batchrec_list_to_transcription_record(BatchrecRecord, TransRecord) :-
	format('~N*** Warning: bad call: ~w~n',
	       [batchrec_list_to_transcription_record(BatchrecRecord, TransRecord)]),
	fail.
	
%---------------------------------------------------------------

add_transcriptions_to_prolog_batchrec_file(PrologBatchrecFile, TranscriptionsFile, PrologBatchrecFileWithTranscriptions) :-
	prolog_file_to_list(PrologBatchrecFile, PrologBatchrecList),
	parse_transcriptions_file_to_list(TranscriptionsFile, TranscriptionsList),
	add_transcriptions_to_prolog_batchrec_list(PrologBatchrecList, TranscriptionsList, PrologBatchrecListWithTranscriptions),
	list_to_prolog_file(PrologBatchrecListWithTranscriptions, PrologBatchrecFileWithTranscriptions),
	!.
add_transcriptions_to_prolog_batchrec_file(PrologBatchrecFile, TranscriptionsFile, PrologBatchrecFileWithTranscriptions) :-
	format('~N*** ERROR: bad call: ~w~n', [add_transcriptions_to_prolog_batchrec_file(PrologBatchrecFile, TranscriptionsFile, PrologBatchrecFileWithTranscriptions)]),
	fail.

add_transcriptions_to_prolog_batchrec_list([], _TranscriptionList, []) :-
	!.
add_transcriptions_to_prolog_batchrec_list([F | R], TranscriptionList, [F1 | R1]) :-
	add_transcriptions_to_prolog_batchrec_list_item(F, TranscriptionList, F1),
	!,
	add_transcriptions_to_prolog_batchrec_list(R, TranscriptionList, R1).

add_transcriptions_to_prolog_batchrec_list_item(Item, _TranscriptionList, Item) :-
	nonvar(Item),
	Item = rec_stats(_),
	!.
add_transcriptions_to_prolog_batchrec_list_item(Item, TranscriptionList, Item1) :-
	Item = batchrec_item(List),
	member(wavfile=Wavfile, List),
	member(transcription(Wavfile, Transcription), TranscriptionList),
	append(List, [transcription=Transcription], List1),
	Item1 = batchrec_item(List1),
	!.
add_transcriptions_to_prolog_batchrec_list_item(Item, TranscriptionList, Item1) :-
	format('~N*** ERROR: bad call: ~w~n', [add_transcriptions_to_prolog_batchrec_list_item(Item, TranscriptionList, Item1)]),
	fail.

%----------------------------------------------------------------------

null_transcription_item(Atom) :-
	atom(Atom),
	atom_codes(Atom, Chars),
	null_transcription_chars(Chars),
	!.

null_transcription_chars([0'# | _]) :-
	!.
null_transcription_chars(Chars) :-
	all_whitespace_chars(Chars),
	!.

all_whitespace_chars([]).
all_whitespace_chars([F | R]) :-
	member(F, " \t"),
	!,
	all_whitespace_chars(R).

%------------------------------------------------------------------------------------

% BATCHREC WARNING: Skipping file C:\home\speech\Corpora\Checklist\bah_082902\utt01xxx.wav which does not exist.

batchrec_line(warning(Text)) -->
	"BATCHREC WARNING: ",
	!,
	arbitrary_sequence(Text).

% File 30: swe_logfiles/2000/09September/28/17-31-07-ukcamdv09-Microsoft_Sound_Mapper/utt07.wav

batchrec_line(wavfile(Wavfile)) -->
	"File",
	!,
	spaces,
	integer_word(_N),
	":",
	spaces,
	non_space_word(Wavfile).

% Transcription: switch on the light

batchrec_line(transcription(Words)) -->
	"Transcription:",
	!,
	spaces,
	rec_result_list(Words),
	spaces.

% Result #0:     sätt på spisen (conf: 59, NL conf: 61)
% Result #0:     load water sampling procedure (conf: 73, NL conf: 72)(Num_Words=  4 Word  0  68 Word  1  83 Word  2  57 Word  3  82)

batchrec_line(result_confidence_nlconf(N, Rec, Conf, NLConf, WordConf)) -->
	"Result #",
	!,
	integer_word(N),
	":",
	!,
	spaces,
	rec_result_list(Rec),
	spaces,
	"(conf:",
	spaces,
	integer_word(Conf),
	optional_nl_conf(NLConf),
	")",
	spaces,
	optional_word_confidence_scores(WordConf).

% Added by DMC, 15-12-03: we sometimes only have conf, not NL conf.
% In this case, we set NLConf to Conf.
batchrec_line(result_confidence_nlconf(N, Rec, Conf, Conf, WordConf)) -->
	"Result #",
	!,
	integer_word(N),
	":",
	!,
	spaces,
	rec_result_list(Rec),
	spaces,
	"(conf:",
	spaces,
	integer_word(Conf),
	%", NL conf:",
	%spaces,
	%integer_word(NLConf),
	")",
	spaces,
	optional_word_confidence_scores(WordConf).

% Rec Errors: 0 ins, 0 del, 0 sub = 0.00% of 3 words.

batchrec_line(rec_errors(ins=Ins, del=Del, sub=Sub)) -->
	"Rec Errors:",
	!,
	spaces,
	integer_word(Ins),
	spaces,
	"ins,",
	spaces,
	integer_word(Del),
	spaces,
	"del,",
	spaces,
	integer_word(Sub),
	spaces,
	"sub",
	arbitrary_sequence.


% NL Res.#0:     <device1 "spis"> <onoff_direction "på"> <onoff_level 100> <operation "command"> <spec1 "det">
% NL Res.#0[0]:     <choice "A1">

% Only keep first alternative for now

batchrec_line(nl_result(N, Slots)) -->
	"NL Res.#",
	integer_word(N),
	(   ":" ;
	    %"[", integer_word(_Alterative), "]:"
	    "[0]:"
	),
	!,
	spaces,
	slot_value_list(Slots).	

% NL Status:  correct

batchrec_line(nl_status(nl_status=Status)) -->
	"NL Status:",
	!,
	spaces,
        arbitrary_sequence(Status).	

%Rec Final Total: 0 ins, 0 del, 0 sub = 0.00% of 5 words (0.00% of 5 files).

batchrec_line(rec_final_total(wer=Wer,w_num=WordNumber,ser=Ser,f_num=FileNumber)) -->
        "Rec Final Total: ",
        integer_word(_Insertions),
        " ins, ",
        integer_word(_Deletions),
        " del, ",
        integer_word(_Substitutions),
        " sub = ",
        !,
        float_word(Wer),
        "% of ",
        integer_word(WordNumber),
        " words (",
        float_word(Ser),
        "% of ",
        integer_word(FileNumber),
        arbitrary_sequence(_X).

%NL Final Total: 0 rejects, 0 incorrect = 0.00% error on 5 files.

batchrec_line(semantic_error_rate(sem=Sem)) -->
        "NL Final Total: ",
        !,
        integer_word(_Rejects),
        " rejects, ",
        integer_word(_Incorrect),
        " incorrect = ",
        float_word(Sem),
        arbitrary_sequence(_X).

%Total processing times: user=0.12 system=0.00, elapsed=0.13

/*
batchrec_line(system_time_taken(time=SystemTime)) -->
        "Total processing times: user=",
        float_word(_UserTime),
        " system=",
        !,
        float_word(SystemTime),
        arbitrary_sequence(_X).
*/

% Ave. Times: 0.52 secs 0.162xRT (0.41 usr 0.00 sys 0.1287xcpuRT) 79%cpu

batchrec_line(average_times_rt(AbsoluteTime, TimesRT)) -->
        "Ave. Times: ",
	!,
        float_word(AbsoluteTime),
        " secs ",
        float_word(TimesRT),
        arbitrary_sequence(_X).

batchrec_line(discard(X)) -->
	arbitrary_sequence(X),
	!.

batchrec_line(discard(empty)) -->
	[].

%------------------------------------------------------------------------------------

nlref_line(Wavfile, Value) -->
	non_space_word(Wavfile),
	spaces,
	slot_value_list(Value).

%------------------------------------------------------------------------------------

nltool_output_line(number_of_interpretations(N)) -->
	"Number of Interpretations:",
	spaces,
	integer_word(N),
	!.
nltool_output_line(value(Value)) -->
	"{",
	slot_value_list(Value),
	"}",
	!.
nltool_output_line(other) -->
	arbitrary_sequence,
	!.

%------------------------------------------------------------------------------------

transcription_words(Transcription) -->
	word_list(Transcription).
transcription_words([no_transcription]) -->
	"(none)",
	spaces.

slot_value_list([F|R]) -->
	slot(F),
	!,
	spaces,
	slot_value_list(R).
slot_value_list([]) --> 
	[].
slot_value_list([]) -->
	"(none)",
	spaces.
slot_value_list(['<rejected>']) -->
	"<rejected>",
	spaces.

slot(slot(SlotName, Value)) -->
	"<",
	word(SlotName),
	spaces,
	slot_value(Value),
	">",
	!.

slot_value([]) -->
	"(",
	spaces,
	")",
	!.
slot_value(List) -->
	"(",
	slot_value_list_constrct(List),
	")",
	!.
slot_value(List) -->
	"[",
	slot_value_list(List),
	"]",
	!.
slot_value(Value) -->
	"""",
	non_inverted_comma_word(Value),
	"""",	
	!.
slot_value(Value) -->
	integer_word(Value).
slot_value(Value) -->
	word(Value).
slot_value(Value) -->
	word_including_spaces(Value),
	!.

slot_value_list_construct([F | R]) -->
	slot_value(F),
	!,
	slot_value_rest_list_construct(R).

slot_value_rest_list_construct(R) -->
	[Char], {white_space_char(Char)},
	white_spaces,
	!,
	slot_value_list_construct(R).
slot_value_rest_list_construct([]) -->
	[].

rec_result_list(['<rejected>']) -->	
	"<rejected>".
rec_result_list(Rec) -->
	%word_list(Rec).
	non_space_or_paren_word_list(Rec).

optional_nl_conf(NLConf) -->
	", NL conf:",
	spaces,
	integer_word(NLConf),
	!.
optional_nl_conf(0) -->
	spaces,
	!.

optional_word_confidence_scores(WordConf) -->
	word_confidence_scores(WordConf),
	!.
optional_word_confidence_scores('*no_word_confidence_scores*') -->
	[].

% (Num_Words=  4 Word  0  68 Word  1  83 Word  2  57 Word  3  82)
word_confidence_scores(WordConf) -->
	"(Num_Words=",
	spaces,
	integer_word(_NumWords),
	spaces,
	word_confidence_scores_body(WordConf),
	")",
	!.

word_confidence_scores_body([F | R]) -->
	word_confidence_score_item(F),
	spaces,
	!,
	word_confidence_scores_body(R).
word_confidence_scores_body([]) -->
	spaces,
	!.

word_confidence_score_item(Score) -->
	"Word",
	spaces,
	integer_word(_Index),
	spaces,
	integer_word(Score),
	!.

non_space_or_paren_word_list([F | R]) -->
	non_space_or_paren_word(F),
	!,
	white_spaces,
	non_space_or_paren_word_list(R).
non_space_or_paren_word_list([]) -->
	[].

non_space_or_paren_word(Word) -->
	non_space_or_paren_char_list(L), {L \== [], atom_codes(Word, L)}.

non_space_or_paren_char_list([F | R]) -->
	[F], {non_space_or_paren_char(F)},
	!,
	non_space_or_paren_char_list(R).
non_space_or_paren_char_list([]) --> [].

non_space_word(Word) -->
	nonspace_char_list(L), {L \== [], atom_codes(Word, L)}.

nonspace_char_list([F | R]) -->
	[F], {nonspace_char(F)},
	!,
	nonspace_char_list(R).
nonspace_char_list([]) --> [].

non_whitespace_word(Word) -->
	non_whitespace_char_list(L), {L \== [], atom_codes(Word, L)}.

non_whitespace_char_list([F | R]) -->
	[F], {non_whitespace_char(F)},
	!,
	non_whitespace_char_list(R).
non_whitespace_char_list([]) --> [].	

non_inverted_comma_word(Word) -->
	noninverted_comma_char_list(L), {L \== [], atom_codes(Word, L)}.

noninverted_comma_char_list([F | R]) -->
	[F], {dif(F, 0'")},   %"
	!,
	noninverted_comma_char_list(R).
noninverted_comma_char_list([]) --> [].

non_equals_word(Word) -->
	non_equals_char_list(L), {L \== [], atom_codes(Word, L)}.

non_equals_char_list([F|R]) -->
	[F], {non_equals_char(F)},
	!,
	non_equals_char_list(R).
non_equals_char_list([]) --> [].

word_including_spaces(Word) -->
	chars_for_space_word_list(L), {L \== [], atom_codes(Word, L)}.

chars_for_space_word_list([F|R]) -->
	[F], {char_for_space_word_list(F)},
	!,
	chars_for_space_word_list(R).
chars_for_space_word_list([]) --> [].

word(Word) -->
	alphanum_char_list(L), {L \== [], atom_codes(Word, L)}.

alphanum_char_list([F|R]) -->
	[F], {alphanum_char(F)},
	!,
	alphanum_char_list(R).
alphanum_char_list([]) --> [].

word_or_parenthesized_word_list(List) -->
	parenthesized_word_list(List),
	!.
word_or_parenthesized_word_list(Word) -->
	word(Word).

word_or_word_list(List) -->
	word_list(List),
	!.
word_or_word_list(Word) -->
	word(Word).

parenthesized_word_list(List) -->
	"(",
	word_list(List),
	")".

word_list_joined_by_hyphens([F|R]) -->
	word(F),
	!,
	word_list_joined_by_hyphens_rest(R).

word_list_joined_by_hyphens_rest(R) -->
	"-",
	!,
	word_list_joined_by_hyphens(R).
word_list_joined_by_hyphens_rest([]) -->
	[].

word_list([F|R]) -->
	word(F),
	!,
	white_spaces,
	word_list(R).
word_list([]) -->
	[].

word_list_joined_by_spaces_or_parens([F|R]) -->
	word(F),
	!,
	white_spaces_including_parens,
	word_list_joined_by_spaces_or_parens(R).
word_list_joined_by_spaces_or_parens([]) -->
	[].

float_word(Number) -->
        digit_char_list_word(BeforePoint),
        ".",
        digit_char_list_word(AfterPoint),
	{   atom_codes(BeforePoint, BeforePointChars),
	    atom_codes(AfterPoint, AfterPointChars),
	    append_list([BeforePointChars, ".", AfterPointChars], AllChars),
	    number_codes(Number, AllChars)
	}.

integer_word(Word) -->
	digit_char_list(L), {L \== [], number_codes(Word, L)}.

digit_char_list_word(Word) -->
	digit_char_list(L), {L \== [], atom_codes(Word, L)}.

digit_char_list([F|R]) -->
	[F], {digit_char(F)},
	!,
	digit_char_list(R).
digit_char_list([]) --> [].

white_spaces -->
	[Char], {white_space_char(Char)},
	!,
	white_spaces.
white_spaces -->
	[].    

white_spaces_including_parens -->
	[Char], {white_space_char(Char) ; parenthesis_char(Char)},
	!,
	white_spaces_including_parens.
white_spaces_including_parens -->
	[].    

white_spaces_including_tabs -->
	[Char], {white_space_char(Char) ; tab_char(Char)},
	!,
	white_spaces_including_tabs.
white_spaces_including_tabs -->
	[].    

tab_surrounded_by_whitespaces -->
	white_spaces,
	[TabChar], {tab_char(TabChar)},
	white_spaces.

tab_preceded_by_whitespaces -->
	white_spaces,
	[TabChar], {tab_char(TabChar)}.

spaces -->
	[Char], {space_char(Char)},
	!,
	spaces.
spaces -->
	[].    

arbitrary_sequence -->
	[_Char], 
	!,
	arbitrary_sequence.
arbitrary_sequence -->
	[].    

arbitrary_sequence(Word) -->
	char_list(L), {L \== [], atom_codes(Word, L)}.

char_list([F|R]) -->
	[F], 
	!,
	char_list(R).
char_list([]) --> [].

%------------------------------------------------------------------------------------

alphanum_char(0'a).
alphanum_char(0'à).
alphanum_char(0'á).
alphanum_char(0'â).
alphanum_char(0'ä).
alphanum_char(0'å).
alphanum_char(0'b).
alphanum_char(0'c).
alphanum_char(0'ç).
alphanum_char(0'd).
alphanum_char(0'ð).
alphanum_char(0'þ).
alphanum_char(0'Þ).
alphanum_char(0'e).
alphanum_char(0'é).
alphanum_char(0'è).
alphanum_char(0'ê).
alphanum_char(0'ë).
alphanum_char(0'f).
alphanum_char(0'g).
alphanum_char(0'h).
alphanum_char(0'i).
alphanum_char(0'í).
alphanum_char(0'ì).
alphanum_char(0'î).
alphanum_char(0'ï).
alphanum_char(0'j).
alphanum_char(0'k).
alphanum_char(0'l).
alphanum_char(0'm).
alphanum_char(0'n).
alphanum_char(0'ñ).
alphanum_char(0'o).
alphanum_char(0'ò).
alphanum_char(0'ó).
alphanum_char(0'ô).
alphanum_char(0'ö).
alphanum_char(0'ø).
alphanum_char(0'p).
alphanum_char(0'q).
alphanum_char(0'r).
alphanum_char(0's).
alphanum_char(0'ß).
alphanum_char(0't).
alphanum_char(0'u).
alphanum_char(0'ù).
alphanum_char(0'ú).
alphanum_char(0'ü).
alphanum_char(0'û).
alphanum_char(0'v).
alphanum_char(0'w).
alphanum_char(0'x).
alphanum_char(0'y).
alphanum_char(0'ý).
alphanum_char(0'z).
alphanum_char(0'A).
alphanum_char(0'À).
alphanum_char(0'Á).
alphanum_char(0'Ä).
alphanum_char(0'Â).
alphanum_char(0'Å).
alphanum_char(0'B).
alphanum_char(0'C).
alphanum_char(0'Ç).
alphanum_char(0'D).
alphanum_char(0'E).
alphanum_char(0'É).
alphanum_char(0'È).
alphanum_char(0'Ê).
alphanum_char(0'Ë).
alphanum_char(0'F).
alphanum_char(0'G).
alphanum_char(0'H).
alphanum_char(0'I).
alphanum_char(0'Í).
alphanum_char(0'Ì).
alphanum_char(0'Î).
alphanum_char(0'Ï).
alphanum_char(0'J).
alphanum_char(0'K).
alphanum_char(0'L).
alphanum_char(0'M).
alphanum_char(0'N).
alphanum_char(0'Ñ).
alphanum_char(0'O).
alphanum_char(0'Ô).
alphanum_char(0'Ò).
alphanum_char(0'Ó).
alphanum_char(0'Ö).
alphanum_char(0'Ø).
alphanum_char(0'P).
alphanum_char(0'Q).
alphanum_char(0'R).
alphanum_char(0'S).
alphanum_char(0'T).
alphanum_char(0'U).
alphanum_char(0'Ù).
alphanum_char(0'Ú).
alphanum_char(0'Ü).
alphanum_char(0'Û).
alphanum_char(0'V).
alphanum_char(0'W).
alphanum_char(0'X).
alphanum_char(0'Y).
alphanum_char(0'Ý).
alphanum_char(0'Z).
alphanum_char(0'1).
alphanum_char(0'2).
alphanum_char(0'3).
alphanum_char(0'4).
alphanum_char(0'5).
alphanum_char(0'6).
alphanum_char(0'7).
alphanum_char(0'8).
alphanum_char(0'9).
alphanum_char(0'0).
alphanum_char(0'_).
alphanum_char(0'-).
% Following required for Nuance phone set...
alphanum_char(0'&).
alphanum_char(0'\').
alphanum_char(0'*).
alphanum_char(0'^).
alphanum_char(0'~).
alphanum_char(0'@).
alphanum_char(0'?).

% change by peter ljunglöf, 2008-01-14
% this predicate is imported from PrologLib/utilities.pl:
% digit_char(0'1).
% digit_char(0'2).
% digit_char(0'3).
% digit_char(0'4).
% digit_char(0'5).
% digit_char(0'6).
% digit_char(0'7).
% digit_char(0'8).
% digit_char(0'9).
% digit_char(0'0).

white_space_char(0' ).

any_whitespace_char(0' ).
any_whitespace_char(0'\n).
any_whitespace_char(0'\t).

asterisk_char(0'*).

space_char(0' ).

equals_char(0'=).

char_for_space_word_list(X) :-
	integer(X),
	\+ bad_char_for_space_word_list(X).

bad_char_for_space_word_list(0'<).
bad_char_for_space_word_list(0'>).
bad_char_for_space_word_list(0'[).
bad_char_for_space_word_list(0']).
bad_char_for_space_word_list(0'{).
bad_char_for_space_word_list(0'}).
bad_char_for_space_word_list(0'=).

non_space_or_paren_char(X) :-
	\+ space_or_paren_char(X).

space_or_paren_char(X) :-
	space_char(X),
	!.
space_or_paren_char(X) :-
	parenthesis_char(X),
	!.

nonspace_char(X) :-
	\+ space_char(X).

non_whitespace_char(X) :-
	\+ standard_white_space_char(X).

non_equals_char(X) :-
	\+ equals_char(X).

parenthesis_char(0'().
parenthesis_char(0')).

newline_char(0'\n).

tab_char(0'\t).

%------------------------------------------------------------------------------------

make_batchrec_tmp_file(FileName, Directory, FullFileName) :-
	absolute_file_name(Directory, AbsDirectory),
	append_atoms([AbsDirectory, FileName], 0'/, FullFileName).

