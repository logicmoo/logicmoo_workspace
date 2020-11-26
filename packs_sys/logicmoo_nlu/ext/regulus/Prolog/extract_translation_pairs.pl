
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(extract_translation_pairs,
	  [extract_translation_pairs/2,
	   extract_translation_pairs/3,
	   extract_failed_translations/2,
	   full_translation_pairs_to_smt_data/3,
	   full_translation_pairs_to_smt_data/4,
	   full_translation_pairs_to_smt_data/7,
	   full_translation_pairs_to_smt_data/8,
	   full_translation_pairs_to_smt_data/11,
	   compare_translation_results/6,
	   make_blank_bad_sent_file/2,

	   remove_duplicate_entries_in_smt_data/4]
	 ).

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/PrologLib/utilities').
:- use_module(library(lists)).

/*

extract_translation_pairs(+InFile, +OutFile, Mode)

Mode is either 'source_and_target' or 'full_info'
InFile is a file produced by batch Regulus translation
OutFile is a compacted version containing records whose form is determined by Mode:
  - If Mode = source_and_target, then translation(Source, Target)
  - If Mode = full_info, then translation(Source, InterlinguaSurface, Target)

%----------------------------------------------------------------------

extract_translation_pairs(+InFile, +OutFile)

Equivalent to extract_translation_pairs(+InFile, +OutFile, source_and_target)

%----------------------------------------------------------------------

extract_failed_translations(+InFile, +OutFile)

InFile is a file produced by batch Regulus translation
OutFile is a Unicode file consisting of the source examples which failed to produce a translation.

%----------------------------------------------------------------------

full_translation_pairs_to_smt_data(+TranslationPairFile,
				   +SourceFile, +TargetFile,
				   +IncludeBadTranslations)

Output files are one sentence per line with each record split up into source and target.

IncludeBadTranslations should be either 'include_bad_translations' or 'dont_include_bad_translations'

%----------------------------------------------------------------------

full_translation_pairs_to_smt_data(+TranslationPairFile,
				   +SourceTrainingFile, +InterlinguaTrainingFile, +TargetTrainingFile,
				   +SourceDevFile, +InterlinguaDevFile, +TargetDevFile,
				   +SourceTestFile, +InterlinguaTestFile, +TargetTestFile,
				   [+ProportionDev, +ProportionTest])

TranslationPairFile is an output file produced by running
extract_translation_pairs with Mode = full_info. The material is
divided up ProportionDev to dev, ProportionTest to test, rest to
training. Output files are one sentence per line with each record
split up into source and target.

%----------------------------------------------------------------------

full_translation_pairs_to_smt_data(+TranslationPairFile,
				   +SourceTrainingFile, +TargetTrainingFile,
				   +SourceDevFile, +TargetDevFile,
				   +SourceTestFile, +TargetTestFile,
				   [+ProportionDev, +ProportionTest])

TranslationPairFile is an output file produced by running
extract_translation_pairs with Mode = full_info The material is
divided up ProportionDev to dev, ProportionTest to test, rest to
training. Output files are one sentence per line with each record
split up into source and target.

full_translation_pairs_to_smt_data(+TranslationPairFile,
				   +SourceTrainingFile, +TargetTrainingFile,
				   +SourceDevFile, +TargetDevFile,
				   +SourceTestFile, +TargetTestFile)

Equivalent to

full_translation_pairs_to_smt_data(+TranslationPairFile,
				   +SourceTrainingFile, +TargetTrainingFile,
				   +SourceDevFile, +TargetDevFile,
				   +SourceTestFile, +TargetTestFile,
				   [0.05, 0.1])


%----------------------------------------------------------------------

remove_duplicate_entries_in_smt_data(+OldSourceFileList,
				     +OldTargetFileList,
				     +NewSourceFileList,
				     +NewTargetFileList)

Each list should contain the same number of files. The "new" versions will be
the "old" ones, but with duplicates removed. If a pair of entries has occurred
in ANY previous file, it is deleted.
				     
*/

%----------------------------------------------------------------------

extract_translation_pairs(InFile, OutFile) :-
	extract_translation_pairs(InFile, OutFile, source_and_target).	

extract_translation_pairs(InFile, OutFile, Mode) :-
	absolute_file_name(InFile, AbsInFile),
	absolute_file_name(OutFile, AbsOutFile),

	format('~N--- Reading file: ~w~n', [AbsInFile]),
	%prolog_file_to_list(AbsInFile, InList),
	read_translation_output_file(AbsInFile, InList),
	length(InList, NInList),
	format('~N--- Read file (~d records)~n', [NInList]),

	extract_translation_pairs1(InList, OutList, Mode),
	length(OutList, NOutList),

	list_to_prolog_file(OutList, OutFile),
	format('~N--- Written file (~d records): ~w~n', [NOutList, AbsOutFile]).
extract_translation_pairs(InFile, OutFile, Mode) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [extract_translation_pairs(InFile, OutFile, Mode)]),
	fail.

extract_translation_pairs1([], [], _Mode).
extract_translation_pairs1([F | R], [F1 | R1], Mode) :-
	extract_translation_record(F, F1, Mode),
	!,
	extract_translation_pairs1(R, R1, Mode).
extract_translation_pairs1([_F | R], R1, Mode) :-
	!,
	extract_translation_pairs1(R, R1, Mode).

extract_translation_record(Record, Record1, source_and_target) :-
	Record = translation(Source, Target, _Info, Judgement),
	\+ member(Judgement, [error, bad]),
	Target \== no_translation_due_to_bad_recognition,
	Record1 = translation(Source, Target),
	!.
extract_translation_record(Record, Record1, full_info) :-
	Record = translation(Source, Target, Info, _Judgement),
	(   member(interlingua_surface=InterlinguaSurface, Info) ->
	    true
	;
	    InterlinguaSurface = no_interlingua
	),
	Record1 = translation(Source, InterlinguaSurface, Target),
	!.

%----------------------------------------------------------------------

extract_failed_translations(InFile, OutFile) :-
	absolute_file_name(InFile, AbsInFile),
	absolute_file_name(OutFile, AbsOutFile),

	format('~N--- Reading file: ~w~n', [AbsInFile]),
	%prolog_file_to_list(AbsInFile, InList),
	read_translation_output_file(AbsInFile, InList),
	length(InList, NInList),
	format('~N--- Read file (~d records)~n', [NInList]),

	extract_failed_translations1(InList, OutList),
	length(OutList, NOutList),

	write_atom_list_to_unicode_file(OutList, AbsOutFile),
	format('~N--- Written file (~d sentences): ~w~n', [NOutList, AbsOutFile]).
extract_failed_translations(InFile, OutFile) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [extract_failed_translations(InFile, OutFile)]),
	fail.

extract_failed_translations1([], []).
extract_failed_translations1([F | R], [F1 | R1]) :-
	extract_bad_translation_source(F, F1),
	!,
	extract_failed_translations1(R, R1).
extract_failed_translations1([_F | R], R1) :-
	!,
	extract_failed_translations1(R, R1).

extract_bad_translation_source(Record, Source) :-
	Record = translation(Source, Target, _Info, Judgement),
	member(Judgement, [error, bad]),
	Target \== no_translation_due_to_bad_recognition,
	!.

%----------------------------------------------------------------------

full_translation_pairs_to_smt_data(TranslationPairFile, SourceFile, TargetFile) :-
	full_translation_pairs_to_smt_data(TranslationPairFile, SourceFile, TargetFile, dont_include_failed_translations).

full_translation_pairs_to_smt_data(TranslationPairFile, SourceFile, TargetFile, IncludeFailedTranslations) :-
	safe_absolute_file_name(TranslationPairFile, AbsTranslationPairFile),
	
	format('~N--- Reading file: ~w~n', [AbsTranslationPairFile]),
	prolog_file_to_list(AbsTranslationPairFile, InList),
	length(InList, NInList),
	format('~N--- Read file (~d records): ~w~n', [NInList, AbsTranslationPairFile]),

	full_translation_pairs_list_to_smt_data(InList, SourceFile, TargetFile, IncludeFailedTranslations),
	!.
full_translation_pairs_to_smt_data(TranslationPairFile, SourceFile, TargetFile, IncludeFailedTranslations) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [full_translation_pairs_to_smt_data(TranslationPairFile, SourceFile, TargetFile, IncludeFailedTranslations)]),
	fail.

%----------------------------------------------------------------------

full_translation_pairs_to_smt_data(TranslationPairFile,
				   SourceTrainingFile, InterlinguaTrainingFile, TargetTrainingFile,
				   SourceDevFile, InterlinguaDevFile, TargetDevFile,
				   SourceTestFile, InterlinguaTestFile, TargetTestFile) :-
	full_translation_pairs_to_smt_data(TranslationPairFile,
					   SourceTrainingFile, InterlinguaTrainingFile, TargetTrainingFile,
					   SourceDevFile, InterlinguaDevFile, TargetDevFile,
					   SourceTestFile, InterlinguaTestFile, TargetTestFile,
					   [0.05, 0.1]).

full_translation_pairs_to_smt_data(TranslationPairFile,
				   SourceTrainingFile, InterlinguaTrainingFile, TargetTrainingFile,
				   SourceDevFile, InterlinguaDevFile, TargetDevFile,
				   SourceTestFile, InterlinguaTestFile, TargetTestFile,
				   [ProportionDev, ProportionTest]) :-
	safe_absolute_file_name(TranslationPairFile, AbsTranslationPairFile),
	
	format('~N--- Reading file: ~w~n', [AbsTranslationPairFile]),
	prolog_file_to_list(AbsTranslationPairFile, InList),
	length(InList, NInList),
	format('~N--- Read file (~d records): ~w~n', [NInList, AbsTranslationPairFile]),

	split_list_into_training_dev_and_test(InList, TrainingList, DevList, TestList, ProportionDev, ProportionTest),

	full_translation_pairs_list_to_smt_data(TrainingList, SourceTrainingFile, InterlinguaTrainingFile, TargetTrainingFile, dont_include_failed_translations),
	full_translation_pairs_list_to_smt_data(DevList, SourceDevFile, InterlinguaDevFile, TargetDevFile, dont_include_failed_translations),
	full_translation_pairs_list_to_smt_data(TestList, SourceTestFile, InterlinguaTestFile, TargetTestFile, dont_include_failed_translations),
	!.
full_translation_pairs_to_smt_data(TranslationPairFile,
				   SourceTrainingFile, InterlinguaTrainingFile, TargetTrainingFile,
				   SourceDevFile, InterlinguaDevFile, TargetDevFile,
				   SourceTestFile, InterlinguaTestFile, TargetTestFile,
				   DevAndTest) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [full_translation_pairs_to_smt_data(TranslationPairFile,
							 SourceTrainingFile, InterlinguaTrainingFile, TargetTrainingFile,
							 SourceDevFile, InterlinguaDevFile, TargetDevFile,
							 SourceTestFile, InterlinguaTestFile, TargetTestFile,
							 DevAndTest)]),
	fail.


full_translation_pairs_list_to_smt_data(InList, SourceFile, InterlinguaFile, TargetFile, IncludeFailedTranslations) :-
	safe_absolute_file_name(SourceFile, AbsSourceFile),
	safe_absolute_file_name(InterlinguaFile, AbsInterlinguaFile),
	safe_absolute_file_name(TargetFile, AbsTargetFile),

	full_translation_pair_list_to_source_interlingua_and_target_lists(InList, SourceList, InterlinguaList, TargetList, IncludeFailedTranslations),
	length(SourceList, NSourceList),
	length(InterlinguaList, NInterlinguaList),
	length(TargetList, NTargetList),
	
	write_atom_list_to_unicode_file(SourceList, AbsSourceFile),
	format('~N--- Written source file (~d records): ~w~n', [NSourceList, AbsSourceFile]),
	
	write_atom_list_to_unicode_file(InterlinguaList, AbsInterlinguaFile),
	format('~N--- Written interlingua file (~d records): ~w~n', [NInterlinguaList, AbsInterlinguaFile]),
	
	write_atom_list_to_unicode_file(TargetList, AbsTargetFile),
	format('~N--- Written target file (~d records): ~w~n', [NTargetList, AbsTargetFile]),
	!.
full_translation_pairs_list_to_smt_data(InList, SourceFile, InterlinguaFile, TargetFile, IncludeFailedTranslations) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [full_translation_pairs_list_to_smt_data(InList, SourceFile, InterlinguaFile, TargetFile, IncludeFailedTranslations)]),
	fail.

full_translation_pair_list_to_source_interlingua_and_target_lists([], [], [], [], _IncludeFailedTranslations).
full_translation_pair_list_to_source_interlingua_and_target_lists([F | R], [F1 | R1], [F2 | R2], [F3 | R3], IncludeFailedTranslations) :-
	full_translation_pair_to_source_interlingua_and_target(F, F1, F2, F3, IncludeFailedTranslations),
	!,
	full_translation_pair_list_to_source_interlingua_and_target_lists(R, R1, R2, R3, IncludeFailedTranslations).
full_translation_pair_list_to_source_interlingua_and_target_lists([_F | R], R1, R2, R3, IncludeFailedTranslations) :-
	!,
	full_translation_pair_list_to_source_interlingua_and_target_lists(R, R1, R2, R3, IncludeFailedTranslations).

full_translation_pair_to_source_interlingua_and_target(translation(Source, Interlingua, Target0),
						       Source, Interlingua, Target,
						       IncludeFailedTranslations) :-
	!,
	(   atom(Target0) ->
	    Target = Target0
	;
	    IncludeFailedTranslations = include_failed_translations ->
	    Target = error
	).
full_translation_pair_to_source_interlingua_and_target(F, F1, F2, F3, F4) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [full_translation_pair_to_source_and_target(F, F1, F2, F3, F4)]),
	fail.

%----------------------------------------------------------------------

full_translation_pairs_to_smt_data(TranslationPairFile,
				   SourceTrainingFile, TargetTrainingFile,
				   SourceDevFile, TargetDevFile,
				   SourceTestFile, TargetTestFile) :-
	full_translation_pairs_to_smt_data(TranslationPairFile,
					   SourceTrainingFile, TargetTrainingFile,
					   SourceDevFile, TargetDevFile,
					   SourceTestFile, TargetTestFile,
					   [0.05, 0.1]).

full_translation_pairs_to_smt_data(TranslationPairFile,
				   SourceTrainingFile, TargetTrainingFile,
				   SourceDevFile, TargetDevFile,
				   SourceTestFile, TargetTestFile,
				   [ProportionDev, ProportionTest]) :-
	safe_absolute_file_name(TranslationPairFile, AbsTranslationPairFile),
	
	format('~N--- Reading file: ~w~n', [AbsTranslationPairFile]),
	prolog_file_to_list(AbsTranslationPairFile, InList),
	length(InList, NInList),
	format('~N--- Read file (~d records): ~w~n', [NInList, AbsTranslationPairFile]),

	split_list_into_training_dev_and_test(InList, TrainingList, DevList, TestList, ProportionDev, ProportionTest),

	full_translation_pairs_list_to_smt_data(TrainingList, SourceTrainingFile, TargetTrainingFile),
	full_translation_pairs_list_to_smt_data(DevList, SourceDevFile, TargetDevFile),
	full_translation_pairs_list_to_smt_data(TestList, SourceTestFile, TargetTestFile),
	!.
full_translation_pairs_to_smt_data(TranslationPairFile,
				   SourceTrainingFile, TargetTrainingFile,
				   SourceDevFile, TargetDevFile,
				   SourceTestFile, TargetTestFile,
				   DevAndTest) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [full_translation_pairs_to_smt_data(TranslationPairFile,
							 SourceTrainingFile, TargetTrainingFile,
							 SourceDevFile, TargetDevFile,
							 SourceTestFile,  TargetTestFile,
							 DevAndTest)]),
	fail.


split_list_into_training_dev_and_test(InList, TrainingList, DevList, TestList, ProportionDev, ProportionTest) :-
	length(InList, NInList),
	NDevList is integer(NInList * ProportionDev),
	NTestList is integer(NInList * ProportionTest),
	split_off_firstn(InList, NDevList, DevList, TestAndTrainingList),
	split_off_firstn(TestAndTrainingList, NTestList, TestList, TrainingList),
	!.
split_list_into_training_dev_and_test(InList, TrainingList, DevList, TestList, ProportionDev, ProportionTest) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [split_list_into_training_dev_and_test(InList, TrainingList, DevList, TestList, ProportionDev, ProportionTest)]),
	fail.

full_translation_pairs_list_to_smt_data(InList, SourceFile, TargetFile) :-
	full_translation_pairs_list_to_smt_data(InList, SourceFile, TargetFile, dont_include_failed_translations).

full_translation_pairs_list_to_smt_data(InList, SourceFile, TargetFile, IncludeFailedTranslations) :-
	safe_absolute_file_name(SourceFile, AbsSourceFile),
	safe_absolute_file_name(TargetFile, AbsTargetFile),

	full_translation_pair_list_to_source_and_target_lists(InList, SourceList, TargetList, IncludeFailedTranslations),
	length(SourceList, NSourceList),
	length(TargetList, NTargetList),
	
	write_atom_list_to_unicode_file(SourceList, AbsSourceFile),
	format('~N--- Written source file (~d records): ~w~n', [NSourceList, AbsSourceFile]),
	
	write_atom_list_to_unicode_file(TargetList, AbsTargetFile),
	format('~N--- Written target file (~d records): ~w~n', [NTargetList, AbsTargetFile]),
	!.
full_translation_pairs_list_to_smt_data(InList, SourceFile, TargetFile, IncludeFailedTranslations) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [full_translation_pairs_list_to_smt_data(InList, SourceFile, TargetFile, IncludeFailedTranslations)]),
	fail.

full_translation_pair_list_to_source_and_target_lists([], [], [], _IncludeFailedTranslations).
full_translation_pair_list_to_source_and_target_lists([F | R], [F1 | R1], [F2 | R2], IncludeFailedTranslations) :-
	full_translation_pair_to_source_and_target(F, F1, F2, IncludeFailedTranslations),
	!,
	full_translation_pair_list_to_source_and_target_lists(R, R1, R2, IncludeFailedTranslations).
full_translation_pair_list_to_source_and_target_lists([_F | R], R1, R2, IncludeFailedTranslations) :-
	!,
	full_translation_pair_list_to_source_and_target_lists(R, R1, R2, IncludeFailedTranslations).

full_translation_pair_to_source_and_target(translation(Source, _Interlingua, Target0),
					   Source,
					   Target,
					   IncludeFailedTranslations) :-
	!,
	(   atom(Target0) ->
	    Target = Target0
	;
	    IncludeFailedTranslations = include_failed_translations ->
	    Target = error
	).
full_translation_pair_to_source_and_target(F, F1, F2) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [full_translation_pair_to_source_and_target(F, F1, F2)]),
	fail.

%----------------------------------------------------------------------

make_blank_bad_sent_file(InFile, OutputFile) :-
	read_unicode_file_to_atom_list_and_inform(InFile, InList),
	sent_list_to_blank_bad_sent_list(InList, OutList),
	length(OutList, N),
	safe_absolute_file_name(OutputFile, AbsOutputFile),
	list_to_prolog_file(OutList, AbsOutputFile),
	format('~N--- Written (~d records) ~w~n', [N, AbsOutputFile]),
	!.

sent_list_to_blank_bad_sent_list([], []).
sent_list_to_blank_bad_sent_list([F | R], [F1 | R1]) :-
	F1 = sentence_judgement(F, good, judge),
	!,
	sent_list_to_blank_bad_sent_list(R, R1).

%----------------------------------------------------------------------

:- dynamic training_example/1.

compare_translation_results(SourceTrainingFile,
			    JudgementFile,
			    SourceTestFile,
			    TargetTestFile,
			    SMTTargetFile,
			    OutputFile) :-

	init_compare_translation_results(JudgementFile, SourceTrainingFile, AssocIn),

	read_unicode_file_to_atom_list_and_inform(SourceTestFile, SourceTestList),
	read_unicode_file_to_atom_list_and_inform(TargetTestFile, TargetTestList),
	read_unicode_file_to_atom_list_and_inform(SMTTargetFile, SMTTargetList),

	compare_translation_results1(SourceTestList, TargetTestList, SMTTargetList, OutList, AssocIn-AssocOut),

	length(OutList, N),
	safe_absolute_file_name(OutputFile, AbsOutputFile),
	list_to_prolog_file(OutList, AbsOutputFile),
	format('~N--- Written (~d records) ~w~n', [N, AbsOutputFile]),

	assoc_generic_to_list(AssocOut, InfoList),

	format('~N--- Info:~n', []),
	prettyprint(InfoList),
	!.

compare_translation_results1([], [], [], [], AssocOut-AssocOut).
compare_translation_results1([F | R], [F1 | R1], [F2 | R2], [F3 | R3], AssocIn-AssocOut) :-
	compare_translation_results2(F, F1, F2, F3, AssocIn-AssocNext),
	!,
	compare_translation_results1(R, R1, R2, R3, AssocNext-AssocOut).

compare_translation_results2(Source, TargetRule, TargetSMT, Out, AssocIn-AssocOut) :-
	(   training_example(Source) ->
	    InTraining = yes
	;
	    InTraining = no
	),
	(   translations_match(TargetRule, TargetSMT) ->
	    RuleAndSMTMatch = yes
	;
	    RuleAndSMTMatch = no
	),
	(   ( InTraining = no, RuleAndSMTMatch = yes ) ->
	    RuleAndSMTMatchNotInTraining = yes
	;
	    RuleAndSMTMatchNotInTraining = no
	),
	(   good_example(Source) ->
	    GoodExample = yes
	;
	    GoodExample = no
	),
	
	Key = [good_example = GoodExample,
	       in_training = InTraining,
	       rule_and_smt_match = RuleAndSMTMatch],
	       
	inc_assoc_generic(AssocIn, Key, AssocOut),

	Out = translation(source=Source,
			  rule_target=TargetRule,
			  smt_target=TargetSMT,
			  good_example = GoodExample,
			  in_training = InTraining,
			  rule_and_smt_match = RuleAndSMTMatch),
	!.
compare_translation_results2(Source, TargetRule, TargetSMT, Out, Assoc) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [compare_translation_results2(Source, TargetRule, TargetSMT, Out, Assoc)]),
	fail.

init_compare_translation_results(JudgementFile, SourceTrainingFile, AssocInit) :-
	retractall(training_example(_)),
	read_unicode_file_to_atom_list_and_inform(SourceTrainingFile, List),
	store_training_examples(List),

	(   JudgementFile = none ->
	    true
	;
	    otherwise ->
	    compile(JudgementFile)
	),
	
	empty_assoc_generic(AssocInit),
	!.

store_training_examples([]).
store_training_examples([F | R]) :-
	assertz(training_example(F)),
	!,
	store_training_examples(R).

read_unicode_file_to_atom_list_and_inform(File, List) :-
	safe_absolute_file_name(File, AbsFile),
	read_unicode_file_to_atom_list(AbsFile, List),
	length(List, N),
	format('~N--- Read (~d records) ~w~n', [N, AbsFile]),
	!.

translations_match(Trans1, Trans2) :-
	Trans1 = Trans2,
	!.
translations_match(Trans1, Trans2) :-
	atom(Trans1),
	atom(Trans2),
	split_atom_into_words(Trans1, Trans1Words),
	split_atom_into_words(Trans2, Trans2Words),
	Trans1Words = Trans2Words,
	!.

good_example(Source) :-
	current_predicate(sentence_judgement/3),
	sentence_judgement(Source, good, _Judge).

%----------------------------------------------------------------------

read_translation_output_file(InFile, List) :-
	open(InFile, read, SIn),
	read_translation_output_stream(SIn, List-[], 0),
	close(SIn),
	!.
read_translation_output_file(InFile, List) :-
	format('~N*** Error: bad call: ~w~n', [read_translation_output_file(InFile, List)]),
	fail.

read_translation_output_stream(SIn, ListIn-ListOut, N) :-
	safe_read(SIn, Term),
	(   ( 0 is N mod 1000, N > 0 ) ->
	    
	    format(' ~d', [N]),
	    flush_output(user) ;
	    
	    true
	),
	N1 is N + 1,
	!,
	read_translation_output_stream1(Term, SIn, ListIn-ListOut, N1).

read_translation_output_stream1(Term, _SIn, ListIn-ListIn, _N) :-
	Term = end_of_file,
	!.
read_translation_output_stream1(Term, SIn, ListIn-ListOut, N) :-
	compact_translation_record(Term, Term1),
	ListIn = [Term1 | ListNext],
	!,
	read_translation_output_stream(SIn, ListNext-ListOut, N).

compact_translation_record(Term, Term1) :-
	Term = translation(Source, Target, Info, Judgement),
	(   member(interlingua_surface=InterlinguaSurface, Info) ->
	    Info1 = [interlingua_surface=InterlinguaSurface]
	;
	    Info1 = []
	),
	Term1 = translation(Source, Target, Info1, Judgement),
	!.
compact_translation_record(Term, Term).

%----------------------------------------------------------------------

remove_duplicate_entries_in_smt_data(OldSourceFileList, OldTargetFileList,
				     NewSourceFileList, NewTargetFileList) :-
	check_args_for_remove_duplicate_entries_in_smt_data(OldSourceFileList, OldTargetFileList,
							    NewSourceFileList, NewTargetFileList),
	init_remove_duplicate_entries_in_smt_data,
	remove_duplicate_entries_in_smt_data1(OldSourceFileList, OldTargetFileList,
					      NewSourceFileList, NewTargetFileList),
	!.
remove_duplicate_entries_in_smt_data(OldSourceFileList, OldTargetFileList,
				     NewSourceFileList, NewTargetFileList) :-
	format('~N*** Error: bad call: ~w~n',
	       [remove_duplicate_entries_in_smt_data(OldSourceFileList, OldTargetFileList,
						     NewSourceFileList, NewTargetFileList)]),
	fail.

:- dynamic already_seen_source_sent/1.

init_remove_duplicate_entries_in_smt_data :-
	retractall(already_seen_source_sent(_)).

check_args_for_remove_duplicate_entries_in_smt_data(OldSourceFileList, OldTargetFileList,
						    NewSourceFileList, NewTargetFileList) :-
	all_same_length_lists([OldSourceFileList, OldTargetFileList, NewSourceFileList, NewTargetFileList],
			      _Length),
	list_of_existing_files(OldSourceFileList),
	list_of_existing_files(OldTargetFileList).

all_same_length_lists([], _N).
all_same_length_lists([F | R], N) :-
	is_list(F),
	length(F, N),
	!,
	all_same_length_lists(R, N).

list_of_existing_files([]).
list_of_existing_files([F | R]) :-
	(   safe_file_exists(F) ->
	    list_of_existing_files(R)
	;
	    format('~N*** Error: unable to find file: ~w~n', [F]),
	    fail
	).

remove_duplicate_entries_in_smt_data1([], [], [], []).
remove_duplicate_entries_in_smt_data1([F | R], [F1 | R1], [F2 | R2], [F3 | R3]) :-
	remove_duplicate_entries_in_smt_data2(F, F1, F2, F3),
	!,
	remove_duplicate_entries_in_smt_data1(R, R1, R2, R3).

remove_duplicate_entries_in_smt_data2(OldSourceFile, OldTargetFile, NewSourceFile, NewTargetFile) :-
	read_unicode_file_to_atom_list_and_inform(OldSourceFile, OldSourceList),
	read_unicode_file_to_atom_list_and_inform(OldTargetFile, OldTargetList),
	
	remove_duplicate_entries_in_smt_data3(OldSourceList, OldTargetList, NewSourceList, NewTargetList),
	
	length(NewSourceList, NNewSourceList),
	length(NewTargetList, NNewTargetList),
	safe_absolute_file_name(NewSourceFile, AbsNewSourceFile),
	safe_absolute_file_name(NewTargetFile, AbsNewTargetFile),
	
	write_atom_list_to_unicode_file(NewSourceList, AbsNewSourceFile),
	format('~N--- Written source file (~d records): ~w~n', [NNewSourceList, AbsNewSourceFile]),
	
	write_atom_list_to_unicode_file(NewTargetList, AbsNewTargetFile),
	format('~N--- Written target file (~d records): ~w~n', [NNewTargetList, AbsNewTargetFile]),
	!.

remove_duplicate_entries_in_smt_data3([], [], [], []).
remove_duplicate_entries_in_smt_data3([Source | R], [Target | R1], [Source | R2], [Target | R3]) :-
	\+ already_seen_source_sent(Source),
	assertz(already_seen_source_sent(Source)),
	!,
	remove_duplicate_entries_in_smt_data3(R, R1, R2, R3).
remove_duplicate_entries_in_smt_data3([_Source | R], [_Target | R1], R2, R3) :-
	!,
	remove_duplicate_entries_in_smt_data3(R, R1, R2, R3).
