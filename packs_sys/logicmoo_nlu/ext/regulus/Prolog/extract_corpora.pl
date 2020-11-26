
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(extract_corpora,
	  [extract_corpus_file/3,
	   extract_corpus_file_clauses_only/4,
	   split_training_corpus_file_into_clauses_and_non_clauses/5,
	   sents_file_or_files_to_slm_training_file/2,
	   sents_file_to_slm_training_file/2,

	   translation_corpus_results_to_grammar_probs_data_file/3,
	   translation_corpus_results_to_sent_corpus_file/2]
      ).

:- use_module('$REGULUS/Prolog/regulus_read').
:- use_module('$REGULUS/Prolog/regulus_utilities').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%-----------------------------------------------------------------------------------

extract_corpus_file(InFile, DomainTags, OutFile) :-
	absolute_file_name(InFile, AbsInFile),
	absolute_file_name(OutFile, AbsOutFile),
	
	%prolog_file_to_list(AbsInFile, InList),
	read_corpus_file(AbsInFile, InList),
	length(InList, NIn),
	format('~N~n--- Read combined domains file ~w (~d entries)~n', [AbsInFile, NIn]),
	
	extract_corpus_list(InList, DomainTags, OutList),
	length(OutList, NOut),
	list_to_prolog_file(OutList, AbsOutFile),
	format('~N--- Extracted corpus file for tags ~w (~d entries), ~w~n', [DomainTags, NOut, AbsOutFile]),
	!.
extract_corpus_file(InFile, DomainTags, OutFile) :-
	format2error('~N*** Error: bad call: ~w~n', [extract_corpus_file(InFile, DomainTags, OutFile)]),
	fail.

%-----------------------------------------------------------------------------------

extract_corpus_list([], _DomainTags, []).
extract_corpus_list([F | R], DomainTags, [F1 | R1]) :-
	extract_corpus_element(F, DomainTags, F1),
	!,
	extract_corpus_list(R, DomainTags, R1).
extract_corpus_list([_F | R], DomainTags, R1) :-
	!,
	extract_corpus_list(R, DomainTags, R1).

extract_corpus_element(sent(Words, Domains, _Constraints), DomainTags, sent(Words)) :-
	member(Domain, Domains),
	member(Domain, DomainTags),
	!.
extract_corpus_element(sent(Words, Domains), DomainTags, Result) :-
	extract_corpus_element(sent(Words, Domains, _Constraints), DomainTags, Result),
	!.
extract_corpus_element(sent(Words), DomainTags, Result) :-
	extract_corpus_element(sent(Words, [default], _Constraints), DomainTags, Result),
	!.
extract_corpus_element(sent(_Words, _Domains, _Constraints), _DomainTags, _Out) :-
	!,
	fail.
extract_corpus_element(sent(_Words, _Domains), _DomainTags, _Out) :-
	!,
	fail.
extract_corpus_element(sent(_Words), _DomainTags, _Out) :-
	!,
	fail.
extract_corpus_element(X, DomainTags, Y) :-
	format2error('~N~n*** Error: bad call: ~w~n', [extract_corpus_element(X, DomainTags, Y)]),
	fail.

%-----------------------------------------------------------------------------------

extract_corpus_file_clauses_only(InFile, Tags, ClauseElements, OutFile) :-
	absolute_file_name(InFile, AbsInFile),
	absolute_file_name(OutFile, AbsOutFile),

	prolog_file_to_list(AbsInFile, InList),
	length(InList, InN),
	format('~N~n--- Extracting for tags: ~w and clause elements: ~w~n', [Tags, ClauseElements]),
	format('~N--- Read file (~d entries) ~w~n', [InN, AbsInFile]),

	extract_clauses_list(InList, Tags, ClauseElements, OutList),
	length(OutList, OutN),

	list_to_prolog_file(OutList, AbsOutFile),
	format('~N--- Written file (~d entries) ~w~n', [OutN, AbsOutFile]).

extract_clauses_list([], _Tags, _ClauseElements, []).
extract_clauses_list([F | R], Tags, ClauseElements, [F1 | R1]) :-
	extract_clause(F, Tags, ClauseElements, F1),
	!,
	extract_clauses_list(R, Tags, ClauseElements, R1).
extract_clauses_list([_F | R], Tags, ClauseElements, R1) :-
	!,
	extract_clauses_list(R, Tags, ClauseElements, R1).

/*

example('.MAIN',phrase('.MAIN',line_info(207,500-505,'c:/home/speech/regulus/grammar/general_eng.regulus'),phrase(top,line_info(209,511-517,'c:/home/speech/regulus/grammar/general_eng.regulus'),(phrase(utterance_intro,line_info(211,525-527,'c:/home/speech/regulus/grammar/general_eng.regulus'),empty_constituent),phrase(utterance,line_info(223,596-601,'c:/home/speech/regulus/grammar/general_eng.regulus'),phrase(np,line_info(341,1935-1950,'c:/home/speech/regulus/grammar/general_eng.regulus'),(phrase(np,line_info(328,1820-1828,'c:/home/speech/regulus/grammar/general_eng.regulus'),phrase(nbar,line_info(344,1975-1985,'c:/home/speech/regulus/grammar/general_eng.regulus'),phrase(n,line_info(390,682-682,'c:/home/speech/speechtranslation/medslt2/eng/regulus/med_lex.regulus'),lex(coffee)))),phrase(post_mods,line_info(286,1379-1385,'c:/home/speech/regulus/grammar/general_eng.regulus'),empty_constituent)))),phrase(utterance_coda,line_info(217,556-558,'c:/home/speech/regulus/grammar/general_eng.regulus'),empty_constituent)))),[[utterance_type,phrase],[cause,coffee]],[coffee],[abdominal_pain,default]).

*/

extract_clause(example(_Grammar, _Tree, LF, Words, TagsForExample),
	       Tags, ClauseElements, 
	       sent(WordsAtom)) :-
	member(Tag, TagsForExample),
	member(Tag, Tags),
	
	member(ClauseElement, ClauseElements),
	member(ClauseElement, LF),

	join_with_spaces(Words, WordsAtom),
	!.

%-----------------------------------------------------------------------------------

split_training_corpus_file_into_clauses_and_non_clauses(InFile, TreebankFile, ClauseElements,
							ClausesFile, NonClausesFile) :-
	absolute_file_name(InFile, AbsInFile),
	absolute_file_name(TreebankFile, AbsTreebankFile),
	absolute_file_name(ClausesFile, AbsClausesFile),
	absolute_file_name(NonClausesFile, AbsNonClausesFile),
	
	%prolog_file_to_list(AbsInFile, InList),
	read_corpus_file(AbsInFile, InList),
	length(InList, NInList),
	format('~N--- Read corpus file (~d entries) ~w~n', [NInList, AbsInFile]),
	       
	prolog_file_to_list(AbsTreebankFile, TreebankList),
	length(TreebankList, NTreebankList),
	format('~N--- Read treebank file (~d entries) ~w~n', [NTreebankList, AbsTreebankFile]),

	(   NInList = NTreebankList ->
	    true
	;
	    format2error('~N*** Error: corpus file and treebank file have different numbers of elements~n', []),
	    fail
	),

	split_training_corpus_list(InList, TreebankList, ClauseElements, ClausesList, NonClausesList),
	length(ClausesList, NClauses),
	length(NonClausesList, NNonClauses),

	list_to_prolog_file(ClausesList, AbsClausesFile),
	format('~N--- Written file (~d entries) ~w~n', [NClauses, AbsClausesFile]),

	list_to_prolog_file(NonClausesList, AbsNonClausesFile),
	format('~N--- Written file (~d entries) ~w~n', [NNonClauses, AbsNonClausesFile]),
	!.
split_training_corpus_file_into_clauses_and_non_clauses(InFile, TreebankFile, ClauseElements,
							ClausesFile, NonClausesFile) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [split_training_corpus_file_into_clauses_and_non_clauses(InFile, TreebankFile, ClauseElements,
									ClausesFile, NonClausesFile)]),
	fail.

split_training_corpus_list([], [], _ClauseElements, [], []).
split_training_corpus_list([F | R], [FTree | RTree], ClauseElements, ClausesList, NonClausesList) :-
	(   tree_record_is_for_clause(FTree, ClauseElements) ->
	    
	    ClausesList = [F | RClausesList],
	    NonClausesList = RNonClausesList ;

	    ClausesList = RClausesList,
	    NonClausesList = [F | RNonClausesList]
	),
	!,
	split_training_corpus_list(R, RTree, ClauseElements, RClausesList, RNonClausesList).

tree_record_is_for_clause(TreeRecord, ClauseElements) :-
	TreeRecord = example(_Grammar, _Tree, LF, _Words, _TagsForExample),
	
	member(ClauseElement, ClauseElements),
	member(ClauseElement, LF),
	!.

%-----------------------------------------------------------------------------------

sents_file_to_slm_training_file(InFile, OutFile) :-
	sents_file_or_files_to_slm_training_file([InFile], OutFile).

sents_file_or_files_to_slm_training_file(InFiles, OutFile) :-
	(   is_list(InFiles) ->
	    InFiles1 = InFiles
	;
	    InFiles1 = [InFiles]
	),
	
	sents_files_to_slm_training_list(InFiles1, []-InList),
	absolute_file_name(OutFile, AbsOutFile),
	sents_list_to_plain_atoms_list(InList, OutList),
	length(OutList, NOut),

	write_atom_list_to_file(OutList, AbsOutFile),
	format('~N--- Written SLM training file ~w (~d entries)~n', [AbsOutFile, NOut]),
	!.

sents_files_to_slm_training_list([], List-List).
sents_files_to_slm_training_list([F | R], ListIn-ListOut) :-
	absolute_file_name(F, AbsF),
	
	%prolog_file_to_list(AbsF, List),
	read_corpus_file(AbsF, List),
	length(List, N),
	format('~N~n--- Read sents file ~w (~d entries)~n', [AbsF, N]),
	append(ListIn, List, ListNext),
	!,
	sents_files_to_slm_training_list(R, ListNext-ListOut).

sents_list_to_plain_atoms_list([], []) :-
	!.
sents_list_to_plain_atoms_list([F | R], [F1 | R1]) :-
	sent_item_to_plain_atom(F, F1),
	!,
	sents_list_to_plain_atoms_list(R, R1).
sents_list_to_plain_atoms_list([_F | R], R1) :-
	!,
	sents_list_to_plain_atoms_list(R, R1).

sent_item_to_plain_atom(Record, S) :-
	\+ fake_sent_record(Record),
	functor(Record, sent, _),
	arg(1, Record, S),
	!.

%-----------------------------------------------------------------------------------

translation_corpus_results_to_grammar_probs_data_file(ResultsFile, NuanceGrammar, GrammarProbsFile) :-
	safe_absolute_file_name(ResultsFile, AbsResultsFile),
	safe_absolute_file_name(GrammarProbsFile, AbsGrammarProbsFile),
	prolog_file_to_list(AbsResultsFile, InList),
	length(InList, InN),
	format('~N--- Read translation results file (~d records) ~w~n', [InN, AbsResultsFile]),
	translation_results_to_grammar_probs_data(InList, NuanceGrammar, OutList),
	length(OutList, OutN),
	write_atom_list_to_file(OutList, AbsGrammarProbsFile),
	format('~N--- Written grammar probs training file (~d records) ~w~n', [OutN, AbsGrammarProbsFile]),
	!.

translation_results_to_grammar_probs_data([], _NuanceGrammar, []) :-
	!.
translation_results_to_grammar_probs_data([F | R], NuanceGrammar, [F1 | R1]) :-
	translation_result_to_grammar_probs_data_item(F, NuanceGrammar, F1),
	!,
	translation_results_to_grammar_probs_data(R, NuanceGrammar, R1).
translation_results_to_grammar_probs_data([_F | R], NuanceGrammar, R1) :-
	!,
	translation_results_to_grammar_probs_data(R, NuanceGrammar, R1).

translation_result_to_grammar_probs_data_item(TranslationRecord, NuanceGrammar, GrammarProbsItem) :-
	TranslationRecord = translation(_Source, Target, _Stats, Judgement),
	member(Judgement, [good, ok]),
	format_to_atom('~w ~w', [NuanceGrammar, Target], GrammarProbsItem),
	!.

%-----------------------------------------------------------------------------------

translation_corpus_results_to_sent_corpus_file(ResultsFile, SentCorpusFile) :-
	safe_absolute_file_name(ResultsFile, AbsResultsFile),
	safe_absolute_file_name(SentCorpusFile, AbsSentCorpusFile),
	prolog_file_to_list(AbsResultsFile, InList),
	length(InList, InN),
	format('~N--- Read translation results file (~d records) ~w~n', [InN, AbsResultsFile]),
	translation_results_to_sent_corpus_list(InList, OutList),
	length(OutList, OutN),
	list_to_prolog_file(OutList, AbsSentCorpusFile),
	format('~N--- Written sent corpus file (~d records) ~w~n', [OutN, AbsSentCorpusFile]),
	!.

translation_results_to_sent_corpus_list([], []) :-
	!.
translation_results_to_sent_corpus_list([F | R], [F1 | R1]) :-
	translation_result_to_sent_corpus_item(F, F1),
	!,
	translation_results_to_sent_corpus_list(R, R1).
translation_results_to_sent_corpus_list([_F | R], R1) :-
	!,
	translation_results_to_sent_corpus_list(R, R1).

translation_result_to_sent_corpus_item(TranslationRecord, SentCorpusItem) :-
	TranslationRecord = translation(_Source, Target, _Stats, Judgement),
	member(Judgement, [good, ok]),
	SentCorpusItem = sent(Target),
	!.
