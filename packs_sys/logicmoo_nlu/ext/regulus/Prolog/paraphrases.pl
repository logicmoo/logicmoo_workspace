% Top-level for translation

:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(paraphrases,
	[update_paraphrase_file_from_out_of_coverage_file/2,
	 check_paraphrase_file/1,
	 load_paraphrase_file_if_defined/0,
	 paraphrase_for_sentence_atom/2,
	 paraphrase_for_sentence_words/2
	]).

:- use_module('$REGULUS/Prolog/regulus_utilities').

:- use_module('$REGULUS/PrologLib/batchrec_tools').
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

update_paraphrase_file_from_out_of_coverage_file(OutOfCoverageFile, ParaphraseFile) :-
	safe_absolute_file_name(OutOfCoverageFile, AbsOutOfCoverageFile),
	safe_absolute_file_name(ParaphraseFile, AbsParaphraseFile),

	parse_transcriptions_file_to_list(AbsOutOfCoverageFile, OOCList),
	length(OOCList, NOOC),
	format('~N--- Read file (~d records) ~w~n', [NOOC, AbsOutOfCoverageFile]),
	
	(   safe_file_exists(AbsParaphraseFile) ->
	    prolog_file_to_list(AbsParaphraseFile, Paraphrases0),
	    length(Paraphrases0, NParaphrases),
	    format('~N--- Read file (~d records) ~w~n', [NParaphrases, AbsParaphraseFile])
	;
	    format('~N--- Paraphrase file ~w not found, creating new one~n', [AbsParaphraseFile]),
	    Paraphrases0 = []
	),
	remove_dummy_elements_from_paraphrase_list(Paraphrases0, Paraphrases),

	new_dummy_elements_for_paraphrase_list(OOCList, Paraphrases, NewDummyList0),
	sort(NewDummyList0, NewDummyList),
	append(Paraphrases, NewDummyList, NewParaphrases),
	length(NewDummyList, NNew),
	list_to_prolog_file(NewParaphrases, AbsParaphraseFile),
	format('~N--- Written file (~d new dummy records) ~w~n', [NNew, AbsParaphraseFile]),
	!.
update_paraphrase_file_from_out_of_coverage_file(OutOfCoverageFile, ParaphraseFile) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [update_paraphrase_file_from_out_of_coverage_file(OutOfCoverageFile, ParaphraseFile)]),
	fail.

%---------------------------------------------------------------

check_paraphrase_file(ParaphraseFile) :-
	safe_absolute_file_name(ParaphraseFile, AbsParaphraseFile),
	(   safe_file_exists(AbsParaphraseFile) ->
	    prolog_file_to_list(AbsParaphraseFile, Paraphrases),
	    length(Paraphrases, NParaphrases),
	    format('~N--- Read file (~d records) ~w~n', [NParaphrases, AbsParaphraseFile])
	;
	    format('~N--- Paraphrase file ~w not found, creating new one~n', [AbsParaphraseFile]),
	    Paraphrases = []
	),
	check_paraphrase_list(Paraphrases, TaggedParaphrases),
	(   member(bad-_, TaggedParaphrases) ->
	    write_checked_paraphrase_file(AbsParaphraseFile, TaggedParaphrases)
	;
	    format('~N--- All paraphrases are in coverage~n', [])
	).

check_paraphrase_list([], []).
check_paraphrase_list([F | R], [F1 | R1]) :-
	check_paraphrase_item(F, F1),
	format('.', []),
	flush_output(user),
	!,
	check_paraphrase_list(R, R1).

check_paraphrase_item(paraphrase(Transcript, ParaphraseAtom), GoodOrBad-paraphrase(Transcript, ParaphraseAtom)) :-
	(   ParaphraseAtom = '' ->
	    GoodOrBad = good
	;
	    user:sent_atom_is_in_coverage_according_to_current_parser(ParaphraseAtom) ->
	    GoodOrBad = good
	;
	    GoodOrBad = bad
	),
	!.
check_paraphrase_item(F, F1) :-
	format2error('~N*** Error: bad call: ~w~n', [check_paraphrase_item(F, F1)]),
	fail.

write_checked_paraphrase_file(AbsParaphraseFile, TaggedParaphrases) :-
	open(AbsParaphraseFile, write, S),
	write_checked_paraphrase_list(TaggedParaphrases, S, 0-N),
	close(S),
	format('~N--- Checked paraphrase file (~d paraphrases marked as out of coverage) ~w~n', [N, AbsParaphraseFile]),
	!.

write_checked_paraphrase_list([], _S, N-N).
write_checked_paraphrase_list([F | R], S, In-Out) :-
	write_checked_paraphrase_item(F, S, In-Next),
	!,
	write_checked_paraphrase_list(R, S, Next-Out).

write_checked_paraphrase_item(GoodOrBad-paraphrase(Transcript, ParaphraseAtom), S, In-Out) :-
	format(S, '~N~q.', [paraphrase(Transcript, ParaphraseAtom)]),
	(   GoodOrBad = good ->
	    format(S, '~n', []),
	    Out = In
	;
	    format(S, ' % Paraphrase is out of coverage~n', []),
	    Out is In + 1
	),
	!.
write_checked_paraphrase_item(Other, S, InOut) :-
	format2error('~N*** Error: bad call: ~w~n', [write_checked_paraphrase_item(Other, S, InOut)]),
	fail.
	
%---------------------------------------------------------------

remove_dummy_elements_from_paraphrase_list([], []).
remove_dummy_elements_from_paraphrase_list([F | R], R1) :-
	F = paraphrase(_, ''),
	!,
	remove_dummy_elements_from_paraphrase_list(R, R1).
remove_dummy_elements_from_paraphrase_list([F | R], [F | R1]) :-
	!,
	remove_dummy_elements_from_paraphrase_list(R, R1).

new_dummy_elements_for_paraphrase_list([], _Paraphrases, []).
new_dummy_elements_for_paraphrase_list([F | R], Paraphrases, [F1 | R1]) :-
	new_dummy_element_for_paraphrase_list(F, Paraphrases, F1),
	!,
	new_dummy_elements_for_paraphrase_list(R, Paraphrases, R1).
new_dummy_elements_for_paraphrase_list([_F | R], Paraphrases, R1) :-
	!,
	new_dummy_elements_for_paraphrase_list(R, Paraphrases, R1).

new_dummy_element_for_paraphrase_list(TranscriptionElement, Paraphrases, DummyElement) :-
	TranscriptionElement = transcription(_Wavfile, Words),
	join_with_spaces(Words, Transcription),
	\+ member(paraphrase(Transcription, _), Paraphrases),
	DummyElement = paraphrase(Transcription, ''),
	!.

%---------------------------------------------------------------

load_paraphrase_file_if_defined :-
	current_predicate(user:regulus_config/2),
	user:regulus_config(paraphrase_corpus, ParaphraseFile),
	safe_absolute_file_name(ParaphraseFile, AbsParaphraseFile),
	(   safe_file_exists(AbsParaphraseFile) ->
	    abolish_if_defined(paraphrase/2),
	    safe_compile(user, AbsParaphraseFile),
	    format('~N--- Loaded paraphrase file ~w~n', [AbsParaphraseFile])
	;
	    otherwise ->
	    format2error('~N*** Warning: unable to find paraphrase file ~w~n', [AbsParaphraseFile])
	),
	!.
load_paraphrase_file_if_defined.

%---------------------------------------------------------------

paraphrase_for_sentence_atom(Sent, Paraphrase) :-
	current_predicate(user:paraphrase/2),
	user:paraphrase(Sent, Paraphrase),
	Paraphrase \== ''.

%---------------------------------------------------------------

paraphrase_for_sentence_words(Words, ParaphraseWords) :-
	join_with_spaces(Words, Sent),
	paraphrase_for_sentence_atom(Sent, Paraphrase),
	split_atom_into_words(Paraphrase, ParaphraseWords).
