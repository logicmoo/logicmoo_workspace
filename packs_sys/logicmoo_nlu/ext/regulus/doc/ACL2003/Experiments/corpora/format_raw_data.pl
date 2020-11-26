


:- use_module(library(utilities)).
:- use_module(library(lists)).

format_raw_data :-
	format_raw_data('raw_training_data.txt', 'psa_data_full.pl').

format_raw_data(InFile, OutFile) :-
	read_file_to_atom_list(InFile, Atoms),
	atoms_to_sents(Atoms, Sents),
	list_to_prolog_file(Sents, OutFile).

atoms_to_sents([], []).
atoms_to_sents([F | R], Out) :-
	(   atom_to_sent(F, F1) ->
	    Out = [F1 | R1] ;
	    format('~NWarning: skipping record "~w"~n', [F]),
	    Out = R1
	),
	!,
	atoms_to_sents(R, R1).

atom_to_sent(Atom, sent(Sent)) :-
	split_atom_into_words(Atom, Words),
	Words = [_F | R],
	join_with_spaces(R, Sent).

