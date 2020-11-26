
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%---------------------------------------------------------------

:- module(make_prep_transforms,
	  [make_prep_list/2,
	   make_multiword_prep_list/2,
	   test_make_prep_list/1
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/CorpusTools/tokenize_sents').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

test_make_prep_list(preps) :-
	make_prep_list('$ACCEPT/MT/Europarl/Mmorph/raw_prepositions.pl',
		       '$ACCEPT/MT/Europarl/Mmorph/prepositions.pl').

test_make_prep_list(multiwords) :-	
	make_multiword_prep_list('$ACCEPT/MT/Europarl/Mmorph/raw_prepositions.pl',
				 '$ACCEPT/MT/Europarl/Mmorph/multiword_prepositions.pl').

%---------------------------------------------------------------

make_prep_list(RawFile, PrepListFile) :-
	init_make_prep_list(RawFile),
	make_prep_list1(PrepListFile).

make_prep_list1(File) :-
	safe_absolute_file_name(File, AbsFile),
	find_preps(Preps),
	write_out_transforms(Preps, AbsFile).

%---------------------------------------------------------------

make_multiword_prep_list(RawFile, PrepListFile) :-
	init_make_prep_list(RawFile),
	make_multiword_prep_list1(PrepListFile).

make_multiword_prep_list1(File) :-
	safe_absolute_file_name(File, AbsFile),
	find_multiword_preps(Preps),
	write_out_transforms(Preps, AbsFile).

%---------------------------------------------------------------

init_make_prep_list(File) :-
	safe_absolute_file_name(File, AbsFile),
	format('~N--- Loading preps file: ~w~n', [AbsFile]),
	compile(File).

%---------------------------------------------------------------

% mmorph_entry([surface='A_l\'instar_du',lemma='à_l\'instar_de',pos='Prep',number=[singular],tag=[prep],form=[surface]]).

find_preps(Entries) :-
	findall(prep(Surface),
		(   mmorph_entry([surface=Surface, lemma=_Lemma0, pos='Prep' | _OtherFeats])
		),
		Entries0
	       ),
	sort(Entries0, Entries).

find_multiword_preps(Entries) :-
	findall(prep(Surface, SurfaceAtom),
		(   mmorph_entry([surface=Surface0, lemma=_Lemma0, pos='Prep' | _OtherFeats]),
		    convert_underscores_to_spaces(Surface0, SurfaceAtom),
		    tokenize_sent(SurfaceAtom, Surface1),
		    remove_start_and_end_markers(Surface1, Surface),
		    length(Surface, Len),
		    Len > 1
		),
		Entries0
	       ),
	sort_by_length_and_convert_to_diff_lists(Entries0, Entries).

sort_by_length_and_convert_to_diff_lists(EntriesIn, EntriesOut) :-
	tag_by_length_and_convert_to_diff_lists(EntriesIn, TaggedEntries),
	keysort(TaggedEntries, TaggedEntries1),
	reverse(TaggedEntries1, TaggedEntries2),
	unkey_list(TaggedEntries2, EntriesOut),
	!.
sort_by_length_and_convert_to_diff_lists(EntriesIn, EntriesOut) :-
	format('~N*** Error: bad call: ~w~n', [sort_by_length_and_convert_to_diff_lists(EntriesIn, EntriesOut)]),
	fail.

tag_by_length_and_convert_to_diff_lists([], []).
tag_by_length_and_convert_to_diff_lists([F | R], [F1 | R1]) :-
	tag_by_length_and_convert_to_diff_list(F, F1),
	!,
	tag_by_length_and_convert_to_diff_lists(R, R1).

tag_by_length_and_convert_to_diff_list(prep(Surface, SurfaceAtom),
				       Len-multiword(Surface1-Rest1, [SurfaceAtom | Rest2]-Rest2)) :-
	length(Surface, Len),
	append(Surface, Rest1, Surface1),
	!.
tag_by_length_and_convert_to_diff_list(F, F1) :-
	format('~N*** Error: bad call: ~w~n', [tag_by_length_and_convert_to_diff_list(F, F1)]),
	fail.

convert_underscores_to_spaces(Surface, Surface1) :-
	atom_codes(Surface, Str),
	convert_underscores_to_spaces1(Str, Str1),
	atom_codes(Surface1, Str1),
	!.

convert_underscores_to_spaces1([], []).
convert_underscores_to_spaces1([F | R], [F1 | R1]) :-
	(   F = 0'_ ->
	    F1 = 0'  
	;
	    otherwise ->
	    F = F1
	),
	!,
	convert_underscores_to_spaces1(R, R1).

%---------------------------------------------------------------

write_out_transforms(Transforms, AbsTransformFile) :-
	length(Transforms, N),
	list_to_prolog_file(Transforms, AbsTransformFile),
	format('~N--- Written preposition file (~d elements): ~w~n', [N, AbsTransformFile]),
	!.

