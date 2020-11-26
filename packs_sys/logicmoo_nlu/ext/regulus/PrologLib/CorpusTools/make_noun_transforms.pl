
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%---------------------------------------------------------------

:- module(make_noun_transforms,
	  [make_noun_list/2,
	   make_multiword_noun_list/2,
	   test_make_noun_list/1
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/CorpusTools/tokenize_sents').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

test_make_noun_list(nouns) :-
	make_noun_list('$ACCEPT/MT/Europarl/Mmorph/raw_nouns.pl',
		       '$ACCEPT/MT/Europarl/Mmorph/nouns.pl').

%---------------------------------------------------------------

make_noun_list(RawFile, NounListFile) :-
	init_make_noun_list(RawFile),
	make_noun_list1(NounListFile).

make_noun_list1(File) :-
	safe_absolute_file_name(File, AbsFile),
	find_nouns(Nouns),
	write_out_transforms(Nouns, AbsFile).

%---------------------------------------------------------------

init_make_noun_list(File) :-
	safe_absolute_file_name(File, AbsFile),
	format('~N--- Loading nouns file: ~w~n', [AbsFile]),
	compile(File).

%---------------------------------------------------------------

% "compressiom\0350tres" = "compressiom\0350tre" Noun[ gender=masculine number=plural form=surface ]

find_nouns(Entries) :-
	findall(noun(Surface, Gender, Number),
		(   mmorph_entry([surface=Surface, lemma=_Lemma, pos='Noun' | OtherFeats]),
		    member(gender=Genders, OtherFeats),
		    member(number=Numbers, OtherFeats),
		    member(Gender, Genders),
		    member(Number, Numbers)
		),
		Entries0
	       ),
	sort(Entries0, Entries).

%---------------------------------------------------------------

write_out_transforms(Transforms, AbsTransformFile) :-
	length(Transforms, N),
	list_to_prolog_file(Transforms, AbsTransformFile),
	format('~N--- Written noun file (~d elements): ~w~n', [N, AbsTransformFile]),
	!.

