
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%---------------------------------------------------------------

:- module(make_adj_transforms,
	  [make_adj_list/2,
	   test_make_adj_list/1
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/CorpusTools/tokenize_sents').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

test_make_adj_list(adjs) :-
	make_adj_list('$ACCEPT/MT/Europarl/Mmorph/raw_adjs.pl',
		      '$ACCEPT/MT/Europarl/Mmorph/adjs.pl').

%---------------------------------------------------------------

make_adj_list(RawFile, AdjListFile) :-
	init_make_adj_list(RawFile),
	make_adj_list1(AdjListFile).

make_adj_list1(File) :-
	safe_absolute_file_name(File, AbsFile),
	find_adjs(Adjs),
	write_out_transforms(Adjs, AbsFile).

%---------------------------------------------------------------

init_make_adj_list(File) :-
	safe_absolute_file_name(File, AbsFile),
	format('~N--- Loading adjs file: ~w~n', [AbsFile]),
	compile(File).

%---------------------------------------------------------------

% mmorph_entry([surface=cryptiques,lemma=cryptique,pos='Adjective',gender=[masculine],number=[plural],degree=[positive],form=[surface]]).

find_adjs(Entries) :-
	findall(adj(Surface, Gender, Number),
		(   mmorph_entry([surface=Surface, lemma=_Lemma, pos='Adjective' | OtherFeats]),
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
	format('~N--- Written adj file (~d elements): ~w~n', [N, AbsTransformFile]),
	!.

