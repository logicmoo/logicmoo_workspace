
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%---------------------------------------------------------------

:- module(make_adv_transforms,
	  [make_adv_list/2,
	   test_make_adv_list/1
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/CorpusTools/tokenize_sents').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

test_make_adv_list(advs) :-
	make_adv_list('$ACCEPT/MT/Europarl/Mmorph/raw_advs.pl',
		      '$ACCEPT/MT/Europarl/Mmorph/advs.pl').

%---------------------------------------------------------------

make_adv_list(RawFile, AdvListFile) :-
	init_make_adv_list(RawFile),
	make_adv_list1(AdvListFile).

make_adv_list1(File) :-
	safe_absolute_file_name(File, AbsFile),
	find_advs(Advs),
	write_out_transforms(Advs, AbsFile).

%---------------------------------------------------------------

init_make_adv_list(File) :-
	safe_absolute_file_name(File, AbsFile),
	format('~N--- Loading advs file: ~w~n', [AbsFile]),
	compile(File).

%---------------------------------------------------------------

% mmorph_entry([surface=virulemment,lemma=virulemment,pos='Adverb',tag=[adv],form=[surface]]).

find_advs(Entries) :-
	findall(adv(Surface),
		(   mmorph_entry([surface=Surface, lemma=_Lemma, pos='Adverb' | _OtherFeats])
		),
		Entries0
	       ),
	sort(Entries0, Entries).

%---------------------------------------------------------------

write_out_transforms(Transforms, AbsTransformFile) :-
	length(Transforms, N),
	list_to_prolog_file(Transforms, AbsTransformFile),
	format('~N--- Written adv file (~d elements): ~w~n', [N, AbsTransformFile]),
	!.

