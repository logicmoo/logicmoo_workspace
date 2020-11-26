
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%---------------------------------------------------------------

:- module(extract_bicorpus,
	  [extract_bicorpus/5,
	   test_extract_bicorpus/1
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/CorpusTools/match_patterns').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

test_extract_bicorpus(small_ez) :-
	extract_bicorpus('$ACCEPT/MT/Europarl/Data/europarl-v6.fr-en-small.fr',
			 '$ACCEPT/MT/Europarl/Data/europarl-v6.fr-en-small.en',
			 and("ez ",
			     not(or("chez", "assez"))),
			 '$ACCEPT/MT/Europarl/Generated/europarl_ez_filtered_small_fr.txt',
			 '$ACCEPT/MT/Europarl/Generated/europarl_ez_filtered_small_en.txt').

test_extract_bicorpus(full_ez) :-
	extract_bicorpus('$ACCEPT/MT/Europarl/Data/europarl-v6.fr-en.fr',
			 '$ACCEPT/MT/Europarl/Data/europarl-v6.fr-en.en',
			 and("ez ",
			     not(or("chez", "assez"))),
			 '$ACCEPT/MT/Europarl/Generated/europarl_ez_filtered_fr.txt',
			 '$ACCEPT/MT/Europarl/Generated/europarl_ez_filtered_en.txt').

test_extract_bicorpus(filter_ez) :-
	extract_bicorpus('$ACCEPT/MT/Europarl/Generated/europarl_ez_transformed_fr.txt',
			 '$ACCEPT/MT/Europarl/Generated/europarl_ez_filtered_en.txt',
			 not("*** NO TRANSFORM - DISCARD ***"),
			 '$ACCEPT/MT/Europarl/Generated/europarl_ez.fr',
			 '$ACCEPT/MT/Europarl/Generated/europarl_ez.en').

test_extract_bicorpus(small_questions) :-
	extract_bicorpus('$ACCEPT/MT/Europarl/Data/europarl-v6.fr-en-small.fr',
			 '$ACCEPT/MT/Europarl/Data/europarl-v6.fr-en-small.en',
			 and("?",
			     or("-vous", "-nous", "-il", "-elle")),
			 '$ACCEPT/MT/Europarl/Generated/europarl_questions_filtered_small_fr.txt',
			 '$ACCEPT/MT/Europarl/Generated/europarl_questions_filtered_small_en.txt').

test_extract_bicorpus(full_questions) :-
	extract_bicorpus('$ACCEPT/MT/Europarl/Data/europarl-v6.fr-en.fr',
			 '$ACCEPT/MT/Europarl/Data/europarl-v6.fr-en.en',
			 and("?",
			     or("-vous", "-nous", "-il", "-elle")),
			 '$ACCEPT/MT/Europarl/Generated/europarl_questions_filtered_fr.txt',
			 '$ACCEPT/MT/Europarl/Generated/europarl_questions_filtered_en.txt').

test_extract_bicorpus(questions) :-
	extract_bicorpus('$ACCEPT/MT/Europarl/Data/europarl-v6.fr-en.fr',
			 '$ACCEPT/MT/Europarl/Data/europarl-v6.fr-en.en',
			 or("-je", "-nous", "-tu", "-vous", "-il", "-elle", "-ils", "-elles", "-ce"),
			 '$ACCEPT/MT/Europarl/Generated/europarl_questions_fr.txt',
			 '$ACCEPT/MT/Europarl/Generated/europarl_questions_en.txt').

test_extract_bicorpus(est_ce_que) :-
	extract_bicorpus('$ACCEPT/MT/Europarl/Data/europarl-v6.fr-en.fr',
			 '$ACCEPT/MT/Europarl/Data/europarl-v6.fr-en.en',
			 or("est-ce que", "Est-ce que", "est ce que", "Est ce que"),
			 '$ACCEPT/MT/Europarl/Generated/europarl_est_ce_que_fr.txt',
			 '$ACCEPT/MT/Europarl/Generated/europarl_est_ce_que_en.txt').

			 
test_extract_bicorpus(vous) :-
	extract_bicorpus('$ACCEPT/MT/Europarl/Data/europarl-v6.fr-en.fr',
			 '$ACCEPT/MT/Europarl/Data/europarl-v6.fr-en.en',
			 or("vous", "Vous"),
			 '$ACCEPT/MT/Europarl/Generated/europarl_vous_fr.txt',
			 '$ACCEPT/MT/Europarl/Generated/europarl_vous.txt').

test_extract_bicorpus(je) :-
	extract_bicorpus('$ACCEPT/MT/Europarl/Data/europarl-v6.fr-en.fr',
			 '$ACCEPT/MT/Europarl/Data/europarl-v6.fr-en.en',
			 or("je ", "Je ", "j'", "J'"),
			 '$ACCEPT/MT/Europarl/Generated/europarl_je_fr.txt',
			 '$ACCEPT/MT/Europarl/Generated/europarl_je.txt').
			 
%---------------------------------------------------------------

/*

InSource and OutSource are an aligned UTF-8 encoded bicorpus.

Contains is a string, and DoesntContain is a possible empty list of strings.

OutSource, OutTarget is sub-aligned bicorpus, containing all the pairs <S, T>
such that S has the substring Contains and none of the substrings DoesntContain.

*/

extract_bicorpus(InSource, InTarget, Pattern, OutSource, OutTarget) :-
	safe_absolute_file_name(InSource, AbsInSource),
	safe_absolute_file_name(InTarget, AbsInTarget),
	safe_absolute_file_name(OutSource, AbsOutSource),
	safe_absolute_file_name(OutTarget, AbsOutTarget),

	open(AbsInSource, read, SInSource, [encoding('UTF-8')]),
	open(AbsInTarget, read, SInTarget, [encoding('UTF-8')]),
	open(AbsOutSource, write, SOutSource, [encoding('UTF-8')]),
	open(AbsOutTarget, write, SOutTarget, [encoding('UTF-8')]),

	extract_bicorpus1(SInSource, SInTarget, Pattern, SOutSource, SOutTarget, 0-N, 0-N1),

	close(SInSource),
	close(SInTarget),
	close(SOutSource),
	close(SOutTarget),

	format('~N--- Read ~d records from ~w and ~w~n', [N, AbsInSource, AbsInTarget]),
	format('~N--- Written ~d records to ~w and ~w~n', [N1, AbsOutSource, AbsOutTarget]),
	!.

extract_bicorpus1(SInSource, SInTarget, Pattern, SOutSource, SOutTarget, In-Out, In1-Out1) :-
	read_line(SInSource, SourceLine),
	read_line(SInTarget, TargetLine),
	Next is In + 1,
	(   0 is Next mod 10000 ->
	    format(' ~d', [Next])
	;
	    otherwise ->
	    true
	),
	!,
	extract_bicorpus2(SourceLine, TargetLine, Pattern,
			  SInSource, SInTarget, SOutSource, SOutTarget, Next-Out, In1-Out1).

extract_bicorpus2(SourceLine, TargetLine, _Pattern, 
		  _SInSource, _SInTarget, _SOutSource, _SOutTarget, In-Out, In1-Out1) :-
	( SourceLine = end_of_file ; TargetLine = end_of_file ),
	!,
	% Subtract one because we don't want to count the end_of_file
	Out is In - 1,
	Out1 = In1.
extract_bicorpus2(SourceLine, TargetLine, Pattern,
		  SInSource, SInTarget, SOutSource, SOutTarget, In-Out, In1-Out1) :-
	ok_source_line(SourceLine, Pattern),
	format(SOutSource, '~N~s~n', [SourceLine]),
	format(SOutTarget, '~N~s~n', [TargetLine]),
	Next1 is In1 + 1,
	!,
	extract_bicorpus1(SInSource, SInTarget, Pattern, SOutSource, SOutTarget, In-Out, Next1-Out1).
extract_bicorpus2(_SourceLine, _TargetLine, Pattern,
		  SInSource, SInTarget, SOutSource, SOutTarget, In-Out, In1-Out1) :-
	!,
	extract_bicorpus1(SInSource, SInTarget, Pattern, SOutSource, SOutTarget, In-Out, In1-Out1).

ok_source_line(SourceLine, Pattern) :-
	match(Pattern, SourceLine),
	!.


	