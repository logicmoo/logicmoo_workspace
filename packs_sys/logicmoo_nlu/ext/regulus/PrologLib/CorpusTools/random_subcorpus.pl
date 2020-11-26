
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%---------------------------------------------------------------

:- module(random_subcorpus,
	  [random_subcorpus/4,
	   random_sub_bicorpus/7,
	   subcorpus/3,
	   
	   test_random_subcorpus/1
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/CorpusTools/match_patterns').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

test_random_subcorpus(small_tu) :-
	random_subcorpus('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/all_files_small.txt',
			 or("Tu ", " tu ", "-tu ", " te ", " t'"),
			 10,
			 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/all_files_tu_small.txt').

test_random_subcorpus(all_tu) :-
	random_subcorpus('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/all_files_v2.txt',
			 or("Tu ", " tu ", "-tu ", " te ", " t'"),
			 200,
			 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/tu_200_v2.txt').

test_random_subcorpus(all_est_ce_que) :-
	random_subcorpus('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/all_files_v2.txt',
			 or("est-ce que", "Est-ce que", "est ce que", "Est ce que"),
			 200,
			 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/est_ce_que_200.txt').

test_random_subcorpus(forum_questions) :-
	random_subcorpus('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/all_files_v2.txt',
			 "?",
			 250,
			 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/questions_250.txt').

test_random_subcorpus(all_forum_questions) :-
	subcorpus('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/all_files_v2.txt',
		  "?",
		  '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/all_questions.txt').
			 
test_random_subcorpus(questions) :-			 
	random_sub_bicorpus('$ACCEPT/MT/Europarl/Generated/europarl_questions_fr.txt',
			    '$ACCEPT/MT/Europarl/Generated/europarl_questions_en.txt',
			    5000,
			    '$ACCEPT/MT/Europarl/Generated/europarl_questions_fr_dev.txt',
			    '$ACCEPT/MT/Europarl/Generated/europarl_questions_en_dev.txt',
			    '$ACCEPT/MT/Europarl/Generated/europarl_questions_fr_test.txt',
			    '$ACCEPT/MT/Europarl/Generated/europarl_questions_en_test.txt').			 
			 
%---------------------------------------------------------------

random_subcorpus(InFile, Pattern, N, OutFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),
	
	read_unicode_file_to_string_list(AbsInFile, InList),
	length(InList, NIn),
	format('~N--- Read file (~d lines) ~w~n', [NIn, AbsInFile]),

	extract_elements_matching_pattern(InList, Pattern, FilteredList),
	length(FilteredList, NFiltered),
	format('~N--- Filtered elements (~d left)~n', [NFiltered]),

	set_random_generator_state_from_time,
	pick_n_random_members_from_list(N, FilteredList, OutList),
	length(OutList, NOut),

	write_string_list_to_unicode_file(OutList, AbsOutFile),
	format('~N--- Written file (~d lines) ~w~n', [NOut, AbsOutFile]),
	!.

subcorpus(InFile, Pattern, OutFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),
	
	read_unicode_file_to_string_list(AbsInFile, InList),
	length(InList, NIn),
	format('~N--- Read file (~d lines) ~w~n', [NIn, AbsInFile]),

	extract_elements_matching_pattern(InList, Pattern, FilteredList),
	length(FilteredList, NFiltered),

	write_string_list_to_unicode_file(FilteredList, AbsOutFile),
	format('~N--- Written file (~d lines) ~w~n', [NFiltered, AbsOutFile]),
	!.

extract_elements_matching_pattern([], _Pattern, []).
extract_elements_matching_pattern([F | R], Pattern, Out) :-
	(   match(Pattern, F) ->
	    Out = [F | R1]
	;
	    otherwise ->
	    Out = R1
	),
	!,
	extract_elements_matching_pattern(R, Pattern, R1).

%---------------------------------------------------------------

random_sub_bicorpus(InSource, InTarget, N, OutSource1, OutTarget1, OutSource2, OutTarget2) :-
	safe_absolute_file_name(InSource, AbsInSource),
	safe_absolute_file_name(InTarget, AbsInTarget),
	safe_absolute_file_name(OutSource1, AbsOutSource1),
	safe_absolute_file_name(OutTarget1, AbsOutTarget1),
	safe_absolute_file_name(OutSource2, AbsOutSource2),
	safe_absolute_file_name(OutTarget2, AbsOutTarget2),
	
	read_unicode_file_to_string_list(AbsInSource, InSourceList),
	length(InSourceList, NInSource),
	format('~N--- Read file (~d lines) ~w~n', [NInSource, AbsInSource]),

	read_unicode_file_to_string_list(AbsInTarget, InTargetList),
	length(InTargetList, NInTarget),
	format('~N--- Read file (~d lines) ~w~n', [NInTarget, AbsInTarget]),

	(   NInSource \== NInTarget ->
	    format('~N*** Error: different numbers of records in source and target files~n', []),
	    fail
	;
	    otherwise ->
	    set_random_generator_state_from_time,
	    lists_paired_list(InSourceList, InTargetList, InList),
	    pick_n_random_members_from_list(N, InList, OutList1, OutList2),
	    lists_paired_list(OutSourceList1, OutTargetList1, OutList1),
	    lists_paired_list(OutSourceList2, OutTargetList2, OutList2),
	    
	    length(OutList1, NOut1),
	    length(OutList2, NOut2),
	    
	    write_string_list_to_unicode_file(OutSourceList1, AbsOutSource1),
	    format('~N--- Written file (~d lines) ~w~n', [NOut1, AbsOutSource1]),
	    write_string_list_to_unicode_file(OutTargetList1, AbsOutTarget1),
	    format('~N--- Written file (~d lines) ~w~n', [NOut1, AbsOutTarget1]),

	    write_string_list_to_unicode_file(OutSourceList2, AbsOutSource2),
	    format('~N--- Written file (~d lines) ~w~n', [NOut2, AbsOutSource2]),
	    write_string_list_to_unicode_file(OutTargetList2, AbsOutTarget2),
	    format('~N--- Written file (~d lines) ~w~n', [NOut2, AbsOutTarget2])	    
	).

lists_paired_list([], [], []) :-
	!.
lists_paired_list([F1 | R1], [F2 | R2], [F1-F2 | R3]) :-
	!,
	lists_paired_list(R1, R2, R3).
