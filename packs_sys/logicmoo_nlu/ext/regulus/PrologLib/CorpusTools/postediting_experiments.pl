:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%---------------------------------------------------------------

:- module(postediting_experiments,
	  [read_postediting_csv/2,
	   read_postediting_csv/4,
	   
	   format_postediting_csv/2,

	   analyse_postediting_csv/3,

	   read_xliff/2,

	   test_postediting/1
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/CorpusTools/tokenize_sents').
:- use_module('$REGULUS/PrologLib/CorpusTools/utils').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(xml)).

%---------------------------------------------------------------

test_postediting(smart_diff1) :-
	TargA = 'I have not all versions in mind',
	Post1A = 'I do not have all versions in mind',
	tokenize_sent_atom(TargA, Targ),
	tokenize_sent_atom(Post1A, Post1),
	smart_diff(Post1, Targ, Diff1),
	format('~NTarget1: "~w"~n', [TargA]),
	format('~NTarget2: "~w"~n', [Post1A]),
	format('~N   Diff: "~w"~n', [Diff1]),
	!.

test_postediting(load_main_corpus_ngram_frequencies) :-
	safe_compile(user, '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams.pl').

test_postediting(summit_read) :-
	read_postediting_csv('$ACCEPT/MT/PostEdition/Data/summitPostEdData.csv',
			     '$ACCEPT/MT/PostEdition/Data/summitPostEdData.pl').

test_postediting(summit_analyse) :-	
	analyse_postediting_csv('$ACCEPT/MT/PostEdition/Data/summitPostEdData.pl',
				'$ACCEPT/MT/PostEdition/Results/summitPostEd.txt',
				4).

test_postediting(fti_students_read) :-
	read_postediting_csv('$ACCEPT/MT/PostEdition/FTIStudents2014/FRdataToPostEdit_200ph_MarieRachel.csv',
			     '$ACCEPT/MT/PostEdition/FTIStudents2014/FRdataToPostEdit_200ph_MarieRachel.pl',
			     [_Id, SourceA, TargA, Post1A, Post2A],
			     [pe_record([type=preedited, source=SourceA, target=TargA, pe(1)=Post1A, pe(2)=Post2A])]).

test_postediting(fti_students_analyse) :-	
	analyse_postediting_csv('$ACCEPT/MT/PostEdition/FTIStudents2014/FRdataToPostEdit_200ph_MarieRachel.pl',
				'$ACCEPT/MT/PostEdition/FTIStudents2014/FRdataToPostEdit_200ph_MarieRachel.html',
				1).

test_postediting(fti_students_format) :-
	format_postediting_csv('$ACCEPT/MT/PostEdition/FTIStudents2014/FRdataToPostEdit_200ph_MarieRachel.pl',
			       '$ACCEPT/MT/PostEdition/FTIStudents2014/FRdataToPostEdit_200ph_MarieRachel_simple.html').

test_postediting(fti_students_read_525) :-
	read_postediting_csv('$ACCEPT/MT/PostEdition/FTIStudents2014/FRdataToPostEdit_525ph_MarieRachel.csv',
			     '$ACCEPT/MT/PostEdition/FTIStudents2014/FRdataToPostEdit_525ph_MarieRachel.pl',
			     [_Id, SourceA, TargA, Post1A, Post2A],
			     [pe_record([type=preedited, source=SourceA, target=TargA, pe(1)=Post1A, pe(2)=Post2A])]).

test_postediting(fti_students_analyse_525) :-	
	analyse_postediting_csv('$ACCEPT/MT/PostEdition/FTIStudents2014/FRdataToPostEdit_525ph_MarieRachel.pl',
				'$ACCEPT/MT/PostEdition/FTIStudents2014/FRdataToPostEdit_525ph_MarieRachel.html',
				1).

test_postediting(fti_students_format_525) :-
	format_postediting_csv('$ACCEPT/MT/PostEdition/FTIStudents2014/FRdataToPostEdit_525ph_MarieRachel.pl',
			       '$ACCEPT/MT/PostEdition/FTIStudents2014/FRdataToPostEdit_525ph_MarieRachel_simple.html').
 
test_postediting(fti_students_read_1k) :-
	read_postediting_csv('$ACCEPT/MT/PostEdition/FTIStudents2014/FR_postEditedData_1k.csv',
			     '$ACCEPT/MT/PostEdition/FTIStudents2014/FR_postEditedData_1k.pl',
			     [_Id, SourceA, TargA, Post1A, Post2A],
			     [pe_record([type=preedited, source=SourceA, target=TargA, pe(1)=Post1A, pe(2)=Post2A])]).

test_postediting(fti_students_analyse_1k) :-	
	analyse_postediting_csv('$ACCEPT/MT/PostEdition/FTIStudents2014/FR_postEditedData_1k.pl',
				'$ACCEPT/MT/PostEdition/FTIStudents2014/FR_postEditedData_1k.html',
				1).

test_postediting(fti_students_format_1k) :-
	format_postediting_csv('$ACCEPT/MT/PostEdition/FTIStudents2014/FR_postEditedData_1k.pl',
			       '$ACCEPT/MT/PostEdition/FTIStudents2014/FR_postEditedData_1k_simple.html').
 
test_postediting(lexcelera_read) :-	
	read_xliff('$ACCEPT/MT/PostEdition/Data/Chapter1BilingualProject/Chapter1_BilingualProject_all.xliff',
		   '$ACCEPT/MT/PostEdition/Data/Chapter1BilingualProject/Chapter1_BilingualProject_all.pl',
		   raw).

test_postediting(lexcelera_analyse) :-	
	analyse_postediting_csv('$ACCEPT/MT/PostEdition/Data/Chapter1BilingualProject/Chapter1_BilingualProject_all.pl',
				'$ACCEPT/MT/PostEdition/Data/Chapter1BilingualProject/Chapter1_BilingualProject_all.txt',
				3).

test_postediting(amt1234) :-
	test_postediting(amt1),
	test_postediting(amt2),
	test_postediting(amt3),
	test_postediting(amt4),
	test_postediting(combine_amt1234).

test_postediting(amt1) :-	
	read_xliff('$ACCEPT/MT/PostEdition/Data/AMTXLIFF/AMT1.xml',
		   '$ACCEPT/MT/PostEdition/Data/AMTXLIFF/AMT1.pl',
		   raw).

test_postediting(amt2) :-	
	read_xliff('$ACCEPT/MT/PostEdition/Data/AMTXLIFF/AMT2.xml',
		   '$ACCEPT/MT/PostEdition/Data/AMTXLIFF/AMT2.pl',
		   raw).

test_postediting(amt3) :-	
	read_xliff('$ACCEPT/MT/PostEdition/Data/AMTXLIFF/AMT3.xml',
		   '$ACCEPT/MT/PostEdition/Data/AMTXLIFF/AMT3.pl',
		   raw).

test_postediting(amt4) :-	
	read_xliff('$ACCEPT/MT/PostEdition/Data/AMTXLIFF/AMT4.xml',
		   '$ACCEPT/MT/PostEdition/Data/AMTXLIFF/AMT4.pl',
		   raw).

test_postediting(amt1_analyse) :-	
	analyse_postediting_csv('$ACCEPT/MT/PostEdition/Data/AMTXLIFF/AMT1.pl',
				'$ACCEPT/MT/PostEdition/Data/AMTXLIFF/AMT1.txt',
				2).

test_postediting(combine_amt1234) :-
	prolog_file_or_files_to_list(['$ACCEPT/MT/PostEdition/Data/AMTXLIFF/AMT1.pl',
				      '$ACCEPT/MT/PostEdition/Data/AMTXLIFF/AMT2.pl',
				      '$ACCEPT/MT/PostEdition/Data/AMTXLIFF/AMT3.pl',
				      '$ACCEPT/MT/PostEdition/Data/AMTXLIFF/AMT4.pl'
				      ],
				     List,
				     'UTF-8'
				     ),
	length(List, N),
	safe_absolute_file_name('$ACCEPT/MT/PostEdition/Data/AMTXLIFF/AMT1234.pl', OutFile),
	list_to_prolog_file_prettyprint_unicode(List, OutFile),
	format('~N--- Written combined file (~d records), ~w~n', [N, OutFile]).				     

test_postediting(amt1234_analyse) :-	
	analyse_postediting_csv('$ACCEPT/MT/PostEdition/Data/AMTXLIFF/AMT1234.pl',
				'$ACCEPT/MT/PostEdition/Data/AMTXLIFF/AMT1234.html',
				1).

%---------------------------------------------------------------

read_postediting_csv(InFile, OutFile) :-
	read_postediting_csv(InFile, OutFile,
			     [SourceA, TargA, Post1A, Post2A, Post3A, PreEdA, TargPrA, PostPr1A, PostPr2A, PostPr3A],
			     [pe_record([type=raw, source=SourceA, target=TargA, pe(1)=Post1A, pe(2)=Post2A, pe(3)=Post3A]),
			      pe_record([type=preedited, source=PreEdA, target=TargPrA, pe(1)=PostPr1A, pe(2)=PostPr2A, pe(3)=PostPr3A])]).

read_postediting_csv(InFile, OutFile, Pattern, Records) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),
	
	csv_file_to_list_of_lists(AbsInFile, InList),
	InList = [_Header1, _Header2 | InList1],
	length(InList1, NIn),
	format('~N--- Read postediting file (~d records) ~w~n', [NIn, AbsInFile]),
	
	process_postediting_csv_list(InList1, NextList1, Pattern, Records),
	format('~N--- Processing grouped lines~n', []),
	process_grouped_lines(NextList1, OutList, 0),
	format('~N--- Finished processing grouped lines~n', []),
	
	length(OutList, NOut),
	list_to_prolog_file_prettyprint(OutList, AbsOutFile),
	format('~N--- Written postediting file (~d records) ~w~n', [NOut, AbsOutFile]),
	!.

%---------------------------------------------------------------

read_xliff(InFile, OutFile, Type) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),

	read_unicode_file_to_string(AbsInFile, Str),
	xml_parse(Str, XMLTerm),
	(   XMLTerm = malformed(_, _) ->
	    format('~N*** Error: malformed XML~n', []),
	    fail
	;
	    otherwise ->
	    xliff_prolog_to_list(XMLTerm, List, Type),
	    length(List, NOut),

	    list_to_prolog_file_prettyprint_unicode(List, AbsOutFile),
	    format('~N--- Written postediting file (~d records) ~w~n', [NOut, AbsOutFile])
	),
	!.
internalise_script_file1(File, _Script, _LessonId) :-
	format('~N*** Error: unable to internalise script file "~w"~n', [File]),
	fail.
	
%---------------------------------------------------------------

format_postediting_csv(InFile, OutFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),
	
	load_postediting_csv(AbsInFile),

	format_postediting_csv1(AbsOutFile).

%---------------------------------------------------------------

analyse_postediting_csv(InFile, OutFile, MinFreq) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),
	
	load_postediting_csv(AbsInFile),

	collect_and_show_basic_edit_operations(AbsOutFile, MinFreq).

load_postediting_csv(File) :-
	compile(File).

%---------------------------------------------------------------

process_postediting_csv_list([], [], _Pattern, _Records).
process_postediting_csv_list([F | R], Out, Pattern, Records) :-
	copy_term([Pattern, Records], [Pattern1, Records1]),
	process_postediting_csv_line(F, Pattern1, Records1),
	append(Records1, R1, Out),
	!,
	process_postediting_csv_list(R, R1, Pattern, Records).

process_postediting_csv_line(Line, Pattern, _Records) :-
	Line = Pattern,
	!.
process_postediting_csv_line(Line, Pattern, _Records) :-
	length(Line, LLen),
	length(Pattern, PLen),
	format('~NLine    ~w (length ~d) fails to match~n', [Line, LLen]),
	format('~Npattern ~w (length ~d)~n', [Pattern, PLen]),
	fail.

%---------------------------------------------------------------

process_grouped_lines([], [], _I).
process_grouped_lines([F | R], [F1 | R1], I) :-
	process_grouped_line(F, F1),
	format(user, '.', []),
	flush_output(user),
	I1 is I + 1,
	(   0 is I1 mod 100 ->
	    
	    format(' (~d) ~n', [I1]),
	    flush_output(user)
	;
	    otherwise ->
	    true
	),
	!,
	process_grouped_lines(R, R1, I1).

%process_grouped_line(F, F1) :-
%	F = pe_record([type=Type, source=SourceA, target=TargA, pe(1)=Post1A, pe(2)=Post2A, pe(3)=Post3A]),
%	%tokenize_sent_atom(SourceA, _Source),
%	tokenize_sent_atom(TargA, Targ),
%	tokenize_sent_atom(Post1A, Post1),
%	tokenize_sent_atom(Post2A, Post2),
%	tokenize_sent_atom(Post3A, Post3),
%	smart_diff(Post1, Targ, Diff1),
%	smart_diff(Post2, Targ, Diff2),
%	smart_diff(Post3, Targ, Diff3),
%	F1 = pe_record([type=Type, source=SourceA, target=TargA,
%			pe(1)=Post1A, pe(2)=Post2A, pe(3)=Post3A,
%			diff(1)=Diff1, diff(2)=Diff2, diff(3)=Diff3]).

process_grouped_line(F, F1) :-
	F = pe_record(InList),
	member(type=Type, InList),
	member(source=SourceA, InList),
	member(target=TargA, InList),
	tokenize_sent_atom(TargA, Targ),
	
	findall([I, PostA, Diff],
		(   member(pe(I)=PostA, InList),
		    tokenize_sent_atom(PostA, Post),
		    smart_diff(Post, Targ, Diff)
		),
		Triples),
	pe_elements_in_grouped_line_triples(Triples, PEElements),
	diff_elements_in_grouped_line_triples(Triples, DiffElements),

	append_list([[type=Type, source=SourceA, target=TargA],
		     PEElements,
		     DiffElements],
		    OutList),

	F1 = pe_record(OutList),
	!.
process_grouped_line(F, F1) :-
	format('~N*** Error: bad call: ~w~n',
	       [process_grouped_line(F, F1)]),
	fail.

pe_elements_in_grouped_line_triples([], []).
pe_elements_in_grouped_line_triples([[I, PostA, _Diff] | R], [pe(I)=PostA | R1]) :-
	!,
	pe_elements_in_grouped_line_triples(R, R1).

diff_elements_in_grouped_line_triples([], []).
diff_elements_in_grouped_line_triples([[I, _PostA, Diff] | R], [diff(I)=Diff | R1]) :-
	!,
	diff_elements_in_grouped_line_triples(R, R1).


%---------------------------------------------------------------

tokenize_sent_atom(Atom, Tokens) :-
	atom_codes(Atom, Str),
	tokenize_sent(Str, Tokens).

smart_diff(Targ, Post, Diff2) :-
	remove_spaces(Targ, Targ1),
	remove_spaces(Post, Post1),
	insertions_deletions_substitutions_and_matches(Targ1, Post1, _, _, _, _, Diff),
	simplify_diff(Diff, Diff1, null, null),
	find_complex_editing_operations(Diff1, Diff2),
	!.

remove_spaces([], []).
remove_spaces([' ' | R], R1) :-
	!,
	remove_spaces(R, R1).
remove_spaces([F | R], [F | R1]) :-
	!,
	remove_spaces(R, R1).

%---------------------------------------------------------------

simplify_diff([], Result, Current, Preceding) :-
	(   Current = null ->
	    Result = []
	;
	    Current = same(_) ->
	    Result = []
	;
	    otherwise ->
	    Result = [Preceding/Current/null]
	),
	!.
simplify_diff([same(X) | R], R1, null, Preceding) :-
	!,
	update_preceding(Preceding, same(X), NewPreceding),
	simplify_diff(R, R1, null, NewPreceding).
simplify_diff([same(X) | R], R1, same(Y), _Preceding) :-
	!,
	update_preceding([Y], same(X), NewPreceding),
	simplify_diff(R, R1, null, NewPreceding).
simplify_diff([same(X) | R], [Preceding/Current/Following | R1], Current, Preceding) :-
	!,
	initial_context([same(X) | R], Following),
	update_preceding(Preceding, Current, NewPreceding0),
	update_preceding(NewPreceding0, same(X), NewPreceding),
	simplify_diff(R, R1, null, NewPreceding).
simplify_diff([F | R], Result, Current, Preceding) :-
	combine_diff(Current, F, NewCurrent),
	!,
	simplify_diff(R, Result, NewCurrent, Preceding).
simplify_diff([F | R], R1, null, Preceding) :-
	!,
	simplify_diff(R, R1, F, Preceding).
simplify_diff([F | R], [Preceding/Current/Following | R1], Current, Preceding) :-
	update_preceding(Preceding, Current, Preceding1),
	!,
	initial_context(R, Following),
	simplify_diff(R, R1, F, Preceding1).

update_preceding(Preceding, Operation, NewPreceding) :-
	coerce_to_list(Preceding, PrecedingList),
	coerce_to_list(Operation, OperationList),
	append(PrecedingList, OperationList, NewPreceding0),
	at_most_last_two(NewPreceding0, NewPreceding),
	!.

initial_context(Following, Context) :-
	coerce_to_list(Following, Context0),
	at_most_first_two(Context0, Context),
	!.
initial_context(_Other, null).

%---------------------------------------------------------------

simplify_diff_no_context([], Result, Current) :-
	(   Current = null ->
	    Result = []
	;
	    otherwise ->
	    Result = [Current]
	),
	!.
simplify_diff_no_context([same(X) | R], [same(X) | R1], null) :-
	!,
	simplify_diff_no_context(R, R1, null).
simplify_diff_no_context([same(X) | R], [Current, same(X) | R1], Current) :-
	!,
	simplify_diff_no_context(R, R1, null).
simplify_diff_no_context([F | R], Result, Current) :-
	combine_diff(Current, F, NewCurrent),
	!,
	simplify_diff_no_context(R, Result, NewCurrent).
simplify_diff_no_context([F | R], R1, null) :-
	!,
	simplify_diff_no_context(R, R1, F).
simplify_diff_no_context([F | R], [Current| R1], Current) :-
	!,
	simplify_diff_no_context(R, R1, F).

%---------------------------------------------------------------

combine_diff(null, Current, Current) :-
	!.
combine_diff(del(A1), del(B1), del(AB1)) :-
	append_atoms_or_lists(A1, B1, AB1),
	!.
combine_diff(ins(A1), ins(B1), ins(AB1)) :-
	append_atoms_or_lists(A1, B1, AB1),
	!.
combine_diff(sub(A1, A2), sub(B1, B2), sub(AB1, AB2)) :-
	append_atoms_or_lists(A1, B1, AB1),
	append_atoms_or_lists(A2, B2, AB2),
	!.
combine_diff(sub(A1, A2), del(B2), sub(A1, AB2)) :-
	append_atoms_or_lists(A2, B2, AB2),
	!.
combine_diff(del(B2), sub(A1, A2), sub(A1, BA2)) :-
	append_atoms_or_lists(B2, A2, BA2),
	!.
combine_diff(sub(A1, A2), ins(B1), sub(AB1, A2)) :-
	append_atoms_or_lists(A1, B1, AB1),
	!.
combine_diff(ins(B1), sub(A1, A2), sub(BA1, A2)) :-
	append_atoms_or_lists(B1, A1, BA1),
	!.

%---------------------------------------------------------------

find_complex_editing_operations([], []).
% [[and,the]/del(address)/['HP',is],[address,'HP']/ins(address)/[is]] -> [and,the]/exch(address, 'HP')/[is]
find_complex_editing_operations([Before/del(X)/[Y|_], [X,Y]/ins(X)/After | R], [Before/exch(X, Y)/After | R1]) :-
	!,
	find_complex_editing_operations(R, R1).
% "[[*start*,I]/sub(do,have)/[not,all],[have,not]/ins(have)/[all,versions]]"
find_complex_editing_operations([Before/sub(Do,Have)/[Not|_], [Have,Not]/ins(Have)/After | R],
				[Before/sub([Do,Not,Have], [Have,Not])/After | R1]) :-
	!,
	find_complex_editing_operations(R, R1).
find_complex_editing_operations([F | R], [F | R1]) :-
	!,
	find_complex_editing_operations(R, R1).

%---------------------------------------------------------------

remove_same([], []).
remove_same([same(_) | R], R1) :-
	!,
	remove_same(R, R1).
remove_same([F | R], [F | R1]) :-
	!,
	remove_same(R, R1).

%===============================================================

format_postediting_csv1(File) :-
	findall([Source, Target],
		%find_edit_operation(EditOperation),
		find_source_target_pair(Source, Target),
		Pairs),
	show_source_target_pairs(File, Pairs).

find_source_target_pair(Source, Target) :-
	pe_record(List),
	member(source=Source, List),
	member(target=Target, List).

show_source_target_pairs(File, Pairs) :-
	length(Pairs, NPairs),
	format('~NPrinting data for ~d source-target pairs~n', [NPairs]),
	open(File, write, S, [encoding('UTF-8')]),
	print_html_opening(S),
	show_source_target_pairs1(Pairs, S),
	print_html_closing(S),
	close(S),
	format('~N--- Results written to: ~w~n', [File]).

show_source_target_pairs1([], _S).
show_source_target_pairs1([F | R], S) :-
	show_source_target_pair(F, S),
	!,
	show_source_target_pairs1(R, S).

show_source_target_pair([Source, Target], S) :-
	pe_record(List),
	member(source=Source, List),
	member(target=Target, List),
	findall([I, Target1, Edited1],
		target_edited_pair_in_list(List, Target, I, Target1, Edited1),
		Triples),
	show_source_target_pair1(S, Source, Triples),
	!.

target_edited_pair_in_list(List, Target, I, Target1, Edited1) :-
	member(pe(I)=Edited, List),
	mark_diffs_in_target_and_edited(Target, Edited, Target1, Edited1).

show_source_target_pair1(S, Source, Triples) :-
	format(S, '~N~n<p>Source: ~w<br>~n', [Source]),
	show_target_edited_triples(S, Triples),
	format(S, '~N</p>~n', []),
	!.

show_target_edited_triples(_S, []).
show_target_edited_triples(S, [F | R]) :-
	show_target_edited_triple(S, F),
	!,
	show_target_edited_triples(S, R).

show_target_edited_triple(S, [I, Target1, Edited1]) :-
	format(S, '~NSubject ID: ~d<br>~n', [I]),
	format(S, '~NTarget: ~w<br>~n', [Target1]),
	format(S, '~NEdited: ~w<br>~n', [Edited1]),
	!.
show_target_edited_triple(S, Triple) :-
	format('~N*** Error: bad call: ~w~n',
	       [show_target_edited_triple(S, Triple)]),
	fail.

%===============================================================

collect_and_show_basic_edit_operations(File, MinFreq) :-
	findall(EditOperation,
		%find_edit_operation(EditOperation),
		find_context_free_edit_operation(EditOperation),
		EditOperations),
	show_edit_operations(File, EditOperations, MinFreq).
 
%show_edit_operations(File, EditOperations, MinFreq) :-
%	list_to_ordered_multiset(EditOperations, MultiSet),
%	length(MultiSet, NEditOperations),
%	format('~NPrinting data for ~d possible edit operations~n', [NEditOperations]),
%	open(File, write, S, [encoding('UTF-8')]),
%	print_html_opening(S),
%	format(S, '~N<p>Total number of edit operations: ~d<br>~n', [NEditOperations]),
%	format(S, '~NFrequency-ordered list:</p>~n', []),
%	show_edit_operations1(MultiSet, MinFreq, S, 1),
%	print_html_closing(S),
%	close(S),
%	format('~N--- Results written to: ~w~n', [File]).
show_edit_operations(File, EditOperations, MinFreq) :-
	store_edit_operation_source_correspondences(EditOperations, SourcesWithCounts),
	length(SourcesWithCounts, NSources),
	format('~NPrinting data for ~d possible edit operation LHSs~n', [NSources]),
	open(File, write, S, [encoding('UTF-8')]),
	print_html_opening(S),
	format(S, '~N<p>Total number of edit operation LHSs: ~d<br>~n', [NSources]),
	format(S, '~NFrequency-ordered list:</p>~n', []),
	show_edit_operations_for_source(SourcesWithCounts, MinFreq, S, 1),
	print_html_closing(S),
	close(S),
	format('~N--- Results written to: ~w~n', [File]).

%--------------------------------------------------

:- dynamic edit_operation_source_count/3.

store_edit_operation_source_correspondences(EditOperations, SourcesWithCounts) :-
	retractall(edit_operation_source_count(_, _, _)),
	list_to_ordered_multiset(EditOperations, MultiSet),
	store_edit_operation_source_correspondences1(MultiSet),
	all_edit_operation_sources(Sources),
	length(Sources, NSources),
	format('~N--- Getting source counts (~d possible sources)~n', [NSources]),
	edit_operation_source_counts(Sources, SourcesWithCounts).

all_edit_operation_sources(Sources) :-
	findall(Source,
		edit_operation_source_count(Source, _, _),
		Sources0),
	sort(Sources0, Sources).

store_edit_operation_source_correspondences1([]).
store_edit_operation_source_correspondences1([Freq-EditOperation | R]) :-
	matching_ngram_for_edit_operation(EditOperation, Source),
	assertz(edit_operation_source_count(Source, EditOperation, Freq)),
	!,
	store_edit_operation_source_correspondences1(R).

edit_operation_source_counts(Sources, SourcesWithCounts) :-
	edit_operation_source_counts1(Sources, SourcesWithCounts0, 0),
	keysort(SourcesWithCounts0, SourcesWithCounts1),
	reverse(SourcesWithCounts1, SourcesWithCounts).

edit_operation_source_counts1([], [], _I).
edit_operation_source_counts1([F | R], [Count-F | R1], I) :-
	edit_operation_source_count(F, Count),
	I1 is I + 1,
	(   0 is I1 mod 1000 ->
	    format('~d ', [I1]),
	    flush_output(user)
	;
	    otherwise ->
	    true
	),
	!,
	edit_operation_source_counts1(R, R1, I1).

edit_operation_source_count(Source, TotalFreq) :-
	findall(Freq,
		edit_operation_source_count(Source, _EditOperation, Freq),
		Freqs),
	safe_sum_list(Freqs, TotalFreq).

%--------------------------------------------------

print_html_opening(S) :-
	format(S, '~N<html>~n', []),
	format(S, '~N<body>~n', []),
	!.

print_html_closing(S) :-
	format(S, '~N</body>~n', []),
	format(S, '~N</html>~n', []),
	!.

show_edit_operations1([], _MinFreq, _S, _I).
show_edit_operations1([Freq-EditOperation | R], MinFreq, S, I) :-
	(   Freq < MinFreq ->
	    true
	;
	    otherwise ->
	    show_edit_operation(Freq-EditOperation, S)
	),
	I1 is I + 1,
	(   0 is I1 mod 1000 ->
	    format('~d ', [I1]),
	    flush_output(user)
	;
	    otherwise ->
	    true
	),
	!,
	show_edit_operations1(R, MinFreq, S, I1).

show_edit_operations_for_source([], _MinFreq, _S, _I).
show_edit_operations_for_source([Freq-EditOperationSource | R], MinFreq, S, I) :-
	(   Freq < MinFreq ->
	    true
	;
	    trivial_edit_operation_source(EditOperationSource) ->
	    true
	;
	    otherwise ->
	    show_edit_operations_for_source1(Freq-EditOperationSource, S)
	),
	I1 is I + 1,
	(   0 is I1 mod 1000 ->
	    format('~d ', [I1]),
	    flush_output(user)
	;
	    otherwise ->
	    true
	),
	!,
	show_edit_operations_for_source(R, MinFreq, S, I1).

trivial_edit_operation_source([]).
 
show_edit_operation(Freq-EditOperation, S) :-
	%get_main_corpus_frequency_for_edit_operation(EditOperation, MatchingNGram, MainCorpusFreq),
	matching_ngram_for_edit_operation(EditOperation, MatchingNGram),
	target_ngram_for_edit_operation(EditOperation, TargetNGram),
	MatchingNGram \== [],
	%\+ trivial_edit_operation(MatchingNGram, MainCorpusFreq),
	ReadableEditOperation = (MatchingNGram --> TargetNGram),
	format(S, '~N~n<p>-----------------------------</p>', []),
	%format(S, '~NOperation: ~w, freq: ~d~n', [EditOperation, Freq]),
	format(S, '~NOperation: ~w, freq: ~d<br>~n', [ReadableEditOperation, Freq]),
	%format(S, '~NFreq in training corpus of "~w": ~w</p>~n', [MatchingNGram, MainCorpusFreq]),
	find_examples_of_edit_operation(EditOperation, Examples),
	show_examples_of_edit_operation(Examples, S),
	!.
show_edit_operation(_, _S).

show_edit_operations_for_source1(Freq-EditOperationSource, S) :-
	format(S, '~N~n<p>-----------------------------</p>', []),
	format(S, '~NEdited string: ~w, freq: ~d<br>~n', [EditOperationSource, Freq]),
	findall(Freq1-EditOperation,
		edit_operation_source_count(EditOperationSource, EditOperation, Freq1),
		EditOperations0),
	keysort(EditOperations0, EditOperations1),
	reverse(EditOperations1, EditOperations),
	show_edit_operations_list(EditOperations, S).

show_edit_operations_list([], _S).
show_edit_operations_list([F | R], S) :-
	show_edit_operation_short(F, S),
	!,
	show_edit_operations_list(R, S).

show_edit_operation_short(Freq-EditOperation, S) :-
	matching_ngram_for_edit_operation(EditOperation, MatchingNGram),
	target_ngram_for_edit_operation(EditOperation, TargetNGram),
	ReadableEditOperation = (MatchingNGram --> TargetNGram),
	format(S, '~NOperation: ~w, freq: ~d<br>~n', [ReadableEditOperation, Freq]),
	find_examples_of_edit_operation(EditOperation, Examples),
	show_examples_of_edit_operation(Examples, S),
	!.
show_edit_operation_short(_, _S).


trivial_edit_operation(_MatchingNGram, MainCorpusFreq) :-
	member(MainCorpusFreq, [unknown, undefined]),
	!.
trivial_edit_operation(_MatchingNGram, MainCorpusFreq) :-
	MainCorpusFreq > 500.

show_examples_of_edit_operation([], _S).
show_examples_of_edit_operation([F | R], S) :-
	show_example_of_edit_operation(F, S),
	!,
	show_examples_of_edit_operation(R, S).

show_example_of_edit_operation(Example, S) :-
	member(type=Type, Example),
	member(source=Source, Example),
	member(target=Target, Example),
	member(edited=Edited, Example),
	format(S, '~N~n<p>Source: ~w (~w)<br>~n', [Source, Type]),
	format(S, '~NTarget: ~w<br>~n', [Target]),
	format(S, '~NEdited: ~w</p>~n', [Edited]),
	!.

%---------------------------------------------------------------

/*
pe_record([(type=raw), 
           (source='Je ne peux donc plus utiliser mon téléphone suite à un pb de carte SIM.'), 
           (target='I can no longer use my phone pb following a SIM card.'), 
           (pe(1)='I can no longer use my phone following a SIM card problem.'), 
           (pe(2)='I can no longer use my phone following a problem with the SIM card.'), 
           (pe(3)='I can no longer use my phone following a SIM card problem.'), 
           (diff(1)=[del(pb),ins(problem)]), (diff(2)=[del(pb),ins([problem,with,the])]), 
           (diff(3)=[del(pb),ins(problem)])]).
*/

find_context_free_edit_operation(EditOperation) :-
	find_edit_operation(EditOperation),
	context_free_free_edit_operation(EditOperation).

find_edit_operation(EditOperation) :-
	pe_record(List),
	member(diff(_I)=Diffs, List),
	member(EditOperation0, Diffs),
	abstract_edit_operation(EditOperation0, EditOperation),
	make_ground(EditOperation).

abstract_edit_operation(Before/Edit/After, Before/Edit/After).
abstract_edit_operation(_Before/Edit/After, _AnyBefore/Edit/After).
abstract_edit_operation(_Before/Edit/[After1, _After2], _AnyBefore/Edit/[After1, _AnyAfter2]).
abstract_edit_operation(Before/Edit/_After, Before/Edit/_AnyAfter).
abstract_edit_operation([_Before1, Before2]/Edit/_After, [_AnyBefore1, Before2]/Edit/_AnyAfter).
abstract_edit_operation(_Before/Edit/_After, _AnyBefore/Edit/_AnyAfter).

find_examples_of_edit_operation(EditOperation, Examples) :-
	findall(Example,
		example_of_edit_operation(EditOperation, Example),
		Examples).

example_of_edit_operation(GroundedEditOperation, Example) :-
	unground(GroundedEditOperation, EditOperation),
	pe_record(List),
	member(diff(I)=Diffs, List),
	member(EditOperation1, Diffs),
	safe_subsumes_chk(EditOperation, EditOperation1),
	member(type=Type, List),
	member(source=Source, List),
	member(target=Target0, List),
	member(pe(I)=Edited0, List),
	%( EditOperation = _/sub([as,a],to)/_ -> true ; fail ),
	%mark_edit_operation_in_target(Target0, Edited, EditOperation, Target),
	mark_diffs_in_target_and_edited(Target0, Edited0, Target, Edited),
	Example = [type=Type, source=Source, target=Target, edited=Edited].

context_free_free_edit_operation(GroundedEditOperation) :-
	unground(GroundedEditOperation, EditOperation),
	EditOperation = Before/_Operation/After,
	var(Before),
	var(After).

%---------------------------------------------------------------

mark_diffs_in_target_and_edited(Target0, Edited0, Target, Edited) :-
	colour_mark_diffs(Target0, Edited0, Target),
	colour_mark_diffs(Edited0, Target0, Edited),
	!.
mark_diffs_in_target_and_edited(Target0, Edited0, Target0, Edited0).

colour_mark_diffs(Atom0, RefAtom0, ColouredDiff) :-
	insertions_deletions_substitutions_and_matches(Atom0, RefAtom0, _Total, _I, _D, _S, Diff),
	turn_diff_into_colour_markings(Diff, ColouredDiff).
	
turn_diff_into_colour_markings(Diff, ColouredDiff) :-
	turn_diff_into_colour_markings1(Diff, ColouredDiffElements),
	join_with_spaces(ColouredDiffElements, ColouredDiff),
	!.
turn_diff_into_colour_markings(Diff, ColouredDiff) :-
	format('~N*** Error: bad call: ~w~n',
	       [turn_diff_into_colour_markings(Diff, ColouredDiff)]),
	fail.

turn_diff_into_colour_markings1([], []).
turn_diff_into_colour_markings1([F | R], [F1 | R1]) :-
	turn_diff_element_into_colour_markings(F, F1),
	!,
	turn_diff_into_colour_markings1(R, R1).

turn_diff_element_into_colour_markings(same(X), X) :-
	!.
turn_diff_element_into_colour_markings(sub(X, _Y), Output) :-
	mark_with_color(X, red, Output),
	!.
turn_diff_element_into_colour_markings(ins(X), Output) :-
	mark_with_color(X, red, Output),
	!.
turn_diff_element_into_colour_markings(del(X), Output) :-
	mark_with_strikethrough(X, X1),
	%mark_with_italics(X, X1),
	mark_with_color(X1, blue, Output),
	!.
turn_diff_element_into_colour_markings(Other, Output) :-
	format('~N*** Error: bad call: ~w~n',
	       [turn_diff_element_into_colour_markings(Other, Output)]),
	fail.

mark_with_color(X, ColorID, Output) :-
	color_id_to_html_color(ColorID, HTMLColor),
	format_to_atom('<FONT COLOR="~w">~w</FONT>', [HTMLColor, X], Output),
	!.

mark_with_strikethrough(X, Output) :-
	format_to_atom('<s>~w</s>', [X], Output),
	!.

mark_with_italics(X, Output) :-
	format_to_atom('<i>~w</i>', [X], Output),
	!.

% #ff0033  red
% #00ff33  green
% #0000cc  blue

color_id_to_html_color(red, '#ff0033').    %red
color_id_to_html_color(green, '#00ff33').  %green
color_id_to_html_color(blue, '#0000cc').   %blue

%---------------------------------------------------------------

% Target, Edited and AnnotatedTarget are atoms. EditOperation is an ungrounded edit operation.

mark_edit_operation_in_target(Target, _Edited, _EditOperation, AnnotatedTarget) :-
	AnnotatedTarget = Target,
	!.
mark_edit_operation_in_target(Target, Edited, EditOperation, AnnotatedTarget) :-
	tokenize_sent_atom(Target, TargetWords0),
	tokenize_sent_atom(Edited, EditedWords0),
	%smart_diff(EditedWords, TargetWords, Diff),
	remove_spaces(TargetWords0, TargetWords),
	remove_spaces(EditedWords0, EditedWords),
	insertions_deletions_substitutions_and_matches(EditedWords, TargetWords, _, _, _, _, Diff0),
	simplify_diff_no_context(Diff0, Diff, null),
	EditOperation = LeftContext/Operation/RightContext,
	coerce_to_list(LeftContext, LeftContextList),
	coerce_to_list(RightContext, RightContextList),
	EditOperation1 = [context(LeftContextList), Operation, context(RightContextList)],
	mark_edit_operation_in_target1(Diff, EditOperation1, AnnotatedDiff),
	annotated_diff_to_atom(AnnotatedDiff, AnnotatedTarget),
	!.
mark_edit_operation_in_target(Target, Edited, EditOperation, AnnotatedTarget) :-
	format('~N*** Warning: bad call "~w"~n',
	       [mark_edit_operation_in_target(Target, Edited, EditOperation, AnnotatedTarget)]),
	AnnotatedTarget = Target.

mark_edit_operation_in_target1(Diff, EditOperation, AnnotatedDiff) :-
	match_edit_operation_to_diff(EditOperation, Diff-RestDiff, MatchedElements-[]),
	append(['<b><i>' | MatchedElements], ['</i></b>' | RestDiff], AnnotatedDiff),
	!.
mark_edit_operation_in_target1([F | Diff], EditOperation, [F | AnnotatedDiff]) :-
	!,
	mark_edit_operation_in_target1(Diff, EditOperation, AnnotatedDiff).

match_edit_operation_to_diff([], Diff-Diff, MatchedElements-MatchedElements).
match_edit_operation_to_diff([F | R], DiffIn-DiffOut, MatchedElementsIn-MatchedElementsOut) :-
	match_edit_operation_component_to_diff(F, DiffIn-DiffNext, MatchedElementsIn-MatchedElementsNext),
	!,
	match_edit_operation_to_diff(R, DiffNext-DiffOut, MatchedElementsNext-MatchedElementsOut).

match_edit_operation_component_to_diff(context(List), DiffIn-DiffOut, MatchedElementsIn-MatchedElementsOut) :-
	!,
	match_literal_list_to_diff(List, DiffIn-DiffOut, MatchedElementsIn-MatchedElementsOut).
match_edit_operation_component_to_diff(Other, [Other | Diff]-Diff, [Other | MatchedElements]-MatchedElements) :-
	!.

match_literal_list_to_diff([], Diff-Diff, MatchedElements-MatchedElements).
match_literal_list_to_diff([F | R], [same(F) | DiffNext]-DiffOut, [same(F) | MatchedElementsNext]-MatchedElementsOut) :-
	!,
	match_literal_list_to_diff(R, DiffNext-DiffOut, MatchedElementsNext-MatchedElementsOut).

annotated_diff_to_atom(AnnotatedDiff, AnnotatedTarget) :-
	coerce_to_list(AnnotatedDiff, AnnotatedDiffList),
	remove_start_and_end_markers(AnnotatedDiffList, AnnotatedDiffList1),
	join_with_spaces(AnnotatedDiffList1, AnnotatedTarget).

%---------------------------------------------------------------

get_main_corpus_frequency_for_edit_operation(EditOperation, NGram, MainCorpusFreq) :-
	matching_ngram_for_edit_operation(EditOperation, NGram),
	(   NGram = [] ->
	    MainCorpusFreq = undefined
	;
	    \+ current_predicate(user:ngram/2) ->
	    MainCorpusFreq = unknown
	;
	    user:ngram(NGram, MainCorpusFreq) ->
	    true
	;
	    MainCorpusFreq = unknown
	).

matching_ngram_for_edit_operation(EditOperation, NGram) :-
	unground(EditOperation, UngroundedEditOperation),
	UngroundedEditOperation = Before/Operation/After,
	coerce_to_list([Before, Operation, After], NGram0),
	lowercase_atom_list(NGram0, NGram),
	!.
matching_ngram_for_edit_operation(EditOperation, NGram) :-
	format('~N*** Error: bad call "~w"~n', [matching_ngram_for_edit_operation(EditOperation, NGram)]),
	fail.

target_ngram_for_edit_operation(EditOperation, NGram) :-
	unground(EditOperation, UngroundedEditOperation),
	UngroundedEditOperation = Before/Operation/After,
	coerce_to_target_list([Before, Operation, After], NGram0),
	lowercase_atom_list(NGram0, NGram),
	!.
target_ngram_for_edit_operation(EditOperation, NGram) :-
	format('~N*** Error: bad call "~w"~n', [target_ngram_for_edit_operation(EditOperation, NGram)]),
	fail.

%---------------------------------------------------------------

xliff_prolog_to_list(XMLTerm, List, Type) :-
	format('~NReplacing strings with atoms... ', []),
	replace_strings_with_atoms_in_xml(XMLTerm, XMLTerm1),
	format('~Ndone ', []),

	format('~NRemoving comments... ', []),
	remove_comments_in_xml(XMLTerm1, XMLTerm2),
	format('~Ndone ', []),
	
	xliff_prolog_to_list1(XMLTerm2, Type, List).

/*
<xliff version="1.2">
  <file original="twb_7550_enfr_chapter1_1_task1_proj31" source-language="en" target-language="fr" datatype="" category="" product-name="">
    <header>
      ....
    </header>
    <body>
      <trans-unit id="3">
        <source>Diseases can be divided into a few large broad categories based on their main causes (see Table 1.1).</source>
        <target phase-name="r3.1">Les maladies peuvent être divisées en plusieurs grandes catégories en fonction de leurs causes principales (cf. Tableau 1).</target>
        <alt-trans phase-name="mt_baseline">Les maladies peuvent être divisés en quelques grands grandes catégories basée sur leurs causes principales (Table 1).</alt-trans>
      </trans-unit>
      ...
   </body>
  </file>
*/

xliff_prolog_to_list1(XMLTerm, Type, List) :-
	XMLTerm = xml(_VersionInfo, 
		      [element(xliff, _, Files)]),
	xliff_files_to_list(Files, Type, List-[]),
	!.

xliff_files_to_list([], _Type, List-List).
xliff_files_to_list([F | R], Type, ListIn-ListOut) :-
	xliff_file_to_list(F, Type, ListIn-ListNext),
	!,
	xliff_files_to_list(R, Type, ListNext-ListOut).

xliff_file_to_list(element(file, _Attrs, List), Type, ListIn-ListOut) :-
	xliff_file_contents(List, Type, ListIn-ListOut).

xliff_file_contents([], _Type, ListIn-ListIn).
xliff_file_contents([F | R], Type, ListIn-ListOut) :-
	xliff_file_element(F, Type, ListIn-ListNext),
	!,
	xliff_file_contents(R, Type, ListNext-ListOut).

xliff_file_element(element(body, _Attrs, List), Type, ListIn-ListOut) :-
	xliff_body(List, Type, ListIn-ListOut),
	!.
xliff_file_element(_Other, _Type, ListIn-ListIn).

xliff_body([], _Type, ListIn-ListIn).
xliff_body([F | R], Type, ListIn-ListOut) :-
	xliff_body_element(F, Type, ListIn-ListNext),
	!,
	xliff_body(R, Type, ListNext-ListOut).

/*
<trans-unit id="3">
        <source>Diseases can be divided into a few large broad categories based on their main causes (see Table 1.1).</source>
        <target phase-name="r3.1">Les maladies peuvent être divisées en plusieurs grandes catégories en fonction de leurs causes principales (cf. Tableau 1).</target>
        <alt-trans phase-name="mt_baseline">Les maladies peuvent être divisés en quelques grands grandes catégories basée sur leurs causes principales (Table 1).</alt-trans>
      </trans-unit>
*/      

xliff_body_element(element('trans-unit', _Attrs, List), Type, ListIn-ListOut) :-
	xliff_trans_unit_elements(List, List1),
	trans_unit_with_postediting(List1),
	List2 = [type=Type | List1],
	Record = pe_record(List2),
	add_diffs_to_xliff_record(Record, Record1),	
	ListIn = [Record1 | ListOut],
	!.
xliff_body_element(_Other, _Type, ListIn-ListIn).

xliff_trans_unit_elements([], []).
xliff_trans_unit_elements([F | R], [F1 | R1]) :-
	xliff_trans_unit_element(F, F1),
	!,
	xliff_trans_unit_elements(R, R1).

xliff_trans_unit_element(element(source, _Attrs, [Source]), source=Source) :-
	!.
xliff_trans_unit_element(element(TargetOrAlttrans, ['phase-name'='mt_baseline'], [TargetElement]), target=Target) :-
	target_or_alt_trans(TargetOrAlttrans),
	target_element_to_target(TargetElement, Target),
	!.
xliff_trans_unit_element(element(TargetOrAlttrans, ['phase-name'=Version], [TargetElement]), pe(N)=Target) :-
	target_or_alt_trans(TargetOrAlttrans),
	target_element_to_target(TargetElement, Target),
	atomic(Version),
	atom_codes(Version, Str),
	pe_tag(_VersionNum, N, Str, []),
	!.

target_or_alt_trans(target).
target_or_alt_trans('alt-trans').

target_element_to_target(TargetElement, Target) :-
	atomic(TargetElement),
	!,
	TargetElement = Target.
target_element_to_target(element(target, _Atr, [Target]), Target) :-
	atomic(Target),
	!.
target_element_to_target(TargetElement, Target) :-
	format('~N*** Error: bad call: ~w~n', [target_element_to_target(TargetElement, Target)]),
	fail.

trans_unit_with_postediting(List) :-
	member(source=_Source, List),
	member(target=_Target, List),
	member(pe(_)=_PE, List),
	!.

pe_tag(Id, N) -->
	"r",
	integer(Id),
	".",
	integer(N).

integer(N) -->
	digit_string(Str),
	{number_codes(N, Str)}.

digit_string([F | R]) -->
	[F],
	{digit_char(F)},
	!,
	digit_string(R).
digit_string([]) -->
	[].
	
%----------------------------------------------------------------------

add_diffs_to_xliff_record(pe_record(List), pe_record(List1)) :-
	member(target=TargA, List),
	tokenize_sent_atom(TargA, Targ),
	add_diffs_to_xliff_record1(List, Targ, List1),
	!.

add_diffs_to_xliff_record1([], _Targ, []).
add_diffs_to_xliff_record1([pe(I)=PostA | R], Targ, [pe(I)=PostA, diff(I)=Diff | R1]) :-
	tokenize_sent_atom(PostA, Post),
	smart_diff(Post, Targ, Diff),
	!,
	add_diffs_to_xliff_record1(R, Targ, R1).
add_diffs_to_xliff_record1([Other | R], Targ, [Other | R1]) :-
	!,
	add_diffs_to_xliff_record1(R, Targ, R1).

%----------------------------------------------------------------------

replace_strings_with_atoms_in_xml(Atom, Atom) :-
	atomic(Atom),
	!.
replace_strings_with_atoms_in_xml(pcdata(String), Atom) :-
	safe_string_to_atom_or_number(String, Atom),
	!.
replace_strings_with_atoms_in_xml(pcdata(String), Atom) :-
	Atom = 'UNMAPPABLE_STRING',
	show_unmappable_string(String),
	!.
replace_strings_with_atoms_in_xml(String, Atom) :-
	is_list_of_non_negative_integers(String),
	safe_string_to_atom_or_number(String, Atom),
	!.
replace_strings_with_atoms_in_xml(Term, Term1) :-
	functor(Term, F, N),
	functor(Term1, F, N),
	replace_strings_with_atoms_in_xml_args(N, Term, Term1).

replace_strings_with_atoms_in_xml_args(I, _Term, _Term1) :-
	I =< 0,
	!.
replace_strings_with_atoms_in_xml_args(I, Term, Term1) :-
	I > 0,
	arg(I, Term, Arg),
	arg(I, Term1, Arg1),
	replace_strings_with_atoms_in_xml(Arg, Arg1),
	I1 is I - 1,
	!,
	replace_strings_with_atoms_in_xml_args(I1, Term, Term1).

warn_if_overlong_string(String) :-
	length(String, N),
	(   N > 1000 ->
	    length(Prefix, 50),
	    append(Prefix, _, String),
	    format('~N*** Warning: long string (~d characters) "~s"~n', [N, String]),
	    fail
	;
	    otherwise ->
	    true
	).

%======================================================================

safe_string_to_atom_or_number(String0, Atom) :-
	%is_prolog_string(String0),
	is_list_of_non_negative_integers(String0),
	remove_bad_chars_and_warn_if_necessary(String0, String),
	warn_if_overlong_string(String),
	(   safe_number_codes(Atom, String)
	;
	    atom_codes(Atom, String)
	),
	!.

remove_bad_chars_and_warn_if_necessary(Str, Str1) :-
	remove_bad_chars(Str, Str1, BadChars-[]),
	(   BadChars = [] ->
	    true
	;
	    format('~N*** Warning: suspicious chars ~w removed from "~s"~n', [BadChars, Str1])
	),
	!.

remove_bad_chars([], [], Bad-Bad).
remove_bad_chars([BadChar | R], Out, [BadChar | BadNext]-BadOut) :-
	bad_char(BadChar),
	!,
	remove_bad_chars(R, Out, BadNext-BadOut).
remove_bad_chars([F | R], [F | R1], BadIn-BadOut) :-
	!,
	remove_bad_chars(R, R1, BadIn-BadOut).

% We seem to get odd chars with values around 65000 from Excel - probable BOM marks
bad_char(BadChar) :-
	BadChar > 65000.

show_unmappable_string(String) :-
	format('~N*** Warning: unmappable string: ~w~n', [String]),
	!.

%======================================================================

remove_comments_in_xml(Var, Var) :-
	var(Var),
	!.
remove_comments_in_xml(Atom, Atom) :-
	atomic(Atom),
	!.
remove_comments_in_xml(List, List1) :-
	is_list(List),
	remove_comments_in_xml_list(List, List1),
	!.
remove_comments_in_xml(Term, Term1) :-
	functor(Term, F, N),
	functor(Term1, F, N),
	remove_comments_in_xml_args(N, Term, Term1).

remove_comments_in_xml_list([], []).
remove_comments_in_xml_list([comment(_) | R], R1) :-
	!,
	remove_comments_in_xml_list(R, R1).
remove_comments_in_xml_list([F | R], [F1 | R1]) :-
	remove_comments_in_xml(F, F1),
	!,
	remove_comments_in_xml_list(R, R1).

remove_comments_in_xml_args(I, _Term, _Term1) :-
	I =< 0,
	!.
remove_comments_in_xml_args(I, Term, Term1) :-
	I > 0,
	arg(I, Term, Arg),
	arg(I, Term1, Arg1),
	remove_comments_in_xml(Arg, Arg1),
	I1 is I - 1,
	!,
	remove_comments_in_xml_args(I1, Term, Term1).

%---------------------------------------------------------------

append_atoms_or_lists(A, B, [A, B]) :-
	atomic(A),
	atomic(B),
	!.
append_atoms_or_lists(A, B, [A | B]) :-
	atomic(A),
	is_list(B),
	!.
append_atoms_or_lists(A, B, AB) :-
	is_list(A),
	atomic(B),
	append(A, [B], AB),
	!.
append_atoms_or_lists(A, B, AB) :-
	is_list(A),
	is_list(B),
	append(A, B, AB),
	!.

%---------------------------------------------------------------

coerce_to_list(Var, []) :-
	var(Var),
	!.
coerce_to_list([], []) :-
	!.
coerce_to_list(null, []) :-
	!.
coerce_to_list(X, [X]) :-
	atomic(X),
	!.
coerce_to_list(same(X), List) :-
	coerce_to_list(X, List),
	!.
coerce_to_list(del(X), List) :-
	coerce_to_list(X, List),
	!.
coerce_to_list(ins(_X), []) :-
	!.
coerce_to_list(sub(_X, Y), List) :-
	coerce_to_list(Y, List),
	!.
coerce_to_list(exch(X, Y), List) :-
	coerce_to_list([X, Y], List),
	!.
coerce_to_list([F | R], List) :-
	coerce_to_list(F, FList),
	coerce_to_list(R, RList),
	append(FList, RList, List),
	!.
coerce_to_list(_Other, []).

%---------------------------------------------------------------

coerce_to_target_list(Var, []) :-
	var(Var),
	!.
coerce_to_target_list([], []) :-
	!.
coerce_to_target_list(null, []) :-
	!.
coerce_to_target_list(X, [X]) :-
	atomic(X),
	!.
coerce_to_target_list(same(X), List) :-
	coerce_to_target_list(X, List),
	!.
coerce_to_target_list(del(_X), []) :-
	!.
coerce_to_target_list(ins(X), List) :-
	coerce_to_target_list(X, List),
	!.
coerce_to_target_list(sub(X, _Y), List) :-
	coerce_to_target_list(X, List),
	!.
coerce_to_target_list(exch(X, Y), List) :-
	coerce_to_target_list([Y, X], List),
	!.
coerce_to_target_list([F | R], List) :-
	coerce_to_target_list(F, FList),
	coerce_to_target_list(R, RList),
	append(FList, RList, List),
	!.
coerce_to_target_list(_Other, []).

%---------------------------------------------------------------

at_most_last_two([], []) :-
	!.
at_most_last_two([X], [X]) :-
	!.
at_most_last_two([X, Y], [X, Y]) :-
	!.
at_most_last_two([_F | R], LastTwo) :-
	!,
	at_most_last_two(R, LastTwo).

%---------------------------------------------------------------

at_most_first_two([], []) :-
	!.
at_most_first_two([X], [X]) :-
	!.
at_most_first_two([X, Y | _], [X, Y]) :-
	!.

%---------------------------------------------------------------

