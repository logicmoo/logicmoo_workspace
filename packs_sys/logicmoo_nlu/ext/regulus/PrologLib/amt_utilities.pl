
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%======================================================================

:- module(amt_utilities,
	  [amt_csv_file_to_list_of_alists/2,
	   amt_csv_file_to_list_of_alists/4,
	   amt_csv_file_to_list_of_alists/5,
	   reorganise_cvs_file/5,
	   reorganise_cvs_file/6,

	   check_csv_file_for_duplicate_lines/3]
    ).

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%======================================================================

amt_csv_file_to_list_of_alists(InFile, FinalContents) :-
	amt_csv_file_to_list_of_alists(InFile, 0'", 0',, FinalContents).  %" To keep Emacs happy

amt_csv_file_to_list_of_alists(InFile, DelimiterChar, SeparatorChar, FinalContents) :-
	amt_csv_file_to_list_of_alists(InFile, DelimiterChar, SeparatorChar, default_encoding, FinalContents).

amt_csv_file_to_list_of_alists(InFile, DelimiterChar, SeparatorChar, Encoding, FinalContents) :-
	read_batch_file(InFile, DelimiterChar, SeparatorChar, Encoding, Header, Contents1),
	parse_header(Header, ParsedHeader),
	tag_contents(Contents1, ParsedHeader, FinalContents),
	!.

%======================================================================

reorganise_cvs_file(InFile, NCopiesPerLine, Header, Filler, OutFile) :-
	reorganise_cvs_file(InFile, default_encoding, NCopiesPerLine, Header, Filler, OutFile).

reorganise_cvs_file(InFile, Encoding, NCopiesPerLine, Header, Filler, OutFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),

	csv_file_to_list_of_lists(AbsInFile, Encoding, 0'", 0',, InList), %" To keep Emacs happy
	length(InList, NIn),
	format('~N--- Read CSV file (~d records) ~w~n', [NIn, AbsInFile]),

	warn_if_bad_header_or_filler(InList, Header, header),
	warn_if_bad_header_or_filler(InList, Filler, filler),

	full_header(NCopiesPerLine, 1, Header, FullHeader-[]),
	       
	reorganise_cvs_list(InList, NCopiesPerLine, Filler, OutList),

	length(OutList, NOut),
	list_of_lists_to_csv_file([FullHeader | OutList], AbsOutFile, Encoding),
	format('~N--- Written CSV file (~d records, ~d items per line) ~w~n', [NOut, NCopiesPerLine, AbsOutFile]),
	!.
reorganise_cvs_file(InFile, Encoding, NCopiesPerLine, Header, Filler, OutFile) :-
	format('~N*** Error: bad call: ~w~n',
	       [reorganise_cvs_file(InFile, Encoding, NCopiesPerLine, Header, Filler, OutFile)]),
	fail.

warn_if_bad_header_or_filler([], _Filler, _Type).
warn_if_bad_header_or_filler(InList, Filler, Type) :-
	InList = [FirstItem | _],
	length(FirstItem, N),
	(   ( is_list_of_atoms(Filler), length(Filler, N) ) ->
	    true
	;
	    format('~N*** Error: bad value for ~w "~w". Should be list with ~d atomic elements', [Type, Filler, N]),
	    fail
	).

full_header(N, _I, _Header, In-In) :-
	N =< 0,
	!.
full_header(N, I, Header, In-Out) :-
	add_number_to_header(Header, I, In-Next),
	N1 is N - 1,
	I1 is I + 1,
	!,
	full_header(N1, I1, Header, Next-Out).

add_number_to_header([], _I, In-In).
add_number_to_header([F | R], I, In-Out) :-
	format_to_atom('~w~d', [F, I], F1),
	In = [F1 | Next],
	!,
	add_number_to_header(R, I, Next-Out).	

reorganise_cvs_list([], _NCopiesPerLine, _Filler, []).
reorganise_cvs_list(InList, NCopiesPerLine, Filler, OutList) :-
	length(InList, N),
	N >= NCopiesPerLine,
	length(InitialSegment, NCopiesPerLine),
	append(InitialSegment, InRest, InList),
	append_list(InitialSegment, NewRecord),
	OutList = [NewRecord | OutRest],
	!,
	reorganise_cvs_list(InRest, NCopiesPerLine, Filler, OutRest).
reorganise_cvs_list(InList, NCopiesPerLine, Filler, [LastRecord]) :-
	length(InList, N),
	N < NCopiesPerLine,
	NMissing is NCopiesPerLine - N,
	append_n_copies(NMissing, [Filler], Missing),
	append(InList, Missing, CompletedSegment),
	append_list(CompletedSegment, LastRecord),
	!.
reorganise_cvs_list(InList, NCopiesPerLine, Filler, OutList) :-
	format('~N*** Error: bad call: ~w~n', [reorganise_cvs_list(InList, NCopiesPerLine, Filler, OutList)]),
	fail.	

%======================================================================

read_batch_file(InFile, DelimiterChar, SeparatorChar, Encoding, Header, Contents) :-
	safe_absolute_file_name(InFile, AbsInFile),
	csv_file_to_list_of_lists(AbsInFile, Encoding, DelimiterChar, SeparatorChar, List),
	List = [Header | Contents],
	length(Contents, N),
	format('~N--- Read file (~d records) ~w~n', [N, AbsInFile]),
	!.

%======================================================================

parse_header([], []).
parse_header([F | R], [F1 | R1]) :-
	parse_header_element(F, F1),
	!,
	parse_header(R, R1).

parse_header_element(Elt, Elt1) :-
	atom_codes(Elt, Str),
	parse_header_element1(Str, Elt1),
	!.
parse_header_element(Elt, Elt) :-
	!.
parse_header_element(Elt, Elt1) :-
	format('~N*** Error: bad call: ~w~n', [parse_header_element(Elt, Elt1)]),
	fail.

parse_header_element1(Str, Elt) :-
	header_element(Elt, Str, []).

%----------------------------------------------------------------------

/*

HITId	HITTypeId	Title	Description	Keywords	Reward	CreationTime	MaxAssignments	RequesterAnnotation	AssignmentDurationInSeconds	AutoApprovalDelayInSeconds	Expiration	NumberOfSimilarHITs	LifetimeInSeconds	AssignmentId	WorkerId	AssignmentStatus	AcceptTime	SubmitTime	AutoApprovalTime	ApprovalTime	RejectionTime	RequesterFeedback	WorkTimeInSeconds	LifetimeApprovalRate	Last30DaysApprovalRate	Last7DaysApprovalRate	Input.Command1	Input.Command2	Input.Command3	Input.Command4	Input.Command5	Input.Command6	Input.Command7	Input.Command8	Input.Command9	Input.Command10	Input.Command11	Input.Command12	Input.Command13	Input.Command14	Input.Command15	Input.Command16	Input.Command17	Input.Command18	Input.Command19	Input.Command20	Input.Command21	Input.Command22	Input.Command23	Input.Command24	Input.Command25	Answer.Command4	Answer.Comments	Answer.Command16	Answer.Command5	Answer.Gender	Answer.Command17	Answer.Command6	Answer.Command18	Answer.Location	Answer.Command7	Answer.Command19	Answer.Command20	Answer.Command8	Answer.Command21	Answer.Command9	Answer.Command10	Answer.Command22	Answer.StartTime	Answer.Command11	Answer.Command23	Answer.Command12	Answer.Command24	Answer.Command1	Answer.Command13	Answer.Command25	Answer.Command2	Answer.Command14	Answer.Command3	Answer.EndTime	Answer.Command15	Approve	Reject

*/

header_element(input(Command)) -->
	"Input.",
	command_name(Command).

header_element(answer(Command)) -->
	"Answer.",
	command_name(Command).

command_name(Command) -->
	char_sequence(Str),
	{atom_codes(Command, Str)}.

char_sequence([F | R]) -->
	[F],
	!,
	char_sequence(R).
char_sequence([]) --> [].

%----------------------------------------------------------------------

%tag_contents(Contents, ParsedHeader, TaggedContents)

tag_contents([], _ParsedHeader, []).
tag_contents([F | R], ParsedHeader, [F1 | R1]) :-
	tag_contents_line(F, ParsedHeader, F1),
	!,
	tag_contents(R, ParsedHeader, R1).

tag_contents_line([], [], []).
tag_contents_line([F | R], [HF | HR], [HF-F | R1]) :-
	!,
	tag_contents_line(R, HR, R1).
% There may be missing fields at the end of the line. Fill them in with null values
tag_contents_line([], [HF | HR], [HF-'' | R1]) :-
	!,
	tag_contents_line([], HR, R1).

%----------------------------------------------------------------------

check_csv_file_for_duplicate_lines(InFile, Encoding, Separator) :-
	safe_absolute_file_name(InFile, AbsInFile),
	csv_file_to_list_of_lists(AbsInFile, Encoding, 0'", Separator, InList), %" To keep Emacs happy
	length(InList, NIn),
	format('~N--- Read CSV file (~d records) ~w~n', [NIn, AbsInFile]),
	list_to_ordered_multiset(InList, Multiset),
	print_duplicate_lines_in_multiset(Multiset),
	!.

print_duplicate_lines_in_multiset([]).
print_duplicate_lines_in_multiset([N-Item | R]) :-
	(   N > 1 ->
	    print_line_from_multiset(N, Item)
	;
	    otherwise ->
	    true
	),
	!,
	print_duplicate_lines_in_multiset(R).

print_line_from_multiset(N, Item) :-
	format('~N~nMultiplicity: ~d~n', [N]),
	print_lines_from_multiset1(Item).

print_lines_from_multiset1([]).
print_lines_from_multiset1([F | R]) :-
	format('~N~w~n', [F]),
	!,
	print_lines_from_multiset1(R).

