% CELT Corpus Testing

:- style_check(-singleton).      % no singletons warnings
:- style_check(-discontiguous).  % allows more creative predicate placement

:-write('Loading CELT Corpus Testing Code'),nl.

%-------------------------------
% CORPUS TESTING CODE
%-------------------------------

% Each corpus is expected to be a file of sentences or queries
% where each text input is separated from the next by two carriage
% returns, e.g.,
%
% John walks the dog.
%
% He enters the bank.
%
% Who enters the bank?
%
% etc....

test1 :- read_corpus_and_print_results('openmind.txt').

test2 :- read_corpus_and_save_results_to_file('openmind.txt','results_openmind.txt').

test3 :- read_corpus_and_print_results('BrownCorpus.txt').

test4 :- read_corpus_and_save_results_to_file('BrownCorpus.txt','results_browncorpus.txt').


initialize_flags :-
	flag(tested, _, 0), % number of corpus lines tested
	flag(parsed, _, 0), % number of corpus lines that parsed
	flag(failed, _, 0). % number of corpus lines that failed to parse	

% summarize_corpus_testing writes out a summary like this:

% Attempted to parse 2456 corpus lines.
% Parsed 3 lines (0%) and failed to parse 2453 lines (100%).

% or hopefully something more encouraging...!

summarize_corpus_testing :-
	flag(tested, N, N),
	flag(parsed, Parsed, Parsed),
	flag(failed, Failed, Failed),	
	(N > 0 -> Ratio_Parsed is (Parsed / N) * 100; Ratio_Parsed is 0),
        Ratio_Failed is 100 - Ratio_Parsed,
	format("~n~nAttempted to parse ~D corpus lines.~n",
	       [N]),
	format("Parsed ~D lines ( ~w %) and failed to parse ~D lines ( ~w %).~n",
	       [Parsed,Ratio_Parsed,Failed,Ratio_Failed]).

read_corpus_and_save_results_to_file(FILE1,FILE2) :- see(FILE1),
	initialize_flags,
	tell(FILE2),
	read_all_lines(1000000),
	summarize_corpus_testing,
	seen,
	told.

read_corpus_and_print_results(FILE) :-
	initialize_flags,
	see(FILE),
	read_all_lines(1000000),
	summarize_corpus_testing,
	seen.

read_all_lines(0) :- !,write('\%Done.'),nl.

read_all_lines(N) :- at_end_of_stream,!,write('\%End of file'),nl.

read_all_lines(N) :-
	M is N - 1,!,
	read_corpus_line,
	read_all_lines(M).

read_all_lines(N) :- write('Fell through read_all_lines ??'),nl.

% read_corpus_line reads characters until TWO end of line
% markers are found. 

read_corpus_line :-
	read_corpus_line([],Corpus_Line),
	flag(tested, N, N+1),
	M is N+1,
	nl,write(M),write('. '),
	write(Corpus_Line),
	nl,
	eng2log(Corpus_Line) ->
	(write('Parsed OK: '),write(Corpus_Line),nl,flag(parsed, Parsed, Parsed+1));
	(write('Parse FAILED.'),nl,flag(failed, Failed, Failed+1)).

read_corpus_line(Chars, Corpus_Line) :-
	at_end_of_stream,
	write('End of file found while reading a comment'),!.

read_corpus_line(Chars, Corpus_Line) :-
	peek_code(Code),
	end_of_line(Code),
	get0(Code),
	peek_code(Code),
	end_of_line(Code),
	!,
	get0(Code),
	reverse(Chars,InOrder),
	name(Corpus_Line,InOrder).

read_corpus_line(Chars, Corpus_Line) :-
	get0(Code),
	read_corpus_line([Code|Chars],Corpus_Line).

end_of_line(10).    /* \n  */

separator(32).    /* ' ' */
separator(10).    /* \n  */
separator(9).     /* \t  */
separator(40).    /* '(' */
separator(41).    /* ')' */

whitespace(32).    /* ' ' space */
whitespace(9).     /* \t  tab */
whitespace(10).    /* \n  newline */

