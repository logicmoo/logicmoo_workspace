% CELT interface to automatic theorem provers

% Copyright © 2002 Teknowledge Corporation
% This software released under the GNU Public License <http://www.gnu.org/copyleft/gpl.html>.

:- style_check(-singleton).      % no singletons warnings
:- style_check(-discontiguous).  % allows more creative predicate placement

:-write('Loading CELT Version 2(b) ...interface to automatic theorem prover...'),nl.

%----------------------------------
% ATP Interface via Simple File I/O
%----------------------------------

% The file I/O interface to a theorem prover is the simplest
% possible. Basically, CELT writes queries as files to a shared
% directory that the theorem prover monitors.

% When a new query file has been written the theorem prover reads it in,
% executes the query or assertion, and then returns its output in an
% answer file.

% The shared directory is set by parameters in both this file and the
% ATP code and must match of course. The default is 'C:\CELT_ATP_IO',
% which may need to be created when loading CELT on a new machine.

% Initially there is only one file in that shared directory, a text file
% called 'counter.txt' which holds this text:

%           1

% When CELT wishes to create a new file it uses the value of that
% variable and creates a file with a "Q" prefix. When the theorem prover
% has answered the query, or processed the assertion, it writes it
% results in a file with an "A" prefix. The numbers for each query and
% answer always correspond:

% Q1.txt is answered by A1.txt
% Q2.txt is answered by A2.txt

% and so on. Thus we also have a trace of the queries and answers
% between CELT and the theorem prover to assist in debugging.

% When the theorem prover writes out its answer it first updates the
% file 'counter.txt' to bump the index, ready for the next query.

no_atp_interface  :- flag(atp_interface,X,0). % if the interface should be disabled or is not present.
use_atp_interface :- flag(atp_interface,X,1). % the default, assumes the ATP interface is present and working.

% ask_query_or_make_assertion(+KIF,+Kind_Of_Input,+Speech_Act)
% KIF is the KIF assertion or query.
% Kind_Of_Input is sentence or query.
% Speech_Act is either 'assertion', query(wh,VARS), or query(yes_no,[]).

% Disable all ATP processing if atp_interface flag is 0.
ask_query_or_make_assertion(KIF,Kind_Of_Input,Speech_Act) :- flag(atp_interface,X,X),X==0,!.

ask_query_or_make_assertion(KIF,sentence,assertion) :- !, make_assertion(KIF).

ask_query_or_make_assertion(KIF,query,query(Kind_Of_Query,Query_Vars)) :- !, ask_query(KIF,Kind_Of_Query,Query_Vars).

ask_query_or_make_assertion(KIF,Other) :-
	format('ATP interface currently does nothing with ~w, so ~w is discarded.~n',
	       [Other,KIF]).
	

% ask_query(+Query,+Kind_Of_Query,+Query_Vars)
% asks a query. The counter is accessed and a new query file
% written out then we sleep until the theorem prover writes
% its answer back, at which point we read in the answer file
% and return that as the answer. Both the query and answer
% variables are strings.

ask_query(Query,Kind_Of_Query,Query_Vars) :-
	read_counter_file,
	flag(counter, Counter, Counter),
	concat_atom(['c:\\celt_atp_io\\Q',Counter,'.txt'],NextQueryFileName),
	format('~nWriting ~w query to file ~w.~n',[Kind_Of_Query,NextQueryFileName]),
	(access_file(NextQueryFileName,write) -> format('Check: OK for writing.~n'); format('ERROR: Not OK for writing.~n')),
	pretty_print_to_string(Query_Vars,no,Query_Vars_in_KIF),
	tell(NextQueryFileName),
	format('Query~n~w~n~n~w~n~n',[Query,Query_Vars_in_KIF]),
	told.

make_assertion(Assertion) :-
	read_counter_file,
	flag(counter, Counter, Counter),
	concat_atom(['c:\\celt_atp_io\\Q',Counter,'.txt'],NextQueryFileName),
	format('~nWriting assertion to file ~w.~n',[NextQueryFileName]),
	(access_file(NextQueryFileName,write) -> format('Check: OK for writing.~n'); format('ERROR: Not OK for writing.~n')),
	tell(NextQueryFileName),
	format('Assertion~n~w~n~n',Assertion),
	told.

initialize_flags :-
	flag(counter, _, 0),  % same value as number in 'counter.txt' once that has been read in at least once.
	flag(read, _, 0),     % number of lines read in
	flag(asked, _, 0),    % number of queries asked
	flag(asserted, _, 0), % number of assertions made
	flag(answers, _, 0).  % number of answers collected for either queries or assertions.

% read_counter_file
% reads the file 'counter.txt' and writes that value to the counter flag to be used in
% generating file names for the next query file.	

read_counter_file :-
	access_file('c:\\celt_atp_io\\counter.txt',read),
	see('c:\\celt_atp_io\\counter.txt'),
	read_file_line(Contents),                % the contents should be a string representing a number, e.g., '2'
	atom_to_term(Contents,Term,Bindings),    % convert from a string to a term, which should be a number
	Query = Term,
	flag(counter,_,Query),                   % ensure that counter flag is the same number as stored in 'counter.txt'
	% format('Current index for next query = ~d~n',Query),
	seen.

% read_file_line reads characters until TWO end of line
% markers are found or the end of file is reached.

read_file_line(File_Line) :-
	read_file_line([],File_Line),
	flag(read, N, N+1),
	M is N+1,
	% nl,write(M),write('. '),
	% write(File_Line),
	% nl,
	% here we could perform any special processing on the line
	% e.g., eng2log(File_Line) -> flag(parsed, Parsed, Parsed+1); flag(failed, Failed, Failed+1)
	nl.

read_file_line(Chars, File_Line) :-
	at_end_of_stream,
	write('End of file in ATP interface found while reading'),!.

read_file_line(Chars, File_Line) :-
	peek_code(Code),
	end_of_line(Code),
	get0(Code),
	peek_code(Code),
	end_of_line(Code),
	!,
	get0(Code),
	reverse(Chars,InOrder),
	name(File_Line,InOrder).

read_file_line(Chars, File_Line) :-
	get0(Code),
	read_file_line([Code|Chars],File_Line).

end_of_line(10).    /* \n  */

separator(32).    /* ' ' */
separator(10).    /* \n  */
separator(9).     /* \t  */
separator(40).    /* '(' */
separator(41).    /* ')' */

whitespace(32).    /* ' ' space */
whitespace(9).     /* \t  tab */
whitespace(10).    /* \n  newline */

	