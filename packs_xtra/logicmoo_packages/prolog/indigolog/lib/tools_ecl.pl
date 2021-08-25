%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  FILE   : lib/tools_ecl.pl
%
%	Library of tools for ECLIPSE Prolog (sockets, strings, OS tools)
%
%  AUTHOR : Sebastian Sardina (2003)
%  EMAIL  : ssardina@cs.toronto.edu
%  WWW    : www.cs.toronto.edu/~ssardina
%  TYPE   : system dependent code
%  TESTED : ECLIPSE 5.4 http://www.icparc.ic.ac.uk/eclipse/
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% PROVIDED FROM ISO and Quintus Libraries:
%
% catch/3, throw/1, multifile/1, assertz/1, retractall/1, sub_atom/5
%
%
% -- tab/1       : Writes N tabs spaces  
% -- get0(-Ascii): Obtain next Ascii character from input stream
% -- wait_for_input(StreamList, ReadyList, TimeOut) :
%                  Returns streams from StreamList which are ready for I/O, 
%                  blocking at most TimeOut seconds.
% -- select_stream/3 : equivalent to select/3
% -- turn_on_gc  : turns on the automatic garbage collector
% -- turn_off_gc : turns off the automatic garbage collector
% -- random/3    : generates a random number between two integers
% -- is_list/1   : check for list type
% -- last/2      : get the last element of a list
% -- proc_wait/2   : wait for process to finish and get its status
% -- proc_exists/1 : check whether process exists
% -- proc_kill/1   : kill process
% -- file_exists/1 : check whether file exists
%
% Strings:
%
% -- remove_nl/2       : remove all line breaks from a string
% -- string_to_atom/2  : convert between strings an atoms
% -- string_to_list/2  : convert between strings an lists of chars
% -- string_to_term/2  : convert between strings an terms
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Set the standard quotes for strings (`), list of strings ("), and atoms (')
%:- set_chtab(0'`, string_quote).
%:- set_chtab(0'", list_quote).
%:- set_chtab(34, list_quote).
%:- set_chtab(0'', atom_quote).

:- module(tools_ecl).
:- local chtab(0'`, string_quote), chtab(0'", list_quote).

%% A - EXPORT ECLIPSE SPECIFIC TOOLS
:- export                
   % 1 - SOCKETS
   tcp_socket/1,          % Compatibility with SWI
   tcp_bind/2,            % Compatibility with SWI
   tcp_listen/2,          % Compatibility with SWI
   tcp_accept_socket/5,   % Compatibility with SWI
   % 2 - STRINGS
   string_to_atom/2,      % Compatibility with SWI
   string_to_list/2,      % Compatibility with SWI
   string_to_term/2,      % Compatibility with SWI
   string_to_number/2,
   % 3 - OS TOOLS
   turn_on_gc/0,
   turn_off_gc/0,
   proc_exists/1,
   proc_kill/1,
   proc_wait/2,
   file_exists/1,
   gethostname/1,         % Compatibility with SWI
   call_to_exec/3,
   time/1,		  % (Partial) compatibility with SWI
   catch_fail/2,
   catch_succ/2,
   % 5 - CONSTRAINTS
   indomain_rand/1,
   % 6 - OTHER TOOLS
   %catch/3,              % RE-EXPORTED
   %call_succ/2,
   %call_fail/2,
   %thhrow/1,             % RE-EXPORTED 
   %multifile/1,          % RE-EXPORTED
   %assertz/1,            % RE-EXPORTED
   %retractall/1,         % RE-EXPORTED
   %shuffle/2,
   term_to_atom/2,
   atom_number/2,	  % Compatibulity with SWI
   %number_chars/2, 	  % RE-EXPORTED (iso)
   %atom_chars/2,	  % RE-EXPORTED (iso)
   tab/1,
   wait_for_input/3,      % Compatibility with SWI
   get0/1,
   random/3,
   is_list/1,             % Compatibility with SWI
   last/2,                % Compatibility with SWI
   set_backquoted_string/0,
   reset_backquoted_string/0,
   style_check/1,    % Compatibility with SWI
   module/2.              % Compatibility with SWI

%:- export set_chtab(0'`, string_quote).

%% B - RE-EXPORT FROM OTHER PACKAGES AND LIBRARIES

style_check(_).

% From ISO
:- ensure_loaded(library(iso)).
:- reexport catch/3, throw/1, multifile/1, sub_atom/5,
            assertz/1, flush_output/0, atom_chars/2,
	    number_chars/2  from iso.
:- import atom_chars/2, number_chars/2 from iso.


% From QUINTUS
%:- ensure_loaded(library(quintus)).
:- reexport retractall/1 from quintus.

% From LISTS package
:- ensure_loaded(library(lists)).
:- reexport maplist/3, shuffle/2 from lists.

% From APPLY
:- ensure_loaded(library(apply)).
:- reexport apply/2 from apply.

% From CIO (C-PROLOG COMPAT)
:- ensure_loaded(library(cio)).
:- reexport tell/1, telling/1, see/1, seeing/1, seen/0, told/0 from cio.



%% C - EXPORT FROM COMMON FILE
:- include(common).
:- export(extract_substring/6).        % String manipulation
:- export(any_to_number/2).
:- export(any_to_string/2).
:- export(lany_to_string/2).
:- export(emptyString/1).
:- export(build_string/2).
:- export(string_replace/4).
:- export(join_atom/3).
:- export(split_atom/4).                                 

:- export(send_data_socket/2).         % Protocol communication via sockets
:- export(receive_list_data_socket/2).                                 
:- export(receive_data_socket/2).                                 

:- export(report_message/2).           % Show a message
:- export(set_debug_level/1).          % Set debug level to N
:- export(proc_term/1).                % Check if process is terminated
:- export(catch_succ/2).
:- export(catch_fail/2).
:- export(get_argument/2).             
:- export(get_list_arguments/1).
:- export(subv/4).
:- export(sublist/2).
:- export(get_integer/3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1 - SOCKETS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Compatibility with SWI 
tcp_socket(S)    :- socket(internet, stream, S).
tcp_bind(S, P)   :- bind(S, P).
tcp_listen(S, N) :- listen(S,N).

tcp_accept_socket(S, R, W, RHost, RPort) :- 
        accept(S, RHost/RPort, RW), R=RW, W=RW.
tcp_accept_socket(S, R, W, RHost, RPort) :- 
        accept(S, RHost/RPort, RW), R=RW, W=RW.
        

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2 - STRINGS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Convertion between strings, and atoms, lists and terms
string_to_atom(S, A)   :- string(S), atom_string(A, S).
string_to_atom(A, A)   :- atom(A).
string_to_list(S, L)   :- string_list(S, L).
string_to_term(S, T)   :- term_string(T, S).
string_to_number(S, N) :- number_string(N, S).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3 - OPERATING SYSTEM TOOLS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Turn on/off the automatic garbage collector
turn_on_gc  :- set_flag(gc, on).
turn_off_gc :- set_flag(gc, off).

proc_exists(Pid)       :- kill(Pid, 0).
proc_kill(Pid)         :- kill(Pid, 9).
proc_wait(Pid, S)      :- wait(Pid, S).
file_exists(File)      :- exists(File).

% Host is the name of the current host machine
gethostname(Host) :- get_flag(hostname, Host).

% Command2 executes Command in plataform unix
call_to_exec(unix, Command, Command2) :-
        concat_atom(['sh -c \"', Command, '\"'], Command2).

% time(Goal) : Execute Goal just like once/1 (i.e., leaving no choice points), 
% but print used time
:- tool(time/1, time/2).
time(G,M) :- cputime(X1), 
	     (call(once(G))@M -> 
		cputime(X2),
		X3 is X2-X1,
		time_mesg(I, X3)
	     ; 
		cputime(X2),
		X3 is X2-X1,
		time_mesg(I, X3),
		fail
	     ).

time_mesg(I, S) :-
	     write('% '),
	     write(I),
             write(' inferences in '),
	     write(S),
	     write('  seconds'),
	     nl.


% Perform a call catching it if there is an exception
% If so, print message and then either fail or succeed
:- tool(catch_fail/2, catch_fail/3).
catch_fail(Call, Message, Module) :-
	catch(Call,E,
		(report_message(warning,[Message, ' ---> ', E]),
	     fail)
	    )@Module.
:- tool(catch_succ/2, catch_succ/3).
catch_succ(Call, Message, Module) :-
	catch(Call,E,
		(report_message(warning,[Message, ' ---> ', E]),
	     true)
	    )@Module.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 4 - CONSTRAINTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%:- import fd and fd_search libraries
:- lib(fd).
:- lib(fd_search).

% Values for X are tried in a random order. 
% On backtracking, the previously tried value is removed. 
indomain_rand(X) :-
        % Find out how many domain elements we have to choose from.
        dvar_domain(X, Dom),
        dom_size(Dom, Size),
        % Get the domain elements.
        X :: L,
        % Choose one at random.
        Index is 1 + (random mod Size),
        nth_value(L, Index, Try),
        % Try assigning it.
        indomain_rand(X, Try).

indomain_rand(X, X).
indomain_rand(X, Try) :-
        X #\= Try,
        indomain_rand(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 5 - OTHER TOOLS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Writes N tabs
tab(N):- N=<0.
tab(N):- N>0, write('\t'), N2 is N-1, tab(N2).

% ECLiPSe Prolog lacks get0/1
% get0(-Ascii): Obtain next Ascii character from input stream
get0(X) :-  get(input, X).

% Returns streams from StreamList which are ready for I/O, blocking at most
% TimeOut seconds. (compatible with SWI)
wait_for_input(StreamList, ReadyList, 0) :- !, 
        select(StreamList, block, ReadyList).
wait_for_input(StreamList, ReadyList, TimeOut) :- 
        select(StreamList, TimeOut, ReadyList).

% Generates a random number N between [L,U]
random(Lower, Upper, N) :- 
        N is Lower + (random mod Upper).

% is L a list?
is_list(L):- functor(L,'.',_).

% last(Last, List): Last is the last element of List
last(Last,[Head|Tail]) :-
	last_1(Tail, Head, Last).
last_1([],Last,Last).
last_1([Head|Tail], _, Last) :-
	last_1(Tail, Head, Last).

% Convert between terms and atoms
term_to_atom(T, A) :- ground(T), term_string(T, S), atom_string(A, S).
term_to_atom(T, A) :- ground(A), atom_string(A, S), term_string(T, S).

% Convert between atoms and numbers
atom_number(A, N) :- ground(A), !, atom_chars(A, C), number_chars(N,C).
atom_number(A, N) :- ground(N), atom_chars(A, C), number_chars(N,C).

% Module definition compatible with SWI Prolog
module(Name, LExports) :- create_module(Name),
                          export_list(LExports).
export_list([]).
export_list([P|L]) :- export P, export_list(L).

% Sets string and list of chars constructs to the module that called
:- tool(set_backquoted_string/0, set_backquoted_string/1).
set_backquoted_string(M) :- call(set_chtab(0'`, string_quote))@M,
                            call(set_chtab(0'", list_quote))@M.
:- tool(reset_backquoted_string/0, reset_backquoted_string/1).
reset_backquoted_string(M) :- call(set_chtab(0'", string_quote))@M,
                            call(set_chtab(0'`, list_quote))@M.
      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: lib/tools_ecl.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%