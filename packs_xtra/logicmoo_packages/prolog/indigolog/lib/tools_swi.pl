%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  FILE   : lib/tools_swi.pl
%
%	Library of tools for SWI Prolog (sockets, strings, OS tools, others)
%
%  AUTHOR : Sebastian Sardina (2003)
%  EMAIL  : ssardina@cs.toronto.edu
%  WWW    : www.cs.toronto.edu/~ssardina
%  TYPE   : system dependent code
%  TESTED : SWI Prolog 5.0.4 on Linux 7.1-8.0
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(tools_swi,[
           % STRINGS
           string_to_term/2,
           string_to_number/2,
           replace_char_string/4,
           % OS TOOLS
           thread_kill/1,
           thread_wait/2,
           call_to_exec/3,           
           proc_kill/1,
           proc_wait/2,
    	   file_exists/1,
%           register_stream_sigio/2,
%           unregister_stream_sigio/1,
           % OTHER TOOLS
           turn_on_gc/0,
           turn_off_gc/0,
           set_backquoted_string/0,
           reset_backquoted_string/0,
	       catch_fail/2,
    	   catch_succ/2,
           %
	   % FROM 'common.pl' file
	   subv/4,
	   sublist/2,
	   extract_substring/6,
	   get_integer/3,
	   extract_option/4,
	   extract_option/3,
       any_to_number/2,
       any_to_string/2,
	   lany_to_string/2,
 	   emptyString/1,
	   build_string/2,
	   string_replace/4,
	   join_atom/3,
	   split_atom/4,
	   proc_term/1,            % Check if process is terminated
	   proc_exists/1,          % Check if process exists
	   send_data_socket/2,
	   receive_list_data_socket/2,
	   receive_data_socket/2,
	   report_message/2,
           get_argument/2,
	   get_list_arguments/1,
           set_debug_level/1
          ]). % +SocketId

:- style_check(-discontiguous).     % Clauses may be not together
:- set_prolog_flag(backquoted_string, true).

:- use_module(library(socket)).		% Load socket library
:- use_module(eclipse_swi).         % Load compatibility library with ECLIPSE
:- init_eclipse_lib.

% Common tools for any Prolog
:- include(common).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2 - STRINGS
%
% -- string_to_term(?String, ?Term)
% -- string_to_number(?String, ?Term)
% -- replace_char_string(+String, +E1, +E2, -String2) 
%       String2 is string String with all chars E1 replaced by E2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% string_to_term/2 
string_to_term(S, T):- var(T), !,    % S ---> T
                       string_to_atom(S, A), term_to_atom(T, A). 
string_to_term(S, T):- \+ var(T),    % T ---> S
                       term_to_atom(T, A), string_to_atom(S, A).

% string_to_number/2 
string_to_number(S, N):- ground(N),
                         number_chars(N, L), string_to_list(S, L).
string_to_number(S, N):- ground(S),
                         string_to_atom(S, A), 
                         atom_codes(A, CA), 
                         number_codes(N, CA).

% replace_char_string/4
replace_char_string(S, E1, E2, S2) :- 
        atom_codes(E1,[CE1]),
        atom_codes(E2,[CE2]),
        string_to_list(S,SL),
        replace_element_list(SL,CE1,CE2,SL2),
        string_to_list(S2,SL2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3 - OPERATING SYSTEM TOOLS
%
%
% -- call_to_exec(+System, +Command, -Command2)
%      Command2 executes Command in plataform System
% -- thread_kill(+ThreadId)
% -- thread_wait(+ThreadId, -Status)
% -- proc_kill(+Pid)
% -- proc_wait(+Pid, -Status)
% -- file_exists(+File)
%      Succeeds if File exists
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% call_to_exec/3
call_to_exec(unix, Command, sh('-c',Command2)) :-
	string_concat(Command,' ; exit',Command2).

% Killing a thread means signal it with an "abort" event
thread_kill(ThreadId) :- thread_signal(ThreadId, throw(abort)),
                         wait(ThreadId, _).
thread_wait(ThreadId, Status) :- 
        current_thread(ThreadId, _) -> thread_join(ThreadId, Status) ; true.

proc_kill(Pid)    :- kill(Pid, 9).
proc_wait(Pid, S) :- repeat, wait(Pid, S).
file_exists(File) :- exists_file(File).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 4 - OTHER TOOLS
%
% -- turn_on_gc/0
% -- turn_off_gc/0
%       Turn on/off garbage collection
% -- set_backquoted_string/0
%       Set the backquoted_string flag to true (transparent predicate)
% -- catch_succ(+Call,+Message)
% -- catch_fail(+Call,+Message)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Turn on/off the automatic garbage collector
turn_on_gc  :- set_prolog_flag(gc, true).
turn_off_gc :- set_prolog_flag(gc, false).

% Set string construct to be ` to the calling module
:- module_transparent set_backquoted_string/0.
set_backquoted_string :- set_prolog_flag(backquoted_string, true). 
:- module_transparent reset_backquoted_string/0.
reset_backquoted_string :- set_prolog_flag(backquoted_string, false). 



% Perform a call catching it if there is an exception
% If so, print message and then either fail or succeed
catch_fail(Call, Message) :-
	catch(Call,E,
		(report_message(warning,[Message, ' ---> ', E]),
	     fail)
	    ).
catch_succ(Call, Message) :-
	catch(Call,E,
		(report_message(warning,[Message, ' ---> ', E]),
	     true)
	    ).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: lib/tools_swi.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
