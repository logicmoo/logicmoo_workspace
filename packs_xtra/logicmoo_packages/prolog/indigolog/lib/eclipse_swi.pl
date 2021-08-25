%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  FILE      : lib/eclipse_swi.pl
%
%	ECLIPSE Compatibility library for SWI Prolog
%	(i.e., simulates several ECLIPSE goals in SWI)
%
%  AUTHOR : Sebastian Sardina (2003)
%  EMAIL  : ssardina@cs.toronto.edu
%  WWW    : www.cs.toronto.edu/~ssardina
%  TYPE   : system independent code
%  TESTED : SWI Prolog 5.0.10 http://www.swi-prolog.org
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% DESCRIPTION : This package provides some compatibility with ECLIPSE
%     Prolog (http://www.icparc.ic.ac.uk/eclipse/).
%
% To load this library:
%
%     :- use_module(eclipse_swi).  % Load compatibility library with ECLIPSE
%     :- init_eclipse_lib.         % Perform required replacements
%
%
% The tools are divided in the following groups:
%
%   1 - Tools for managing sockets and streams
%   2 - Tools for strings
%   3 - Operating system tools
%   4 - Other tools
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(eclipse_swi,[
           init_eclipse_lib/0,               
	   % 1 - SOCKETS AND STREAMS
           socket/3,              % +internet, +stream, ?SocketId
           bind/2,                % +SocketId, +Host/+Port
           listen/2,              % +SocketId, +Num
           accept/3,              % +SocketId, -From, ?Stream
           connect/2,             % +SocketId, +Host/+Port
	   get_socket_stream/3,
           %
           eclipse_read/2,
           eclipse_write/2,
           eclipse_read_term/3,
           eclipse_write_term/3,
           eclipse_close/1,       % +SocketId
           eclipse_nl/1,          % +SocketId
           eclipse_flush/1,       % +SocketId
           eclipse_select/3,      % +StreamList, +Timeout, ?ReadyStreams
           % 2 - STRINGS
           concat_strings/3,      
           concat_string/2,
           substring/3,
           substring/4,
           substring/5,          % Equivalent to sub_string/5 for SWI
           read_string/4,
           term_string/2,
           number_string/2,
           split_string/4,
           join_string/3,
           % 3 - OS TOOLS
           exec/2,
           exec/3,                                      
           exec_group/3,
           set_interrupt_handler/2,
           current_interrupt/2,
           get_interrupt_handler/3,
           stime/1,
           cputime/1,		
           % 4 - OTHER TOOLS
	   shuffle/2,
           false/0,               
           type_of/2,
           writeln/2,
           flush/1,
           argv/2,
           argc/1,
	   min/2,                  % Minimum of a list
	   max/2                   % Maximum of a list
          ]). 


% This utility will replace every call to read/2, write/2, etc. by their
% corresponding ECLIPSE versions eclipse_read/2, eclipse_write/2, etc.
:- module_transparent init_eclipse_lib/0.
init_eclipse_lib :- 
        context_module(M),
        assert(M:goal_expansion(read(A1,A2),  eclipse_read(A1,A2))),
        assert(M:goal_expansion(write(A1,A2), eclipse_write(A1,A2))),
        assert(M:goal_expansion(stream_select(A1,A2,A3), eclipse_select(A1,A2,A3))),
        assert(M:goal_expansion(read_term(S,T,O), eclipse_read_term(S,T,O))),
        assert(M:goal_expansion(write_term(S,T,O), eclipse_write_term(S,T,O))),
        assert(M:goal_expansion(close(A1), eclipse_close(A1))),
        assert(M:goal_expansion(flush(A1), eclipse_flush(A1))),
        assert(M:goal_expansion(nl(A1),    eclipse_nl(A1))).


% NOTE: Library streampool is required to help providing support for 
% sigio(stream) capabilities in exec/3 and accept/3 predicates.
:- use_module(streampool).
:- use_module(library(socket)).		% Load socket library (e.g., tcp_socket/1)
%:- use_module(library(unix)).		% Load unix library library (e.g., fork/1)



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1 - SOCKETS AND STREAMS
%
% In SWI a socket has 2 different streams associated, one
% for reading and one for writing. In ECLIPSE, the stream associated with
% a stocket can be used for input and output.
%
% -- socket(+Domain, +Type, ?SockStream)
%        Creates a socket of a given type and domain and associates a stream 
%        with it. SO FAR ONLY IMPLEMENTED DOMAIN:internet, TYPE:stream
%
% -- bind(+SockStream, ?Address)
%        Associates an address with a given socket stream.
%
% -- listen(+SockStream, +Queue)
%        Specifies how many connections are accepted for a socket and 
%        makes connections available.
%
% -- accept(+SockStream, ?From, ?NewStream)
%        Accepts a connection for a stream socket and creates a new 
%        socket which can be used for I/O.
%
% -- connect(+SockStream, +Address)
%        Connects a socket with the given address.
%
% -- get_socket_stream(+SockStream, +Mode, -Stream) 
%        Retrives the input and output streams associated with SockStream
%        Mode can be either "read" or "write"
%
% The main difference with ECLIPSE Prolog is taht SockStream is actually not
% a real stream. Therefore, predicates like read/2, write/2, etc cannot be
% used directly. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Stores info about sockets: 
%       socket_info(SocketId, SocketCode, ReadStream, WriteStream)
% SocketId is actually a wrapper to refer to a pair of streams since SocketId 
% is actually *not* a stream id, it should not be treated as that.
:- dynamic socket_info/4.  

% Creates a socket of a given type and domain and associates a stream with it.
% SWI does not return a stream but the socket ID so socket_info/4 implements
% tha mapping between the socket's id and the socket's streams
socket(internet, stream, SocketId) :- 
               % Check socket does not exist yet
        (atom(SocketId) -> \+ socket_info(SocketId, _, _, _) ; true),
               % Create new socket S
        tcp_socket(S),   % S of the form '$socket'(274326)
        (atom(SocketId) -> true ; S =.. [_, SocketId]),
               % Enter a socket_info/4 entry for new socket
        assert(socket_info(SocketId, S, null, null)).

% Associates an address with a given socket stream.
% OBS: This bind/2 needs to be given an available fix address!
bind(SocketId, _/Port) :-
	retract(socket_info(SocketId, S, _, _)), 
        % (number(Port) -> true ; get_free_port(Port)),  % Not yet done
        tcp_bind(S, Port),
	%
	tcp_open_socket(S, R, _),  % No Write Stream here
	assert(socket_info(SocketId, S, R, null)).


% Specifies how many connections are accepted for a socket and makes
% connections available.
listen(SocketId, N) :-
       socket_info(SocketId, S, _, _),
       tcp_listen(S, N).



% accept/3: 
% Accepts a connection for a stream socket and creates a new socket which can be used for I/O.
accept(SocketId, From, NewSock) :-        % Handle the case for sigio(S)
        \+ var(NewSock), NewSock = sigio(SocketId2), !,  
        accept(SocketId, From, SocketId2),
        retract(socket_info(SocketId2, S, R, W)),
        register_stream_sigio(R, R2),  % Register SocketId2 for IO signal
        assert(socket_info(SocketId2, S, R2, W)).


% Socket is new & Read/Write Streams are still null
accept(SocketId, Host/unknown, NewSocketId2) :-  
       retract(socket_info(SocketId, S, null, null)), !,
       (ground(NewSocketId2) -> \+ socket_info(NewSocketId2, _, _, _) ; true),
       %
       tcp_open_socket(S, R, _),  
       assert(socket_info(SocketId, S, R, null)),
       %
       tcp_accept(R, S2, Host),
       tcp_open_socket(S2, ReadS, WriteS),
       (ground(NewSocketId2) -> 
	       true
       ;                              % Write socket has no alias
	       S2 =.. [_, NewSocketId2]  % because S2= 'socket'(NewSocketId2)
       ),  
       assert(socket_info(NewSocketId2, S2, ReadS, WriteS)).

% Socket is just new but Read stream is not null
accept(SocketId, Host/unknown, NewSocketId2) :-
       socket_info(SocketId, _, R, _), 
       R\=null,             
       %
       tcp_accept(R, S2, Host),
       tcp_open_socket(S2, ReadS, WriteS),
       (ground(NewSocketId2) -> 
       		true ; 
       	S2 =.. [_, NewSocketId2]
       ),
       assert(socket_info(NewSocketId2, S2, ReadS, WriteS)).




% Connects a socket with the given address.
connect(SocketId, Host/Port) :-
        socket_info(SocketId, S, _, _),
        tcp_connect(S, Host:Port),
        tcp_open_socket(S, R, W),
        retract(socket_info(SocketId, S, _, _)),
        assert(socket_info(SocketId, S, R, W)).


           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           % EXTRA PREDICATES TO DEAL WITH SOCKETS %
           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% get_socket_stream/3 
%      Retrives the input/output stream associated to a Socket (works in 2-ways)
%       socket_info(SocketId, SocketCode, ReadStream, WriteStream)
get_socket_stream(SocketId, read, Stream) :- 
        socket_info(SocketId, _, Stream, _).
get_socket_stream(SocketId, write, Stream) :- 
        socket_info(SocketId, _, _, Stream).

% get_real_streams(StreamList, Type, StreamList2)
%     StreamList2 is StreamList with all Socket streams replaced
%     correspondingly by their stream's Type (works in 2-ways)
get_real_streams([], _, []).
get_real_streams([S|StreamList], Type, [RS|RealStreamList]) :-
        get_socket_stream(S, Type, RS), !,  % S is a socket!
        get_real_streams(StreamList, Type, RealStreamList).
get_real_streams([S|StreamList], Type, [S|RealStreamList]) :-
        get_real_streams(StreamList, Type, RealStreamList). % S is not a socket!

% Is S a socket stream?
is_socket(S) :- socket_info(S, _, _, _). 



eclipse_read(S, T) :-
        get_real_streams([S], read, [RS]),
        read(RS, T).
eclipse_write(S, T) :-
        get_real_streams([S], write, [RS]),
        write(RS, T).

eclipse_close(S) :-
        is_socket(S) -> close_socket(S) ; close(S).
close_socket(SocketId) :-
        retract(socket_info(SocketId, S, R, W)),
 %       unregister_stream_sigio(R, R2),  % UnRegister SocketId2 for IO signal
        (R == null -> true ; close(R)),
        (W == null -> true ; close(W)),
        catch(tcp_close_socket(S),_,true).

eclipse_flush(S) :-
        get_real_streams([S], write, [RS]),
        flush_output(RS).
eclipse_nl(S) :-
        get_real_streams([S], write, [RS]),
        nl(RS).

eclipse_write_term(S, T, O) :-
        get_real_streams([S], write, [RS]),
        write_term(RS, T, O).

eclipse_read_term(S, T, O) :-
        get_real_streams([S], read, [RS]),
        read_term(RS, T, [double_quotes(string)|O]).

%  -- stream_select(+StreamList, +Timeout, ?ReadyStreams)
%       Returns streams from StreamList which are ready for I/O, blocking 
%       at most Timeout seconds.
stream_select(StreamList, TimeOut, ReadyList) :- 
	eclipse_select(StreamList, TimeOut, ReadyList).
eclipse_select(StreamList, TimeOut, ReadyList) :- 
        get_real_streams(StreamList, read, RealStreamList),
        select_stream(RealStreamList, TimeOut, ReadyListStreams),
        get_real_streams(ReadyList, read, ReadyListStreams).

select_stream(StreamList, block, ReadyList) :- !,  % block 
        wait_for_input(StreamList, ReadyList, 0).
select_stream(StreamList, 0, ReadyList) :- !,      % wait almost nothing
        wait_for_input(StreamList, ReadyList, 0.000000000000001).
select_stream(StreamList, TimeOut, ReadyList) :-   % wait TimeOut seconds
        wait_for_input(StreamList, ReadyList, TimeOut).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2 - STRINGS
%
% -- concat_strings(+Src1, +Src2, ?Dest)
%        Succeeds if Dest is the concatenation of Src1 and Src2.
% -- concat_string(+List, ?Dest)
%        Succeeds if Dest is the concatenation of the atomic terms 
%        contained in List.
%
% -- substring(+String1, +String2, ?Position)
%        Succeeds if String2 is a substring of String1 beginning at 
%        position Position.
% -- substring(+String1, ?Position, ?Length, ?String2)
%        Succeeds if String2 is the substring of String1 starting at 
%        position Position and of length Length.
% -- substring(+String, ?Before, ?Length, ?After, ?SubString)
%        Succeeds if String2 is a substring of String, with length Length, 
%        preceded by Before, and followed by After characters
%
% -- split_string(+String, +SepChars, +PadChars, ?SubStrings)
%        Decompose String into SubStrings according to separators SepChars 
%        and padding characters PadChars.
% -- join_string(+List, +Glue, ?String)
%        String is the string formed by concatenating the elements of 
%        List with an instance of Glue beween each of them.
%
%
% -- number_string(?Number, ?String)
% -- term_string(?Term, ?String)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% concat_strings/3: concatenate two strings
concat_strings(String1, String2, String3):-
        string_concat(String1, String2, String3).

% concat_string/2 : concatenate a list of strings
concat_string([], EmptyString):- string_to_list(EmptyString,[]).
concat_string([String1|RS], String3):-
        concat_string(RS, RSString2),
        concat_strings(String1, RSString2, String3).


% substring/3: Sub is the substring of String that starts in position Start
% substring/4: Sub is the substring of String that starts in position Start
%              with a length of Length
substring(String, SubString, Pos)    :- substring(String, Pos, _, SubString).
substring(String, Start, Length, SubString):- 
	var(SubString), !,
        sub_string(String, Start, Length, _, SubString).
substring(String, Start, Length, Sub):- 
	string_length(String, LString), 
	LString\=0,
        string_to_atom(Sub, SubAtom),
        sub_string(String, Start, Length, _, SubAtom).

substring(String, Before, Length, After, SubString) :-
	sub_string(String, Before, Length, After, SubString).
	

% read_string/3: read_string(+Delimiters, ?Length, ?String)
% read_string/4: read_string(+Stream, +Delimiters, ?Length, ?String)
%
% Reads a string from the stream Stream up to a delimiter or up to a specified length.
%
% If L is ground, it will be ignored as everything will be read from Stream
% If delimeters are used, it may take a long time to read if the string is long

% read_string(+Delimiter, ?Length, ?String)
read_string(Del, L, String) :- 
        seeing(X),   		% Get *current* user-input stream
        read_string(X, Del, L, String).

% read_string/4: when Delimiters=end_of_line
read_string(Stream, end_of_line, L, S) :- !, 
        get_real_streams([Stream], read, [RStream]), % Stream may be a socket
	read_line_to_codes(RStream, Codes),
	( (var(L), is_list(Codes)) -> length(Codes, L) ; true),
	string_to_list(S, Codes).

% read_string/4: when Delimiters=end_of_file
read_string(Stream, end_of_file, L, S) :- !, 
        get_real_streams([Stream], read, [RStream]), % Stream may be a socket
	read_stream_to_codes(RStream, Codes),
	Codes\=[],
	( (var(L), is_list(Codes)) -> length(Codes, L) ; true),
	string_to_list(S, Codes).

% read_string/4: when Delimiters is something else, that is, a string with delimiters
read_string(Stream, Del, L, String) :-
        get_real_streams([Stream], read, [RStream]), % Stream may be a socket
        string_to_list(Del, LCharDel),
	emptyString(EmptyString),
        read_string2(RStream, LCharDel, L, 0, EmptyString, String),
	\+ emptyString(String).

% read_string2(Stream, Delim, L, CL, StringNow, FinalString) 
%     L is the final length, CL is the current length    
%     StringNow is the string read so far, FinalString is the final string
read_string2(_, _, L, CL, StringNow, StringNow) :- L==CL, !. 
read_string2(Stream, LCharDel, L, CL, StringNow, FinalString) :- 
        wait_for_input([Stream], [Stream], 0),  % Block till something in Stream
        get_code(Stream, CharCode),        % Get one char from stream
        (member(CharCode, [-1|LCharDel]) ->   
             FinalString=StringNow,      % Finalize: delimeter or end of file found
             (var(L) -> L=CL ; true)
        ;
	     CL2 is CL+1,
	     string_to_list(String, [CharCode]),
	     string_concat(StringNow, String, NewStringNow),
             read_string2(Stream, LCharDel, L, CL2, NewStringNow, FinalString)
        ).






% split_string/4
% Decompose String into SubStrings according to separators SepChars and
% 	padding characters PadChars.
% (This implementation should work in any other Prolog)
split_string(String, SepChars, PadChars, SubStrings):-
	string_to_list(SepChars, LSepChars),
 	% Find all the start positions of separators in the string
	findall(Start, (substring(String, Start, 1, Sep),
	                string_to_list(Sep, [SepChar]),
	                member(SepChar, LSepChars)), ListStarts),
	string_length(String, StringLength),
	append(ListStarts,[StringLength], NListStarts),
	divide_string(String, [-1|NListStarts], SubStrings2),
	findall(S2, (member(S, SubStrings2),
                     remove_pad(S, PadChars, S2)), SubStrings).

% join_string(+List, +Glue, -String): 
%    String is the string formed by concatenating the elements of List with
% an instance of Glue beween each of them.
join_string([], _, String) :- !,
	string_to_list(String,[]).
join_string([E|R], Glue, String) :- 
%	any_to_string(E, SE),
	string_concat(E, '', SE), % Convert anything into a string
	join_string(R, Glue, StringR),
	string_length(StringR, LStringR),
	(LStringR=0 -> concat_string([SE, StringR], String) ; 
		       concat_string([SE, Glue, StringR], String)).
	


           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           %     CONVERSION TOOLS FOR STRINGS      %
           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% term_string/2: conversion between terms and strings
term_string(S, T) :- 
        ground(S),
        string_to_atom(S, A), 
        term_to_atom(T, A). 
term_string(S, T) :- 
        ground(T),
        term_to_atom(T, A), 
        string_to_atom(S, A).

% number_string/2: conversion between numbers and strings
number_string(N, S):- 
        ground(N),
        number_chars(N, L), 
        string_to_list(S, L).
number_string(N, S) :- 
        ground(S),
        string_to_atom(S, A), 
        atom_codes(A, CA), 
        number_codes(N, CA).



           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           %  IMPLEMENTATION DETAILS for STINGS    %
           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% divide_string(+String, +ListStarts, -LStrings)
%      	String: string / ListStarts: list numbers / LStrings : List of strings
% Decompose String into SubStrings according to separator locations
%    in ListStarts
% padding characters PadChars.
divide_string(_, [_], []).
divide_string(String, [S,E|Rest], [FString|RString]) :-
	S2 is S+1,
	E2 is E-S-1,
	substring(String, S2, E2, FString),
	divide_string(String, [E|Rest], RString).


% remove_pad(+String, +PadChars, -StringsNoPad) :
%      	String, PadChars, StringNoPad: strings 
%    remove any char in LPadChars appearing in the front or
%    at the end of string String
%
remove_pad(String, PadChars, StringsNoPad) :- 
	string_to_list(PadChars, LPadChars),
	string_to_list(String, LString),
	remove_front(LString, LPadChars, LString2),
	reverse(LString2, RLString2),
	remove_front(RLString2, LPadChars, RLStringsNoPad),
	reverse(RLStringsNoPad, LStringsNoPad),
	string_to_list(StringsNoPad, LStringsNoPad).
	

% remove_front(+LString, +LPadChars, -LString) :
%      	LString, LPadChars, LString : List of chars
%    remove any char in LPadChars appearing in the front of LString
%
remove_front([], _, []) 			:- !.
remove_front(LString, LPadChars, LString) 	:- 
	LString=[C|_], \+ member(C, LPadChars), !.
remove_front([_|LString], LPadChars, LString2) 	:- 
	remove_front(LString, LPadChars, LString2).



% Replace one element for anotherone in a list of elements
replace_element_list([],_,_,[]).
replace_element_list([CE1|R],CE1,CE2,[CE2|RR]):- !,
        replace_element_list(R,CE1,CE2,RR).
replace_element_list([E|R],CE1,CE2,[E|RR]):- 
        replace_element_list(R,CE1,CE2,RR).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3 - OPERATING SYSTEM TOOLS
%
% -- cputime(?Time)
% -- stime(?Time)
%        Succeeds if Time is the elapsed user cpu time in seconds.
%
% Management of interrupts/signals as ECLIPSE does:
%
% -- set_interrupt_handler/2
% -- current_interrupt/2
% -- get_interrupt_handler/3
%        
% Operating system EXEC utilities:from ECLIPSE
%
% -- exec(+Command, ?Streams)
%        A child process Command is forked, its standard streams are 
%        connected to Streams and the ECLiPSe process waits until it terminates.
%
% -- exec(+Command, ?Streams, -Pid)
%        A child process Command is forked, its standard streams are 
%        connected to Streams and its process ID is Pid.
%
%     Description (adapted from ECLIPSE manual)
%
% This predicate is used to fork a child process and to set up pipes to its
% standard streams. After the process is forked, execution continues normally,
% without waiting for the child to terminate.
%
% By specifying the Streams argument it is possible to connect to the
% process' standard streams. The form of
% Streams is [Stdin, Stdout, Stderr]. Stderr is ignored in the current
% implementation. 
% If some of these streams are specified and
% not null, a pipe is opened which connects the standard stream of the child
% process with the specified stream, e.g. Stdin must be an output stream
% because it is connected to the standard input of the child process. 
% Specifying a null stream means that no pipe is set up for this stream.
%
% Stdout can also be specified as sigio(Stream) (BSD systems only). In this
% case a pipe is set up to the stream Stream and in addition the pipe is
% instructed to send the signal io each time new data appears in it. In this way
% the two processes can communicate in a truly asynchronous way. When one
% process sends data to the other one, the interrupt handler is invoked and it
% can read and process the data. When it finishes, it can continue where it was
% interrupted.
%
% If one wants to run a command with the shell, use: sh('-c', Command)
%
% After forking the process, Pid is unified with its process ID, which can be
% used e.g. in wait/2 or kill/2. If the exec system call in the child process
% failed, the child exits with status 128 + errno.
%
% -- exec_group(+Command, ?Streams, ?Pid)
%        A child process Command is forked in a new process group, its 
%        standard streams are connected to Streams and its process ID is Pid.
%     (NOTE: currently, equivalent to exec/3)
%  
%
% -- system(+ShellCommand)
% -- sh(+ShellCommand)
%        The string or atom ShellCommand is passed as a command to the 
%        operating system, and the command is executed there
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Returns the CPU time
stime(T)   :- T is cputime.  
cputime(T) :- T is cputime.   % ECLIPSE compatibility

% Interrupts managements in the way ECLIPSE does
set_interrupt_handler(Signal, default/0)  :- !, on_signal(Signal, _, default).
set_interrupt_handler(Signal, event/1)    :- !, on_signal(Signal, _, throw).
set_interrupt_handler(Signal, N/0)        :- on_signal(Signal, _, N).
current_interrupt(Id, Name)               :- current_signal(Name, Id, _).
get_interrupt_handler(IntId, PredSpec, _) :- current_signal(_, IntId, PredSpec).


% Implementation of exec/2, exec/3 and exec_group/3 (Compat with ECLIPSE)
%
% A child process Command is forked. Its standard streams are connected to 
% [StdIn, StdOut, _] and its process ID is Pid.
% (This is a partial implementation of ECLIPSE exec_group/3)
% Differences: does not run Command in a different process group and it
%              does not set the error channel

% A - exec_group/3
exec_group(C, S, P) :- exec(C, S, P).  % For now I cannot separate the child

% B - exec/2
exec(Command, Streams) :- 
 	 exec(Command, Streams, Pid),
	 wait(Pid, _).

% C - exec/3
exec(Command, [], P) :- 
 	 exec(Command, [null, null, null], P).
exec(Command, [ServerOut], P) :- 
	 exec(Command, [ServerOut, null, null], P),
	 wait(P, _).
exec(Command, [ServerOut, ServerIn], P) :-
	 exec(Command, [ServerOut, ServerIn, null], P).

% Handle the case for sigio(S)
exec(Command, [ServerOut, SIn, _], Pid) :- 
        \+ var(SIn), SIn = sigio(ServerIn), !,
        exec(Command, [ServerOut, ServerIn2, _], Pid),
        register_stream_sigio(ServerIn2, ServerIn3),
        register_stream_name(ServerIn3, ServerIn).


exec(sh('-c', What), SERVIO, PID):-current_prolog_flag(windows,true),
  (atomic(What)->string_concat("/usr/bin/xterm -e /usr/bin/swipl",Rest,What);Rest,What),!,win_fork(Rest,SERVIO,PID).


% Handle the general case
exec(Command, [ServerOut, ServerIn, _], Pid) :- 
        (ServerOut== null -> true ;
                             pipe(CGIIn, ServerOut2),
                             register_stream_name(ServerOut2, ServerOut)),
        (ServerIn == null -> true ; 
                             pipe(ServerIn2, CGIOut),
                             register_stream_name(ServerIn2, ServerIn)),
        fork(Pid),
        (   Pid == child,
	    % detach_IO % may this work to detach the child ?
            (ServerOut == null -> true ; (close(ServerOut), 
					  dup(CGIIn, 0),     % stdin
					  close(CGIIn))),
            (ServerIn  == null -> true ; (close(ServerIn),
				          dup(CGIOut, 1),    % stdout
					  close(CGIOut))),
%           exec('/bin/sh '('-c', Command))
            exec(Command)
        ;   
	    (ServerOut == null -> true ; close(CGIIn)), 
	    (ServerIn  == null -> true ; close(CGIOut))
        ).


system(ShellCommand) :- shell(ShellCommand).
sh(ShellCommand)     :- shell(ShellCommand).


           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           %  IMPLEMENTATION of SIGIO CAPABILITIES %
           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic sigio/3.  % Stores streams that we are watching for IO
                     % (Source, Intermediate, Destination)

register_stream_sigio(Stream1, Stream2) :-
        pipe(Stream2, W),
        assert(sigio(Stream1, W, Stream2)),
        (current_thread(stream_pool_main_loop, _) -> 
             delete_stream_from_pool(Stream1),  % just in case...
             add_stream_to_pool(Stream1, sigio_action(signal)) 
        ;
             add_stream_to_pool(Stream1, sigio_action(signal)), 
             thread_create(stream_pool_main_loop, _, [detached(true)])).

unregister_stream_sigio(Stream) :-
%        sigio(Stream, _, _),  % Check stream is being watched
        %delete_stream_from_pool(Stream),
        %add_stream_to_pool(Stream, sigio_action(justcopy)),
        retract(sigio(Stream, W, _)),
        close(Stream),  % No use any more, no more data arriving to Stream
        close(W).       % Intermediate step not use anymore


register_stream_name(_, Name)      :- Name==user, !.
register_stream_name(Stream, Name) :- atom(Name), !, 
				      set_stream(Stream, alias(Name)).
register_stream_name(Stream, Stream).


sigio_action(T) :-
        findall(S, sigio(S,_,_), LS),
        wait_for_input(LS, [RS|_], 0),
        (at_end_of_stream(RS) ->           % Original read stream is EOF?
             unregister_stream_sigio(RS)   % Then 
        ;
             sigio(RS, W, _),          % Retrive intermediate stream W
             copy_pipe(RS, W),         % Copy from RS ----> W
             (T == signal ->
                  current_prolog_flag(pid, Pid),
                  current_signal(io, IdSignal, _),
                  kill(Pid, IdSignal) 
             ; 
                  true)
        ).

% Copy all current data in input-pipe-stream In to output-pipe-stream Out
copy_pipe(In, Out)   :- 
        wait_for_input([In],[],0.0000000001), !, % Nothing more on In
        flush_output(Out).                       % Everything has been copied
copy_pipe(In, Out) :-
        get_char(In, CharCode),        % Get one char from stream
        (CharCode=(-1) -> 
             true 
        ;
             write(Out, CharCode)
        ),
        copy_pipe(In, Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 4 - OTHER TOOLS
%
% -- false
%         Does not succeed (synonym of fail/0).
%
% -- type_of(?Term, ?Type)
%         Succeeds if Type is the data type of the term Term.
%         The types are atoms from the set: string, atom, var, integer, 
%         float, compound. The rest ECLIPSE types are *not* supported.
%
% -- writeln(+Stream, ?Term)
%         The term Term is written on the output stream Stream according to 
%         the current operator declarations. 
%
% -- flush(+Stream)
%         Flushes the output stream Stream.
%
% -- argc(?Number)
%         Succeeds if Number is the number of arguments given on the command 
% -- argv(+N, ?Argument)
%         Succeeds if the Nth argument given on the command line when 
%         invoking ECLiPSe is the string Argument.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% false/0 is an alias for fail

% :-ignore(( not(user:prolog_type(swi)), assert(false:- fail))).

% X is of type TX
type_of(X, TX):-
        var(X)        -> TX = var ;
        integer(X)    -> TX = integer ;
        float(X)      -> TX = float ;
        atom(X)       -> TX = atom ;
        string(X)     -> TX = string ;
        atomic(X)     -> TX = atomic ;
        compound(X)   -> TX = compound.

% writeln/2, flush/1 are not provided in SWI
writeln(Stream, T) :- write(Stream, T), nl(Stream).
flush(Stream)      :- flush_output(Stream).

% Succeeds if N is the number of arguments given on the command line to
% invoke Prolog .
argc(N) :- 
        current_prolog_flag(argv, L), 
        length(L,N2),
	N is N2+1.

% Succeeds if the Nth argument given on the command line when invoking Prolog
% is the string SA.
argv(N, SA) :- 
        current_prolog_flag(argv, L), 
        nth1(N, L, A), 
        string_to_atom(SA, A).


% min(+L,?X)/max(+L,?X) : minimum/maximum number in a list of numbers
min([X], X).
min([X|L], Y) :- min(L, ML), (X < ML -> Y=X, Y=ML).

max([X], X).
max([X|L], Y) :- min(L, ML), (X > ML -> Y=X, Y=ML).



% shuffle(+List, -ShuffledList) : Shuffle a list, ie randomize the element order
shuffle([],[]).
shuffle(D,DR) :- get_random_element(W,D), 
		 delete(D,W,D2), 
                 shuffle(D2,DR2), DR=[W|DR2].
%get a random element from domain
get_random_element(W,D)  :- 
	length(D,L), 
	L>0,
	I is random(L),
        nth0(I,D,W).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: lib/eclipse_swi.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
