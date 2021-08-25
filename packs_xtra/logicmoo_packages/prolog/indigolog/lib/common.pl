%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FILE: lib/common.pl
%
%       COMMON LIBRARY TOOLS for Prolog
%
%  AUTHOR : Sebastian Sardina (2003)
%  EMAIL  : ssardina@cs.toronto.edu
%  WWW    : www.cs.toronto.edu/~ssardina 
%  TYPE   : system independent code
%  TESTED : SWI Prolog 5.0.10 http://www.swi-prolog.org
%           ECLIPSE 5.4 http://www.icparc.ic.ac.uk/eclipse/
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This files contains system independent predicates that are used by 
% several files (e.g., projectors, interpreters, etc.)
%
% 
% The following predicates are required:
%
%  -- islist/1
%  -- substring/4
%  -- string_length/2
%  -- string_to_atom/2
%  -- string_to_list/2
%  -- emptyString/1 : return the empty string
%  -- system/1
%  -- stream_select/3
%  -- argc/1
%  -- argv/2
%  -- maplist/3
%  -- call_to_exec/3
%  -- get_integer/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1 - GENERAL
%
%  -- subv(+X1,+X2,+T1,-T2)    
%        T2 is T1 with X1 replaced by X2
%  -- get_argument(?Name, ?Value)    
%        Value is the value of argument name Name. (i.e., Name=Value in call)
%  -- get_list_arguments(-L)    
%        L is a list of arguments of the form [Name, Value]
%  -- replace_element_list(+List,+E1,+E2,-List2)
%        List2 is List with element E1 replaced by element E2
%  -- sublist(?SubList, +List)
%        Succeeds if List is the list which contains all elements from SubList 
%  -- get_integer(+Low, ?N, +High)
%        N is an integer between Low and High
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

       /*  T2 is T1 with X1 replaced by X2  */
subv(X1,X2,T1,T2) :- var(T1), T1 == X1, !, T2 = X2.
subv(_,_,T1,T2)   :- var(T1), !, T2 = T1.
subv(X1,X2,T1,T2) :- T1 == X1, !, T2 = X2.
subv(X1,X2,T1,T2) :- T1 =..[F|L1], subvl(X1,X2,L1,L2), T2 =..[F|L2].

subvl(_,_,[],[]).
subvl(X1,X2,[T1|L1],[T2|L2]) :- subv(X1,X2,T1,T2), subvl(X1,X2,L1,L2).


% Command line argument Name has Value
% Name can be a symbolic name when the argument is of the form "Name=Value"
% or the number of the argument 
% OBS: Value is always a string, Name is an atom
get_argument(Name, Value) :- 
        get_list_arguments(L),
        member([Name, Value], L).

% Obtain the list of command line arguments
% L is a list of [Name, Value] where Name is the value of the argument
% and Value is its value when the argument has the form Name=Value
% (e.g., port=2134). Otherwise, if the argument does not have that form, 
% Name is the number of the argument (e.g., [3, "notime"])
% OBS: Value is always a string, Name is an atom
get_list_arguments(L) :-
        argc(N), 
        N2 is N-1, 
        (N2 > 0 -> collect_all_arguments(N2, L) ; L=[]).

collect_all_arguments(0, []) :- !.
collect_all_arguments(N, [[Name,Value]|L]) :-
        argv(N, SArgN),
        (split_string(SArgN, `=`, ``, [SName, Value]),
         string_to_atom(SName, Name) ->
             true
        ;
             Name=N,
             Value=SArgN
        ),
        N2 is N-1,
        collect_all_arguments(N2, L).


% -- replace_element_list(+List,+E1,+E2,-List2)
%     List2 is List with element E1 replaced by element E2
replace_element_list([],_,_,[]).
replace_element_list([CE1|R],CE1,CE2,[CE2|RR]):- !,
        replace_element_list(R,CE1,CE2,RR).
replace_element_list([E|R],CE1,CE2,[E|RR]):- 
        replace_element_list(R,CE1,CE2,RR).




%sublist(?SubList, +List)
%            Succeeds if List is the list which contains all elements 
%            from SubList 
sublist([],_).
sublist([X|R], L) :-
	member(X,L),
	sublist(R,L).


%get_integer(+Low, ?N, +High) :
%            Integer N is between Low and High (included)
/* ECL
get_integer(L, L, H) :- L=<H.
get_integer(L, N, H) :- L<H, L2 is L+1, get_integer(L2, N, H).
*/
get_integer(L, N, H) :- between(L,H,N).


%%	extract_option(+LOptions,?Name,?Value,+Default) 
%%	extract_option(+LOptions,?Name,?Value) 
%
%	Extract Value of option Name(Value) from list of options LOptions
%	If the option is not mentioned in the list, assume value Default
%
extract_option(LOptions,NameOption,Value) :-
	extract_option(LOptions,NameOption,Value,_),
	\+ var(Value).
extract_option(LOptions,NameOption,Value,Default) :-
	ground(NameOption), 
	Option =.. [NameOption|[ValueOption]],
	member(Option,LOptions) -> Value=ValueOption ; Value=Default.
extract_option(LOptions,NameOption,Value,_Default) :-
	\+ ground(NameOption),
	member(Option,LOptions),
	Option =.. [NameOption|[Value]].

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2 - STRINGS AND ATOMS
%
% -- extract_substring(+String1, +Del1, +Del2, -String2, +PAfter, -Pos)
%     Very flexible extraction of a substring
%       String1= ......... Del11 ***** Del12 String2 Del21 ****** Del22
%     String2 starts at position Pos and Del11 is after position PosAfter
%     Del11 and Del22 may be null
%     This procedure will backtrack giving all possible solutions
%
% -- any_to_number(+T, -Number) 
%       Convert an atom, string, or list of chars T into a number
% -- any_to_string(+T, -String) 
%       Convert an atom, string, or list of chars T into string String
% -- lanything_to_string(+ListofT, -String) 
%       Generalizes any_to_string/2 to a list of atoms, strings, or chars
% -- string_replace(+S, +E1, +E2, -S2)
%       String/atom S2 is string/atom S with all chars E1 replaced by E2
% -- join_atom/3
% -- join_atom(+List, +Glue, -Atom)
%       Atom is the atom formed by concatenating the elements of List with an 
%       instance of Glue beween each of them.
% -- split_atom(+Atom, +SepChars, +PadChars, -SubAtoms) 
%       Decompose atom Atom into SubAtoms according to separators SepChars 
%       and padding characters PadChars.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% String1= ......... Del11 ***** Del12 String2 Del21 ****** Del22
% String2 starts at position Pos and Del11 is after position PosAfter
% Del11 and Del22 may be null
% This procedure will backtrack giving all possible solutions

% Case where one of the delimiters is not a pair.
extract_substring(String1, Del1, Del2, String2, PAfter, Pos):-
    (\+ is_list(Del1) ; \+ is_list(Del2)), !,
    (is_list(Del1) -> NDel1=Del1 ; NDel1=[null, Del1]),
    (is_list(Del2) -> NDel2=Del2 ; NDel2=[Del2, null]),
    extract_substring(String1,NDel1,NDel2, String2, PAfter, Pos).

extract_substring(String1,[Del11,Del12], [Del21,Del22], String2, PAfter, Pos):-
        % Convert all delimeters to strings if necessary
        any_to_string(Del11, Del11S),
        any_to_string(Del12, Del12S),
        any_to_string(Del21, Del21S),
        any_to_string(Del22, Del22S), !,
        % Set Pos1, if no Del11 then Pos1=PAfter
        % Pos2 is the first place of Del12 after Pos1
        (Del11=null -> 
             Pos1=PAfter,
             substring(String1, Pos2, _, Del12S), % Allow backtracking on Del12
             Pos2>Pos1
             ;
             substring(String1, Pos1, _, Del11S), 
             Pos1>PAfter,   % Allow backtracking on Del11, not on Del12
             once((substring(String1, Pos2, _, Del12S), 
                   is_after_string(Del11S, Pos1, Pos2)))
        ),
        % Pos3 is the first place of Del21 after Pos2
        once((substring(String1, Pos3, _, Del21S), 
              is_after_string(Del12S, Pos2, Pos3))),
        % If Del22\=null, check that there is a Del22 string after Pos3
        (Del22=null -> 
             true
        ; 
             once((substring(String1, Pos4, _, Del22S), 
                   is_after_string(Del21S, Pos3, Pos4)))
        ),
        % Calculate Pos: where String2 starts
        string_length(Del12S, LDel12),
        Pos is Pos2+LDel12,
        % Calcualte Length: length of String2
        Length is Pos3-Pos,
        % Extract String2 using its Pos and Length
        substring(String1, Pos, Length, String2).

% P2 > Length(S)+P
is_after_string(S, P, P2) :-
        string_length(S, LS),
        SEnd is P+LS,
        P2 > SEnd.

% Convert anything into a number
any_to_number(N, N) :- number(N).
any_to_number(A, N) :- atom(A), atom_number(A, N).
any_to_number(S, N) :- string(S), string_to_number(S, N).


% Convert anything into a string
any_to_string(S, S) :- string(S).
any_to_string(A, S) :- atom(A), string_to_atom(S, A).
any_to_string(A, S) :- number(A), string_to_number(S, A).
any_to_string(A, S) :- is_list(A), 
        ( (member(X,A), \+ number(X)) -> 
              build_string(A, S)   % Manually build string S
        ;
              string_to_list(S, A) % A is list of char codes!
        ).
any_to_string(A, S) :- \+ is_list(A), compound(A), string_to_term(S, A).
any_to_string(A, '_Var') :- var(A).

% Convert a list of anything to into a list of strings
lany_to_string([], []).
lany_to_string([A|R], [SA|SR]) :- any_to_string(A, SA),
                                  lany_to_string(R, SR).

% S is the empty string
emptyString(S) :- string_to_list(S,[]).

build_string([], S)    :- emptyString(S).
build_string([E|R], S) :- 
        build_string(R, S2),
        any_to_string(E, SE),
        concat_string([SE,S2],S).

% -- string_replace(S, E1, E2, S2)
%       String/atom S2 is string/atom S with all chars E1 replaced by E2
string_replace(S, E1, E2, S2) :- 
        lany_to_string([S,E1,E2],[SS,SE1,SE2]), 
        emptyString(ES),
        split_string(SS, SE1, ES, L),
        join_string(L, SE2, S2).

% -- join_atom(List, Glue, Atom)
%      Atom is the atom formed by concatenating the elements of List with an 
%      instance of Glue beween each of them.
join_atom(List, Glue, Atom) :-
        maplist(any_to_string, List, List2),
        join_string(List2, Glue, String),
        string_to_atom(String, Atom).

% -- split_atom(Atom, SepChars, PadChars, SubAtoms) 
%      Decompose atom Atom into SubAtoms according to separators SepChars 
%      and padding characters PadChars.
split_atom(Atom, SepChars, PadChars, SubAtoms) :-
        string_to_atom(SA1, Atom),
        string_to_atom(SA2, SepChars),
        string_to_atom(SA3, PadChars),
        split_string(SA1, SA2, SA3, SL),
        maplist(string_to_atom, SL, SubAtoms).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3 - OPERATING SYSTEM: processes, files
%
% -- proc_exists(+Pid)
% -- proc_term(+Pid)
% -- proc_kill(+Pid)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Process Pid exists if it's listed by <ps -f pid>
proc_exists(Pid):- 
        concat_atom(['ps -f ',Pid], Command),
        call_to_exec(unix, Command, Command2), % Select right command for exec
        exec(Command2, [null, streamout]), 
        (read_string(streamout, end_of_line, _, _),
         read_string(streamout, end_of_line, _, _) -> 
             close(streamout)
        ;
             close(streamout),fail).

% Process Pid is finished if it's listed with status Z with <ps -f pid>
% (SWI does not provide that)
proc_term(Pid):- 
        concat_atom(['ps -f ',Pid], Command),
        call_to_exec(unix, Command, Command2), % Select right command for exec
        exec(Command2,[null, streamout], _), 
        (read_string(streamout, end_of_line, _, _),
         read_string(streamout, end_of_line, _, S) -> 
             close(streamout),
             string_to_atom(Z, 'Z'),
             substring(S, Z, _)
        ;
             close(streamout), fail).

% Kill process PID by sending signal 9 (MOST PROLOG'S PROVIDE THIS)
%proc_kill(Pid):- 
%        concat_atom(['kill -9 ',Pid], Com),
%        system(Com).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 4 - SOCKET COMMUNICATION PROTOCOL
%
%
% DESCRIPTION : This package provides a simple unified protocol via sockets 
%               to communication between the environment manager and each
%               of the individual environments.
%
% Single message = [Source, Data] where Source is the sender and
%                  data is the actual info sent. 
%
% Data has usullay the following form [Type, ...,...,...] where Type
%      defines the kind of message (sensing outcome, exogenous action, etc)
%
% -- send_data_socket(+Socket, +Data) :
%       Writes [Env, Data] to the Socket where Env is the name of the actual 
%       environment. Socket should be already connected to destination
% -- receive_list_data_socket(+Socket, -LMessages) :
%       Read (a possibly empty) list of messages from the Socket
% -- receive_data_socket(+Socket, -Message) :
%       Read 1 Message from the Socket (block if there is nothing yet)
%
% OBS: Requires name_env/1 to be on the DB to recognize the sender
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic name_env/1. % Stores the name of the current module

  
% Send: [Env, Data] where Env is the name of the device (e.g., simulator)        
send_data_socket(Socket, Data) :- 
        (name_env(Env2) -> Env=Env2 ; Env=unknown),
        decode_data(_, DataToSent, [Env, Data]),
        				% Changed quote(true) --> quote(false) so that TERMS can be transmited (e.g., begin(photo,1,id_2))
        				% Otherwise, the term is quoted!
        write_term(Socket, DataToSent, [quoted(false)]),
        write(Socket, '.'),
        nl(Socket),
        flush(Socket).

% Receive a list of [Env, Data] where Env is the id of the sender 
receive_list_data_socket(Socket, []) :- 
        stream_select([Socket], 0, []), !.      % Wait almost nothing
receive_list_data_socket(Socket, [Data|L]) :- 
        receive_data_socket(Socket, Data),
        (Data = [_, [_, end_of_file]] -> 
             L=[]
        ;
             receive_list_data_socket(Socket, L)
        ).
         
receive_data_socket(Socket, TData) :- 
         read_term(Socket, TRead, []),
         decode_data(Socket, TRead, TData).

% decode_data(Socket, Data, CodifiedData)
%      Codify Data as as CodifiedData to send via socket S
decode_data(Socket, end_of_file, [socket(Socket), [system, end_of_file]]) :- !.
decode_data(_, [Env, Data], [Env, Data]) :- !.
decode_data(_, Data, [unknown, Data]).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 5 - TOOL FOR REPORTING MESSAGES
%
% -- report_message(+T, +M)       
%       Report messsage M of type T
% -- set_debug_level(+N) : set the debug level to N (nothing >N is shown)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic 
	debug_level/1,
	warn_off/0.

% Set warn on/off for warnings
set_debug_level(warn_off) :- warn_off -> true ; assert(warn_off).
set_debug_level(warn_on)  :- retractall(warn_off).

% Set te debug level to be below N (the higher the N, the more debug messages)
set_debug_level(N) :- 
	retractall(debug_level(_)),
	assert(debug_level(N)),
        report_message(system(0), ['Debug level set to ',N]).

report_message(T, L) :- 
	is_list(L), !, 
	maplist(any_to_string,L,LS),
	any_to_string(' ', Space),
	join_string(LS, Space, M2), % Include space between each element
	report_message(T, M2).

report_message(system(N), _)    :-   % Do not print this debug message
        debug_level(N2), N2<N, !.
report_message(system(N), T)    :- !,
        N2 is N-1,
        tab(N2),
        write('DEBUG '),  write(N), write(': '), writeln(T).

report_message(warning, T)    :- !,
	(warn_off -> true ; write('!!! WARNING: '), writeln(T)).

report_message(error, T)    :- !,
        write('!!! ERROR ----> '),  writeln(T).

report_message(program, T)    :- !,
        write('  ***** PROGRAM:: '),  writeln(T).

report_message(action, T)    :- !,
        write('>>>>>>>>>>>> ACTION EVENT:: '),  writeln(T).

report_message(sensing, T)    :- !,
        write('--------------> SENSING EVENT:: '),  writeln(T).

report_message(exogaction, T) :- !,
	nl,
        write('=========> EXOGENOUS EVENT:: '), writeln(T).

report_message(user, T) :- !,
        write('  **** USER MESSAGE:: '),  writeln(T).

report_message(_, T) :-
        write('  **** OTHER EVENT:: '),  writeln(T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: lib/common.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
