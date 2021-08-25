%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: Env/env_int_swi.pl
%
%  AUTHOR : Sebastian Sardina (2002)
%  EMAIL  : ssardina@cs.toronto.edu
%  WWW    : www.cs.toronto.edu/~ssardina www.cs.toronto.edu/cogrobo
%  TYPE   : system dependent predicates (SWI threads and http libs)
%  TESTED : SWI Prolog 5.2.8 http://www.swi-prolog.org 
%
% This files provides the environment for working with the internet/web
% It allows the execution of many internet-system actions in paralell
% by using multiple threads (one per action requested to be executed)
%
%
% This environment is self-contained (it automatically loads the required
% libraries). It should be called as follows:
%
%   pl host=<HOST> port=<PORT> -b env_int_swi.pl -e start
%
% where HOST/PORT is the address of the environment manager socket.
%
% Written for SWI Prolog (http://www.swi-prolog.org) running under Linux 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                             June 15, 2000
%
% This software was developed by the Cognitive Robotics Group under the
% direction of Hector Levesque and Ray Reiter.
%
%        Do not distribute without permission.
%        Include this notice in any copy made.
%
%
%         Copyright (c) 2000 by The University of Toronto,
%                        Toronto, Ontario, Canada.
%
%                          All Rights Reserved
%
% Permission to use, copy, and modify, this software and its
% documentation for non-commercial research purpose is hereby granted
% without fee, provided that the above copyright notice appears in all
% copies and that both the copyright notice and this permission notice
% appear in supporting documentation, and that the name of The University
% of Toronto not be used in advertising or publicity pertaining to
% distribution of the software without specific, written prior
% permission.  The University of Toronto makes no representations about
% the suitability of this software for any purpose.  It is provided "as
% is" without express or implied warranty.
% 
% THE UNIVERSITY OF TORONTO DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
% SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
% FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF TORONTO BE LIABLE FOR ANY
% SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
% RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
% CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% This file assumes that the following is defined in env_gen.pl:
%
% -- start/0     : initialization of the environment (called when loaded)
% -- finalize/0  : finalization of the environment (called when exiting)
% -- main_dir/1  : obtain the root IndiGolog directory
% -- report_exog_event(A, M): 
%                  report exogenous event A with message M to the
%                  environment manager
% -- All compatibility libraries depending on the architecture such us:
%    -- compat_swi/compat_ecl compatibility libraries providing:
%
% -- The following two dynamic predicates should be available:
%    -- listen_to(Type, Name, Channel) 
%            listen to Channel of Type (stream/socket) with Name
%    -- terminate/0
%            order the termination of the application
%
% -- The following should be implemented here:
%
%  -- name_dev/1              : mandatory *
%  -- initializeInterfaces(L) : mandatory *
%  -- finalizeInterfaces(L)   : mandatory *
%  -- execute/4               : mandatory *
%  -- handle_steam/1          : as needed
%  -- listen_to/3             : as needed
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module(library('http/http_open')).     % Load simple http library
:- use_module(library('http/http_client')).   % Load expert http library
:- use_module(library(sgml)).		      % Load SGML library

:- include(env_gen).      % INCLUDE THE CORE OF THE DEVICE MANAGER


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSTANTS TO BE USED
%
% name_dev/1 : state the name of the device manager (e.g., simulator, rcx)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Name of the environment: <SIMULATOR>
% Set name of the environment here.
% THIS CONSTANT IS MANDATORY, DO NOT DELETE!
name_dev(internet). 

% Set verbose debug level
:- set_debug_level(1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A - INITIALIZATION AND FINALIZATION OF INTERFACES
%     initializeInterfaces/1 and finalizeInterfaces/1
%
% HERE YOU SHOULD INITIALIZE AND FINALIZE EACH OF THE INTERFACES TO BE USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initializeInterfaces(_) :-
	printKbInstructions.
finalizeInterfaces(_).


% printKbInstructions: Print instructions on this environment
printKbInstructions :-
    writeln('*********************************************************'), 
    writeln('* NOTE: This is the INTERNET and SYSTEM environment'), 
    writeln('*       This environment implements actions on the Internet     '), 
    writeln('*       and system actions (e.g., file-system actions, '),
    writeln('*       processes management, etc.)'),
    writeln('*********************************************************'), nl.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% B - HANDLERS FOR EACH STREAM/SOCKET THAT IS BEING HEARD:  handle_stream/1
%
% HERE YOU SHOULD WRITE HOW TO HANDLE DATA COMMING FROM EACH OF THE
% INTERFACES/CHANNELS USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Handle streams associated with the execution of internet/system actions.
handle_stream(Stream) :- 
             	% There is data in stream S which corresponds to action (A,N)
	listen_to(_, action(A, N), Stream),   
        report_message(system(3), ['Handling data from action ',(A,N)]),
	read(Stream, Data),
	((Data= end_of_file ; Data=finish) ->    
             	% Stream is EOF or requested terminination (action is over)
	     close(Stream),
	     retract(listen_to(_, action(A, N), Stream)),
	     report_message(system(2), ['Action ',(A, N),' has finished completely'])
        ;
		% Data is sensing information, report it using report_sensing/4
        Data = [sensing, Outcome] ->
	     report_sensing(A, N, Outcome, _),
	     change_action_state(A,N,_,Outcome,_)
	;
		% Data is an exog. event, report it using report_exog_event/5
	Data = [exog_action, Action] ->
             report_exog_event(Action, ['Exogenous action ',Action, ' from Web']),
	     change_action_state(A,N,_,_,[Action])
	).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% C - EXECUTION MODULE: execute/4
%
% This part implements the execution capabilities of the environment
%
% execute(Action, Type, N, Sensing) : execute Action of Type and return Sensing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% This part implements the execution capabilities of the environment.
% 
% Each action is executed in an independent thread which can trigger
% exogenous events and sensing result at any time. 
% 
% The output of each action (sensing or exog. event) is treated as a regular
% stream (the same as data comming from the environment manager)
%
% The tools send_sensing/1 and send_exogenous/1 will be used by each thread
% to report sensing and exogenous events
execute(Action, _, N, null) :- 
        report_message(action, ['Executing action: ', '*',(Action,N),'*']),
	  % This pipe will be used to obtain sensing and exogenous events
	  % generated from the action thread. Thread will write in Write stream
	  % Build the alias for the action thread
	pipe(Read,Write),
	term_to_atom(action(Action,N,Write), Alias),
	  % Throw an independent thread to perform Action. 
	  % Action thread should write in stream "Write"
	thread_create(perform(Action,Write), _, [alias(Alias), detached(true)]),
	  % Block until sensing outcome is read
	  % This is not used since sensing is reported in each thread using
	  % the tool send_sensing/1 below
%	read(Read, S), 
%	report_sensing(Action, N, S, _),
          % Register a listen_to/3 in the DB to wait for exogenous events
	  % and sensing coming from the executed action
        assert(listen_to(stream, action(Action, N), Read)).






























%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BELOW IS THE CODE FOR THE ACTUAL IMPLEMENTATION OF THE ENVIRONMENT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 0 - TOOLS FOR SENDING SENSING OUTCOMES AND EXOGENOUS EVENTS
%     FROM EACH ACTION THREAD
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% contains the stream where the thread must write its sensing and exog actions
% actionStream/1 is LOCAL to each thread action so there is no ambiguity
:- thread_local actionStream/1. 


% This is the tool called to execute Prolog tool Command which should write
% its output (sensing/exog events) to stream Stream
% Command is a list-char codifying the command to be executed
perform(Command, Stream) :- 
	assert(actionStream(Stream)),  % Register the action stream
        call(Command),
	fail.
perform(_, Stream) :- 
	retract(actionStream(Stream)), % de-register the action stream and close it
	close(Stream).

% Write the sensing result to the environment via the set-up communication
send_sensing(Data)   :- 
	send([sensing, Data]).

% Write the exogenous event to the environment via the set-up communication
send_exogenous(Exog) :- 
	send([exog_action, Exog]).
	
% Sends any data by writing it as a term, writes a dot, nl, and flush.
% This is used by send_sensing/1 and send_exogenous/1
send(Data) :- 
	actionStream(Stream),	% Get the stream associated with the action
        write_term(Stream, Data, [quoted(true)]), 
        write_term(Stream, '.',[]),
        nl(Stream), 
	flush_output(Stream).
	






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1 - INTERFACE IMPLEMENTATION
%     These are actual prolog predicates that provide different internet
%     a and system functionalities
%     This predicates will actually be executed in independent threads
%     and they should report sensing and exog. events using the tools
%     send_sensing/1 and send_exogenous/1 to write in the right stream
%
%     These predicates are the ones seen and known by the user who uses the
%     internet/system environment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check_string_after(+URL, +S, +PAfter) 
%     sense whether there is a string S in in address A after pos. PAfter
% Sensing Outcome : always succeeds with 1
% Exogenous action: int_bool_after/4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
check_string_after(URL, S, PAfter) :- 
        send_sensing(1),
        (find_pos_web(URL, S, PAfter, Pos) -> 
             (Pos=(-1) ->
                  send_exogenous(int_bool_after(URL, S, PAfter, false))
             ; 
                  send_exogenous(int_bool_after(URL, S, PAfter, true))
             )
        ; 
             send_exogenous(int_bool_after(URL, S, PAfter, failed))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check_pos_string(+URL, +S, +PAfter) 
%     sense the position in address A of string S after position PAfter
% Sensing Outcome : always succeeds with 1
% Exogenous action: int_pos/4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
check_pos_string(URL, S, PAfter) :- 
        send_sensing(1),
        (find_pos_web(URL, S, PAfter, Pos) ->
             send_exogenous(int_pos(URL, S, PAfter, Pos))
        ;
             send_exogenous(int_pos(URL, S, PAfter, failed))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% read_string_between(+URL, +D1, +D2, +PAfter): 
%      sense the string between D1 and D2 in address A after position PAfter
% Sensing Outcome : always succeeds with 1
% Exogenous action: int_string_between/5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_string_between(URL, Del1, Del2, PAfter) :- 
        send_sensing(1),
        (retrive_string_delim_web(URL, Del1, Del2, String, PAfter, _) ->
             post_process(String,S2),    % Remove initial/final padding
             send_exogenous(int_string_between(URL, Del1, Del2, PAfter, S2))
        ;
             send_exogenous(int_string_between(URL, Del1, Del2, PAfter, failed))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% read_html_field(+URL, +FieldName, +Cont, +PAfter): 
%      sense the next string value of field FieldName after position PAfter
%      that includes strings SCont
%      By a field we mean a string of the form: ... FieldName="xxxxxx" .....
%          where xxxxxx is the string sensed that contains substring Cont
% Sensing Outcome : always succeeds with 1
% Exogenous action: int_html_field/6
%                   returns xxxxx and its starting position
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_html_field(URL, FieldName, Cont, PAfter) :- 
        send_sensing(1),
        concat_string([FieldName,'=\"'], FieldTag),
        retrive_string_delim_web(URL, [null, FieldTag], ['\"', null], 
                                 String, PAfter, Pos),
        post_process(String, S2),    % Remove initial/final padding
        any_to_string(Cont, SCont),
        once(substring(String, SCont, _)), !, % Done, good string found
        send_exogenous(int_html_field(URL, FieldName, Cont, PAfter, S2, Pos)).

read_html_field(URL, FieldName, Cont, P) :- 
        send_exogenous(int_html_field(URL, FieldName, Cont, P, failed, failed)).


	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sense_nth_link(+URL, +N)
%      senses the nth link in the URL
%
% Sensing Outcome : atom "none" or term  "link(URL,Name)"
% Exogenous action: nothing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sense_nth_link(URL, N) :- 
	get_nth_link(URL, N, (Link,Name)) ->
        	send_sensing(link(Link,Name)) 
	;
		sense_sensing(none).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sense_no_links(+URL)
%      senses the number of links in URL
%
% Sensing Outcome : number
% Exogenous action: nothing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sense_no_links(URL) :- 
	get_no_links(URL, N) ->
        	send_sensing(N) 
	;
		sense_sensing(failed).
        
	
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% read_string_length(+URL, +D, +L, +PAfter): 
%      sense the string in address A that starts with string D for a length
%      of L and after position PAfter
% Sensing Outcome : always succeed with 1
% Exogenous action: int_string_length/5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_string_length(URL, D, L, PAfter) :- 
        send_sensing(1),
        (retrive_string_poslen_web(URL, D, L, PAfter, T) ->
             post_process(T,T1),
             send_exogenous(int_string_length(URL, D, L, PAfter,T1))
        ;
             send_exogenous(int_string_length(URL, D, L, PAfter, failed))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% download(+URL, +File) 
%        download address URL to file File  sense the process id number
% Sensing Outcome : always succeeds with 1
% Exogenous action: int_bool_download
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
download(URL, File) :- 
        send_sensing(1),
        (download(real, URL, File) -> 
             send_exogenous(int_bool_download(URL, File, ok)) 
        ;
             send_exogenous(int_bool_download(URL, File, failed))). 

% Real implementation: downloads the Address (atom) to FileName (atom)
download(real, URL, FileName):- 
        exists_web_file(URL),        
	open(FileName, write, StreamFile, [type(binary)]),
	http_open(URL, StreamURL, [], null),
	copy_stream_data(StreamURL, StreamFile),
	close(StreamFile),
	close(StreamURL).

% Version for debugging: this version creates the corresponding file but with
% an empty content and it waits 10 second to "simulate" the downloading
download(debug, URL, FileName):- 
        exists_web_file(URL),        
        concat_atom(['echo ', URL, ' > ', '\'', FileName, '\''], Com),
        call_to_exec(unix, Com, Command2), % Select right command for exec
        exec(Command2,[]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check_web_file(+URL)
%      senses whether WebFile exists
% Sensing Outcome : always succeds with 1
% Exogenous action: int_bool_urlexists/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
check_web_file(URL) :- 
        send_sensing(1),
        (exists_web_file(URL) ->  
             send_exogenous(int_bool_urlexists(URL, true)) 
        ;
             send_exogenous(int_bool_urlexists(URL, false))).

exists_web_file(URL) :- 
	http_open(URL, S, [], null), 
	close(S).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ACTIONS FOR THE MANAGEMENT OF (NAMED) WEB-BROWSERS
%
% All this action return 1 as sensing unless they fail to execute.
%
% -- browser_new(+ID) 
% -- browser_close(+ID)
% -- browser_refresh(+ID)
% -- browser_open(+ID, +URL)
% -- browser_get(+ID, -CS, -CT)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic browser/4.	% browser(Id, URL, ContentString, ContentTerm)

% Open a new browser with name ID. 
% Sensing Outcomes could be: 
%	1 : action executed successfuly
%	failed : browser ID alredy exists
browser_new(ID) :-                      
	\+ browser(ID, _, _, _),
	any_to_string('',ES),
	assert(browser(ID, _, ES, '')), !,
	send_sensing(1).
browser_new(_) :-                      
	send_sensing(failed).

% Closes browser ID. 
% Sensing Outcomes could be: 
%	1 : action executed successfuly
%	failed : browser ID does not exist
browser_close(ID) :-
	retract(browser(ID, _, _, _)) -> send_sensing(1) ; send_sensing(failed).

% Refresh the content of browser ID.
% Sensing Outcomes could be: 
%	failed : browser ID does not exist
browser_refresh(ID) :-                 
	browser(ID, URL, _, _), !,
	browser_open(ID, URL).
browser_refresh(_) :-                 
	send_sensing(failed).

% Open web page URL on browser ID. 
% Sensing Outcomes could be: 
%	1 : web page opened successfuly
%	0 : no page at URL
%	failed : browser ID does not exist
browser_open(ID, URL) :-               
	get_page(URL, string, CS),  		% Get page as string CS
	get_page(URL, term, CT),		% Get page as term CT
	retract(browser(ID, _, _, _)), !,
	assert(browser(ID, URL, CS, CT)), 	% Assert the page
	send_sensing(1).
browser_open(ID, URL) :-              
	retract(browser(ID, _, _, _)), 
	any_to_string('',ES),
	assert(browser(ID, URL, ES, '')),  !,	% Set the browser to empty
	send_sensing(0).
browser_open(_, _) :-              		% Failed if browser ID does not exists
	send_sensing(failed).

% Refresh the content of browser ID.
% Sensing Outcomes could be: 
%	1 : action executed successfuly
%	failed : browser ID does not exist
browser_url(ID) :-                 
	broswer(ID, URL, _, _), !,
	send_sensing(URL).
browser_url(_) :-                 
	send_sensing(failed).

	
% This action is not used from programs, but it just used inside this file
% Obtain the (current) content of browser ID
%browser_get(ID, C) :-                  
%	browser(ID, _, C) -> send_sensing(1) ; send_sensing(failed).
browser_get(ID, CS, CT) :-                  
	browser(ID, _, CS, CT).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sense_proc_term(+Pid)
%       senses whether process Pid is finished
% Sensing Outcome: true/false (true= process did terminate)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sense_proc_term(Pid) :- 
        proc_term(Pid) -> send_sensing(true) ;
                          send_sensing(false).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sense_proc_exists(+Pid) 
%       sesnes whether process Pid exists
% Sensing Outcome: true/false (true= process did terminate)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sense_proc_exists(Pid) :- 
        proc_exists(Pid) -> send_sensing(true) ;
                            send_sensing(false).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% kill_proc(+Pid) 
%       kills process Pid 
% Sensing Outcome: true/false (true= process was killed successfully)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
kill_proc(Pid) :- 
        proc_kill(Pid) -> send_sensing(true) ;
                          send_sensing(false).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% wait_proc(+Pid) 
%       waits for process Pid to finish
% Sensing Outcome : always succeds with 1
% Exogenous action: int_proc_waited/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
wait_proc(Pid) :- 
        send_sensing(1),
        (wait_proc(Pid, Status) ->  
             send_exogenous(int_proc_waited(Pid, Status)) 
        ;
             send_exogenous(int_proc_waited(Pid, failed))
        ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sense_file_exists(+File) 
%        senses whether file File exists
% Sensing Outcome: true/false (true= file does exist)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sense_file_exists(File) :- 
        file_exists(File) -> send_sensing(true) ;
                             send_sensing(false).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% say(+Phrase, +Lan) 
%        says Phrase in Lan(guage) via speech (requires festival or similar)
% Sensing Outcome: always return 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
say(List, Lan) :- is_list(List), !, 
        concat_atom(List, Phrase), say(Phrase, Lan).
say(Phrase, english) :- 
        concat_atom(['echo \'', Phrase, '\' | festival --tts'], Command),
        call_to_exec(unix, Command, Command2), % Select right command for exec
        exec(Command2, []),
        send_sensing(1).
say(Phrase, Lan) :- Lan\=english,
        concat_atom(['echo \'', Phrase, '\' | festival --tts --language ',Lan], 
                    Command),
        call_to_exec(unix, Command, Command2), % Select right command for exec
        exec(Command2, []),
        send_sensing(1).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3 - LOW-LEVEL PROCEDURES FOR WORKING ON THE WEB
%     These predicates are never seen or known by the user who uses the
%     internet/system environment
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% IMPORTANT TOOL TO OBTAIN THE WEB-PAGE SOURCE FROM A URL
% get_page(URL, Type, C): C is the HTML with Type string/term of link/browser URL
%
get_page(URL, string, CS)             :-  % URL stands for a current open browser
	browser_get(URL, CS, _), !.
get_page(URL, term, CT)        	      :-  % URL stands for a current open browser
	browser_get(URL, _, CT), !.

get_page(URL, string, Reply)  :- !,
	http_get(URL, ReplyAtom, [], null), !,  % http_get/4 generalizes http_get/3
	any_to_string(ReplyAtom, Reply).
get_page(URL, term, Reply) :- !,
	http_open(URL, Stream, [], null), % http_open/4 generalizes http_open/3
	dtd(html, DTD), 
	load_structure(stream(Stream),Reply,[dtd(DTD),dialect(sgml),space(remove)]), 
	close(Stream), !.

	
% Generalizes http_open/3 to account for errors and accept strings
http_open(URL, Stream, Options, Error) :-
	concat_atom([URL], URLA), 
	catch(http_open(URLA, Stream, Options), error(E1, _), true),
	(\+var(E1), E1=existence_error(_, _) -> 
		Error=error(file)    % File does not exists
	;
	 \+ var(E1), E1=socket_error(_) ->
	        Error=error(host)    % Host cannot be found
	;
	        Error=null           % No error
	).


% Generalizes http_get/3 to account for errors and accept strings
http_get(URL, Reply, Options, Error) :-
	concat_atom([URL], URLA), 
	catch(http_get(URLA, Reply, Options), error(E1, _), true),
	(\+var(E1), E1=existence_error(_, _) -> 
		Error=error(file)    % File does not exists
	;
	 \+ var(E1), E1=socket_error(_) ->
	        Error=error(host)    % Host cannot be found
	;
	        Error=null           % No error
	).
	


	
	
	
	
	
	
		
	
		
% Pos is the position of substring T in address A after position PAfter
find_pos_web(URL, T, PAfter, Pos) :- 
        get_page(URL, string, SA), 
        any_to_string(T, ST),
        (substring(SA, P2, _, ST), P2 > PAfter -> Pos=P2 ; Pos=(-1)).

% URL = ......... Del11 ***** Del12 SResult Del12 ****** Del22
% Retrives SResult wrt two flexible Delimiters and after a Position
retrive_string_delim_web(URL, Del1, Del2, SResult, PAfter, Pos):-
        get_page(URL, string, S),
        extract_substring(S, Del1, Del2, SResult, PAfter, Pos).


% retrive_string_poslen_web/5: Retrive string with length after position
% SResult is a substring in the web page URL that is after
% string Del1 and with length Len
retrive_string_poslen_web(URL, Del1, Len, Pos, SResult) :- 
        get_page(URL, string, SA), 
        any_to_string(Del1, SDel1),
        substring(SA, P1, _, SDel1), P1>Pos, !,
        substring(SA, P1, Len, SResult).

% retrive_bet_apos/5: Retrive Between Strings After a Position
% SResult is a substring in the web page with address URL that is between
% strings Del1 and Del2 and after position P
retrive_bet_apos(URL, Del1, Del2, P, SResult) :- 
        get_page(URL, string, SA), 
        any_to_string(Del1, SDel1),
        any_to_string(Del2, SDel2),
        substring(SA, P1, _, SDel1), P1 > P,
        substring(SA, P2, _, SDel2), P2 > P1, !,
        string_length(SDel1, L1),
        L is P2-P1-1-L1,
        P11 is P1+L1,
        substring(SA, P11, L, SResult).


% Post processing of a string: remove spaces and line breaks at the beginning
% 	                       and at the end of the string
post_process(T, T1):- 
        split_string(T, ``, ` `, [T2]),
        string_replace(T2,'\n','',T1).    % Remove end_of_line


	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
get_nth_link(URL, N, Link) :- 
	get_page(URL, term, Content),
	extract_nth_link(Content, N, Link).
	
get_no_links(URL, N) :-
	get_page(URL, term, Content),
	extract_all_links(Content, Links),
	length(Links, N).
	
% extract_nth_link(Content, N, Link) :
%	given a web-page Content, extract the nth link
extract_nth_link(Content, N, Link) :-
	extract_all_links(Content, LLinks),
	nth1(N, LLinks, Link).

% Trivial case when the content is just an atom with a link (e.g., .rmm files)
extract_all_links([Link], [(Link,'')]) :- atom(Link), !.
% The general case when the Content is complicated HTML source
extract_all_links(Content, LLinks) :-
	extract_all_links2(Content, LLinks2), !,
	flatten(LLinks2, LLinks).

% extract_all_links(Content, LLink) :
%	given a web-page Content, extract all the links into list LLink
extract_all_links2(element(a, LAtrib, LContent), [(Link, Name)]) :- !,
	member(href=Link, LAtrib),
	extract_name(LContent, Name).

extract_all_links2(element(_, _, LContent), LLinks) :- !,
	maplist(extract_all_links2, LContent, LLinks).
	
extract_all_links2(LContent, LLinks) :-
	is_list(LContent), !,
	maplist(extract_all_links2, LContent, LLinks).
	
extract_all_links2(_, []).
	

% extract_name(LContent, Name) :
% 	given an element/3 term in LContent describing the name of a link, 
%	it extracts just the text in Name
extract_name(LContent, NameClean) :-
		% First just get the Name2
	extract_name2(LContent,Name2),
		% Clean up spaces in Name
	split_atom(Name2,' ','',L1),
	delete(L1,'',L2),
	join_atom(L2,' ',NameClean).	
	
% This just extracts the name but it may contain redundant spaces
extract_name2(element(_,_,R), Name) :- 
	extract_name2(R, Name).
extract_name2([A|LR], Name) :- 
	extract_name2(A, NameA),
	extract_name2(LR, NameR),
	concat_atom([NameA, ' ', NameR], Name).
extract_name2(LContent, LContent) :- LContent\=[], atom(LContent), !.
extract_name2(_,'').
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF:  Env/env_int_swi.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
