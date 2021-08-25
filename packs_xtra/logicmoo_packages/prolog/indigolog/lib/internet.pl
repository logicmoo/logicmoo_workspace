%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: Env/internet.pl
%
%  Author    : Sebastian Sardina
%  Time-stamp: <03/09/27 22:52:54 ssardina>
%  email     : ssardina@cs.toronto.edu
%  WWW       : www.cs.toronto.edu/~ssardina
%  TESTED    : SWI Prolog 5.0.10 http://www.swi-prolog.org
%	       ECLiPSe 5.3 on RedHat Linux 6.2-7.2
%  TYPE CODE : system independent predicates
%
% DESCRIPTION: Prolog primitive actions to access the Internet and the Web
%
% This file provides primitive actions for programming an Internet Agent.
% where the IndiGolog program is running. Also, sensing outcomes for that
% actions are asked in the same terminal.
% On the other hand, exogenous events are handle depending the exogenous
% moduled loaded (e.g. another xterm terminal, or a TCL/TK windows, etc.)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                             November 22, 2002
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
% THE UNIVERSITY OF TORONTO DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
% SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
% FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF TORONTO BE LIABLE FOR ANY
% SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
% RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
% CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Primitive actions are called using perform/3:
%
% -- perform(CommandList)
%       CommandL is a list-char codifying the command to be executed
%
%  OBS: If ActionT is the term containing the primitive action to be
%       called, your code using internet.pl should do something like this:
%
%       term_to_atom(perform(ActionT), ActionA), % Convert into a list-chars
%       concat_atom(['eclipse -b ', Dir, 'lib/internet.pl -e ','\'', 
%                     ActionA,'\''], Command),
%       exec(Command). 
%
% or issue the shell comamnd:
%
%     eclipse -b <path>/internet.pl -e 'perform(<ActionAsListChar>'
%
%
% The following low-level internet/system primitive actions are provided
% (i.e., they are legal commands to be used with perform/1)
%
% -- browser_new(+ID) 
%        create a new web browser called IdWeb
% -- browser_close(+ID)
%        remove web browser IdWeb
% -- browser_refresh(+ID)
%        refresh content of URL
% -- browser_open(+ID, +URL)
%        set browser IdWeb to URL
%
%  -- check_string_after(+URL, +S, +PAfter) 
%        sense whether there is a string S in in address A after pos PAfter
%  -- check_pos_string(+A, +S, +PAfter) 
%        sense the position in address A of string S after position PAfter
%  -- read_string_between(+A, +D1, +D2, +PAfter): 
%        sense the string between D1 and D2 in address A after pos PAfter
%  -- read_string_length(+URL, +D, +L, +PAfter): 
%        sense the string in address A that starts with string D for 
%        a length of L and after position PAfter
%  -- read_html_field(+URL, +FieldName, +Cont, +PAfter): 
%        sense the next string value of field FieldName after position PAfter
%        that includes string Cont
%  -- download(+URL, +File) 
%        download address URL to file File  sense the process id number
%  -- check_web_file(+URL)
%        senses whether WebFile exists
%
%  -- sense_proc_term(+Pid)
%        senses whether process Pid is finished
%  -- kill_proc(+Pid)
%        kills process Pid
%  -- wait_proc(+Pid)
%        waits for process Pid to finish
%  -- sense_proc_exists(+Pid) 
%        sesnes whether process Pid exists
%  -- sense_file_exists(+File) 
%        senses whether file File exists
%  -- say(+Phrase, +Language)
%        speak Phrase in Language (requires a voice synthesis 
%        like festival)
%
% The following actions may generate the following exogenous actions:
%     
%  -- int_bool_after(URL, String, PAfter, Bool) :
%         Bool=there is String after PAfter in URL
%  -- int_pos(URL, String, PAfter, Pos) :
%         Pos=the next String after PAfter in URL is at position Pos
%  -- int_string_between(URL, Del1, Del2, PAfter, String) :
%         String=string between Del1 and Del2 in URL after PAfter
%  -- int_string_length(URL, Del, Lenght, PAfter, String) :
%         String=string after Del1 of length Length in URL after PAfter
%  -- int_html_field(URL, FieldName, PAfter, String, Pos) :
%         String=value of field FieldName containing Cont in URL after PAfter
%         Pos=starting postition of String
%  -- int_bool_download(URL, File, Status) :
%         Downloading of URL to File finished with Status (ok/failed)
%  -- int_bool_urlexists(URL, Bool) :
%         Bool= URL exists
%  -- int_proc_waited(Pid, Status)
%         Status = result of waiting process Pid (may be failed)
%
%
% REQUIRED:
%
% -- compat_swi/compat_ecl compatibility libraries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%:- module(internet,
%	[check_string_after/3,		% +URL, +String, +Position
%	 check_pos_string/3, 		% +URL, +String, +Position
%	 read_string_between/4,		% +URL, +Del1, +Del2, +Position
%	 read_string_length/4,		% +URL, +Start, +Length, +Position
%	 download/2,			% +URL, +FileName	
%	 check_web_file/1,		% +URL
%	 sense_proc_term/1,		% +PID
%	 sense_proc_exists/1,		% +PID
%	 sense_file_exists/1,		% +FileName
%	]).

%:-  set_stream(warning_output,3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% AUTOMATIC LOAD OF REQUIRED LIBRARIESSECTION %%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This subsection does the following:
%
%  (a) loads the neccessary compatibility library: compat_ecl or compat_swi
%  (b) sets ` to be the string construct (using set_backquoted_string)

% Path is the root path of the IndiGolog system
% In SWI Pwd will be a string already
main_dir(Path):- getenv('PATH_INDIGOLOG',Pwd),
                 (string(Pwd) -> atom_string(APwd, Pwd) ; APwd=Pwd),
                 concat_atom([APwd, '/'], Path).

% Load required libraries (depending wheter its SWI or ECLIPSE)
:- dynamic library_directory/1.
:- library_directory(_) -> 
       main_dir(Dir),       % We are running in SWI 
       concat_atom([Dir,'lib'], LibDir),
       assert(library_directory(LibDir)),
       use_module(library(eclipse_swi)),
       use_module(library(tools_swi)),
       set_backquoted_string
   ;                        % We are in ECLIPSE (rely on ECLIPSELIBRARYPATH)
       set_stream(warning_output,3),  % Do not write warnings
       use_module(library(tools_ecl)),
       set_backquoted_string. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 0 - TOOLS FOR SENDING SENSING OUTCOMES AND EXOGENOUS EVENTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Sends any data by writing it as a term, writes a dot, nl, and flush.
send(Data) :- 
        write_term(Data, [quoted(true)]), 
        write_term('.',[]),
        nl, flush_output.

% Write the sensing result to the environment via the set-up communication
send_sensing(Data)   :-  send([sensing, Data]).

% Write the exogenous event to the environment via the set-up communication
send_exogenous(Exog) :- send([exog_action, Exog]).


% This is the tool callled from outside. 
% CommandL is a list-char codifying the command to be executed
perform(CommandList) :- 
        string_to_list(CommandS, CommandList), 
        string_to_atom(CommandS, CommandA), 
        term_to_atom(CommandT, CommandA),
        call(CommandT).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1 - INTERFACE IMPLEMENTATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
        concat_atom(['lynx --dump ', URL, ' > ', '\'', FileName, '\''], Com),
        call_to_exec(unix, Com, Command2), % Select right command for exec
        exec(Command2,[]).

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

exists_web_file(WF) :- 
        concat_atom(['lynx --dump -head ', WF], Command),
        call_to_exec(unix, Command, Command2), % Select right command for exec
        exec(Command2, [null, streamout], Pid), 
        read_string(streamout, end_of_line, _, S),
        wait(Pid, _),   % This can be problematic if a signal arrives
        close(streamout),
        substring(S, `OK`, _).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ACTIONS FOR THE MANAGEMENT OF (NAMED) WEB-BROWSERS
%
% All this action return 1 as sensing unless they fail to execute.
%
% -- browser_new(+ID) 
% -- browser_close(+ID)
% -- browser_refresh(+ID)
% -- browser_open(+ID, +URL)
% -- browser_get(+ID, -C)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic browser/3.

% Open a new browser with name ID. Fails if ID already exists
browser_new(ID) :-                      
	\+ browser(ID, _, _),
	any_to_string('',ES),
	assert(browser(ID, _, ES)), !,
	send_sensing(1).
browser_new(_) :-                      
	send_sensing(failed).

% Closes browser ID. Fails if ID does not exist
browser_close(ID) :-
	retract(browser(ID, _, _)) -> send_sensing(1) ; send_sensing(failed).

% Refresh the content of browser ID. Fails if ID does not exist
browser_refresh(ID) :-                 
	broswer(ID, URL, _),
	browser_open(ID, URL), !,
	send_sensing(1).
browser_refresh(_) :-                 
	send_sensing(failed).


% Open web page URL on browser ID. Fails if ID does not exist
browser_open(ID, URL) :-               
	retract(browser(ID, _, _)),
	get_page(URL, C),
	assert(browser(ID, URL, C)), !,
	send_sensing(1).
browser_open(_, _) :-               
	send_sensing(failed).

% This action is not used from programs, but it just used inside this file
% Obtain the (current) content of browser ID
%browser_get(ID, C) :-                  
%	browser(ID, _, C) -> send_sensing(1) ; send_sensing(failed).
browser_get(ID, C) :-                  
	browser(ID, _, C).



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
% 3 - LOW-LEVEL procedures for working on the web.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% C is the HTML string of address URL
get_page(URL, C)             :-  % URL stands for a current open browser
	browser_get(URL, C), !.
get_page(URL, C)             :- 
        any_to_string(URL, S),
        get_page(S, C, preparsed).
get_page(SA, S, preparsed)  :- 
        concat_string([`lynx --source --preparsed `, SA], Command),
        call_to_exec(unix, Command, Command2), % Select right command for exec
        exec(Command2, [null, streamout], Pid),
        read_string(streamout, end_of_file, _, S),
        wait(Pid, 0),
        close(streamout).
get_page(SA, S, raw)        :- 
        concat_string([`lynx --source `, SA], Command),
%        exec_group(C, [null, streamout], _),
        call_to_exec(unix, Command, Command2), % Select right command for exec
        exec(Command2, [null, streamout], Pid),
        read_string(streamout, end_of_file, _, S),
        wait(Pid, 0),
        close(streamout).


% Pos is the position of substring T in address A after position PAfter
find_pos_web(URL, T, PAfter, Pos) :- 
        get_page(URL, SA), 
        any_to_string(T, ST),
        (substring(SA, P2, _, ST), P2 > PAfter -> Pos=P2 ; Pos=(-1)).

% URL = ......... Del11 ***** Del12 SResult Del12 ****** Del22
% Retrives SResult wrt two flexible Delimiters and after a Position
retrive_string_delim_web(URL, Del1, Del2, SResult, PAfter, Pos):-
        get_page(URL, S),
        extract_substring(S, Del1, Del2, SResult, PAfter, Pos).


% retrive_string_poslen_web/5: Retrive string with length after position
% SResult is a substring in the web page URL that is after
% string Del1 and with length Len
retrive_string_poslen_web(URL, Del1, Len, Pos, SResult) :- 
        get_page(URL, SA), 
        any_to_string(Del1, SDel1),
        substring(SA, P1, _, SDel1), P1>Pos, !,
        substring(SA, P1, Len, SResult).

% retrive_bet_apos/5: Retrive Between Strings After a Position
% SResult is a substring in the web page with address URL that is between
% strings Del1 and Del2 and after position P
retrive_bet_apos(URL, Del1, Del2, P, SResult) :- 
        get_page(URL,SA), 
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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: Env/internet.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


