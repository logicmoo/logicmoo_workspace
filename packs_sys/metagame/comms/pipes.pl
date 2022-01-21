%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================


%%% pipes.pl

talk_interface(Command) :- 
	interface_record_streams(Command,_,_).


interface_record_streams(Command,InStream,OutStream) :-
    connect_pipes(Command,InStream,PipeIn,OutStream,PipeOut),
    add_global(instream,InStream),
    add_global(outstream,OutStream),
    add_global(inpipe,PipeIn),
    add_global(outpipe,PipeOut).




% CONNECT_PIPES(+Command,-InStream,-PIn,-OutStream,-POut).
% Calls Command with input from new pipe PipeOut, and output
% to new pipe PipeIn, and opens inverted streams to those
% pipes so that InStream gets command's output, and
% OutStream sends command input.  
% Thus we are fully hooked up to talk to the command.
%

connect_pipes(Command,InStream,PIn,OutStream,POut) :-
    open_new_pipe(read,PIn,InStream),
    open_new_pipe(write,POut,OutStream),
    shell([Command,>,PIn,<,POut,&]).





mode_arrow(read,<).
mode_arrow(write,>).

open_pipe(Pipe,Mode,Stream) :- 
    mode_arrow(Mode,Arrow),
    command_from_args([cat,Arrow,Pipe],Command),
    unix(popen(Command,Mode,Stream)). 

open_new_pipe(Mode,Pipe,Stream) :- 
    new_pipe_file(Pipe),
    open_pipe(Pipe,Mode,Stream).

new_pipe_file(Pipe) :-
	gensym(pipe,P), 
	concat_list(['/tmp/',P,'XXXXXX'],Template),
        mktemp(Template,Pipe), 
	shell([mknod,Pipe,p]).



% If connection has been broken for some reason,
% creates new streams corresponding to our input and
% output pipes. 
% 
reconnect_pipes :-
    reconnect_inpipe,
    reconnect_outpipe.

reconnect_inpipe :-
    global(inpipe,PIn),
    open_pipe(PIn,read,InStream),
    add_global(instream,InStream).

reconnect_outpipe :-
    global(outpipe,POut),
    open_pipe(POut,write,OutStream),
    add_global(outstream,OutStream).


close_int :- interface_close.

interface_close :- 
	interface_close_streams,
	interface_close_pipes.

close_streams :-
	interface_close_streams.

interface_close_streams :-
    global(instream,InStream),
    close(InStream),
    global(outstream,OutStream),
    close(OutStream).

close_pipes :- 
	interface_close_pipes.

interface_close_pipes :- 
    rm_gpipe(inpipe),
    rm_gpipe(outpipe).

rm_gpipe(P) :- 
    global(P,Pipe), 
    shell([rm,Pipe]).



% Use 'trace tellmove' to see what we are telling.
%
tell_outstream(Statement) :- 
    tracing_format(tellmove,"Telling <Opponent>: ~p~n",[Statement]),
    current_output(OldStream),
    global(outstream,O),
    command_from_args(Statement,String),
    format(O,String,[]),
    format(O,"~n",[]),
    flush_output(O),
    set_output(OldStream).


%---------------------------------------------------------------------------
% Pattern Matching in communications
%---------------------------------------------------------------------------

:- abolish(found/1), abolish(found1/1).

% Reads current data stream until finds a string Pattern.
% 
read_until_string(Pattern) :- 
	tracing_format(readmove,"Looking for pattern: ~s~n",[Pattern]),
	found(Pattern),
	tracing_format(readmove,"~nPattern found: ~s~n",[Pattern]).


% FOUND(Symbol)
% 
% Read a sequence of chars until a pattern is found which matches each 
% char.  Uses Sahlin's routines above. 
	
found(Symbol) :- 
	found1(([],Symbol)).

% Use 'trace readmove' to see what we are reading.
found1((_,[])).
found1(SymbInfo) :- 
	get0(C),
	tracing_format(readmove,"~s",[[C]]),
	new(C,SymbInfo,SymbInfoNew),
	found1(SymbInfoNew).

%---------------------------------------------------------------------------
	
%================================================================================
% Interface
%================================================================================

