% library to access a REDIS server via the redis-cli CLI utility
% /Applications/SWI-Prolog8.1.1.app/Contents/MacOS/swipl -l redisclient.pl
%TODO: add psubscribe(Pattern) ?
:- module(redisclient,[
    create/2, create/3, get_key/2, set_key/2, get_keys/1, get_channels/1, kill_all/0
    ]).
:- use_module(library(process)).

:- dynamic client/3. % (_PID,_OutputStream,_InputStream).

% create redis-cli instance, keeping a pair of streams for it; as a Prolog thread
create(Server,Password) :- 
    create(Server,6379,Password).
create(Host,Port,Pass) :- 
    ((client(_,_,_))-> 
    	throw('Redis-cli subprocess already exists; kill it first with kill_all')
    	; true),
    process_create(path("redis-cli"), ["-h", Host, "-p", Port, "-a", Pass], [detached(true),stdout(pipe(Output)),stdin(pipe(Input)),stderr(std),process(PID)]),
    assert(client(PID,Output,Input)).

% set/get one key
get_key(Key,Value) :- 
    must_be(text,Key), client(_PID,Output,Input), 
    with_mutex(redis_singleton,(
        format(Input,"get ~w~n",[Key]), flush_output(Input), read_line_to_string(Output,Value)
    )).

set_key(Key,Value) :- 
    must_be(text,Key), client(_PID,Output,Input), 
    with_mutex(redis_singleton,(
        format(Input,"set ~w ~w~n",[Key,Value]), flush_output(Input), read_line_to_string(Output,"OK")
    )).

get_keys(L) :-
    client(_PID,Output,Input), 
    with_mutex(redis_singleton,(
        format(Input,"dbsize~nkeys *~n",[]), flush_output(Input), 
        read_line_to_string(Output,Count), read_term_from_atom(Count,N,[]),
        read_N_lines(N,Output,L)
    )).

get_channels(L) :-
    Marker = "I_AM_NOT_1942_A_CHANNEL", % alternatively use info stats to get the pubsub_channels count
    client(_PID,Output,Input), 
    with_mutex(redis_singleton,(
        format(Input,"PUBSUB CHANNELS~nPING ~w~n",[Marker]), flush_output(Input), 
        read_lines_until(Marker,Output,L)
    )).

read_N_lines(0,_,[]) :- !.
read_N_lines(N,S,[Line|Lines]) :- 
    read_line_to_string(S,Line), NewN is N-1,
    read_N_lines(NewN,S,Lines).

read_lines_until(Marker,Stream,Lines) :- 
    must_be(string,Marker),
    read_line_to_string(Stream,Line),
    (Line == Marker -> Lines = [] ; read_lines_until(Marker,Stream,More), Lines=[Line|More]).


kill_all :- retract(client(PID,_,_)), process_kill(PID), fail.
kill_all.
