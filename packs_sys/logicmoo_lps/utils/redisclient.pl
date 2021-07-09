% library to access a REDIS server via both the redis-cli CLI utility
% /Applications/SWI-Prolog8.1.1.app/Contents/MacOS/swipl -l redisclient.pl
%TODO: add psubscribe(Pattern) ?
:- module(redisclient,[
    create/2, create/3, get_key/2, set_key/2, get_keys/1, get_channels/1, kill_all/0,
    restapi_login/3, restapi_request/3, restapi_request/2, restapi_request_result/2,
    restapi_server_base/1
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

%%%%%% New interface, using REST API provided by VH for the OpenSCADA project, February 2021
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).

:- thread_local restapi_token/1, restapi_server_base/1.

%! restapi_login(+ServerURLbase,+User,+Password) is det
%  ServerURLbase is the server URL up to and including .../restapi
%  obtain a token and stores it for other predicates to use during the same goal
restapi_login(ServerURL,User,Pass) :-
    (catch(lps_server_UI:user_is_known,_,fail)->true; throw("non authenticated user")), 
    must_be(atomic,User), must_be(atomic,Pass),
    format(string(Text),'{"username":"~a","password":"~a"}',[User,Pass]),
    format(string(URL),"~a/api-token-auth/",[ServerURL]),
    http_post(URL, atom(Text), Result, [request_header('Content-Type'='application/json')]),
    Result = json([token=T]),
    retractall(restapi_token(_)), assert(restapi_token(T)),
    retractall(restapi_server_base(_)), assert(restapi_server_base(ServerURL)).

%! restapi_request(+PartialURL,-ID,+HandlerGoal) is det
% submits an assynchronous request using the stored token, succeeding immediately; then use restapi_request_result/2
% e.g. tag/devices/ , tag/tag2/ , home/homes/
restapi_request(PartialURL,ID,G) :- 
    restapi_server_base(Base), format(atom(URL),"~a~a",[Base,PartialURL]),
    restapi_token(Token), format(string(Auth),"token ~a",[Token]),
    gensym(restapi,ID),
    thread_create((
        catch(
            http_get(URL, Result, [request_header('Content-Type'='application/json'),request_header('Authorization'=Auth)]),
            Ex,
            (print_message(error,Ex), fail)), 
        assert(restapi_request_result_(ID,Result)),
        once(G)),
        _TID,[]).

restapi_request(PartialURL,ID) :- restapi_request(PartialURL,ID,true).

:- dynamic restapi_request_result_/2.

%! restapi_request_result(+RequestID,-Result)
%  Succeeds if there is already a result for RequestID, made with restapi_request/2.
%  Result is a json([...]) term
restapi_request_result(RequestID,Result) :- restapi_request_result_(RequestID,Result).

:- multifile interpreter:premature_system_action/1. % LPS engine hook
interpreter:premature_system_action(restapi_request_result(RequestID,_)) :- \+ restapi_request_result_(RequestID,_), !. % so we do not block the LPS interpreter

:- multifile sandbox:safe_primitive/1.
:- if(current_module(swish)).
sandbox:safe_primitive(redisclient:restapi_login(_,_,_)).
sandbox:safe_primitive(redisclient:restapi_request(_,_)).
:-endif.
