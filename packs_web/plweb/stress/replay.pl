/*  This file is part of ClioPatria.

    Author:
    HTTP:	http://e-culture.multimedian.nl/
    GITWEB:	http://gollem.science.uva.nl/git/ClioPatria.git
    GIT:	git://gollem.science.uva.nl/home/git/ClioPatria.git
    GIT:	http://gollem.science.uva.nl/home/git/ClioPatria.git
    Copyright:  2007, E-Culture/MultimediaN

    ClioPatria is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2 of the License, or
    (at your option) any later version.

    ClioPatria is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with ClioPatria.  If not, see <http://www.gnu.org/licenses/>.
*/

:- module(http_replay,
	  [ http_replay/2		% +Log, +Options
	  ]).
:- use_module(http_cookie).
:- use_module(library(debug)).
:- use_module(library(time)).
:- use_module(library(gensym)).
:- use_module(library(aggregate)).
:- use_module(library(option)).

/** <module> Replay HTTP logfiles to stress-test the server

@tbd	Provide more concurrency
@tbd	Manage 'not modified'
*/

%%	http_replay(+LogFile, +Options) is det.
%
%	Replay requests from LogFile.  Options include
%
%		* host(Server)
%		Replay on the indicated server instead of the host named
%		in the request
%
%		* port(Port)
%		Port on which to access the server.
%
%		* prefix(Prefix)
%		Remove prefix from paths.  Same as prefix(Prefix, '').
%
%		* prefix(Old, New)
%		Replace path prefix Old by New
%
%		* concurrent(Count)
%		Concurrency level (default: 1)
%
%		* count(+Count)
%		Process (at most) Count records.

http_replay(Log, Options) :-
	flag(http_processed, _, 0),
	flag(http_bytes, _, 0),
	option(count(Count), Options, -1),
	setup_call_cleanup(
	    start_dispatchers(Options),
	    setup_call_cleanup(
		open(Log, read, In, [encoding(utf8)]),
		( read(In, T0),
		  replay(T0, In, Count)
		),
		close(In)),
	    join_dispatchers).

replay(end_of_file, _, _) :- !.
replay(_, _, 0) :- !.
replay(Term, In, Count0) :-
	(   dispatch(Term)
	->  true
	;   format(user_error, 'FAILED: Replay ~q~n', [Term])
	),
	read_log(In, Term2),
	Count1 is Count0 - 1,
	replay(Term2, In, Count1).

read_log(In, Term) :-
	read_term(In, Term, [syntax_errors(dec10)]).


		 /*******************************
		 *	    DISPATCHERS		*
		 *******************************/

:- dynamic
	dispatcher/2,			% ThreadID, Queue
	session_on/2,			% Session --> ThreadID
	id_on/2.			% RequestID --> ThreadID

queue_size(10).

dispatch(Term) :-
	dispatcher_for(Term, Thread, Id, Why),
	(   Thread == all
	->  forall(dispatcher(_,Q), thread_send_message(Q, Term))
	;   Thread == none
	->  true
	;   dispatcher(Thread, Queue),
	    get_time(T0),
	    thread_send_message(Queue, Term),
	    get_time(T1),
	    T is T1-T0,
	    debug(replay, 'Sending ~D to ~w (~w; waited ~3f sec)',
		  [Id, Thread, Why, T])
	).

dispatcher_for(quit, all, -, quit) :- !.
dispatcher_for(server(_,_), all, -, server) :- !,
	retractall(session_on(_,_)),
	retractall(id_on(_,_)).
dispatcher_for(request(Id, _Time, Request), Target, Id, session) :-
	memberchk(session(Session), Request),
	session_on(Session, Target), !,
	asserta(id_on(Id, Target)).
dispatcher_for(request(Id, _Time, _Request), Target, Id, new) :- !,
	get_time(T0),
	State = s(nowait),
	repeat,
	aggregate(min(Waiting, Target),
		  waiting(Target, Waiting),
		  min(Waiting, Target)),
	(   queue_size(Max),
	    Waiting >= Max
	->  debug(replay_drain, 'All queues are full; waiting', []),
	    sleep(0.01),
	    nb_setarg(1, State, wait),
	    fail
	;   !,
	    (	arg(1, State, wait)
	    ->	get_time(T1),
		T is T1 - T0,
		debug(replay, 'Waited ~3f sec for queues to drain', [T])
	    ;	true
	    )
	),
	asserta(id_on(Id, Target)).
dispatcher_for(completed(Id, _TimeUsed, _Bytes, _Code, _Reply), Target, Id, completed) :-
	retract(id_on(Id, Target)), !.
dispatcher_for(_, none, -, none).

waiting(Target, Waiting) :-
	dispatcher(Target, Queue),
	message_queue_property(Queue, size(Waiting)).


start_dispatchers(Options) :-
	option(concurrent(N), Options, 1),
	forall(between(1, N, I),
	       (   atom_concat(dispatcher_, I, Id),
		   message_queue_create(Queue, [max_size(10000)]),
		   thread_create(process_event(Queue, Options), _,
				 [alias(Id)]),
		   assertz(dispatcher(Id, Queue))
	       )).

process_event(Queue, Options) :-
	repeat,
	thread_get_message(Queue, Message),
	replay_term(Message, Options),
	Message == quit, !.

join_dispatchers :-
	dispatch(quit),
	forall(retract(dispatcher(Id, Queue)),
	       (   thread_join(Id, _),
		   message_queue_destroy(Queue))).


replay_term(server(_StartStop, _Time), _Options) :- !,
	join_all.
replay_term(request(Id, _Time, Request), Options) :- !,
	request(Id, Request, Options).
replay_term(completed(Id, _TimeUsed, _Bytes, _Code, Reply), Options) :- !,
	completed(Id, Reply, Options).
replay_term(Term, _Options) :-
	debug(replay, 'Unknown log term: ~p~n', [Term]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Restrictions

	* If requests are in paralel, run them in parallel
		- Run request in thread
		- If `completed', do a join on the thread
	* Manage session cookies
		- Use session-id as client-id
		- Run all


- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%%	request(+Id, +Request, +Options) is det.
%
%	Re-sent a request to the server.

request(Id, Request, Options) :-
	memberchk(method(Method), Request),
	ok_method(Method),
	!,
	(   memberchk(session(Session), Request)
	->  true
	;   Session = (-)
	),
	url_parts(Request, Parts, Options),
	request_options(Request, ROptions),
	thread_create(make_request(Id, Session, Method, Parts, ROptions), TID, []),
	assert(thread_map(Id, TID)).
request(Id, Request, _Options) :-
	memberchk(method(Method), Request),
	format(user_error, 'Request ~w using method ~q is not supported~n',
	       [Id, Method]).

ok_method(get).
ok_method(head).


:- thread_local
	session_map/2,			% LogSession, Client
	thread_map/2.			% RequestID, Thread

make_request(Id, Session, Method, Parts, Options) :-
	call_with_time_limit(
	    300,
	    make_request2(Id, Session, Method, Parts, Options)).

make_request2(Id, Session, Method, Parts, Options) :-
	(   session_map(Session, ClientId)
	->  IsNew = old
	;   IsNew = new,
	    gensym(client, ClientId)
	),
	memberchk(path(Path), Parts),
	debug(replay, 'Request ~w for ~q on ~w client ~w',
	      [Id, Path, IsNew, ClientId]),
	get_time(Now),
	open_null_stream(Dest),
	call_cleanup(http_get(ClientId, Parts, _Reply,
			      [ to(stream(Dest)),
				method(Method),
				cert_verify_hook(http_replay:ssl_verify)
			      | Options
			      ]),
		     Reason, done(Path, Reason, Now, Dest)),
	(   IsNew == new,
	    http_current_cookie(ClientId, swipl_session, Session, _)
	->  debug(replay, 'Using client ~w on session ~w~n',
		  [ClientId, Session]),
	    assert(session_map(Session, ClientId))
	;   true
	).

ssl_verify(_SSL,
           _ProblemCertificate, _AllCertificates, _FirstCertificate,
           _Error).

done(Path, Reason, T0, Dest) :-
	get_time(Now),
	Time is Now-T0,
	byte_count(Dest, Count),
	progress(Count),
	close(Dest),
	debug(replay, '~w: (~w) got ~D bytes in ~3f sec',
	      [Path, Reason, Count, Time]).


progress(Count) :-
	flag(http_processed, Requests, Requests+1),
	flag(http_bytes,     Bytes,    Bytes+Count),
	(   Count mod 1000 =:= 0
	->  format(user_error,
		   '\rProcessed: ~`.t ~D~25|~t~D~40|',
		   [Requests, Bytes])
	;   true
	).

%%	url_parts(+Request, -Parts, +Options) is det.
%
%	Create a new request from the log-entry and Options.

url_parts(Request,
	  [ method(Method),
	    host(Host),
	    port(Port),
	    path(Path),
	    scheme(Proto)
	  | Parts
	  ], Options) :-
	option(scheme(Proto), Options, http),
	memberchk(method(Method), Request),
	memberchk(path(Path0), Request),
	map_path(Path0, Path, Options),
	(   memberchk(host(Host), Options)
	->  true
	;   memberchk(host(Host), Request)
	),
	(   memberchk(port(Port), Options)
	->  true
	;   memberchk(port(Port), Request)
	->  true
	;   Port = 80
	),
	more_parts(Request, Parts).

more_parts([], []).
more_parts([H|T0], [H|T]) :-
	cp_part(H), !,
	more_parts(T0, T).
more_parts([_|T0], T) :-
	more_parts(T0, T).

cp_part(search(_)).

map_path(Path0, Path, Options) :-
	memberchk(prefix(Prefix), Options),
	atom_concat(Prefix, Path, Path0), !.
map_path(Path0, Path, Options) :-
	memberchk(prefix(Old, New), Options),
	atom_concat(Old, Path1, Path0), !,
	atom_concat(New, Path1, Path).
map_path(Path, Path, _).


%%	request_options(+Request, -Options) is det.
%
%	Extract additional options  for  the   query  from  the request.
%	Currently, this extracts possible range-options. Future versions
%	may also pass the Accept options.

request_options(Request, [range(Range)]) :-
	memberchk(range(Range), Request), !.
request_options(_, []).


%%	completed(+Id, +Reply, +Options)
%
%	Wait for the completion of request Id.

completed(Id, _Reply, _Options) :-
	retract(thread_map(Id, TID)),
	(   catch(thread_join(TID, State), _, fail)
	->  debug(replay, 'Request ~d ended: ~w', [Id, State])
	;   debug(replay, 'Skipped ~d', [Id])
	).

%%	join_all is det.
%
%	Join all pending threads.

join_all :-
	current_thread(TID, _State),
	retract(thread_map(Id, TID)), !,
	thread_join(TID, State),
	debug(replay, 'Request ~w ended: ~w', [Id, State]),
	join_all.
join_all.
