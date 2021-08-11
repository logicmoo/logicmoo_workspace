/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010-2015, University of Amsterdam,
			      VU University Amsterdam.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(server_stats,
	  [ http_session_table//0,
	    http_server_statistics//0,
	    http_server_pool_table//0
	  ]).
:- use_module(library(option)).
:- use_module(library(pairs)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_stream)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_json)).
:- use_module(openid).
:- use_module(messages).

:- html_meta
	odd_even_row(+, -, html, ?, ?).

/** <module> Server statistics components

*/

:- http_handler(root(stats),	     server_stats,      []).
:- http_handler(root(health),	     server_health,     []).
:- http_handler(root(stats/streams), list_file_streams, []).
:- http_handler(root(stats/stream),  stream_details,    []).
:- http_handler(root(admin/debug),   start_debugger,    []).

server_stats(_Request) :-
	reply_html_page(title('SWI-Prolog server statistics'),
			[ \html_requires(css('stats.css')),
			  h1(class(wiki), 'SWI-Prolog HTTP server statistics'),
			  \http_server_statistics,
			  h2(class(wiki), 'Pool statistics'),
			  \http_server_pool_table
			]).


%%	http_session_table//
%
%	HTML component that writes a table of currently logged on users.

http_session_table -->
	{ findall(S, session(S), Sessions0),
	  sort(Sessions0, Sessions),
	  Sessions \== [], !
	},
	html([ table([ class(block)
		     ],
		     [ tr([th('User'), th('Real Name'),
			   th('On since'), th('Idle'), th('From')])
		     | \sessions(Sessions, 1)
		     ])
	     ]).
http_session_table -->
	html(p('No users logged in')).

%%	session(-Session:s(Idle, User, SessionID, Peer)) is nondet.
%
%	Enumerate all current HTTP sessions.

session(s(Idle, User, SessionID, Peer)) :-
	http_current_session(SessionID, peer(Peer)),
	http_current_session(SessionID, idle(Idle)),
	User = (-).

sessions([], _) --> [].
sessions([H|T], Row) -->
	odd_even_row(Row, Next, \session(H)),
	sessions(T, Next).

session(s(Idle, -, _SessionID, Peer)) -->
	html([td(-), td(-), td(-), td(\idle(Idle)), td(\ip(Peer))]).
session(s(Idle, User, _SessionID, Peer)) -->
	{  RealName = '?',
	   OnSince = 0
	},
	html([td(User), td(RealName), td(\date(OnSince)), td(\idle(Idle)), td(\ip(Peer))]).

idle(Time) -->
	{ Secs is round(Time),
	  Min is Secs // 60,
	  Sec is Secs mod 60
	},
	html('~`0t~d~2|:~`0t~d~5|'-[Min, Sec]).

date(Date) -->
	{ format_time(string(S), '%+', Date)
	},
	html(S).

ip(ip(A,B,C,D)) --> !,
	html('~d.~d.~d.~d'-[A,B,C,D]).
ip(IP) -->
	html('~w'-[IP]).


%%	http_server_statistics//
%
%	HTML component showing statistics on the HTTP server

http_server_statistics -->
	{ findall(Port-ID, http_current_worker(Port, ID), Workers),
	  group_pairs_by_key(Workers, Servers)
	},
	html([ table([ class(block)
		     ],
		     [ \servers_stats(Servers)
		     ])
	     ]).

servers_stats([]) --> [].
servers_stats([H|T]) -->
	server_stats(H), servers_stats(T).

:- if(catch(statistics(process_cputime, _),_,fail)).
cputime(CPU) :- statistics(process_cputime, CPU).
:- else.
cputime(CPU) :- statistics(cputime, CPU).
:- endif.

server_stats(Port-Workers) -->
	{ length(Workers, NWorkers),
	  http_server_property(Port, start_time(StartTime)),
	  format_time(string(ST), '%+', StartTime),
	  cputime(CPU)
	},
	html([ \server_stat('Port:', Port, odd),
	       \server_stat('Started:', ST, even),
	       \server_stat('Total CPU usage:', [\n('~2f',CPU), ' seconds'], odd),
	       \request_statistics,
	       \server_stat('# worker threads:', NWorkers, even),
	       tr(th(colspan(6), 'Statistics by worker')),
	       tr([ th('Thread'),
		    th('CPU'),
		    th('Local'),
		    th('Global'),
		    th('Trail'),
		    th('Limit')
		  ]),
	       \http_workers(Workers, odd)
	     ]).

server_stat(Name, Value, OE) -->
	html(tr(class(OE),
		[ th([class(p_name), colspan(4)], Name),
		  td([class(value),  colspan(4)], Value)
		])).


request_statistics -->
	{ cgi_statistics(requests(Count)),
	  cgi_statistics(bytes_sent(Sent))
	},
	server_stat('Requests processed:', \n(human, Count), even),
	server_stat('Bytes sent:', \n(human, Sent), odd).


http_workers([], _) -->
	[].
http_workers([H|T], OE) -->
	{ odd_even(OE, OE2) },
	http_worker(H, OE),
	http_workers(T, OE2).

http_worker(H, OE) -->
	{ current_prolog_flag(stack_limit, SL),
	  thread_statistics(H, localused, LU),
	  thread_statistics(H, globalused, GU),
	  thread_statistics(H, trailused, TU),
	  thread_statistics(H, cputime, CPU)
	},
	html([ tr(class(OE),
		  [ td(H),
		    \nc('~3f', CPU),
		    \nc(human, LU),
		    \nc(human, GU),
		    \nc(human, TU),
		    \nc(human, SL)
		  ])
	     ]).

odd_even(even, odd).
odd_even(odd, even).


		 /*******************************
		 *	      POOLS		*
		 *******************************/

%%	http_server_pool_table//
%
%	Display table with statistics on thread-pools.

http_server_pool_table -->
	{ findall(Pool, current_thread_pool(Pool), Pools),
	  sort(Pools, Sorted)
	},
	html(table([ id('http-server-pool'),
		     class(block)
		   ],
		   [ tr([th('Name'), th('Running'), th('Size'), th('Waiting'), th('Backlog')])
		   | \server_pools(Sorted, 1)
		   ])).

server_pools([], _) --> [].
server_pools([H|T], Row) -->
	odd_even_row(Row, Next, \server_pool(H)),
	server_pools(T, Next).

server_pool(Pool) -->
	{ findall(P, thread_pool_property(Pool, P), List),
	  memberchk(size(Size), List),
	  memberchk(running(Running), List),
	  memberchk(backlog(Waiting), List),
	  memberchk(options(Options), List),
	  option(backlog(MaxBackLog), Options, infinite)
	},
	html([ th(class(p_name), Pool),
	       \nc(human, Running),
	       \nc(human, Size),
	       \nc(human, Waiting),
	       \nc(human, MaxBackLog)
	     ]).


		 /*******************************
		 *	       BASICS		*
		 *******************************/

%%	n(+Format, +Value)//
%
%	HTML component to emit a number.
%
%	@see nc//2 for details.

n(Fmt, Value) -->
	{ number_html(Fmt, Value, HTML) },
	html(HTML).

number_html(human, Value, HTML) :-
	integer(Value), !,
	human_count(Value, HTML).
number_html(Fmt, Value, HTML) :-
	number(Value), !,
	HTML = Fmt-[Value].
number_html(_, Value, '~p'-[Value]).


human_count(Number, HTML) :-
	Number < 1024, !,
	HTML = '~d'-[Number].
human_count(Number, HTML) :-
	Number < 1024*1024, !,
	KB is Number/1024,
	digits(KB, N),
	HTML = '~*fK'-[N, KB].
human_count(Number, HTML) :-
	Number < 1024*1024*1024, !,
	MB is Number/(1024*1024),
	digits(MB, N),
	HTML = '~*fM'-[N, MB].
human_count(Number, HTML) :-
	TB is Number/(1024*1024*1024),
	digits(TB, N),
	HTML = '~*fG'-[N, TB].

digits(Count, N) :-
	(   Count < 100
	->  N = 1
	;   N = 0
	).


%%	nc(+Format, +Value)// is det.
%%	nc(+Format, +Value, +Options)// is det.
%
%	Numeric  cell.  The  value  is    formatted   using  Format  and
%	right-aligned in a table cell (td).
%
%	@param	Format is a (numeric) format as described by format/2 or
%		the constant =human=.  _Human_ formatting applies to
%		integers and prints then in abreviated (K,M,T) form,
%		e.g., 4.5M for 4.5 million.
%	@param	Options is passed as attributed to the =td= element.
%		Default alignment is =right=.

nc(Fmt, Value) -->
	nc(Fmt, Value, []).

nc(Fmt, Value, Options) -->
	{ class(Value, Class),
	  merge_options(Options,
			[ align(right),
			  class(Class)
			], Opts),
	  number_html(Fmt, Value, HTML)
	},
	html(td(Opts, HTML)).

class(Value, Class) :-
	(   integer(Value)
	->  Class = int
	;   float(Value)
	->  Class = float
	;   Class = value
	).

%%	odd_even_row(+Row, -Next, :Content)//
%
%	Create odd/even alternating table rows from a DCG.

odd_even_row(Row, Next, Content) -->
	{ (   Row mod 2 =:= 0
	  ->  Class = even
	  ;   Class = odd
	  ),
	  Next is Row+1
	},
	html(tr(class(Class), Content)).

%%	list_file_streams(+Request)
%
%	Print a table of open streams that have an associated file name.

list_file_streams(_Request) :-
	findall(S, stream_property(S, type(_)), Streams),
	reply_html_page(
	    title('Server open streams'),
	    [ \html_requires(css('stats.css')),
	      h1(class(wiki), 'Server open streams'),
	      table(class(block),
		    [ tr([ th('No'),
			   th('Stream'),
			   th('Handle'),
			   th('I/O'),
			   th('File name')
			 ])
		    | \list_streams(Streams, 1)
		    ])
	    ]).

list_streams([], _) -->
	[].
list_streams([H|T], N) -->
	html(tr([\nc('~d', N)|\stream(H)])),
	{ N2 is N + 1 },
	list_streams(T, N2).

stream(S) -->
	{ format(atom(Id), '~p', [S]),
	  http_link_to_id(stream_details, [stream(Id)], HREF)
	},
	html(td(a(href(HREF), Id))),
	stream_prop(S, file_no),
	stream_io(S),
	stream_prop(S, file_name).

stream_io(S) -->
	(   { catch((stream_property(S, input),Val=input),
		    _, Val=closed)
	    }
	->  html(td(Val))
	;   html(td(output))
	).

stream_prop(S, Prop) -->
	(   { Term =.. [Prop,Val],
	      catch(stream_property(S, Term),_,fail)
	    }
	->  html(td('~p'-[Val]))
	;   html(td(-))
	).


%%	stream_details(+Request)
%
%	Print details on stream. Requires  user   to  be  logged on with
%	admin right because streams may reveal sensitive information.

stream_details(Request) :-
	site_user_logged_in(User),
	site_user_property(User, granted(admin)), !,
	http_parameters(Request,
			[ stream(Name, [])
			]),
	with_output_to(string(S), stream_info(Name)),
	reply_html_page(
	    title('Details for stream'),
	    [ \html_requires(css('stats.css')),
	      pre(S)
	    ]).
stream_details(Request) :-
	option(path(Path), Request),
	throw(http_reply(forbidden(Path))).


%%	server_health(+Request)
%
%	HTTP handler that replies with the overall health of the server

server_health(_Request) :-
	get_server_health(Health),
	reply_json(Health).

get_server_health(Health) :-
	findall(Key-Value, health(Key, Value), Pairs),
	dict_pairs(Health, health, Pairs).

health(up, true).
health(uptime, Time) :-
	get_time(Now),
	(   http_server_property(_, start_time(StartTime))
	->  Time is round(Now - StartTime)
	).
health(requests, RequestCount) :-
	cgi_statistics(requests(RequestCount)).
health(bytes_sent, BytesSent) :-
	cgi_statistics(bytes_sent(BytesSent)).
health(open_files, Streams) :-
	aggregate_all(count, N, stream_property(_, file_no(N)), Streams).
health(loadavg, LoadAVG) :-
	catch(setup_call_cleanup(
		  open('/proc/loadavg', read, In),
		  read_string(In, _, String),
		  close(In)),
	      _, fail),
	split_string(String, " ", " ", [One,Five,Fifteen|_]),
	maplist(number_string, LoadAVG, [One,Five,Fifteen]).
health(dir_scan_time, Time) :-
	get_time(T0),
	expand_file_name(*, _),
	get_time(T),
	Time is T - T0.
:- if(current_predicate(malloc_property/1)).
health(heap, json{inuse:InUse, size:Size}) :-
	malloc_property('generic.current_allocated_bytes'(InUse)),
	malloc_property('generic.heap_size'(Size)).
:- endif.

start_debugger(_Request) :-
	site_user_logged_in(User),
	site_user_property(User, granted(admin)), !,
	call_showing_messages(
	    prolog_server(4242, []),
	    [ head(title('SWI-Prolog -- Starting debugger'))]).

