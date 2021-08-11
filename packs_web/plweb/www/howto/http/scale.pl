:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(thread_pool)).
:- use_module(library(error)).
:- use_module(library(debug)).

% Allow for maximal 5 computation jobs

:- thread_pool_create(compute, 5, []).

http:location(compute, root(compute), []).

% Declare the handlers. We locate all   our  computations below /compute
% and setup a _prefix_  handler  to   specify  default  options  for all
% compute-handlers.

:- http_handler(root(.),	 say_hi, []).
:- http_handler(compute(.),      undef,  [ prefix,
					   time_limit(infinite),
					   spawn(compute)
					 ]).
:- http_handler(compute(pi),	 pi,	 []).
:- http_handler(compute(ite_pi), ite_pi, []).

%%	server(?Port) is det.
%
%	Start the server.

server(Port) :-
	http_server(http_dispatch,
		    [ port(Port),
		      workers(10)
		    ]).

%%	say_hi(+Request)
%
%	The welcome page of our server.

say_hi(_Request) :-
	http_link_to_id(pi, [], PIRef),
	http_link_to_id(pi, [], PIRef),
	reply_html_page(title('Hello World'),
			[ h1('Hello World'),
			  p(['This example demonstrates generating HTML ',
			     'messages from Prolog'
			    ]),
			  ul([ li(\link_pred(pi,     'Compute PI')),
			       li(\link_pred(ite_pi, 'Iterative computation'))
			     ])
			]).

link_pred(Pred, Label) -->
	{ http_link_to_id(Pred, [], HREF)
	},
	html(a(href(HREF), Label)).

%%	undef(+Request)
%
%	Implementation of the prefix handler. This  will be called if no
%	specific handler matches. It shows how   we can program handling
%	of non-existing pages.

undef(Request) :-
	memberchk(path(Path), Request),
	existence_error(http_location, Path).

%%	pi(+Request)
%
%	Implementation of /pi?i=N; computing PI   with N iterations. The
%	parameter =i= is  demanded  to  be   an  integer  using  1000 as
%	default. The call to debug/3 shows  tracing messages that can be
%	activated using =|?- debug(pi).|=

pi(Request) :-
	http_parameters(Request,
			[ i(I, [integer, default(1000)])
			]),
	thread_self(Me),
	debug(pi, 'Computing ~D iterations', [I]),
	thread_statistics(Me, cputime, T0),
	pi(I, PI),
	thread_statistics(Me, cputime, T1),
	T is T1-T0,
	http_link_to_id(pi, [], HREF),
	reply_html_page(title('Approximating PI'),
			[ h1('Approximating PI'),
			  form(action(HREF),
			       [ p([ 'The Gregory-Leibniz approximation of PI ',
				     'after ', input([value(I),name(i)]),
				     input([type(submit), value('iterations')]),
				     ' is ~20f (used ~2f seconds CPU)'-[PI, T]
				   ])
			       ])

			]).

%%	ite_pi(+Request)
%
%	Compute PI with increasing precision.   This  handler shows that
%	output can be send to the   client incrementally using _chunked_
%	encoding.

ite_pi(_Request) :-
	format('Content-type: text/plain~n'),
	format('Transfer-encoding: chunked~n~n'),
	format('Iterating PI~n~n'),
	format('~w~t~w~20| ~w~n', ['Iterations', 'Sec', 'Approximation']),
	format('~`=t~43|~n'),
	thread_self(Me),
	forall(between(1, 20, I),
	       (   K is 2**I,
		   format('~D', [K]), flush_output,
		   thread_statistics(Me, cputime, T0),
		   pi(K, PI),
		   thread_statistics(Me, cputime, T1),
		   T is T1-T0,
		   format('~t~2f~20| ~20f~n', [T, PI])
	       )).


%%	pi(+Iterations, -Approximation)
%
%	Compute _pi_ using the simple basic Gregory-Leibniz series. This
%	is the slowest way to compute pi,   which  statisfies our aim of
%	stressing the server with computationally hard jobs.

pi(N, Pi) :-
	pi4(0, N, 0, Pi4),
	Pi is Pi4*4.

pi4(N, N, Pi4, Pi4) :- !.
pi4(K, N, Pi40, Pi4) :-
	Pi41 is Pi40 + ((-1)**K) rdiv (2*K+1),
	K1 is K + 1,
	pi4(K1, N, Pi41, Pi4).
