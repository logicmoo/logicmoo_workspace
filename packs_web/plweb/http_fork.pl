:- module(http_fork,
	  [ forked_server/2		% +Port, +Options
	  ]).
:- use_module(library(unix)).
:- use_module(library(socket)).
:- use_module(library(option)).
:- use_module(library(debug)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

/** <module> Manage HTTP servers using forking

This module is designed to create  robust Prolog-based HTTP servers. The
main Prolog process loads the program and   preforks  _N_ times, each of
the childs runs a normal multi-threaded HTTP   server.  If a child dies,
the main server forks again to create a new server.

@compat This library is limited to systems providing a proper
	copy-on-write implementation of the POSIX fork() system call.
	In practice, these are Unix versions (including Linux, MacOSX, etc.
	but excluding Windows).

@tbd	The current implementation does not support session-management
	if multiple servers are preforked because sessions may `hop'
	between servers.  This could be fixed using TIPC to establish
	a distributed communication service between the cooperating
	clients.

@tbd	Deal with logging of the clients.  Apache does so using a log
	process.  The main server creates a pipe.  The input is used
	by each client for writing the log.  The output is processed
	by the log process.  This process merely combines all terms
	and writes them to the logfile.  Note that we cannot use
	threads because fork() doesn't play well with threads, so
	the main server must remain single-threaded.

@tbd	At a next step, the threaded  HTTP   server  must be extended
	to support scheduled graceful suicide. By gracefully  comitting
	suicide, the server can avoid suffering too much from memory leaks.
*/


%%	forked_server(+Port, +Options)
%
%	Similar to http_server/2 from   library(http/thread_httpd),  but
%	the main process starts  and  monitors   a  pool  of  processes.
%	Additional options processed:
%
%	    * prefork(+Count)
%	    The number of servers to fork (default 1)
%	    * init(:Goal)
%	    Goal to run in a new server after the fork and before
%	    starting the HTTP server.

forked_server(Port, Options) :-
	tcp_socket(Socket),
	tcp_setopt(Socket, reuseaddr),
	tcp_bind(Socket, Port),
	tcp_listen(Socket, 5),
	thread_httpd:make_addr_atom('httpd@', Port, Queue),
	prefork_servers([ queue(Queue),
			  tcp_socket(Socket),
			  port(Port)
			| Options
			]).

prefork_servers(Options) :-
	option(prefork(Count), Options, 1),
	prefork_servers(Count, Options, PIDS),
	monitor_servers(PIDS, Options).

prefork_servers(Count, Options, [PID|PIDS]) :-
	Count > 0, !,
	prefork_server(Options, PID),
	Count2 is Count - 1,
	prefork_servers(Count2, Options, PIDS).
prefork_servers(_, _, []).

prefork_server(Options, PID) :-
	fork(PID),
	(   PID == child
	->  option(goal(Goal), Options, http_dispatch),
	    option(init(Init), Options, true),
	    call(Init),
	    http_server(Goal, Options),
	    forall(thread_get_message(Goal),
		   Goal)
	;   debug(http(fork), 'Preforked server (PID=~d)', [PID])
	).

:- multifile
	thread_httpd:make_socket_hook/3.

thread_httpd:make_socket_hook(_Port, Options, Options) :-
	memberchk(tcp_socket(_), Options), !.

monitor_servers(PIDS, Options) :-
	wait(PID, Status),
	debug(http(fork), 'Forked server ~d died with status ~p',
	      [PID, Status]),
	delete(PIDS, PID, Rest),
	prefork_server(Options, New),
	monitor_servers([New|Rest], Options).
