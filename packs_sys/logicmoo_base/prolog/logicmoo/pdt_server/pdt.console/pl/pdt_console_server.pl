/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

/* NOTE: This file contains third-party code!

   Most of this file was borrowed from the swi-prolog library 
   prolog_server. Many thanks to the original authors for making their 
   work available to the public. 
   
   I changed the following things:
   1) added a way to gracefully stop the console server accept loop
   2) changed the naming policy for thread alias names, so that more
      than one client may connect from the same host.
   
   The copyright header of the original file 
   follows.
   	--lu
*/

/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker & Steve Prior
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/
   
   

:- module(pdt_console_server,[
	pdt_current_console_server/1,
	pdt_start_console_server/2,
	pdt_stop_console_server/0,
	console_thread_name/1
]).
:- use_module(library(socket)).
:- use_module(library(lists)).
 

console_thread_name(Name) :-
	console_thread_name__(Name).

:- dynamic(console_thread_name__/1).

:- dynamic(pdt_console_client_id/1).

reset_pdt_console_client_id :-
	retractall(pdt_console_client_id(_)),
	assertz(pdt_console_client_id(0)).

next_pdt_console_client_id(Id) :-
	retract(pdt_console_client_id(Id)),
	!,
	Next is Id + 1,
	asserta(pdt_console_client_id(Next)).

prolog_server(Port, Name, Options) :-
	tcp_socket(ServerSocket),
	tcp_setopt(ServerSocket, reuseaddr),
	tcp_bind(ServerSocket, Port),
	tcp_listen(ServerSocket, 5),
	thread_create(server_loop(ServerSocket, Name, Options), _,
		      [ alias(pdt_console_server)
		      ]).
 
server_loop(ServerSocket, Name, Options):-
    server_loop_impl(ServerSocket, Name, Options),
    thread_exit(0).
server_loop_impl(ServerSocket, Name, Options) :-
	tcp_accept(ServerSocket, Slave, Peer),
	server_loop_impl_X(ServerSocket,Name,Options,Slave,Peer).

server_loop_impl_X(ServerSocket,_,_,Slave,_) :-
	recorded(pdt_console_server_flag,shutdown,Ref),
	!,
	erase(Ref),
	% the accepted connection is just a "wakeup call" we can savely discard it.
    tcp_close_socket(Slave),
    % that's it, we are closing down business.
    tcp_close_socket(ServerSocket).	
server_loop_impl_X(ServerSocket,Name,Options,Slave,Peer):-	
	tcp_open_socket(Slave, InStream, OutStream),
	set_stream(InStream,encoding(utf8)),
    set_stream(OutStream,encoding(utf8)),
%	tcp_host_to_address(Host, Peer),
    next_pdt_console_client_id(Id),
	atomic_list_concat(['pdt_console_client_',Id,'_',Name],Alias),
	thread_create(service_client(InStream, OutStream, Peer, Options),
		      ID,
		      [ alias(Alias)
		      ]),
	retractall(console_thread_name__(_)),
	assertz(console_thread_name__(ID)),
	server_loop_impl(ServerSocket, Name, Options).
 
service_client(InStream, OutStream, Peer, Options) :-
	allow(Peer, Options), !,
	thread_self(Id),
	set_prolog_IO(InStream, OutStream, OutStream),
    set_stream(user_error,encoding(utf8)),
    catch(set_prolog_flag(color_term, false), _, true),
	format(user_error,
	       'Welcome to the SWI-Prolog server on thread ~w~n~n',
	       [Id]),
	run_prolog,
	close(InStream),
	close(OutStream),
	thread_detach(Id).
service_client(InStream, OutStream, _, _):-
	thread_self(Id),
	format(OutStream, 'Go away!!~n', []),
	close(InStream),
	close(OutStream),
	thread_detach(Id).


run_prolog :-
	catch(prolog, E,
	      ( print_message(error, E),
%		E = error(_, _),
		run_prolog)).


allow(Peer, Options) :-
	(   member(allow(Allow), Options)
	*-> Peer = Allow,
	    !
	;   Peer = ip(127,0,0,1)
	).

% TODO make this dependency explicit!
%:- use_module(library('org/cs3/pdt/runtime/consult_server')).

% server(-Port)
%
% used internally to store information about running servers
:- dynamic(server/1).

%:- initialization(mutex_create(pdt_console_server_mux)).
%:- at_halt(mutex_destroy(pdt_console_server_mux)).

% pdt_current_console_server(-Port, -LockFile).
% retrieve information about running servers
pdt_current_console_server(Port) :-
    with_mutex(pdt_console_server_mux,
	    server(Port)
    ).
    

% pdt_start_console_server(?TCPPort)
% starts a new console server.
% UDPPort is used for sending back a sync when the server is up.
pdt_start_console_server(Port, Name) :-
    with_mutex(pdt_console_server_mux,
    	start_server(Port, Name)
    ).

% pdt_stop_console_server(+LockFile)
% stops the console server, removing the Lockfile.
pdt_stop_console_server:-
    with_mutex(pdt_console_server_mux,
    	stop_server
    ).

:- multifile(consult_server:process_shutdown_hook/0).
:- dynamic(consult_server:process_shutdown_hook/0).
consult_server:process_shutdown_hook:-
    pdt_stop_console_server.

start_server(Port, Name) :-
    \+ thread_property(_, alias(pdt_console_server)),
    reset_pdt_console_client_id,
    prolog_server(Port, Name, []),
    assertz(server(Port)).

stop_server :-
	server(Port),
	!,
	do_stop_server(Port).
stop_server.

do_stop_server(Port) :-
	recordz(pdt_console_server_flag, shutdown, _),
	tcp_socket(Socket),
	tcp_connect(Socket, localhost:Port),
	tcp_close_socket(Socket),
	retractall(server(Port)).


