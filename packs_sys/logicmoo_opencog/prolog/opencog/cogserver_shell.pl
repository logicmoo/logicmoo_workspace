#!/usr/bin/env swipl
/*
 * LOGICMOO CogServer Socket Server
 *
 * Copyright (c) 2022 Logicmoo Co <support@logicmoo.org>
 *
 * LICENSE:
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

:- module(cogserver_shell, [start_cogserver/0,run_cogshell/0]).

:- reexport(library(opencog/atomspace)).

:- ensure_loaded(library(logicmoo_cogserver)).

start_cogserver(Call,Port,Description):- PortNum is Port,
  must(prolog_cogserver(PortNum, [allow(_),call(Call),description(Description)])),!.

prolog_cogserver(Port, Options):-  
 \+ member(alias(_),Options),
 option(call(Call),Options,cogshell_telnet),
 atomic_list_concat([Call,'_',Port],Alias),!, 
 prolog_cogserver(Port, [alias(Alias)|Options]).

prolog_cogserver(_Port, Options) :- 
  member(alias(Alias),Options),thread_property(Base, status(running)),Base==Alias,!.

prolog_cogserver(Port, Options) :-
    tcp_socket(ServerSocket),
    tcp_setopt(ServerSocket, reuseaddr),
    tcp_bind(ServerSocket, Port),
    tcp_listen(ServerSocket, 5),
    option(alias(Alias),Options,prolog_cogserver),
    option(description(Desc),Options,Alias),
    dmsg(Port=Desc),
    serv_cogserver_in_thread(ServerSocket, Options, Alias).

serv_cogserver_in_thread(ServerSocket, Options, Alias):- fail,
    thread_create(cogserver_loop(ServerSocket, Options), _,
                  [ alias(Alias)
                  ]),!.
serv_cogserver_in_thread(ServerSocket, Options, _Alias):-  ignore(catch(cogserver_loop(ServerSocket, Options),E,wdmsg(E))).

peer_to_host(Peer,Host):- catch(tcp_host_to_address(Host, Peer),_,fail),!.
peer_to_host(Peer,Host):- atom(Peer),Peer=Host,!.
peer_to_host(Peer,Host):- compound(Peer),catch((Peer=..PeerL,atomic_list_concat(PeerL,'.',Host)),_,fail),!.
peer_to_host(Peer,Host):- term_to_atom(Peer,Host),!.

set_stream_carelessly(X,Y):- ignore(catch(set_stream(X,Y),_,fail)).

cogserver_loop(ServerSocket, Options) :-
    tcp_accept(ServerSocket, PeerSock, Peer),
    tcp_open_socket(PeerSock, In, Out),
  set_stream_carelessly(In, close_on_abort(false)),
  set_stream_carelessly(Out, close_on_abort(false)),
  set_stream_carelessly(In, close_on_exec(true)),
  set_stream_carelessly(Out, close_on_exec(true)),
    peer_to_host(Peer,Host),
    gensym(inst_,Num),
    option(alias(ServerAlias),Options,prolog_cogserver),
    atomic_list_concat(['peer_',Host,'_',Num, '@', ServerAlias], Alias),
    catch(thread_create(
              call_service_cogshell_peer(Host, Alias, PeerSock, In, Out, Peer, Options),
              _,
              [ alias(Alias),detached(true)
              ]),
          error(permission_error(create, thread, Alias), _),
          fail),
    !,
    cogserver_loop(ServerSocket, Options).


call_service_cogshell_peer(Host, Alias, PeerSock, In, Out, Peer, Options):-
  call(call,service_cogshell_peer(Host, Alias, PeerSock, In, Out, Peer, Options)).


service_cogshell_peer(Host,Alias,PeerSock,In,Out,Peer,Options) :-
    stream_property(Main_error, file_no(2)),
    option(allow(PeerAllow),Options,ip(127,0,0,1))-> PeerAllow=Peer,
    !,
    thread_self(Id),
    set_prolog_flag(tty_control, true),
    set_prolog_IO(In, Out, Out),
    format(Main_error,'~N~n~q~n~n',[service_cogshell_peer_call(Call,Id,Alias,PeerSock,In,Out,Host,Peer,Options)]),
    format(user_error,
           'LogicMOO CogServerShell (~q) on thread ~w~n~n',
           [Call,Id]),
    option(call(Call), Options, prolog),
    call_cleanup(Call,
                 ( close(In),
                   close(Out),
                   thread_detach(Id))),!.

service_cogshell_peer(Host,Alias,PeerSock,In,Out,Peer,Options) :-
    stream_property(Main_error, file_no(2)),
    option(allow(PeerAllow),Options,ip(127,0,0,1))-> PeerAllow=Peer,
    !,
    thread_self(Id),
    set_prolog_flag(tty_control, true),
    set_prolog_IO(In, Out, Out),    
    %set_stream_carelessly(user_input, tty(true)),
    % TODO figure out how to get immedate results
    % set_stream_carelessly(In, buffer_size(1)),
    set_stream_carelessly(user_output, tty(true)),
    set_stream_carelessly(user_input, close_on_abort(false)),
    set_stream_carelessly(user_output, close_on_abort(false)),
    set_stream_carelessly(user_input, close_on_exec(true)),
    set_stream_carelessly(user_output, close_on_exec(true)),
    set_stream_carelessly(user_error, close_on_exec(true)),
    set_stream_carelessly(user_output, buffer(false)),
    set_thread_error_stream(Id,user_error),
    current_prolog_flag(encoding, Enc),
    set_stream_carelessly(user_input, encoding(Enc)),
    set_stream_carelessly(user_output, encoding(Enc)),
    set_stream_carelessly(user_error, encoding(Enc)),
    set_stream_carelessly(user_input, newline(detect)),
    set_stream_carelessly(user_output, newline(dos)),
    set_stream_carelessly(user_error, newline(dos)),
    call(retractall,thread_util:has_console(Id, _, _, _)),
    thread_at_exit(call(retractall,thread_util:has_console(Id, _, _, _))),
    call(asserta,thread_util:has_console(Id, In, Out, Out)),
    option(call(Call), Options, prolog),
    format(Main_error,'~N~n~q~n~n',[service_cogshell_peer_call(Call,Id,Alias,PeerSock,In,Out,Host,Peer,Options)]),
    format(user_error,
           'LogicMOO CogServerShell (~q) on thread ~w~n~n',
           [Call,Id]),
    call_cleanup(Call,
                 ( close(In),
                   close(Out),
                   thread_detach(Id))).

service_cogshell_peer(Host,Alias,PeerSock,In,Out,Peer,Options):-
    thread_self(Id),option(call(Call), Options, prolog),
    format(main_error,'~N~n~q~n~n',[rejecting(Call,Id,Alias,PeerSock,In,Out,Host,Peer,Options)]),    
    format(Out, 'Bye!!~n', []),
    close(In),
    close(Out),
    thread_detach(Id).


make_peer_alias(Host,Alias):- thread_self(Prefix),make_peer_alias3(Prefix,Host,Alias).

make_peer_alias3(Prefix,Host,AliasH):- is_list(Host),must(atomic_list_concat([Prefix,'peer'| Host], '.', AliasH)),!.
make_peer_alias3(Prefix,Host,AliasH):- compound(Host),Host=..HostL,make_peer_alias3(Prefix,HostL,AliasH).
make_peer_alias3(Prefix,Host,AliasH):- term_to_atom(Host,AHost),must(atomic_list_concat([Prefix,'peer', AHost], '_', AliasH)).


call_close_and_detatch(In, Out, Id, Call):-
    call_cleanup(call(Call),( close_peer_connection(In, Out),ignore(thread_detach(Id)))).


close_peer_connection(In, Out) :-
        call(retractall,thread_util:has_console(_,In,Out,_)),
        ignore(catch(close(In, [force(true)]),_,true)),
        ignore(catch(close(Out, [force(true)]),_,true)).


port_busy_ocs(Port):-
    tcp_socket(ServerSocket),
    tcp_setopt(ServerSocket, reuseaddr),
    catch((tcp_bind(ServerSocket, Port),tcp_listen(ServerSocket, 5)),Error,true),    
    tcp_close_socket(ServerSocket),
    !,nonvar(Error).

:- dynamic(started_cogshell_telnet/1).
start_cogserver(Port):- \+ number(Port), atom_number(Port, Num),!,start_cogserver(Num).
start_cogserver(Port):- started_cogshell_telnet(Port),!,threads.
start_cogserver(Port):- port_busy_ocs(Port),!, NewPort is Port+100, start_cogserver(NewPort).
start_cogserver(Port):-      
      asserta(started_cogshell_telnet(Port)),
      start_cogserver(run_cogshell , Port  , "CogServerShell").

%run_cogshell:- current_predicate(guile/0),!,guile.
%run_cogshell:- current_predicate(lisp/0),!,lisp.

run_cogshell:- repeat, catch(run_cogshell_ex,_,fail).
run_cogshell_ex:- 
  writeln(';; LABS development version dropping you to prolog/0 shell'),
  ignore(prolog),close(user_input),close(user_output).

start_cogserver:-
   Port is 17001+4000,
   start_cogserver(Port),!.

start_cogserver_main:-   current_prolog_flag(argv,List), append(_,['--port', Port|_],List),
  start_cogserver(Port),!, run_cogshell.
start_cogserver_main:- start_cogserver, run_cogshell.


:- initialization(start_cogserver_main,main).
