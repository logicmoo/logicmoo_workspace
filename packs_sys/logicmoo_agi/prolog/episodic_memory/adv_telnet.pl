/*
% NomicMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
% Bits and pieces:
%
% LogicMOO, Inform7, FROLOG, Guncho, PrologMUD and Marty's Prolog Adventure Prototype
%
% Copyright (C) 2004 Marty White under the GNU GPL
% Sept 20, 1999 - Douglas Miles
% July 10, 1996 - John Eikenberry
%
% Logicmoo Project changes:
%
% Main file.
%
*/
:- '$set_source_module'(mu).

:- dynamic(mu_tmp:no_autostart/0).
:- volatile(mu_tmp:no_autostart/0).

:- dynamic(mu_global:wants_quit/3).

:- dynamic(mu_global:console_tokens/2).
:- volatile(mu_global:console_tokens/2).

:- dynamic(mu_global:console_io_conn_history/7).
:- dynamic(mu_global:console_io_player/3).
:- volatile(mu_global:console_io_player/3).
:- volatile(mu_global:console_io_conn_history/7).

:- use_module(library(socket)).

adv_server(Port) :-
 dbug1(adv_server(Port)),
 tcp_socket(ServerSocket),
 tcp_setopt(ServerSocket, reuseaddr),
 tcp_bind(ServerSocket, Port),
 tcp_listen(ServerSocket, 5),
 atom_concat('mu_', Port, Alias),
 thread_create(adv_server_loop(Port, ServerSocket), _,
   [ alias(Alias)
   ]).

address_to_host(Peer,Host):- % exception(error(socket_error(host_not_found,_),_))
  notrace(catch(tcp_host_to_address(Host, Peer),_,fail)),!.
address_to_host(Peer,Term):- Peer=..List,atomic_list_concat(List,'-',Term),!.
address_to_host(Term,Term):- !.

peer_alias(Prefix, Peer, Host, Alias):-
 notrace(catch(address_to_host(Peer,Host),_,Peer=Host)),
 format(string(S), '~w@~w_', [Host, Prefix]),
 gensym(S, Alias), !.

adv_server_loop(Prefix, ServerSocket):-
 setup_call_cleanup(
      asserta(mu_tmp:no_autostart),
      once(must(adv_server_loop_1(Prefix, ServerSocket))),
      retractall(mu_tmp:no_autostart)).

adv_server_loop_1(Prefix, ServerSocket) :-
 tcp_accept(ServerSocket, Slave, Peer),
 tcp_open_socket(Slave, InStream, OutStream),
 % set_stream(InStream, buffer(false)),
 set_stream(InStream, close_on_exec(true)),
 set_stream(OutStream, close_on_exec(true)),
 set_stream(InStream, close_on_abort(true)),
 set_stream(OutStream, close_on_abort(true)),
 peer_alias(Prefix, Peer, Host, Alias),
 ignore(catch(thread_create(
  adv_serve_client(InStream, OutStream, Host, Peer, Alias),
  _,
  [ alias(Alias)
  ]),
  error(permission_error(create, thread, Alias), _),
  fail)),
 !,
 adv_server_loop(Prefix, ServerSocket).

setup_IO_props(InStream, OutStream):-
 set_stream(InStream, tty(true)),
 set_stream(OutStream, tty(true)),
 % set_prolog_flag(tty_control, false), % JanW
 % set_prolog_flag(tty_control, true), % Dmiles
 current_prolog_flag(encoding, Enc),
 set_stream(user_input, encoding(Enc)),
 %set_stream(user_input, buffer(false)),
 set_stream(user_output, encoding(Enc)),
 %set_stream(user_error, encoding(Enc)),
 set_stream(user_input, newline(detect)),
 set_stream(user_output, newline(dos)),
 set_stream(user_input, eof_action(eof_code)), !.


copy_IO_props(InStream, OutStream):- 
 stream_property(UserIn,file_no(0)),
 copy_stream_props(UserIn,InStream),
 stream_property(UserOut,file_no(1)),
 copy_stream_props(UserOut,OutStream),!.

copy_stream_props(From,To):-
 forall(member(Prop,[tty,timeout,representation_errors,record_position,newline,locale,
   eof_action,encoding,close_on_exec,close_on_abort,buffer_size,buffer]),
   copy_stream_prop(From,Prop,To)).
copy_stream_prop(From,Prop,To):- functor(SP,Prop,1),
  ignore((stream_property(From,SP), notrace(catch(set_stream(To,SP),_,true)))).


adv_serve_client(InStream, OutStream, Host, Peer, Alias) :-
 !,
 nodebug, set_prolog_flag(gui_tracer, false),
 thread_self(Id),

 set_prolog_IO(InStream, OutStream, OutStream),
 % set_stream(user_error, newline(dos)),
 set_stream(OutStream, alias(user_error)),
 set_stream(OutStream, alias(user_output)),

 setup_IO_props(InStream, OutStream),

 % set_stream(user_input, eof_action(reset)),
 set_stream(user_input, close_on_exec(false)),
 set_stream(user_input, close_on_abort(false)),
 set_stream(user_output, close_on_exec(false)),
 set_stream(user_output, close_on_abort(false)),

 format(OutStream,
  'Welcome to the SWI-Prolog Adventure Server!~n~q~n~n', 
  [adv_serve_client(Id, Alias, InStream, OutStream, Host, Peer)]), !,
 call_cleanup(srv_catch(adventure_client_process(Id, Alias, InStream, OutStream, Host, Peer)),
   adventure_client_cleanp(Id, Alias, InStream, OutStream)).

srv_catch(Goal):- catch(once(call(call, Goal)), E, ((notrace(dbug1(error_srv_catch(E, Goal))), !, fail))).
ignore_srv_catch(Goal):- ignore(srv_catch(Goal)).
tflush(OutStream):- ignore_srv_catch((flush_output(OutStream), ttyflush)).

adventure_client_cleanp(Id, Alias, InStream, OutStream):-
 ignore(notice_agent_discon(Id, Alias, InStream, OutStream)),
 retractall(mu_global:console_io_player(_, OutStream, _)),
 retractall(mu_global:console_io_player(InStream, _, _)),
 retractall(mu_global:wants_quit( Id, _, _)),
 retractall(mu_global:wants_quit( _, _, _)), % might coause a bug later
 ignore_srv_catch(close(InStream)),
 ignore_srv_catch(close(OutStream)),
 ignore_srv_catch(thread_detach(Id)).

notice_agent_discon(Id, Alias, InStream, OutStream):-
  once((
    mu_global:console_io_player(InStream, _, Agent);
    mu_global:console_io_player(_, OutStream, Agent);
    (mu_global:console_io_conn_history(_, Alias, _, _, _, _, Agent), \+ mu_global:console_io_player(_, _, Agent));
    (mu_global:console_io_conn_history(Id, _, _, _, _, _, Agent), \+ mu_global:console_io_player(_, _, Agent)))),
 assertz(mu_global:agent_discon(Agent)),
 dbug1((mu_global:agent_discon(Agent))), !.


:- dynamic(mu_global:peer_character/2).
:- dynamic(mu_global:peer_agent/2).
:- dynamic(mu_global:agent_character/2).
:- dynamic(mu_global:agent_discon/1).

guess_previous_agent_0(_, Peer, Agent):- mu_global:peer_agent(Peer, Agent), !.
guess_previous_agent_0(Host, _, Agent):- mu_global:peer_agent(Host, Agent), !.

guess_previous_agent(Host, Peer, Agent):- guess_previous_agent_0(Host, Peer, Agent),
 \+ mu_global:console_io_player(_, _, Agent).

guess_previous_agent(_Host, _Peer, Agent):- gensym('telnet~', Agent).

prompt_for_agent(Id, Alias, InStream, OutStream, Host, Peer, Agent, Name):-
 guess_previous_agent(Host, Peer, Agent),
 ignore(mu_global:agent_character(Agent, Name)),
 ignore(mu_global:peer_character(Peer, Name)),
 ignore(mu_global:peer_character(Host, Name)),
 (var(Name) -> format(OutStream, 'Enter your name [or leave bank for "~w"]: ', [Agent]), read_line_to_string(InStream, Name) ; true),
 accept_agent(Id, Alias, InStream, OutStream, Host, Peer, Agent, Name).

aaifn(A):- clause(A, true)->true;asserta(A).

:- dynamic(mu_temp:needs_agent_conn/4).
:- volatile(mu_temp:needs_agent_conn/4).

accept_agent(Id, Alias, InStream, OutStream, Host, Peer, Agent, Name):-
 aaifn(mu_global:agent_character(Agent, Name)),
 aaifn(mu_global:peer_character(Peer, Name)),
 aaifn(mu_global:peer_character(Host, Name)),
 aaifn(mu_global:peer_agent(Peer, Agent)),
 aaifn(mu_global:peer_agent(Host, Agent)),
 %set_stream(user_output, alias(Agent)),
 asserta(mu_global:console_io_conn_history(Id, Alias, InStream, OutStream, Host, Peer, Agent)),

   retractall(mu_global:console_io_player(InStream, _, _)),
   retractall(mu_global:console_io_player(_, OutStream, _)),
   retractall(mu_global:console_io_player(_, _, Agent)),

   retractall(mu_global:console_tokens(Agent, _)), 
   retractall(mu_global:wants_quit( _, _, Agent)),
   retractall(mu_global:agent_discon(Agent)),
   asserta(mu_global:console_io_player(InStream, OutStream, Agent)),
   clear_already_consumed_input(Agent),
 assertz(mu_temp:needs_agent_conn(Agent, Name, Alias, adventure_client_process(Id, Alias, InStream, OutStream, Host, Peer))), !.

welcome_adv_tnet(OutStream):-
  format(OutStream, '==============================================~n', []),
  format(OutStream, 'Welcome to Marty\'s Prolog Adventure Prototype~n', []),
  format(OutStream, '==============================================~n', []),
  !.

adventure_client_process(Id, Alias, InStream, OutStream, Host, Peer):-
 prompt_for_agent(Id, Alias, InStream, OutStream, Host, Peer, Agent, _Name),
 retractall(mu_global:wants_quit(_, InStream, _)),
 retractall(mu_global:wants_quit(Id, _, _)),
 welcome_adv_tnet(OutStream),
 overwrote_prompt(Agent),
 setup_console,
 repeat,
 mu_global:console_io_player(InStream, OutStream, CurrentAgent),
 adv_tlnet_readloop(Id, InStream, OutStream, CurrentAgent),
 needs_logout_p(Id, InStream, CurrentAgent, Why),
 wdmsg(needs_logout_p(Id, InStream, CurrentAgent, Why)), !.
 

needs_logout_p(_, InStream, _Agent, no_input_stream):-
  \+ mu_global:console_io_player(InStream, _, _).
needs_logout_p(_, InStream, Agent, agent_wants_quit(Id, IS, Agent)):-
  mu_global:wants_quit(Id, IS, Agent),
  \+ (( mu_global:console_io_player(InStream, _, Other), Other\==Agent)),
  mu_global:wants_quit(Id, IS, Agent),!.
needs_logout_p(Id, _InStream, _Agent, id_wants_quit(Id, IS, WasAgent)):- 
  mu_global:wants_quit(Id, IS, WasAgent),!.
needs_logout_p(_Id, InStream, _Agent, instream_wants_quit(WasId, InStream, WasAgent)):- 
  mu_global:wants_quit(WasId, InStream, WasAgent),!.


adv_tlnet_readloop(Id, InStream, _OutStream, Agent):-
 needs_logout_p(Id, InStream, Agent, Why),
 wdmsg(Why),
 sleep(0.1), !.
adv_tlnet_readloop(_Id, _InStream, OutStream, Agent):-
 tflush(OutStream),
 fail,
 mu_global:console_tokens(Agent, _Words),
 tflush(OutStream),
 sleep(0.1), !.
adv_tlnet_readloop(_Id, InStream, OutStream, Agent):-
 ensure_has_prompt(Agent),
 wait_for_input([InStream], Found, 2.0),
 Found==[], !,
 tflush(OutStream).
adv_tlnet_readloop(Id, InStream, OutStream, Agent):-
 read_line_to_tokens(Agent, InStream, [], Words),
 tflush(OutStream),
 adv_tlnet_words(Id, InStream, Agent, Words), !.


adv_tlnet_words(Id, InStream, Agent, Words0):-
  exclude(=(' '), Words0, Words), Words0\==Words, !,
  adv_tlnet_words(Id, InStream, Agent, Words).
adv_tlnet_words(Id, InStream, Agent, []):-
  adv_tlnet_words( Id, InStream, Agent, [wait]).

adv_tlnet_words(Id, InStream, Agent, [Quit]):- Quit == quit,
  adv_tlnet_words(Id, InStream, Agent, end_of_file).

adv_tlnet_words(Id, InStream, Agent, end_of_file):- fail, 
  asserta(mu_global:wants_quit(Id, InStream, Agent)),
  dbug(always, '~q~n', [end_of_file -> mu_global:wants_quit(Id, InStream, Agent)]), !.

adv_tlnet_words(_Id, _InStream, _Agent, [prolog]):- !, prolog.
adv_tlnet_words(Id, InStream, Agent, Words):-
  assertz(mu_global:console_tokens(Agent, Words)),
  nop((dbug(telnet, '~q~n', [adv_tlnet_words(Id, InStream, Agent, Words)]))),
 % nop((mu_global:console_io_player(InStream, OutStream, Agent), format(OutStream, '~NYou: ~q~n', [mu_global:console_tokens(Agent, Words)]))),
 !.

