end_of_file.

% :-module(mud_irc, [irc_mud_event_hook/3, deliver_to_irc/2 ]).
/*
module  Initial IRC/Text console 
% ALL IRC client MUD logic is here (removed from everywhere else!)
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

:- include(prologmud(mud_header)).


:- multifile baseKB:agent_action_queue/3.
:- dynamic baseKB:agent_action_queue/3.

:-ain_expanded(( prologDynamic(baseKB:irc_user_plays(tAgent,ftAtom,ftAtom)))).
:-ain_expanded(( prologOrdered(agent_action_queue(tAgent,ftTerm,ftTerm)))).

:-ain(( baseKB:deliver_event_hooks(Agent,Event):- fail,ignore(once(deliver_to_irc(Agent,Event))))).
baseKB:irc_event_hooks(Channel,User,Stuff):- fail,ignore(once(irc_mud_event_hook(Channel,User,Stuff))).

invite_to_mud(Nick):-eggdrop:to_egg('.tcl putserv "PRIVMSG ~w :DCC CHAT chat 73.37.100.94 4000"',[Nick]).

deliver_to_irc(_,actNotice(_,done(_,AL))):-actLook==AL.
deliver_to_irc(_,actNotice(_,begin(_,AL))):-actLook==AL.
deliver_to_irc(Agent,Event):-  baseKB:irc_user_plays(Agent,User,Channel), Channel=='##prolog',!,dmsg(deliver_to_irc(Agent,User,Channel,Event)),!.
deliver_to_irc(Agent,Event):-  baseKB:irc_user_plays(Agent,User,Channel) -> 
  eggdrop:say(Channel,[Agent,': ',Event]) ; nop(eggdrop:say(Agent,Event)).


irc_mud_event_hook(Channel,User,Stuff):- string(User),string_to_atom(User,AUser),!,irc_mud_event_hook(Channel,AUser,Stuff).
irc_mud_event_hook(Channel,User,Stuff):- string(Channel),string_to_atom(Channel,AChannel),!,irc_mud_event_hook(AChannel,User,Stuff).
irc_mud_event_hook(Channel,User,ctcp("ACTION", ACT)):-!,irc_mud_event_hook(Channel,User,actDo(ACT)).

irc_mud_event_hook(Channel,User,call('?-'(foc_current_agent(Agent)),_Vs)):- 
 foc_current_agent(Agent),
 get_session_id(ID),
 retractall(lmcache:agent_session(_,ID)),
 retractall(lmcache:session_agent(ID,_)),
 ain(baseKB:irc_user_plays(Agent,User,Channel)),
   asserta_if_new(lmcache:agent_session(Agent,ID)),
   asserta_if_new(lmcache:session_agent(ID,Agent)),!.

irc_mud_event_hook(Channel,User,actDo(TODO)):- baseKB:irc_user_plays(Agent,User,Channel),ground(baseKB:irc_user_plays(Agent,User,Channel)),irc_action_queue(Agent,TODO,Channel).
irc_mud_event_hook(Channel,User,say(TODO)):- baseKB:irc_user_plays(Agent,User,Channel),ground(baseKB:irc_user_plays(Agent,User,Channel)),irc_action_queue(Agent,TODO,Channel).

irc_skipped(W):- catch(clpfd:read_term_from_atom(W,CMD,[double_quotes(string)]),_,fail),!,CMD= ('?-'(_)).

irc_action_queue(_,W,_):-irc_skipped(W),!.
irc_action_queue(Agent,TODO,Channel):-  get_session_id(ID), enqueue_session_action(Agent,TODO,ID).




:-ain(( (baseKB:irc_user_plays(Agent,User,Channel)/
  ( baseKB:irc_user_plays(OAgent,User,Other), (Other\=Channel;OAgent\=Agent) ))
   ==> \+ baseKB:irc_user_plays(OAgent,User,Other))).


:-ain(( ~baseKB:irc_user_plays(Agent,User,_) ==> {retractall(lmcache:agent_session(Agent,User)),retractall(lmcache:session_agent(User,Agent))} )).

end_of_file.


:-dynamic(wotp_server_port/1).
wotp_server_port(61996).

wotp_lambda(A,    Call,A0):- copy_term(Call+A0,CCall+A),CCall.
wotp_lambda(A,B,  Call,A0,B0):- copy_term(Call+A0+B0,CCall+A+B),CCall.
wotp_lambda(A,B,C,Call,A0,B0,C0):- copy_term(Call+A0+B0+C0,CCall+A+B+C),CCall.

with_out_pred(HookPred, Call):- !, call(Call).
with_out_pred(HookPred, Call):- fail,with_output_to(string(Atom),Call),atomic_list_concat(List,'\n',Atom), 
  forall(member(E,List),call(HookPred,E)).

% Redirect output stream and read_line_to_string to write to a predicate 
with_out_pred(HookPred, Call):- 
   with_out_pred(read_line_to_string, HookPred, Call).


% Redirect output stream to write to a predicate with ReaderPred line [read_line_to_string,get_char,..]
with_out_pred(ReaderPred, HookPred, Call):- thread_self(ID), 
   wotp_client(ReaderPred, wotp_lambda(Data,thread_signal(ID,call(HookPred,Data))),Call).


with_out_pred_direct(ReaderPred, HookPred, Call):- 
   wotp_client(ReaderPred, HookPred, Call).


wotp_client(ReaderPred,HookPred, Call):-
  must_det_l((
    wotp_create_server,
    must(wotp_server_port(Port)),
    tcp_socket(Socket),
    tcp_connect(Socket, localhost:Port),
    tcp_open_socket(Socket, StreamPair),
    stream_pair(StreamPair, In, Out),
    wotp_io_setup(In, Out),
    read(In,ServID),
    format(Out,'~N~q.~n~q.~n',[ReaderPred,HookPred]),
    flush_output(Out))),!,
    setup_call_cleanup(tell(Out),on_x_debug(Call),
     ignore(((format(Out,'~N',[]),signal_done(ServID),told,ignore(catch(close(StreamPair, [force(true)]),_,true)))))).

signal_done(ServID):- call(call,true),thread_signal(ServID,call(call,true)).

wotp_create_server(Port):- thread_property(_T,alias(wotp_server)),!.
wotp_create_server(Port):-
        (wotp_server_port(Port) -> true ; asserta(wotp_server_port(Port))),
    	tcp_socket(ServerSocket),
	tcp_setopt(ServerSocket, reuseaddr),
	tcp_bind(ServerSocket, Port),
	tcp_listen(ServerSocket, 5),
	thread_create(wotp_server_loop(ServerSocket), _,[ alias(wotp_server)]).

wotp_server_loop(ServerSocket) :-
	tcp_accept(ServerSocket, Client, _Peer),
	tcp_open_socket(Client, StreamPair),
        gensym(wotp_client_,Alias),
	thread_create(wotp_service(StreamPair),_,[alias(Alias),detached(true)]),
	wotp_server_loop(ServerSocket).
 
wotp_service(StreamPair):-
         stream_pair(StreamPair, In, Out),
         wotp_io_setup(In, Out),
         thread_self(TID),
         format(Out,'~N~q.~n',TID),
         flush_output(Out),
         read(In,ReaderPred),read(In,HookPred), debug(wotp,'~q. ==> ~q. ~n',[ReaderPred,HookPred]),!,
         repeat,
         call(ReaderPred,In,Data), debug(wotp,'~q ==> ~q.~n',[read_data(ReaderPred,In,Data),HookPred]),
         ((Data \==end_of_file, Data \== -1 ) -> (once(call(HookPred,Data)),fail) ;
         ignore(catch(close(StreamPair, [force(true)]),_,true))).
        

wotp_io_setup(In, Out):-           
      current_prolog_flag(encoding, Enc),
      set_stream(In, encoding(Enc)),
      set_stream(Out, encoding(Enc)),
      set_stream(In, newline(detect)),
      set_stream(Out, newline(dos)),
      set_stream(In, close_on_abort(false)),
      set_stream(Out, close_on_abort(false)).

wotp_create_server:- (wotp_server_port(Port)->wotp_create_server(Port);wotp_create_server(61996)).


:- wotp_create_server.

% Example  
% :- with_out_pred(say(dmiles), format('Hello Worldly~n',[])).




