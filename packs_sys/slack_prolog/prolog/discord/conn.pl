:- if(current_prolog_flag(xref,true);(prolog_load_context(file,F),prolog_load_context(source,F))).
:- module(discord_conn,[]).
:- endif.

:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(url)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/websocket)).

:- if(exists_source(library(dicts))).
	:- use_module(library(dicts)).
:- endif.

:- dynamic(tmp:discord_websocket_event/2).
:- dynamic(tmp:discord_chat_event/2).
:- dynamic(tmp:last_disconnect/1).

%gw_op(0,'dispatch','receive','an event was dispatched.').
%gw_op(1,'heartbeat',_,'fired periodically by the client to keep the connection alive.').
gw_op(2,'identify','send','starts a new session during the initial handshake.').
gw_op(3,'presence update','send','update the client''s presence.').
gw_op(4,'voice state update','send','used to join/leave or move between voice channels.').
gw_op(6,'resume','send','resume a previous session that was disconnected.').
gw_op(7,'reconnect','receive','you should attempt to reconnect and resume immediately.').
gw_op(8,'request guild members','send','request information about offline guild members in a large guild.').
gw_op(9,'invalid session','receive','the session has been invalidated. you should reconnect and identify/resume accordingly.').
gw_op(10,'hello','receive','sent immediately after connecting and contains the heartbeat_interval to use.').
gw_op(11,heartbeat_ack(11),'receive','sent in response to receiving a heartbeat to acknowledge that it has been received.').



% should return wss://gateway.discord.gg
%discord_get_websocket_url('wss://127.0.0.1:50051/'):-!.
discord_get_websocket_url('wss://gateway.discord.gg/?v=9&encoding=json'):-!.
discord_get_websocket_url(URL):- discord_http(gateway), discord_dd(url,URL).

into_discord_url_object(UCmd,Prop):- atom(UCmd),atomic_list_concat([Prop2,_|_],'?',UCmd),!,into_discord_url_object(Prop2,Prop).
into_discord_url_object(UCmd,Prop):- atom(UCmd),atomic_list_concat([L,I|ST],'/',UCmd),
  reverse([L,I|ST],Rev),
  append(_,[Number,Type|_],Rev),
  atom_number(Number,Prop),
  add_id_type(Prop,Type),!.
into_discord_url_object(UCmd,Prop):- atomic_list_concat([_,I|ST],'/',UCmd),member_rev(Prop2,[I|ST]), \+ atom_number(Prop2,_),!,into_discord_url_object(Prop2,Prop).
into_discord_url_object(Prop,Prop).

member_rev(X,[_|L]):-  member_rev(X,L).
member_rev(X,[X|_]).

check_pausing:- ignore((retract(tmp : discord_info(_,retry_after,Sleep)),sleep(Sleep),sleep(0.3))).

discord_http(Cmd):- discord_http(Cmd,[]),!.

discord_http(Cmd,Opts):- notrace(discord_http_0(Cmd,Opts)).
discord_http_0(Cmd,Opts):-
 must_det_l((
  expand_discoure_host_post(Cmd,UCmd),
  into_discord_url_object(UCmd,Prop),
  discord_http(Prop,Cmd,Opts))).

correct_opts(Opts,O):- select(json(JSON),Opts,OptsWo), \+ is_dict(JSON), \+ atomic(JSON),
  any_to_json_dict(JSON,Dict), correct_opts([json(Dict)|OptsWo],O),!.
correct_opts(Opts,O):- select(post(json(JSON)),Opts,OptsWo), \+ is_dict(JSON), \+ atomic(JSON),
  any_to_json_dict(JSON,Dict), correct_opts([post(json(Dict))|OptsWo],O),!.
correct_opts(Opts,O):- select(post([json(JSON)|More]),Opts,OptsWo), \+ is_dict(JSON), \+ atomic(JSON),
  any_to_json_dict(JSON,Dict), correct_opts([post([json(Dict)|More])|OptsWo],O),!.
correct_opts(Opts,O):- member(method(_),Opts), \+ member(post(_),Opts),!, correct_opts([post([])|Opts],O).
correct_opts(O,O).

discord_http(Prop,Cmd,Opts0):-
 correct_opts(Opts0,Opts),
 must_det_l((  
  expand_discoure_host_post(Cmd,UCmd),
  http_discord_token_string(TokenHeader),
  sformat(URL,'https://discord.com/api/v9/~w',[UCmd]))),
  %wdmsg(URL=Opts),
  check_pausing,
  http_open(URL, In, [status_code(Status), 
          request_header('Authorization'=TokenHeader), 
          request_header('User-Agent'='DiscordBot'),
          request_header('Content-Type'='application/json')|Opts]),
  ignore((\+ ok_satus_code(Status),dmsg((discord_http(Prop,Cmd,Opts)->URL:Opts=Status)))),
  must_det_l((
    read_stream_to_codes(In,Codes),notrace_catch(close(In)),text_to_string(Codes,Str),
    read_codes_response(Status,Str,Term),discord_receive(Prop,Term))),
  !, nop(ok_satus_code(Status)).

ok_satus_code(Code):- var(Code),!,fail.
ok_satus_code(200).
ok_satus_code(204).

read_codes_response(_Status,String,Term):- String\=="",string_to_dict(String,Term),!.
read_codes_response(Status,Term,reply(Status,Term)).


discord_token_string(S):-tmp:discord_token(T),atom_string(T,S).
bot_token_string(TokenHeader):- discord_token_string(Token),sformat(TokenHeader,"Bot ~w",[Token]).
ima_bot:- discord_ddd(X,instanceOf,'@me'),discord_ddd(X,bot,true),!.
ima_bot:- discord_token_string(S),atom_concat("Nzc",_,S),!.
http_discord_token_string(X):- ima_bot,!, bot_token_string(X),!.
http_discord_token_string(X):- discord_token_string(X),!.

%curl -H "Authorization: Bot $AUTH_TOK" -H "User-Agent: DiscordBot" -H "Content-Type: application/json" 
%  https://discord.com/api/v9/


expand_discoure_host_post(A,O):- \+ compound(A),!,A=O.
expand_discoure_host_post('$'(A),O):- !, discord_dd(A,M),expand_discoure_host_post(M,O),!.
expand_discoure_host_post(A / B,O):- !, expand_discoure_host_post(A,AA),expand_discoure_host_post(B,BB),!,sformat(O,"~w/~w",[AA,BB]).
expand_discoure_host_post({A - B},O):- !, discord_dd(A,B,M),expand_discoure_host_post(M,O).
expand_discoure_host_post(A,O):- A=O.

:- dynamic(tmp:discord_websocket/3).

% use dmiles proxy impl first?
discord_get_websocket(WS):- tmp:jpl_websocket(WS),!.
discord_get_websocket(WS):- tmp:discord_websocket(WS,_,_),!.
discord_get_websocket(WS):-
   discord_get_websocket_url(URL),!,
   discord_open_websocket(URL,WS),!.

:- dynamic(tmp:jpl_websocket/1).

discord_open_websocket(URL,WS):-
   ignore(tmp:discord_websocket(OLD_WS,_,_)),
   %atom_concat(URL,'?v=9&encoding=json',UrlV9),
   http_open_websocket(URL, WS, []),
   stream_pair(WS,I,O),
   show_call(asserta(tmp:discord_websocket(WS,I,O))),
   (nonvar(OLD_WS)->discord_remove_websocket(OLD_WS);true).
   

discord_remove_websocket(OLD_WS):-
   ignore(retract(tmp:discord_websocket(OLD_WS,_,_))),
   ignore(catch(my_ws_close(OLD_WS,1000,''),_,true)).

% ===============================================
% Property Names
% ===============================================

discord_start_gateway:- disable_gateway,!.
discord_start_gateway:- tmp:jpl_websocket(_),!.
discord_start_gateway:- is_thread_running(discord_start_gateway),!.
%discord_start_gateway:- discord_gateway_proc,!.
discord_start_gateway:- \+ thread_self(discord_start_gateway), 
  !,thread_create(discord_gateway_proc,_,[alias(discord_start_gateway)]),!.


make_url_params({In},Out):-!,make_url_params(In,Out).
make_url_params((A,B),Out):-!,make_url_params(A,AA),make_url_params(B,BB),format(atom(Out),'~w&~w',[AA,BB]).
make_url_params([A|B],Out):-!,make_url_params(A,AA),make_url_params(B,BB),format(atom(Out),'~w&~w',[AA,BB]).
make_url_params([A],Out):-!,make_url_params(A,Out).
make_url_params(KV,Out):-get_kvd(KV,K,A),www_form_encode(A,AA),format(atom(Out),'~w=~w',[K,AA]).


discord_send(DataI):- any_to_curls(DataI,Data),discord_send000(Data).
%discord_send_receive(DataI,Recv):- any_to_curls(DataI,Data),discord_send_receive000(Data,Recv).

% @TODO comment the above and fix this next block
discord_send000(Data):-  discord_send_ws(_WebSocket,Data),!.

% Java version (Until JanW fixes SWI-Prolog's builtin impl
discord_send_ws(WebSocket,Data):- tmp:jpl_websocket(WebSocket),!,
  with_output_to(atom(S),writeq(Data)), jpl_call(WebSocket,send_message,[S],_),!. 

discord_send_ws(WebSocket,Data):- tmp:discord_websocket(WebSocket, _WsInput, WsOutput), !,  
 must_det_l((flush_output(WsOutput),
  wdmsg(ws_send(WsOutput,text(Data))),
  ws_send(WsOutput,text(Data)),!,flush_output(WsOutput))),!.

discord_send_ws(WebSocket,Data):- tmp:discord_websocket(WebSocket, _WsInput, WsOutput), !,  
 must_det_l((flush_output(WsOutput), any_to_json_dict(Data,Dict),
  wdmsg(ws_send(WsOutput,json(Dict))),
  ws_send(WsOutput,json(Dict)),!,flush_output(WsOutput))),!.
%discord_send_ws(WsOutput,Data):- is_stream(WsOutput), format(WsOutput,'~q',[Data]),flush_output(WsOutput),ddbg_always(discord_sent(Data)),flush_output.

system:discord_websocket_hook(Type,Message):- discord_client:discord_websocket_client_hook(Type,Message).

discord_websocket_client_hook(onOpen,_):- ping_discord.
discord_websocket_client_hook(Event,Message):- discord_reconn_after(Event),!, 
  writeln(user_error,discord_event(Event,Message)), discord_reconnect.
discord_websocket_client_hook(Event,Message):- assertz(tmp:discord_websocket_event(Event,Message)).

discord_gateway_proc:- tmp:jpl_websocket(_),!.
discord_gateway_proc:- discord_connect,!.
discord_gateway_proc:-
 call_cleanup((  
  repeat,
  once(discord_get_websocket(WS)),
  once(my_ws_receive(WS,Data,[format(json)])),
  (Data==
    end_of_file->!;
  (once(discord_receive(gateway,Data)),flush_output,fail))),
  discord_remove_websocket(WS)).

my_ws_close(Ws,Data,Options):- ws_close(Ws,Data,Options).
my_ws_receive(Ws,Data,Options):- ws_receive(Ws,Data,Options).

undict(ID,IDO):- is_dict(ID),ID.IDK=IDV,IDK=id,IDO=IDV.
undict(ID,ID).

discord_websocket_hook_1(_,_):- remember_task(handle_discord_websocket_events),fail.
discord_websocket_hook_1(Type,Message):- \+ atomic(Message),!,writeln(user_error,discord_event(Type,Message)),discord_event(Type,Message).
discord_websocket_hook_1(onMessage,Message):- atom_contains(Message,'"op":11'),!.
%discord_websocket_hook_1(onMessage,Message):- atom_json_dict(Message,Dict,[as(string)]),!,discord_event(gateway,Dict).
discord_websocket_hook_1(onMessage,Message):- string_to_dict(Message,Dict),!,discord_event(gateway,Dict).
%discord_websocket_hook_1(Type,Message):- notrace_catch(atom_json_term(Message,Dict,[as(string)])),!,discord_event(Type,Dict).
%discord_websocket_hook_1(Type,Message):-notrace_catch(atom_to_term(Message,Dict,_)),!,discord_event(Type,Dict).
discord_websocket_hook_1(Type,Message):- writeln(user_error,discord_event(Type,Message)),discord_event(Type,Message),!.

discord_reconn_after(onCreateError).
discord_reconn_after(sendTextError).
discord_reconn_after(onClose).
discord_reconn_after(sendTextError).



discord_event(onMessage,Data):- !,discord_event(gateway,Data).
discord_event(Type,{O}):- !, any_to_json_dict({O},Dict),!,discord_event(Type,Dict).

% OP==0 an event was dispatched.
discord_event(_,O):- get_prop(O,op,0),get_prop(O,t,Type),get_prop(O,s,S),get_prop(O,d,Data),!,
  discord_add(s,S),!, discord_event(Type,Data).  
discord_event("PRESENCE_UPDATE",Data):- !, discord_addd(presence,hasMember,Data).
discord_event(gateway,O):- get_prop(O,op,Type),!,discord_receive(Type,O),!.
%discord_event(gateway,O):- is_dict(O),O.Key=Type,Key=id,!,discord_receive(Type,O),!.
% simplify the data objects
%discord_event(Type,O):- is_dict(O),O.Key=Data,Key=data,!,discord_receive(Type,Data),!.
discord_event(Type,O):- get_prop(O,data,Data),!,discord_receive(Type,Data),!.
% typify the data objects
%discord_event(gateway,O):- is_dict(O),O.Key=Type,Key=type,!,discord_receive(Type,O),!.
discord_event(_,O):- get_prop(O,type,Type),!,discord_receive(Type,O),!.
discord_event(Evt,end_of_file):- !, throw(discord_event(Evt,end_of_file)).
discord_event(Type,Dict):- discord_receive(Type,Dict),!.


discord_unused(user_typing).
discord_unused(reconnect_url).

discord_receive(heartbeat_ack(11),_Data):- !.
discord_receive(Type,Data):- number(Type), gw_op(Type,Dispatch,_,_), Type\==Dispatch,!,discord_receive(Dispatch,Data).
discord_receive(Type,Data):- string(Data),string_to_dict(Data,Dict),!,discord_receive(Type,Dict).
%discord_receive(Type,Data):- ddbg_always((discord_receive(Type,Data))),fail.
discord_receive(Type,Data):- once(discord_add(Type,Data)),fail.
discord_receive(_Type,_Data).


discord_connect:- with_mutex(connection_mutex,discord_connect_0).
discord_disconnect:- with_mutex(connection_mutex,discord_disconnect_0).
discord_reconnect:- with_mutex(connection_mutex,discord_reconnect_0).


%discord_disconnect:- \+ mutex_trylock(connection_mutex), mutex_unlock(connection_mutex).
discord_disconnect_0:- retractall(tmp:last_disconnect(_)), get_time(Time), asserta(tmp:last_disconnect(Time)),fail.
discord_disconnect_0:- tmp:jpl_websocket(O), catch(jpl_call(O,close,[],_),_,true), fail.
discord_disconnect_0:- retractall(tmp:jpl_websocket(_)).

% This may be triggered by several events at once (so it has to hapen at least 10 seconds appart
discord_reconnect_0:- tmp:last_disconnect(Before), get_time(Time), Before+10 <  Time,!.
discord_reconnect_0:- discord_disconnect_0, fail.
discord_reconnect_0:- discord_connect_0, fail.
discord_reconnect_0:- discord_resume.

discord_connect_0:- tmp:jpl_websocket(_),!.
discord_connect_0:- %setenv('CLASSPATH','/opt/logicmoo_workspace/packs_sys/swicli/build/application/classes:/opt/logicmoo_workspace/packs_sys/swicli/lib/javax.websocket-api-1.0.jar'),
  discord_get_websocket_url(URL),
  jpl_new('PrologWebSocket',[URL,'discord_websocket_hook'],O),
  assert(tmp:jpl_websocket(O)),!.

discord_identify:-
 discord_add(s,null),
 discord_send(
  {"op": 2,
  d: {
    "token": $token,
    "properties": {
      "$os": "linux",
      "$browser": "disco",
      "$device": "disco"
    }, 
  "intents": 65535
  }
}),
 nop(discord_add_slash).

discord_add_slash:- 
 discord_http(applications/772113231757574185/command,
 [post(
 json({
    "name": "blep",
    "type": 1,
    "description": "Send a random adorable animal photo",
    "options": [
        {
            "name": "animal",
            "description": "The type of animal",
            "type": 3,
            "required": true,
            "choices": [
                {
                    "name": "Dog",
                    "value": "animal_dog"
                },
                {
                    "name": "Cat",
                    "value": "animal_cat"
                },
                {
                    "name": "Penguin",
                    "value": "animal_penguin"
                }
            ]
        },
        {
            "name": "only_smol",
            "description": "Whether to show only baby animals",
            "type": 5,
            "required": false
        }
    ]
}))]).

discord_resume:- 
 discord_send( {
  "op": 6,
  d: {
    "token": $token,
    "session_id": $session_id,
    "seq": $s
  }
}).

