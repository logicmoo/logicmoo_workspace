/*  Part of SHRDLURN

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.logicmoo.org
    Copyright (C): 2016-2020, VU University Amsterdam
			      CWI Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(shrdlurn_eval,
	  [ eval_broadcast/1,		% +Message
	    eval_broadcast/2,		% +Message, +Channel
	    eval_to_profile/2,		% +ProfileID, :HTML
	    eval_about/2,		% +DocID, +Message
      js_eval/2,
      js_eval/3,
      js_eval_ws/3,
	    notifications//1,		% +Options
	    broadcast_bell//1		% +Options
	  ]).
:- use_module(library(http/hub)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/websocket)).
:- use_module(library(http/json)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(debug)).
:- use_module(library(uuid)).
:- use_module(library(random)).
:- use_module(library(base64)).
:- use_module(library(apply)).
:- use_module(library(broadcast)).
:- use_module(library(ordsets)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).
:- if(exists_source(library(user_profile))).
:- use_module(library(user_profile)).
:- endif.
:- use_module(library(aggregate)).

/*
:- use_module(swish(lib/storage)).
:- use_module(swish(lib/gitty).
:- use_module(swish(lib/config)).
:- use_module(swish(lib/avatar)).
:- use_module(swish(lib/noble_avatar)).
:- use_module(swish(lib/evalstore)).
:- use_module(swish(lib/authenticate)).
:- use_module(swish(lib/pep)).
:- use_module(swish(lib/content_filter)).
*/

:- html_meta(eval_to_profile(+, html)).

/** <module> The SHRDLURN collaboration backbone

We have three levels of identity as   enumerated  below. Note that these
form a hierarchy: a particular user  may   be  logged  on using multiple
browsers which in turn may have multiple SHRDLURN windows opened.

  1. Any open SHRDLURN window has an associated websocket, represented
     by the identifier returned by hub_add/3.
  2. Any browser, possibly having multiple open SHRDLURN windows, is
     identified by a session cookie.
  3. The user may be logged in, either based on the cookie or on
     HTTP authentication.
*/

:- multifile shrdlurn_config:config/2.

shrdlurn_config:config(hangout, 'Hangout.swinb').
shrdlurn_config:config(avatars, svg).		% or 'noble'


		 /*******************************
		 *	ESTABLISH WEBSOCKET	*
		 *******************************/


:- http_handler(swish('jseval_ws'), http_upgrade_to_websocket(echo_eval, []), [spawn([])]).
echo_eval(WebSocket) :-
    ws_receive(WebSocket, Message),
    with_output_to(user_error,fmt(ws_receive(WebSocket, Message))),
    (   Message.opcode == close
    ->  dead_eval(WebSocket)
    ;   setup_js_eval(WebSocket, Message),
        (repeat,sleep(10.0),fail) %echo_eval(WebSocket)
    ).

:- dynamic(js_eval_wsinfo/1).
:- volatile(js_eval_wsinfo/1).
:- dynamic(js_eval_wsinfo/2).
:- volatile(js_eval_wsinfo/2).

setup_js_eval(WebSocket, Message):- asserta(js_eval_wsinfo(WebSocket)),asserta(js_eval_wsinfo(WebSocket,Message)).

dead_eval(WebSocket):- retractall(js_eval_wsinfo(WebSocket)).

coerce_js_result(WebSocket,Message,throw(WebSocket,Message)):-  Message.opcode == close,!,dead_eval(WebSocket).
coerce_js_result(_,Message,Out):- Message.opcode == text, !, Data = Message.data , coerce_js_result_data(Data,Out).
coerce_js_result(_,Out,Out).

coerce_js_result_data(Out,Out):- \+ atomic(Out),!.
coerce_js_result_data(Res,Out):-coerce_js_result_data_p1(Res,M),coerce_js_result_data_p2(M,Out),!.
coerce_js_result_data(Out,unk(Out,unk)):- !.

coerce_js_result_data_p1(Out,Out):- \+ atomic(Out),!.
coerce_js_result_data_p1(Res0,Out):- atomic(Res0),any_to_atom(Res0,Res),coerce_js_result_atom_data_p1a(Res,Out),!.

coerce_js_result_data_p2(M,Out):- is_list(M),!,maplist(coerce_js_result_data_p2,M,Out).
coerce_js_result_data_p2(M,Out):- atomic(M), coerce_js_result_data_p1(M,Out),!.
coerce_js_result_data_p2(M,Out):- \+ compound(M),!,Out=M.
coerce_js_result_data_p2(M,Out):- 
  compound_name_arguments(M,C,A),
  maplist(coerce_js_result_data_p2,A,AA),!,
  compound_name_arguments(Out,C,AA).

coerce_js_result_atom_data_p1a([],[]):-!.
coerce_js_result_atom_data_p1a('[]',[]):-!.
coerce_js_result_atom_data_p1a('',""):-!.
coerce_js_result_atom_data_p1a(Res,Out):- atom_concat('+<',_,Res),atom_concat('+',Res0,Res), 
 atom_to_memory_file(Res0, Handle), 
 setup_call_cleanup(
  open_memory_file(Handle,read,Stream,[free_on_close(true)]),
  on_x_fail(load_structure(stream(Stream), Out, [space(remove),dialect(html)])),
  close(Stream)),!.
coerce_js_result_atom_data_p1a(Res,Out):- on_x_fail(json:atom_json_dict(Res,Out,[as(string)])),!.
coerce_js_result_atom_data_p1a(Res,Out):- on_x_fail(json:atom_json_term(Res,Out,[as(string)])),!.
coerce_js_result_atom_data_p1a(Res,Out):- fail, on_x_fail(atom_to_term(Res,Out,VS)),maplist(call,VS),!.
coerce_js_result_atom_data_p1a(Res,Res).

js_eval_ws_raw(WebSocket,Dict,Result):-  
  js_eval_wsinfo(WebSocket), 
  ws_send(WebSocket, Dict),
  ws_receive(WebSocket,Result),
   ((Result.opcode \== close) -> true ;
   (dead_eval(WebSocket),throw(Result))).

js_eval_ws(WebSocket,Dict,Out):-  
  js_eval_ws_raw(WebSocket,Dict,Result),
  ignore(coerce_js_result(WebSocket,Result,Out)).

js_eval(Js,Result):- js_eval_wsinfo(WebSocket),js_eval(WebSocket,Js, Result),!.

js_eval(WebSocket,Js,Result):- var(Js),!,throw(var_js_eval(WebSocket,Js,Result)).
js_eval(WebSocket,res(Js),Result):- !,js_eval_ws(WebSocket,Js,Result).
js_eval(WebSocket,raw(Js),Result):- !,js_eval_ws_raw(WebSocket,Js,Result).
js_eval(WebSocket,Js,Result):- is_list(Js),!,maplist(js_eval(WebSocket),Js,Result).
js_eval(WebSocket,Js,Result):- atomic(Js),!, atom_concat('+',Js,S), js_eval_ws(WebSocket,text(S),Result).
js_eval(WebSocket,Js,Result):- is_dict(Js),!,js_eval_ws(WebSocket,Js,Result).
js_eval(WebSocket,text(Js),Result):- !,js_eval_ws(WebSocket,text(Js),Result).
js_eval(WebSocket,Js,Result):- sformat(S,'~w',[Js]),js_eval_ws(WebSocket,text(S),Result).



:- fixup_exports.

