/* Part of SHRDLURN

  Author:    Jan Wielemaker
  E-mail:    J.Wielemaker@cs.vu.nl
  WWW:      http://www.logicmoo.org
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
	 [ cvt_js_result_mesg/2, 		% +Result
	  js_eval_ws_raw/3, 		% +DocID, +Result
   js_eval/2,
   js_eval/3,
   js_eval_ws/3
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

%:- html_meta(eval_to_profile(+, html)).

/** <module> The SHRDLURN collaboration backbone

We have three levels of identity as  enumerated below. Note that these
form a hierarchy: a particular user may  be logged on using multiple
browsers which in turn may have multiple SHRDLURN windows opened.

 1. Any open SHRDLURN window has an associated websocket, represented
   by the identifier returned by hub_add/3.
 2. Any browser, possibly having multiple open windows, is
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
  ws_receive(WebSocket, Result),
  with_output_to(user_error, fmt(ws_receive(WebSocket, Result))),
  (  Result.opcode == close
  -> dead_eval(WebSocket)
  ;  setup_js_eval(WebSocket, Result),
    (repeat, sleep(10.0), fail) %echo_eval(WebSocket)
  ).

:- dynamic(js_eval_wsinfo/1).
:- volatile(js_eval_wsinfo/1).
:- dynamic(js_eval_wsinfo/2).
:- volatile(js_eval_wsinfo/2).

setup_js_eval(WebSocket, Result):- asserta(js_eval_wsinfo(WebSocket)), asserta(js_eval_wsinfo(WebSocket, Result)).

dead_eval(WebSocket):- retractall(js_eval_wsinfo(WebSocket)).

cvt_js_result_mesg(Jsv, Term):- \+ atomic(Jsv), cvt_js_result_structure(Jsv, Term), !.
cvt_js_result_mesg(Jsv, Term):- cvt_js_result_ref(Jsv, Term), !.
cvt_js_result_mesg(Jsv, Term):- cvt_js_result_string(Jsv, JsvMid), cvt_js_result_structure(JsvMid, Term), !.
cvt_js_result_mesg(Jsv, Term):- !, Term = err_unk(Jsv, unk).

cvt_js_result_string(Jsv, Term):- string(Jsv), any_to_atom(Jsv, JsvMid), cvt_js_result_atom_data(JsvMid, Term), !.
cvt_js_result_string(Jsv, Term):- atom(Jsv), cvt_js_result_atom_data(Jsv, Term), !.
cvt_js_result_string(Term, Term).

%destructive_js_cvt(Jsv):- var(Jsv), !.
%destructive_js_cvt([]):- !.
destructive_js_cvt(Jsv):- \+ compound(Jsv), !.
destructive_js_cvt(Jsv):- Jsv=[E|T], !, cvt_js_result_structure(E, Term),
 ( E\==Term-> nb_setarg(1, Jsv, Term) ; true), !, destructive_js_cvt(T).
destructive_js_cvt(Jsv):- is_dict(Jsv, Tag), ignore(Tag=json), functor(Jsv, _, L), destructive_js_cvt_dict(2, 2, L, Jsv), !.
destructive_js_cvt(_):-!.

destructive_js_cvt_dict(I, _, L, _):- I > L, !.
destructive_js_cvt_dict(I, P, L, Jsv):-
  arg(I, Jsv, E), cvt_js_result_structure(E, Term),
  ( E\==Term-> nb_setarg(I, Jsv, Term) ; true),
  N is I + P, destructive_js_cvt_dict(N, P, L, Jsv).

into_pair(NV,N-V):- must(NV=..[_,N,V]).
cvt_js_result_structure(Jsv, Term):- string(Jsv), cvt_js_result_ref(Jsv, Term), !.
cvt_js_result_structure(Jsv, Term):- atom(Jsv), cvt_js_result_ref(Jsv, Term), !.
cvt_js_result_structure(Jsv, Term):- \+ compound(Jsv),!,Jsv=Term.
cvt_js_result_structure('@'(Jsv), Term):- !, Term=Jsv.
cvt_js_result_structure(json(List),Term):- 
  must_or_rtrace((maplist(into_pair,List,Pairs),dict_pairs(Jsv, _Tag, Pairs),!,cvt_js_result_structure(Jsv,Term))),!.
cvt_js_result_structure(Jsv, Term):- compound(Jsv), Term = Jsv, destructive_js_cvt(Term), !.
/*
 cvt_js_result_structure(Jsv, Term):- is_dict(Jsv), dict_pairs(Jsv, Tag, Pairs), !,
 ignore(Tag=json), maplist(coerce_pairs, Pairs, PairsC),
 (Pairs =@=PairsC -> Term = Jsv ; dict_pairs(Term, Tag, PairsC)).
cvt_js_result_structure(Jsv, Term):- is_list(Jsv), !, maplist(cvt_js_result_structure, Jsv, Term).
*/
cvt_js_result_structure(Jsv, Term):- Term=Jsv.
/*
cvt_js_result_structure(Jsv, Term):-
 compound_name_arguments(Jsv, C, A),
 maplist(cvt_js_result_structure, A, AA), !,
 compound_name_arguments(Term, C, AA).
coerce_pairs(K-Jsv, K-Term):- cvt_js_result_structure(Jsv, Term).
*/

cvt_js_result_atom_data([], _):- !, fail.
cvt_js_result_atom_data('[]', _):- !, fail.
cvt_js_result_atom_data('', _):- !, fail.
% cvt_js_result_atom_data(Jsv, Term):- cvt_js_result_ref(Jsv, Term),!.
cvt_js_result_atom_data(Jsv, Term):- cvt_js_result_atom_data_json(Jsv, Term),!.
cvt_js_result_atom_data(Jsv, Term):- fail, on_x_fail(atom_to_term(Jsv, Term, VS)), maplist(call, VS), !.
%cvt_js_result_atom_data(Jsv, Jsv).
%cvt_js_result_atom_data_json(Jsv, Term):- on_x_fail(json:atom_json_dict(Jsv, Term, [as(string)])), !.
cvt_js_result_atom_data_json(Jsv, Term):- on_x_fail(json:atom_json_term(Jsv, Term, [value_string_as(string),end_of_file(@(eof))])), !.
%cvt_js_result_atom_data_json(Jsv, Term):- json:atom_json_term(Jsv, Term, [as(string)]), !.

cvt_js_result_ref(Jsv, Term):- atom_concat("<@ ", JsvRef1, Jsv),atom_concat(JsvRef, " @>", JsvRef1), !,
 on_x_fail(json:atom_json_dict(JsvRef, MidTerm, [as(string)])), 
 cvt_js_result_structure(MidTerm, Term).

cvt_js_result_ref(Jsv, Term):- atom_concat("#REF:$", JsvRef, Jsv), Term='ref'(JsvRef), !.
cvt_js_result_ref(Jsv, Term):- atom_concat('+<', _, Jsv), atom_concat(_, '>', Jsv), !,
 atom_concat('+', JsvMid, Jsv),
 cvt_js_result_xml(JsvMid, Term).

cvt_js_result_xml(JsvMid, Term):- 
 atom_to_memory_file(JsvMid, Handle),
 setup_call_cleanup(
 open_memory_file(Handle, read, Stream, [free_on_close(true)]),
 on_x_fail(load_structure(stream(Stream), Term, [space(remove),number(integer), %% shorttag(true),
   case_sensitive_attributes(true),
   case_preserving_attributes(true),
   attribute_value(string),cdata(string), dialect(xml)])),
 close(Stream)), !.

 % cvt_js_result_xml('<a foo=bar>y</a>',X),print_tree(X)
 % cvt_js_result_xml('<a foo=bar>y</a>',X),print_tree(X)

 % bp = 131/68
 % o2/pulse = 96/89
 % wiehth = 237


js_eval_ws_raw(WebSocket, Dict, Result):- 
 ws_send(WebSocket, Dict),
 ws_receive(WebSocket, Result),
 asserta(js_eval_wsinfo(WebSocket, Result)),
 % with_output_to(user_error, fmt(ws_receive(WebSocket, Result))),
  ((Result.opcode \== close) -> true ;
  (dead_eval(WebSocket), throw(Result))).

js_eval_ws(WebSocket, Dict, Term):-
 js_eval_ws_raw(WebSocket, Dict, Result),!,
 cvt_ws_result(WebSocket, Result, Term),!.

cvt_ws_result(WebSocket, Result, throw(WebSocket, Result)):- Result.opcode == close, !, dead_eval(WebSocket).
cvt_ws_result(_, Result, Term):- Result.opcode == text, !, Jsv = Result.data , cvt_js_result_mesg(Jsv, Term).
cvt_ws_result(_, Term, Term).


js_eval(Js, Result):- js_eval_wsinfo(WebSocket)-> js_eval(WebSocket, Js, Result), !.

js_cvt_eval( Js, Result):- var(Js), !, throw(var_js_cvt_eval( Js, Result)).
js_cvt_eval( Js, Js):- is_dict(Js), !.
js_cvt_eval( text(Js), text(Js)):- !.
js_cvt_eval( res(Js), Js).
js_cvt_eval( raw(Js), Result):- js_cvt_eval( (Js), Result).
js_cvt_eval( Js, Result):- on_x_fail(text_to_string(Js,Str)), !, atom_concat('+', Str, S), js_cvt_eval(text(S), Result).
js_cvt_eval( Js, Result):- is_list(Js), !, maplist(js_cvt_eval, Js, Result).
js_cvt_eval( Js, Result):- sformat(S, '~w', [Js]), js_cvt_eval( S, Result).

  
js_eval(WebSocket, Js, Result):- var(Js), !, throw(var_js_eval(WebSocket, Js, Result)).
js_eval(WebSocket, Js, Result):- is_list(Js), !, maplist(js_eval(WebSocket), Js, Result).
js_eval(WebSocket, res(Js), Result):- !, js_cvt_eval( Js, Op), js_eval_ws(WebSocket, Op, Result).
js_eval(WebSocket, raw(Js), Result):- !, js_cvt_eval( Js, Op), js_eval_ws_raw(WebSocket, Op, Result).
js_eval(WebSocket, Js, Result):- !, js_cvt_eval( Js, Op), js_eval_ws(WebSocket, Op, Result).


show_js_test(Code):- \+ atom_contains(Code,"'"), format("~N%?- js_test('~w').~n",[Code]).
show_js_test(Code):-  \+ atom_contains(Code,'"'), format('~N%?- js_test("~w").~n',[Code]).
show_js_test(Code):- format('~N%?- js_test(`~w`).~N',[Code]).

load_theA4_obj(Obj):- 
  js_eval(Obj,X),
  process_theA4Game_obj(Obj, X).

js_test(Code):- \+ string(Code), on_x_fail(text_to_string(Code,Str)), !, js_test(Str).
js_test(Code):- 
  nl,
  show_js_test(Code),!,
  
  ttyflush,
  js_eval(Code,X),
  %print_tab_term(X),
   \+ \+ (wots(S,(display(X))),atom_to_term(S,_,_)),!,
   \+ \+ (wots(S,(print(X))),write(S),atom_to_term(S,_,_)),
   nl,ttyflush,
   \+ \+ (wots(S,(print_tree(X))),write(S),atom_to_term(S,_,_)),!,
   nl,ttyflush,
  process_theA4Game_obj(Code, X),
  nl,ttyflush,!.

js_test:- 
  mmake,  
  js_test('1+1'),
  js_test('null'),
  js_test('this.game=window.theA4Game; null'),
  js_test('Object.keys(this.game)'),  
  js_test(`Term.fromString("property.problem('etaoin'[#id], erased('etaoin-memory'[#id]))", window.theA4Game.ontology)`),
  js_test(`Term.fromString("goal(D:'player'[#id], verb.find(X, 'shrdlu'[#id]))", window.theA4Game.ontology);`),
  js_test('this.game.naturalLanguageParser.rules[0].listenerVariable'),
  % js_test("window.location.reload()"),
  js_test(`window.theA4Game.ontology;`),
  % js_test('window.reload()'),
  !.

js_test0:-
   mmake,
   js_test('this.game.naturalLanguageParser.rules[0]'),!.

js_test1:-
   mmake,
   js_test('this.game.naturalLanguageParser.rules'),!.


:- Limit = 4_294_967_296, 
  prolog_flag(stack_limit, X),( (X<Limit) -> set_prolog_flag(stack_limit, Limit) ; true).
js_test2:- 
  mmake,
  js_eval('this.game',X),
  File = '/opt/logicmoo_workspace/packs_web/shrdlu/world.save.pl',
  setup_call_cleanup(open(File,write,OS), 
    with_output_to(OS,(print_tree(theA4Game(X)),write('.'))), 
    close(OS)),
  consult(File),!.
js_test3:- process_theA4Game.

:- multifile(theA4Game/1).
:- dynamic(theA4Game/1).
:- multifile(theA4Game/2).
:- dynamic(theA4Game/2).
:- multifile(theA4Game/3).
:- dynamic(theA4Game/3).

process_theA4Game_keys(Prefix,Cell,K-V):- !, \+ \+ (K=Cell,process_theA4Game(Prefix,V)).
process_theA4Game_keys(Prefix,Cell,K=V):- \+ \+ (K=Cell,process_theA4Game(Prefix,V)).



process_theA4Game:- js_eval('this.game',X),!,process_theA4Game(X).
process_theA4Game:- consult('/opt/logicmoo_workspace/packs_web/shrdlu/world.save.pl'), theA4Game(X),process_theA4Game(X).

process_theA4Game_obj(Obj0,A4):- text_to_string(Obj0,Obj1),any_to_atom(Obj1,Obj), retractall(theA4Game([Obj|_],_)),process_theA4Game([Obj],A4).


process_theA4Game(Prefix,A4):- is_dict(A4),dict_pairs(A4, _Tag, Pairs),!, append(Prefix,[Cell],PrefixCell),
 maplist(process_theA4Game_keys(PrefixCell,Cell),Pairs).
process_theA4Game(Prefix,A4):- is_list(A4), member(A,A4), \+ atomic(A),!,  append(Prefix,[Cell],PrefixCell), !, process_theA4Game_list(PrefixCell,Cell,0,A4).
process_theA4Game(Prefix,element(N,Props,Subs)):-!, append(Prefix,N,PrefixN), process_theA4Game_e(PrefixN,Props,Subs).
process_theA4Game(Prefix,A4):- retractall(theA4Game(Prefix,_)), assert(theA4Game(Prefix,A4)),
 ((writeq(theA4Game(Prefix)),print_tree(=(A4)))).

process_theA4Game_list(_,_,_,[]):-!.
process_theA4Game_list(Prefix,Cell,N,[A4|More]):- N1 is N+1,
  \+ \+ (Cell=N1,process_theA4Game(Prefix,A4)),
  process_theA4Game_list(Prefix,Cell,N1,More).


% SubElements are the Value
process_theA4Game_e(Prefix,P,List):- member(A,List), atomic(A), A \== [],!, 
 append(Prefix,[props(P)],PrefixP), !,process_theA4Game(PrefixP,value(P)).
% Value is Based on props
process_theA4Game_e(Prefix,P,List):- member(A,List), \+ ( atomic(A), A \== []),!, 
 append(Prefix,[props(P)],PrefixP), !, maplist(process_theA4Game(PrefixP),List).
% Value is the props
process_theA4Game_e_l(Prefix,P,_):- 
 append(Prefix,[Cell],PrefixCell),
 maplist(process_theA4Game_keys(PrefixCell,Cell),P).

identity_prop(tiles).
identity_prop(name).
identity_prop(name).
end_of_file.

process_theA4Game_e(Prefix,P,[Sub|Subs]):-
  process_theA4Game_e(Prefix,P,Sub),
  process_theA4Game_e(Prefix,P,Subs).

process_theA4Game_e_p(Prefix,[N=V|Props],Subs):- !, process_theA4Game(Prefix,N=V),
  process_theA4Game_e(Prefix,Props,Subs).

:- fixup_exports.

