/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
/*
:- module(arc_gojs,
	 [ cvt_js_result_mesg/2, 		% +Result
	  js_eval_ws_raw/3, 		% +DocID, +Result
   js_eval/2,
   js_eval/3,
   js_eval_ws/3
	 ]).
*/
:- export(
	 ( cvt_js_result_mesg/2, 		% +Result
	  js_eval_ws_raw/3, 		% +DocID, +Result
   js_eval/2,
   js_eval/3,
   js_eval_ws/3
	 )).

%:- debug(websocket(_)).
%:- debug(websocket).

:- use_module(library(http/websocket)).
%:- use_module(library(http/hub)).
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
%:- if(exists_source(library(user_profile))).
%:- use_module(library(user_profile)).
%:- endif.
:- use_module(library(aggregate)).
%:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_parameters)).

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

%:- http_handler(swish('jseval_ws'), http_upgrade_to_websocket(echo_eval, []), [spawn([])]).
:- http_handler(('/swish_arc/jseval_ws'), http_upgrade_to_websocket(echo_eval, []), [spawn([])]).
%:- http_handler(root('swish/jseval_ws2'), http_upgrade_to_websocket(echo_eval, []), [spawn([])]).

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
destructive_js_cvt(ref(_)).
destructive_js_cvt(Jsv):- is_dict(Jsv, Tag), ignore(Tag=json), functor(Jsv, _, L), destructive_js_cvt_cmpd(2, 2, L, Jsv), !.
%destructive_js_cvt(ele(_)).
%destructive_js_cvt(element(_,_,_)).
destructive_js_cvt(Jsv):- functor(Jsv, _, L), destructive_js_cvt_cmpd(1, 1, L, Jsv),!.
destructive_js_cvt(Jsv):- Jsv=(_=V), !, cvt_js_result_structure(V, Term), ( V\==Term-> nb_setarg(2, Jsv, Term) ; true).
destructive_js_cvt(Jsv):- Jsv=[E|T], !, cvt_js_result_structure(E, Term),
 ( E\==Term-> nb_setarg(1, Jsv, Term) ; true), !, destructive_js_cvt(T).

destructive_js_cvt(Jsv):- compound_name_arguments(Jsv,_,Args),maplist(destructive_js_cvt,Args),!.
destructive_js_cvt(_):-!.

destructive_js_cvt_cmpd(I, _, L, _):- I > L, !.
destructive_js_cvt_cmpd(I, P, L, Jsv):-
  arg(I, Jsv, E), cvt_js_result_structure(E, Term),
  ( E\==Term-> nb_setarg(I, Jsv, Term) ; true),
  N is I + P, destructive_js_cvt_cmpd(N, P, L, Jsv).

into_pairzs([],[]).
into_pairzs([H|T],[HH|TT]):- must_or_rtrace(into_pairz(H,HH)),!,into_pairzs(T,TT).
into_pairz(NV,N-V):- NV=..[_,N,V].

cvt_js_result_structure(Jsv, Term):- var(Jsv),!,Term=Jsv.
cvt_js_result_structure(Jsv, Term):- string(Jsv), cvt_js_result_ref(Jsv, Term), !.
%cvt_js_result_structure(Jsv, Term):- atom(Jsv), cvt_js_result_ref(Jsv, Term), !.
cvt_js_result_structure(Jsv, Term):- \+ compound(Jsv),!,Jsv=Term.
cvt_js_result_structure(element(T,P,L), Term):- L==[], !, cvt_js_result_structure(json(['_className'=T|P]), Term).
cvt_js_result_structure('@'(Jsv), Term):- !, Term=Jsv.
cvt_js_result_structure('ele'([Jsv]), Term):- !, cvt_js_result_structure(Jsv, Term).
cvt_js_result_structure('ele'(Jsv), Term):- !, cvt_js_result_structure(Jsv, Term).
cvt_js_result_structure('ref'(Jsv), 'ref'(Path)):- atom_to_a4_obj(Jsv,A4),!,obj_path(A4,Path).
cvt_js_result_structure(super=String, super=StringL):- string(String),atomic_list_concat(StringL,',',String).
cvt_js_result_structure(json(List),Term):- fail, !,
  must_or_rtrace(into_pairzs(List,Pairs)),
  must_or_rtrace(dict_pairs(Jsv, _Tag, Pairs)),!,
  must_or_rtrace(cvt_js_result_structure(Jsv,Term)),!.
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

cvt_js_result_ref(Jsv, Term):- atom_concat("#REF:", JsvRef, Jsv), atom_to_a4_obj(JsvRef,Obj), Term='ref'(Obj), !.
cvt_js_result_ref(Jsv, Term):- atom_concat('+<', _, Jsv), atom_concat(_, '>', Jsv), !,
 atom_concat('+', JsvMid, Jsv),
 cvt_js_result_xml(JsvMid, Term).

cvt_js_result_xml(JsvMid, ele(Term)):- 
 atom_to_memory_file(JsvMid, Handle),
 setup_call_cleanup(
 open_memory_file(Handle, read, Stream, [free_on_close(true)]),
 on_x_fail(load_structure(stream(Stream), Term, [space(remove),number(integer), %% shorttag(true),
    case_sensitive_attributes(true), case_preserving_attributes(true), attribute_value(string),cdata(string), dialect(xml)])),
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


js_eval(Js, Result):- call((js_eval_wsinfo(WebSocket)-> js_eval(WebSocket, Js, Result))), !.

js_eval(Js):- js_eval(Js, R),print_tree(R).

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
  process_a4_obj(Obj, X).

js_test(Code):- \+ string(Code), on_x_fail(text_to_string(Code,Str)), !, js_test(Str).
js_test(Code):- 
  nl,
  show_js_test(Code),!,
  
  ttyflush,
  js_eval(Code,X),
  %print_tab_term(X),
   %\+ \+ (wots(S,(display(X))),atom_to_term(S,_,_)),!,
   %\+ \+ (wots(S,(print(X))),write(S),atom_to_term(S,_,_)),
   nl,ttyflush,
   \+ \+ (wots(S,(print_tree(X))),((write(S),atom_to_term(S,_,_)))),!,
   nl,ttyflush,
  text_to_a4obj(Code,Obj),
  abolish(the:a4Game/4),
  dynamic(the:a4Game/4),
  process_a4_obj(Obj, X),
  %listing(the:a4Game([Obj|_],_,_,_)),
  %listing(the:a4Game(_,Obj,_,_)),
  listing(the:a4Game/4),
  nl,ttyflush,!.

js_test:- 
  mmake_hook,
  js_test('1+1'),
  js_test('null'),
  js_test('null'),
 % js_test('this.game=window.theA4Game; !((window.theA4Game))'),
 % js_test('Object.keys(this.game)'),  
 % js_test(`Term.fromString("property.problem('etaoin'[#id], erased('etaoin-memory'[#id]))", window.theA4Game.ontology)`),
 % js_test(`Term.fromString("goal(D:'player'[#id], verb.find(X, 'shrdlu'[#id]))", window.theA4Game.ontology);`),
 % js_test('this.game.naturalLanguageParser.rules[0].listenerVariable'),  
 % js_test('window.theA4Game.ontology;'),
 % js_test("window.location.reload()"),
  !.

mmake_hook:- !.
mmake_hook:- js_test('$.getScript("/ef/files/ws.mount/shrdlu/eval_socket.js"); !!(window.theA4Game)'),mmake.

js_test0:-
   mmake_hook,
   js_test('this.game.naturalLanguageParser.rules[0]'),!.

js_test1:-
   mmake_hook,
   js_test('this.game.naturalLanguageParser.rules'),!.


:- Limit = 4_294_967_296, 
  prolog_flag(stack_limit, X),( (X<Limit) -> set_prolog_flag(stack_limit, Limit) ; true).

wotf(File,Goal):-  setup_call_cleanup(open(File,write,OS),with_output_to(OS, Goal), close(OS)).

js_test2:- 
  mmake_hook,
  retractall(the:a4Game(_)),
%  abolish(the:a4Game(_)),
  js_eval('this.game',X),
  asserta(the:a4Game(X)),
  File = '/opt/logicmoo_workspace/packs_web/shrdlu/world.save.pl',
  wotf(File,((writeq(the:a4Game(X)),write('.')))),!.

js_test3:- 
  mmake_hook,
  % retractall(the:a4Game(_,_,_,_)),
  abolish(the:a4Game/4),
  dynamic(the:a4Game/4),
  process_a4,
  File = '/opt/logicmoo_workspace/packs_web/shrdlu/world.save2.pl',
  format('.~N'),
  wotf(File,((listing(the:a4Game/4)))),!. 
  
%process_a4:- js_eval('this.game',X),!,process_a4(X).
process_a4:- the:a4Game(X),!,cvt_js_result_structure(X,Y),process_a4(['game'],Y).
process_a4:- ensure_loaded('/opt/logicmoo_workspace/packs_web/shrdlu/world.save.pl'), the:a4Game(X),!,
  process_a4(['game'],X).



:- multifile(the:a4Game/1).
:- dynamic(the:a4Game/1).
:- multifile(the:a4Game/4).
:- dynamic(the:a4Game/4).
:- multifile(the:a4Game/3).
:- dynamic(the:a4Game/3).


contains_a4obj(Term):- \+ compound(Term),!,fail.
contains_a4obj(ele(_)):-!.
contains_a4obj(Term):- is_dict(Term),!.
contains_a4obj(Term):- arg(_,Term,E),contains_a4obj(E),!.

ptgf(G):- call(G),!.
ptgf(G):- atrace,G,!.
ptgf(G):- 
 ignore((write_term(failed(G),[max_depth(4)]),!,  
  locally(set_prolog_flag(debugger_write_options,
   [quoted(true),portray(false),max_depth(4),attributes(portray),spacing(next_argument)]),
    (atrace,G)))).

base_obj('game').
base_obj('app').

text_to_atom(Text,Atom):- text_to_string(Text,Str),any_to_atom(Str,Atom).

atom_to_a4_obj(Atom,Obj):- base_obj(Game),atomic_list_concat(['.',Game,'.'],DotGameDot),
  sub_atom(Atom,_,_,After,DotGameDot),sub_atom(Atom,_,After,0,M),
  atom_to_a4_obj(M,MObj),atomic_list_concat([Game,'.',MObj],MObj2),
  atom_to_a4_obj(MObj2,Obj).
atom_to_a4_obj(Atom,Obj):- atom_concat(M,'()',Atom),atom_to_a4_obj(M,Obj).
atom_to_a4_obj(Atom,Obj):- atom_concat(M,'.outerHTML',Atom),atom_to_a4_obj(M,Obj).
atom_to_a4_obj(Atom,Obj):- Atom=Obj.

text_to_a4obj(Text,Obj):- text_to_atom(Text,Atom),atom_to_a4_obj(Atom,Obj),!.

obj_path(Obj,A4):- atom(Obj),atom_number(Obj,A4),!.
obj_path(Obj,A4):- atomic(Obj), arg(_,v('[','.',']'),E), atomic_list_concat([L1,L2|List],E,Obj),!,obj_path([L1,L2|List],A4).
obj_path(Obj,A4):- is_list(Obj),select('',Obj,ObjM),!,obj_path(ObjM,A4).
obj_path(Obj,A4):- is_list(Obj),!,maplist(obj_path,Obj,AM),(Obj==AM-> AM=A4 ; (flatten(AM,AF),obj_path(AF,AM))),!.
obj_path(Obj,A4):- Obj=A4,!.

process_a4_obj(Obj0,A4):- ptgf(process_a4_obj_1(Obj0,A4)).
process_a4_obj_1(Obj0,A4):- text_to_a4obj(Obj0,Obj), retractall(the:a4Game([Obj|_],_,_,_)),process_a4([Obj],A4),!.

process_a4_key_list(PrefixCell,Cell,T):- ptgf(process_a4_key_list_1(PrefixCell,Cell,T)).
process_a4_key_list_1(_,_,[]):-!.
process_a4_key_list_1(PrefixCell,Cell,[H|T]):- 
   \+ \+ process_a4_kv(PrefixCell,Cell,H),
   process_a4_key_list(PrefixCell,Cell,T).

process_a4_kv(A,B,C):- ptgf(process_a4_kv_1(A,B,C)).
process_a4_kv_1(Prefix,Cell,K=V):- \+ \+ (K=Cell,process_a4(Prefix,V)),!.
process_a4_kv_1(Prefix,Cell,K-V):- \+ \+ (K=Cell,process_a4(Prefix,V)),!.


process_a4(A,B):- ptgf(process_a4_1(A,B)).
process_a4_1(Prefix,json(List)):- append(Prefix,[Cell],PrefixCell),!, process_a4_key_list(PrefixCell,Cell,List).
process_a4_1(Prefix,A4):- is_dict(A4),dict_pairs(A4, _Tag, Pairs),!, append(Prefix,[Cell],PrefixCell),!,
 process_a4_key_list(PrefixCell,Cell,Pairs).
process_a4_1(Prefix,A4):- is_list(A4), contains_a4obj(A4), %  member(A,A4), \+ atomic(A),!, 
   append(Prefix,[Cell],PrefixCell), !, process_a4_list(PrefixCell,Cell,0,A4).
process_a4_1(Prefix,  [X]):- compound(X),!,process_a4(Prefix,X).
process_a4_1(Prefix,  ele(X)):- !,process_a4(Prefix,  X).
process_a4_1(Prefix,  element(N,Props,Subs)):- append(Prefix,[N],PrefixN), !, process_a4_e(PrefixN,Props,Subs).

process_a4_1(Prefix,A4):- 
 %retractall(the:a4Game(Prefix,_)), 
 append(Pre,[A,B],Prefix),!,
 assert(the:a4Game(Pre,A,B,A4)), % format('.'),flush_output,
 nop((writeq((Prefix=A4)),format('.~N'))).
process_a4_1(Prefix,A4):- 
 assert(the:a4Game(Prefix,[],[],A4)), % format('.'),flush_output,
 nop((writeq((Prefix=A4)),format('.~N'))).

process_a4_list(A,B,C,D):- ptgf(process_a4_list_1(A,B,C,D)).
process_a4_list_1(_,_,_,[]):-!.
process_a4_list_1(Prefix,Cell,N,[A4|More]):- N1 is N+1,
  \+ \+ (Cell=N1,process_a4(Prefix,A4)),
  process_a4_list_1(Prefix,Cell,N1,More).


process_a4_e(Prefix,P,List):- ptgf(process_a4_e_1(Prefix,P,List)),!.

% Value from props
process_a4_e_1(Prefix,P,[]):-  ptgf(append(Prefix,[Cell],PrefixCell)), process_a4_key_list(PrefixCell,Cell,P).
% SubElements are the Value
process_a4_e_1(Prefix,[],List):- process_a4(Prefix,List).
% SubElements are the Value
% UNCOMMENT process_a4_e_1(Prefix,P,List):-  append(Prefix,[props(P)],PrefixP), process_a4(PrefixP,value(List)).
% Value is Based on props
process_a4_e_1(Prefix,P,List):- ptgf(append(Prefix,[props(P)],PrefixP)), process_a4(PrefixP,List).
% Value is the props %process_a4_e_1(Prefix,P,_):- append(Prefix,[Cell],PrefixCell),  process_a4_key_list(PrefixCell,Cell,P).

identity_prop(tiles).
identity_prop(name).
identity_prop(name).

:- fixup_exports.
