/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/

% INSTRUCTIONS
% =swipl kaggle_arc.pl=
% =:- start_arc_http_server.=
%
% Then navigate to http://localhost:1677 in your browser

/*
:- module(kaggle_arc_ui_html,
  [ start_arc_http_server/0,
    stop_arc_http_server/0
  ]
).
*/
:- include(kaggle_arc_header).
:- use_module(library(xlisting/xlisting_web)).
:- use_module(library(debug)).
%:- use_module(library(thread_pool)).
%:- use_module(library(http/thread_httpd)).
%:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_path)).
:- use_module(library(http/websocket)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_dirindex)).


:- multifile
    http:location/3.                % Alias, Expansion, Options
:- dynamic
    http:location/3.                % Alias, Expansion, Options

%http:location(icons, root(icons), [ priority(-100) ]).
%http:location(css,   root(css),   [ priority(-100) ]).
%http:location(js,    root(js),    [ priority(-100) ]).
http:location(swish,    root(swish),    [ priority(-100) ]).

:- multifile
    user:file_search_path/2.
:- dynamic
    user:file_search_path/2.

%user:file_search_path(icons, library('http/web/icons')).
%user:file_search_path(css,   library('http/web/css')).
%user:file_search_path(js,    library('http/web/js')).


:- meta_predicate(noisey_debug(0)).
noisey_debug(Goal):- w_section(debug,Goal).

avacp(Vars):-  prolog_current_choice(Chp),'$attvars_after_choicepoint'(Chp, Vars).

%print_title(Title):- trim_newlines(ppt(Title)).

/*

memristive device arrays

when NNs first came out , they were oftenj based on trying to emulate the design of physical memristive device arrays
physical memristive device arrays (Neural Networks) were programmed (like in my highschool electronics class in the 1980s) like eeproms.. 
by shining UV light to reinitialize them.  The training routine was that we submitted a latched vector of electrical voltages.. Pushed STORE.. 
latched a new set of voltages pushed STORE etc (Idealy these vortages and resistences were computer from microphone or timed video output ) hoping 
at the end of training the bottem set (the output) of latched voltages ... (that when inverted (inverting the mask patterns) could recreate the input signals 
would be have the correct readings on the ohm meter (as submitted in training))

like for exmaple when i built the speech encoder/recoder in my teens 

*/

:- set_prolog_flag(verbose_autoload, false).

:- meta_predicate(w_section(0)).
w_section(Goal):- w_section_g(Goal),!.
w_section(Goal):- must_det_ll((goal_to_title_key(Goal,Title,Spyable), w_section(Title,Goal,Spyable,maybe))).

:- meta_predicate(w_section(+,0)).
w_section(_,Goal):- w_section_g(Goal),!.
w_section(AmmtShown,Goal):- var(AmmtShown),!,w_section(title(AmmtShown),Goal).
w_section(title(Title),Goal):- !, 
     must_det_ll((into_title_str(Title,TitleStr),title_string_to_functor(TitleStr,Goal,Spyable), 
     w_section(TitleStr,Goal,Spyable,maybe))).
w_section(AmmtShown,Goal):- is_amount_shown(AmmtShown),!,  
  must_det_ll((goal_to_title_key(Goal,Title,Spyable), w_section(Title,Goal,Spyable,AmmtShown))).
%w_section(Spyable,Goal):-  is_arc_spyable(Spyable),!,
%  must_det_ll((into_title_str(Spyable,Title), w_section(Title,Goal,Spyable,maybe))).
w_section(Title,Goal):-
  must_det_ll((into_title_str(Title,TitleStr), title_string_to_functor(TitleStr,Goal,Spyable), 
     w_section(TitleStr,Goal,Spyable,maybe))).


:- meta_predicate(w_section('+',0,'+')).

w_section(_,Goal,_):- w_section_g(Goal),!.

w_section(Title,Goal,AmmtShown):- AmmtShown==ran_collapsed, w_section_4(Title,Goal,debug,ran_collapsed).
w_section(Title,Goal,Spyable):- into_title_str(Title,TitleStr),Title\==TitleStr,!,w_section(TitleStr,Goal,Spyable).
w_section(Title,Goal,AmmtShown):- is_amount_shown(AmmtShown),!,
  must_det_ll((title_string_to_functor(Title,Goal,Spyable), w_section(Title,Goal,Spyable,AmmtShown))).
w_section(Title,Goal,Spyable):- must_det_ll(w_section(Title,Goal,Spyable,maybe)).

w_section_g(Goal):- fail, ignore(call(Goal)),!.
  
:- meta_predicate(w_section(+,0,+,+)).
%w_section(_,Goal,_,_):- w_section_g(Goal),!.
w_section(Title,Goal,Spyable,Showing):-  Showing==maybe,!, 
   (wants_output_for(Spyable) -> w_section(Title,Goal,Spyable,'1'); w_section(Title,Goal,Spyable,'0')).

w_section(Title,Goal,Spyable,Showing):-  Showing==toplevel,!, 
   ((flag('$w_section_depth',X,X), X=<1) -> w_section(Title,Goal,Spyable,'1'); w_section(Title,Goal,Spyable,'0')).

w_section(Title,Goal,Spyable,Showing):- 
  (into_0_or_1(Showing,Bool)->w_section_4(Title,Goal,Spyable,Bool);w_section_4(Title,Goal,Spyable,Showing)).

w_section_4(title(Title),Goal,Spyable,Showing):- nonvar(Title),!, w_section_4(Title,Goal,Spyable,Showing).
w_section_4(Title,Goal,Spyable,Showing):- wants_html, !, w_section_html(Title,Goal,Spyable,Showing).
w_section_4(Title,Goal,Spyable,Showing):- w_section_ansi(Title,Goal,Spyable,Showing).


w_section_ansi(Title0,Goal,Spyable,_Showing):- 
 must_det_ll((
  wots(Str,print(Title0)),
  nl_if_needed,dash_chars,
  MU = '', % was 'mu'
  once(nb_current('$w_section',Was);Was=[]), length(Was,Depth),!,wots(Ident,dash_chars(Depth,' ')),
  setup_call_cleanup(must_det_ll((format('~N~w~w!~w! ~@ |~n',[MU,Ident, Spyable, write(Str)]))),  
                     locally(b_setval('$w_section',[c(Spyable)|Was]),
                                      ignore(once(tabbed_print_im(Depth+2,in_w_section_depth(Goal))))), 
                     must_det_ll((format('~N~w\u00A1~w~w\u00A1 ',[Ident, MU,Spyable])))))).

in_w_section_depth(Goal):- 
   setup_call_cleanup(
       flag('$w_section_depth',Depth,Depth+1),
       catch(must_det_ll(Goal),E,u_dmsg(E-->Goal)),
       flag('$w_section_depth',_,Depth)).

clip_string(Attr,Len,SAttr):- atom_length(Attr,SLen),clip_string(SLen,Attr,Len,SAttr).
clip_string(SLen,Attr,Len,Attr):- SLen=<Len,!.
clip_string(_,Attr,Len,SAttr):- sub_string(Attr, 0, Len, _After, SAttr).


w_section_html(Title,Goal,Spyable,_Showing):- flag('$w_section_depth',X,X),X>2,!, w_section_html_real(Title,Goal,Spyable,panel_hidden,false).
w_section_html(Title,Goal,Spyable,ran_collapsed):- !, w_section_html_real(Title,Goal,Spyable,panel_shown,false).
w_section_html(Title,Goal,Spyable,Showing):- w_section_html_real(Title,Goal,Spyable,panel_shown,Showing).

w_section_html_real(Title,Goal,_Spyable,Class,Showing):-
 must_det_ll(( 
 copy_term(Goal,GoalC),
  gensym('accordian_css_',Sym),
  into_attribute(GoalC,Attr),
  clip_string(Attr,800,SAttr),
  into_attribute(Title,TAttr),
  clip_string(TAttr,800,STAttr),
  flag('$w_section_depth',X,X),
% Class=panel_shown, %(Showing == true -> Class=panel_shown; Class=panel_hidden),
 wots(AC,(wots(S,trim_newlines(wqs_c(Title))),(S==""-> write(Title) ; write(S)))),
 format('<br><button id="~w" onclick="top.clickAccordian(`~w`,false);" class="accordian" title="~w">',[Sym,Sym,STAttr]),
 format('<label for="~w" class="font(1)" class="accordian" title="~w">',[Sym,SAttr]),flush_output, 
 write(AC),
 format('</label>',[]),flush_output, 
 format('</button><br>'),flush_output, 
 format('<div id="~w_panel" onmouseover="top.activateMenu(`~w_link`);" class="~w , panel ">',[Sym,Sym,Class]), flush_output,
 format('<div class="wtopscroller11"><div class="dtopscroller1"></div></div>'),
 format('<div id="~w_scroller" class="wtopscroller22"><div class="dtopscroller2">',[Sym]),
 format('<script>top.add_top_scroller(document.currentScript)</script>',[]),flush_output,
 format('<script>top.addAccordian(document.getElementById("~w"),~w);</script>',[Sym,X]),

 once(ignore((goal_new_lines(in_w_section_depth(Goal),NewChars)))),
 format('<script>top.check_top_scroller(document.getElementById("~w"))</script>',[Sym]),flush_output,
 ignore((format('</div></div><script>top.commonLoading()</script></div>',[]),flush_output,
 ignore((  
   ((Showing==false,(var(NewChars);NewChars<600));(Showing==true,Class==panel_hidden);(Showing==false,Class==panel_shown))
    ->format('<script>document.getElementById("~w").click();</script>',[Sym]))))))).
    


:- meta_predicate(call_e_dmsg(+,0)).
call_e_dmsg(Goal):- catch(ignore(Goal),E,(u_dmsg(error_r(E=Goal)),writeln(E=Goal))).

:- meta_predicate(call_e_dmsg(0,-)).
goal_new_lines(Goal,NewChars):-
  current_output(Out),
  character_count(Out,Start),
  ignore(call_e_dmsg(Goal)),
  character_count(Out,End),
  NewChars is End - Start.


title_to_html(Title,HtmlTitle):- 
  into_title_str(Title,Str), with_pp(plain,into_attribute(Str,HtmlTitle)),!.
/*
old_write_expandable(Title,Goal,Showing):- 
   setup_call_cleanup(flag('$w_section_depth',Depth,Depth+1),
   w_section(Title,Goal,Showing),
   flag('$w_section_depth',_,Depth)).
*/
expandable_inlines:- expandable_mode(javascript).
expandable_mode(How):- var(How),!,luser_getval(expansion,How).
expandable_mode(How):- luser_getval(expansion,V),!,How==V.

:- luser_default(expansion,javascript).
:- luser_default(expansion,bfly).


:- meta_predicate(call_maybe_det(0,-)).
call_maybe_det(Goal,Det):- true,call_e_dmsg(Goal),deterministic(Det),true.


format_s(S):- atomic(S),!,format('~w',[S]).
format_s(S):- format('~s',[S]),flush_output.
format_s(Fmt,Args):-
  sformat(SFmt,'~s',[Fmt]),
  format(SFmt,Args).

:- meta_predicate(tabbed_print_im(+,0)).
tabbed_print_im(_Tab,Goal):- expandable_inlines, !, call_e_dmsg(Goal).
tabbed_print_im(Tab,Goal):- Tab2 is Tab, call_w_pad(Tab2,call_e_dmsg(Goal)).

:- meta_predicate(trim_newlines(0)).
trim_newlines(Goal):- wots(S,Goal),trim_leading_trailing_whitespace(S,SS),write(SS).
trim_leading_trailing_whitespace(In,Out):-
  cr_to_br_html(In,In0),
  cr_to_br_ansi(In0,IIn),
  split_string(IIn, " ", "\s\t\n\r",List), 
  atomics_to_string(List,' ',Out).

:- meta_predicate(goal_to_title_key(+,-,-)).
goal_to_title_key(Term,Title,Spyable):-
   must_det_ll((into_title_str(Term,Title),invent_key(Term,Spyable))).


invent_key(Term,Spyable):- \+ compound(Term),!, must_det_ll((
   into_title_str(Term,Str), string_to_functor(Str,Spyable))).
invent_key(Term,Spyable):- invent_key2(Term,Spyable).
invent_key2(_:Term,Spyable):- !, must_det_ll(invent_key(Term,Spyable)).
%invent_key2((Term,_),Spyable):- !, invent_key(Term,Spyable).
invent_key2(Term,DT):- copy_term(Term,Copy),must_det_ll(invent_key3(Copy,DT)),!.
invent_key3(Term,DT):- sub_term(E,Term), compound(E), \+ overly_plain_functor(E), one_invent_key(E,DT),!.
invent_key3(Term,DT):- one_invent_key(Term,DT).

one_invent_key(Term,Spyable):- \+ compound(Term), invent_key(Term,Spyable),!.
one_invent_key(Term,Spyable):- compound_name_arguments(Term,F,Args), 
   include(is_simple_title_arg,Args,ArgsO),

   ((ArgsO=@=Args;cant_re_arity(Term))-> =(Term,Spyable) ; compound_name_arguments(Spyable,F,ArgsO)),!.

cant_re_arity(S):- is_list(S),!.
cant_re_arity(format(_,_)).

is_simple_title_arg(X):- \+ compound(X),!, \+ is_gridoid(X),!.
is_simple_title_arg(Map):- is_vm_map(Map),!,fail.
is_simple_title_arg(_>_):-!.
is_simple_title_arg(_+_):-!.
is_simple_title_arg(X):- arg(_,X,A), compound(A),!,fail.
is_simple_title_arg(_).

overly_plain_functor(Term):- is_list(Term).
overly_plain_functor(_:_):- !.
overly_plain_functor(Term):- is_gridoid(Term),!.
overly_plain_functor(Term):- predicate_property(Term,meta_predicate(_)),!.
overly_plain_functor(Term):- compound_name_arity(Term,F,_),atom(F),upcase_atom(F,UC),!,downcase_atom(F,UC).


title_string_to_functor(_Str,Goal,Spyable):- invent_key(Goal,Spyable),!.
title_string_to_functor(Str,_Goal,Spyable):- callable(Str),invent_key(Str,Spyable),!.


string_to_functor(F,OO):- 
 to_case_breaks(F,X),include(\=(xti(_,punct)),X,O),my_maplist(arg(1),O,O1),
 my_maplist(any_to_atom,O1,O2),my_maplist(toLowercase,O2,O3),atomic_list_concat(O3,'_',OO),!.
string_to_functor(F,OO):- atom_string(OO,F),!.

%header_arg(_:Term,E):-!,header_arg(Term,E).
%header_arg(Term,E):- sub_term(E,Term), E\=@=Term, compound(E), \+ is_list(E).
%header_arg(Term,DT):- sub_term(Term,E), compound(E), \+ overly_plain_functor(E), data_type(E,DT),!.
%header_arg(Term,DT):- data_type(Term,DT),!.


test_w_section:- 
  w_section(info,
    forall(nth0(N,[a,b,c,d],E),writeln(N=E))).

%test_w_section:- 
     



:- multifile user:file_search_path/2.

%user:file_search_path(document_root,	'/srv/htdocs').
%user:file_search_path(document_root,  AbsolutePath):- arc_sub_path(arc_apps,AbsolutePath).

% user:file_search_path(arc_apps,  AbsolutePath):- arc_sub_path(arc_apps,AbsolutePath).

%:- http_handler('/ARC/', http_reply_from_files(arc_apps, []), [prefix]).


user:file_search_path(arc,  AbsolutePath):- arc_sub_path('.',AbsolutePath).

%:- http_handler('/swish/muarc/swish_config.json', swish_reply_config_root,[priority(200)]).
:- http_handler('/swish/muarc/arcproc_left', arcproc_left, [prefix,chunked,time_limit(30)]).
:- http_handler('/swish/muarc/arcproc_right', arcproc_right, [prefix,time_limit(30)]).
:- http_handler('/swish/muarc/arcproc_main', arcproc_main, [prefix,chunked,time_limit(3600)]). % one hour
:- http_handler('/swish/muarc/arcproc_iframe', arcproc_iframe, [prefix,chunked,time_limit(120)]).
:- http_handler('/arcproc_left', arcproc_left, [prefix,chunked,time_limit(30)]).
:- http_handler('/arcproc_right', arcproc_right, [prefix,time_limit(30)]). 
:- http_handler('/arcproc_main', arcproc_main, [prefix,chunked,time_limit(3600)]).% one hour
:- http_handler('/arcproc_iframe', arcproc_iframe, [prefix,chunked,time_limit(120)]).
:- http_handler('/swish/muarc/', swish_arc, [prefix]).
:- http_handler('/', swish_arc_root, [prefix]).


start_arc_http_server :-
    catch_log((default_port(Port),start_arc_http_server(Port))),
    nop(catch_log((start_arc_http_server(3020)))).



start_arc_http_server(Port):- atom_concat('http@',Port,IDName),thread_property(ID,status(running)),ID==IDName,!.
start_arc_http_server(Port) :-
   % catch_log(http_server(http_dispatch, [port(Port),ip('127.0.0.1'),workers(2)])),
   % catch_log(http_server(http_dispatch, [port(Port),ip('10.0.0.122'),workers(2)])),
    catch_log(http_server(http_dispatch, [port(Port),ip('10.0.0.122'),workers(10)])),
    !.

stop_arc_http_server :-
    default_port(Port),
    stop_arc_http_server(Port).
stop_arc_http_server(Port) :-
    http_stop_server(Port, []).

default_port(Port):- current_prolog_flag(http_port,Port).
default_port(1677).

%! web_socket_echo(+WebSocket) is nondet.
% This predicate is used to read in a message via websockets and echo it
% back to the client
web_socket_echo(WebSocket) :-
    ws_receive(WebSocket, Message, [format(json)]),
    ( Message.opcode == close
    -> true
    ; get_response_echo(Message.data, Response),
      write("Response: "), writeln(Response),
      ws_send(WebSocket, json(Response)),
      web_socket_echo(WebSocket)
    ).

%! get_response(+Message, -Response) is det.
% Pull the message content out of the JSON converted to a prolog dict
% then add the current time, then pass it back up to be sent to the
% client
get_response_echo(Message, Response) :-
  get_time(Time),
  Response = _{message:Message.message, time: Time}.


%:- http_handler('/swish/arc/user'/User), user(Method, User),[ method(Method), methods([get,post,put]) ]).


%:- http_handler('/favicon.ico', http_reply_file('favicon.ico', []), []).

%http:location(images,	root(images), []).

webui_tests:-
  test_print_tree,
  bfly_tests.

no_web_dbg:-!.
no_web_dbg:-
 nop(((
  unsetenv('DISPLAY'),
  no_xdbg_flags,
  no_x_flags,
  nop((set_prolog_flag(xpce,true)))))).

%:- no_web_dbg.

intern_arc_request_data(Request):-
  intern_request_data(Request),
  save_in_luser(Request).

save_in_luser(NV):- \+ compound(NV),!.
save_in_luser(NV):- is_list(NV),!,must_maplist(save_in_luser,NV),!.
save_in_luser(media(_,_,_,_)):-!.
save_in_luser(NV):- NV=..[N,V],save_in_luser(N,V),!.
save_in_luser(N=V):- save_in_luser(N,V),!.
save_in_luser(NV):- dmsg(not_save_in_luser(NV)),!.


save_in_luser(_,V):- is_list(V),save_in_luser(V).
%save_in_luser(session,V):- !, save_in_luser(V).
save_in_luser(N,V):- decode_luser(V,VV),save_in_luser2(N,VV).

save_in_luser2(task,V):- !, set_current_test(V),get_current_test(CT),dmsg(current_test(V-->CT)).
save_in_luser2(test_suite_name,V):- !, nop(maybe_set_suite(V)).
%save_in_luser2(cmd,V):-  !, ignore(set_test_cmd(V)),!.
save_in_luser2(N,V):- luser_setval(N,V), luser_default(N,V), 
  ignore((is_list(V),last(V,E),compound(E),save_in_luser(V))).

decode_luser(V,O):- url_decode_term(V,VV,_),VV\==V,decode_luser(VV,O),!.
decode_luser(V,O):- arc_atom_to_term(V,VV,_),VV\==V,decode_luser(VV,O),!.
decode_luser(V,V).

begin_arc_html_request(LR,Request):- var(Request), current_predicate(get_http_current_request/1),call(call,get_http_current_request,Request),
  nonvar(Request),!, begin_arc_html_request(LR,Request).
begin_arc_html_request(LR,Request):- 
 notrace(as_if_webui(begin_arc_html(LR,Request))).
begin_arc_html(LR,Request):-
  ignore((member(search(List),Request),member(task=Task,List),  atom_id(Task,ID), nop((dmsg(Task-> ID))), set_current_test(ID))),  
  %ignore((current_output(Out), arc_set_stream(Out,buffer(false)))), flush_output,
%  format('<!DOCTYPE html>',[]),
  ignore((LR==right,write_begin_html('ARC Solver',true))),
  ignore((LR==left, fail,write_begin_html('ARC Solver Menu',inline_to_bfly_html))),
  %ignore((is_cgi, (write_begin_html('ARC Solver')))),
  %ignore((is_cgi, (intern_arc_request_data(Request)))),

  /* arc_set_stream(Out,close_on_exec(false)),
  arc_set_stream(Out,close_on_abort(true)),
  arc_set_stream(Out,encoding(octet)),
  arc_set_stream(Out,write_errors(ignore)))),*/
  %ignore(set_test_param),
  
  nop(ensure_readable_html),
  flush_output,
  intern_arc_request_data(Request).

set_test_param:-!.
set_test_param:-
  ignore((as_if_webui((get_param_sess(task,Task), Task\=='',  Task\=="",
  atom_id(Task,ID), dmsg(Task-> ID), set_current_test(ID))))),!.

:- http_handler('/swish', http_redirect(moved, '/swish/'), []).
:- http_handler('/swish/muarc', http_redirect(moved, '/swish/muarc/'), []).
:- http_handler('/', http_redirect(moved, '/swish/muarc/kaggle_arc_ui_html.html'), []).

swish_arc(Request):- swish_arc_root(Request).
%  muarc_tmp:arc_directory(ARC_DIR),
%  http_reply_from_files(ARC_DIR, [unsafe(true), static_gzip(true)], Request).
wfln(_):- !.
wfln(P):- stream_property(X,file_no(2)),nl(X),writeln(X,P),flush_output(X).
%swish_arc_root(Request):-  arc_sub_path('.',DEMO), arc_reply_from_files(DEMO, Request),!.
swish_arc_root(Request):-  wfln(Request), ipe(attempt_file_reply(Request)),!.
swish_arc_root(Request):-  current_predicate(swish_page:swish_reply2/2),
  Options = [], 
  call(call,swish_page:swish_reply2(Options, Request)),!.
swish_arc_root(Request):- ipe(attempt_file_reply(Request)),!.
swish_arc_root(Request):- attempt_file_reply(Request).

ipe(G):- catch(G,E,(is_good_reply(E),throw(E))).

%is_good_reply(E):- wfln(E),fail.
is_good_reply(existence_error(_,_)):-!,fail.
is_good_reply(not_found(_)):-!,fail.
is_good_reply(http_reply(moved_temporary(_))):-!.
is_good_reply(http_reply(not_found(_))):-!,fail.
is_good_reply(http_reply(_)).
is_good_reply(http_reply(_,_)).
is_good_reply(http_reply(_,_,_)).

extra_files_in('/',DEMO):- arc_sub_path('.',DEMO).
extra_files_in(swish,'/opt/logicmoo_workspace/packs_web/swish/web').
%extra_files_in(swish,'/opt/logicmoo_workspace/packs_web/ClioPatria/web').
extra_files_in(www,'/opt/logicmoo_workspace/packs_web/ClioPatria/web').
extra_files_in(plugin,'/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/ext/swish/config-available/web/plugin').
extra_files_in('/','/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/ext/swish/pack/sCASP/prolog/scasp/web').

attempt_file_reply(Request):- 
  wfln(afr(Request)),
  select(path_info(PathInfo), Request, NewRequestP),
  extra_files_in(Swish,Dir),
  chop_begpath(Swish,PathInfo,Rest),
  append([path_info(Rest)], NewRequestP, NewRequest),  
  ipe(arc_reply_from_files(Dir, NewRequest)),!.

attempt_file_reply(Request):- 
  select(path_info(PathInfo), Request, NewRequestP),
  extra_files_in(Swish,_),
  chop_begpath(Swish,PathInfo,Rest),
  extra_files_in(_,Dir),
  append([path_info(Rest)], NewRequestP, NewRequest),  
  ipe(arc_reply_from_files(Dir, NewRequest)),!.

attempt_file_reply(Request):- 
 select(path_info(PathInfo), Request, NewRequestP),
  extra_files_in(Swish,_),
  chop_begpath(Swish,PathInfo,Rest), 
  append([path_info(Rest)], NewRequestP, NewRequest),
  extra_files_in(_,Dir),
  absolute_file_name(Rest, Path, [relative_to(Dir), access(exist)]),
  arc_reply_file(Path, NewRequest),!.

attempt_file_reply(Request):- 
 select(path_info(PathInfo), Request, NewRequestP),
  chop_begpath('/',PathInfo,Rest), 
  append([path_info(Rest)], NewRequestP, NewRequest),
  extra_files_in(_,Dir),
  absolute_file_name(Rest, Path, [relative_to(Dir), access(exist)]),
  arc_reply_file(Path, NewRequest),!.

attempt_file_reply(Request):- 
  extra_files_in(_,Dir),
  ipe(arc_reply_from_files(Dir,Request)),!.

attempt_file_reply(Request):- 
  member(path_info(PathInfo), Request),
  atom_concat('/',Rest,PathInfo),
  extra_files_in(_,Dir), absolute_file_name(Rest, Path, [relative_to(Dir), access(exist)]),
  mime_ext(Path,Ext),
  throw(http_reply(file(Ext,Path),[])).

attempt_file_reply(Request):- 
  member(path_info(PathInfo), Request),
  absolute_file_name(PathInfo, Path),
  mime_ext(Path,Ext),
  throw(http_reply(file(Ext,Path),[])).

attempt_file_reply(Request):- 
  member(path_info(PathInfo), Request),
  extra_files_in(_,Dir), absolute_file_name(PathInfo, Path, [relative_to(Dir), access(exist)]),
  mime_ext(Path,Ext),
  throw(http_reply(file(Ext,Path),[])).


mime_ext(Path,Ext):- file_name_extension(_,Path0,Path),Path0\=='',!,mime_ext(Path0,Ext).
mime_ext(js,'text/javascript'):-!.
mime_ext(txt,'text/plain'):- !.
mime_ext(X,Mime):- mime_ext(X),!,atom_concat('text/',X,Mime).
mime_ext(_,'text/html'):- !.
mime_ext(html).
mime_ext(css).


arc_reply_from_files(Dir, Request):- ipe(http_reply_from_files(Dir, [unsafe(false), static_gzip(true)], Request)).
arc_reply_from_files(Dir, Request):- ipe(http_reply_from_files(Dir, [unsafe(true), static_gzip(true)], Request)).
arc_reply_from_files(Dir, Request):- ipe(http_reply_from_files(Dir, [], Request)).
arc_reply_file(Path,Request):- ipe(http_reply_file(Path, [unsafe(false)], Request)).
arc_reply_file(Path,Request):- ipe(http_reply_file(Path, [unsafe(true)], Request)).
arc_reply_file(Path,Request):- ipe(http_reply_file(Path, [], Request)).

finish_chop(PathInfo,Right):- atom_concat('/',Right,PathInfo),!.
finish_chop(PathInfo,PathInfo).
chop_begpath(Swish,PathInfo,Rest):- atom_concat(Swish,Rest0,PathInfo),finish_chop(Rest0,Rest).
chop_begpath(Swish,PathInfo,Rest):- atom_concat('/',Right,PathInfo),chop_begpath(Swish,Right,Rest).

arcproc_left(Request):- 
 start_arc_html,
  %set_http_debug_error(true),
  wots(_,intern_arc_request_data(Request)),
   with_toplevel_pp(http,
    (as_if_webui(((begin_arc_html_request(left,Request),
      arc_html_format([handler_logicmoo_left,write_end_html])))))),!.
arcproc_left(Request):- xlisting_web:handler_logicmoo_cyclone(Request),!.

%arcproc_right(Request):- swish_arc(Request),!.

arc_html_format(TextAndGoal):- is_cgi,!,arc_inline_html_format(TextAndGoal).
arc_html_format(TextAndGoal):- bfly_in_out(call(call,inline_html_format(TextAndGoal))).

arc_inline_html_format(Var):- var(Var),!, arc_inline_html_format(writeln(var(Var))).
arc_inline_html_format(S):- (string(S);is_codelist(S);is_charlist(S)),!,format('~s',[S]).
arc_inline_html_format(TextAndGoal):- is_list(TextAndGoal),!,my_maplist(arc_inline_html_format,TextAndGoal).
%arc_inline_html_format(Msg):- flush_output_safe, u_dmsg(call(Msg)),fail.
arc_inline_html_format(format(H,V)):-!, format(H,V).
arc_inline_html_format(TextAndGoal):- inline_html_format(TextAndGoal),flush_output_safe.

% arc_find_tests(menu):- ignore(menu).
arc_find_tests(F):- find_tests(F).

:- dynamic(xlisting_whook:offer_testcase/1).
:- multifile(xlisting_whook:offer_testcase/1).
xlisting_whook:offer_testcase(F):- arc_find_tests(F).

handler_logicmoo_left:- handler_logicmoo_menu,!.
handler_logicmoo_menu:-   
 %set_prolog_flag(gui_tracer,false), 
  %ignore(arc_http_nav_menu),
  write_nav('mySideNavL','L','Suite Menu',ignore(full_test_suite_list)),
  write_nav('mySideNavR','R','WebUI Menu',ignore((test_webui_menu,show_tests))),
  !. 

test_webui_menu :- as_if_webui((write_menu_opts('i'))).

write_nav(ID,LR,_Title,Goal):- 
 format('<div id="~w" class="sidenav~w">
  <a href="javascript:void(0)" class="closebtn" onclick="toggleNav~w(\'~w\')">&times;</a>',[ID,LR,LR,ID]),
  call_e_dmsg(Goal),write('</div>').



set_http_debug_error(Bool):- 
  %set_prolog_flag(gui_tracer,false),
  set_prolog_flag(debug,Bool),
  set_prolog_flag(gui_tracer,Bool),
  set_prolog_flag(debug_on_error,Bool),
  set_prolog_flag(debug_on_interrupt,Bool),
  set_prolog_flag(determinism_error,silent),
  set_prolog_flag(report_error,Bool),
  %set_html_stream_encoding,
  current_output(Out),current_input(In),stream_property(Err,file_no(2)),
  if_t(Bool,
  ((http_debug_console,
    arc_set_stream(Out,alias(current_output)),
    arc_set_stream(In,  alias(current_input))))),
  stream_property(Strm,alias(user_error)),
  (Strm\==Out -> true ; 
   (arc_set_stream(Err,alias(user_error)),
    set_prolog_IO(In,Out,Err),
    arc_set_stream(Out,alias(user_output)),
    arc_set_stream(In,  alias(user_input)),
    arc_set_stream(Out, alias(user_output)),
    arc_set_stream(Err, alias(user_error)),
    arc_set_stream(In,  alias(current_input)),
    arc_set_stream(Out, alias(current_output)),
    !)),
  %arc_set_stream(Out,representation_errors(unicode)),
  %arc_set_stream(Out,close_on_exec(false)),
  %arc_set_stream(Out,close_on_abort(false)),
  arc_set_stream(Out,write_errors(ignore)),
  arc_set_stream(Out,buffer(false)),
  arc_set_stream(Out,tty(false)), 
  arc_set_stream(Out,representation_errors(unicode)),
  %set_html_stream_encoding,
  %http_debug_console,
  nop((Bool==false->nop(set_prolog_flag(on_error,halt));set_prolog_flag(on_error,status))).

%http_debug_console:-!.
http_debug_console:- current_prolog_flag(xpce,true),
  getenv('DISPLAY',Addr),atom_length(Addr,Len),Len>1,
  nodebug,notrace,
  current_output(Out),
  nl(Out),flush_output(Out),
  %attach_console,
  current_input(In),
  current_output(Err),
  set_prolog_IO(In,Out,Err).
   %gtrace.

http_debug_console:- nodebug,notrace,!.

echo_file(File):- read_file_to_string(File,Str,[]),write(Str).

:- volatile(wrote_arc_start/1).
:- thread_local(wrote_arc_start/1).

write_arc_start_script(Where):- var(Where), current_output(Where), write_arc_start_script(Where).
write_arc_start_script(Where):- wrote_arc_start(Where),!.
write_arc_start_script(Where):- nop(write_arc_start(Where)).


write_arc_start(Where):- var(Where), current_output(Where), write_arc_start(Where).
write_arc_start(Where):- wrote_arc_start(Where),!.
write_arc_start(Where):- asserta(wrote_arc_start(Where)),
  format(Where,'<script type="text/javascript" href="/swish/muarc/kaggle_arc_ui_html.js"></script>',[]),
  %format(Where,'<script type="text/javascript">~@</script>',[echo_file('kaggle_arc_ui_html.js')]),
  format(Where,'<style>~@</style>',[echo_file('kaggle_arc_ui_html.css')]).
write_arc_end(Where):- retractall(wrote_arc_start(Where)).
/*
old_write_arc_start:- get_time(Now),Now10M is floor(Now * 10_000_000),
  update_changes, format('<html><head>
  <script type="text/javascript" href="./kaggle_arc_ui_html.js?time=~|~`0t~d~5+"></script>
  <link rel="stylesheet" type="text/css" href="./kaggle_arc_ui_html.css?time=~|~`0t~d~5+">
</head>
<body>',[Now10M,Now10M]).
*/

%map_html_entities_mono(I,O):- atom_codes(O,I),!.
map_html_entities_mono(I,O):- map_html_entities(I,O).

map_html_entities(Code,S):- Code>160, !, sformat(S, '&#~w;',[Code]).
map_html_entities(Code,S):- Code<33, !, sformat(S, '&#~w;',[Code]).
/*
map_html_entities(Code,S):- Code == 124,!,sformat(S, '&#~w;',[Code]).
map_html_entities(Code,S):- Code>255, !, sformat(S, '&#~w;',[Code]).
map_html_entities(62,'&gt;'). map_html_entities(60,'&lt;'). map_html_entities(38,'&amp;'). map_html_entities(32,'&nbsp;').
*/
map_html_entities(Code,S):- name(S,[Code]),!.

bformatw(G):- g_out(bformat(G)).

with_style(S,G):-
 (arc_html -> 
   sccs(format('<span style="~w">',[S]),call_e_dmsg(G),write('</span>')) 
    ; call(G)).

html_echo(G)--> [G].

mforeach(Generator, Rule) -->
    foreach(Generator, Rule, []).

:- use_module(library(dcg/high_order),[foreach // 3]).


print_card_list(List):- print_card_list(1,List).
print_card_list(N,List):- with_tag_class(div,'grow',print_card_l(N,List)).
%print_card_list(N,List):- with_tag('p',with_tag_class(div,'grow',print_card_l(N,List))).

print_card_l(_,Nil):- Nil==[],!.
print_card_l(N,[H|T]):- print_card_n(N,H), N2 is N+1, print_card_l(N2,T).

print_card_n(N,Var):- \+ callable(Var),!,print_tb_card(print_wrappable(Var),print_wrappable(N)).

print_card_n(N,H):- H= [I], is_object(I), !,  print_card_n(N,I).
print_card_n(N,H):- H= [I], is_gridoid(I), \+ is_gridoid(H), !,  print_card_n(N,I).

print_card_n(N,card(T,B)):- !, print_tb_card(T,(wqs_c(N),call_e_dmsg(B))).
print_card_n(N,GF):- grid_footer(GF,G,F), is_gridoid(G), data_type(G,DT), print_tb_card(print_grid(G),wqs_c([N,DT,F])),!.


print_card_n(N,I):- is_object(I),g_display(I,G),print_card_n(N,G),!.
print_card_n(N,G):- is_gridoid(G), data_type(G,DT), print_tb_card(print_grid(G),wqs_c([N,DT])),!.

print_card_n(_,H):- string(H),!,write(H).
print_card_n(_,H):- atom(H),!,write(H).
print_card_n(N,H):- callable_arity(H,0),print_tb_card(call_e_dmsg(H),wqs_c([N,H])),!.
print_card_n(N,H):- callable_arity(H,1),call(H,R), data_type(R,DT),print_tb_card(wqs_c([N,H,DT]),R),!.
print_card_n(N,H):- data_type(H,DT),print_tb_card(pp(H),wqs_c([N,DT])),!.


print_tb_card(Top,Bottem):- ( \+ is_cgi ; ansi_main), !, sccs(Top,nl_if_needed,wqs_c(Bottem)),!.

print_tb_card(Top,Bottem):- 
  wots(S,call_e_dmsg(Bottem)),
  replace_in_string(['<br>'='\n','"'=' ','<hr>'='\n','<br/>'='\n','\n'=' ','  '=' ','  '=' '],S,RS),
  %RS=S,
  locally(nb_setval(grid_footer,RS),
    (call_e_dmsg(Top),
     ignore((nb_current(grid_footer,S),S\==[],write(S))))).
/*
print_tb_card(Top,Bottem):-
  with_tag_class(div,"column , wrapper",
   with_tag_class(div,"card , first_div",
     (call(Top), with_tag_class(div,"container, second_div",print_wrappable(Bottem))))).
*/

print_wrappable(L):- \+ is_cgi, !, wqs_c(L).
print_wrappable(L):- with_tag_class(div,wrappable,wqs_c(L)).

print_title(Var):- (var(Var);Var==[]),!.
print_title(Title):- into_title_str(Title,Str), trim_newlines(wqs_c(Str)).

% width: fit-content
% print_table(ListOfLists):- setup_call_cleanup(write('<table style="width: fit-content;m width: 100%; border: 0px">'), my_maplist(html_table_row,ListOfLists), write('</table>')),!.

is_real_grid(Grid):- is_grid(Grid),!,mapgrid(can_be_cell,Grid).
can_be_cell(Cell):-var(Cell),!.
can_be_cell(Cell):- is_color(Cell),!.
can_be_cell(Cell):- integer(Cell),!.
can_be_cell(Cell):- \+ callable(Cell),!,fail.
can_be_cell(Cell):- \+ cant_be_cell(Cell).
can_be_cell(_).

cant_be_cell(T):- is_list(T),!.
cant_be_cell(T):- atom(T), !, \+ is_color(T).


%print_table(Row):- \+ is_list(Row),!,print_cell(Row).
%print_table(table(Rows)):- !, make_rows_same_length(Rows,LLRows),!, print_table_rows(LLRows).
%print_table(Grid):- is_real_grid(Grid),!,print_grid(Grid),!.
% % print_table(Rows):- Rows=[T],\+ is_list(T),!,print_cell(T).
% % print_table(Rows):- Rows=[[T]],!,print_cell([[T]]).
% % print_table(Grid):- is_grid(Grid),!,print_cell(Grid).
%print_table(Rows):- make_rows_same_length(Rows,LLRows),!, print_table_rows(LLRows).
print_table(Rows):- print_table_rows(Rows).
print_table_rows(Row):- \+ is_list(Row),!,print_table_rows([Row]).
print_table_rows(Rows):- with_tag_class('table','tblo', my_maplist(html_table_row,Rows)).

%html_table_row(Row):- \+ is_list(Row),!,html_table_row([Row]).
html_table_row(Cols):- with_tag('tr',my_maplist(html_table_col,Cols)).

html_table_col(td(H)):-with_tag('td',html_table_cell(H)).
html_table_col(th(H)):-with_tag('th',html_table_cell(H)).
html_table_col(H):- with_tag('td',html_table_cell(H)).
%html_table_cell(Grid):- notrace(catch((wots(S,print_cell(Grid)),write(S)),_,true)).
html_table_cell(Grid):- notrace(catch((wots_html(S,print_cell(Grid)),write(S)),_,true)).

print_cell(call(Grid)):- !, ignore(call(Grid)).
print_cell(Grid):- is_real_grid(Grid),!,print_grid(Grid).
print_cell([V]):- !,print_cell(V).
print_cell([]):- !,write_nbsp.
%print_cell(V):- is_list(V),findall([E],member(E,V),L),!,print_table(L).
print_cell(H):- print_card_n('',H).

make_rows_same_length(ListOfLists,ListOfRows):-
  my_maplist(length,ListOfLists,Lens),
  sort(Lens,ListLens),last(ListLens,Len),
  my_maplist(slack_rows(Len),ListOfLists,ListOfRows).

slack_rows(Len,List,Row):-length(Row,Len),slack_into_rows(List,Row).
blank_stuff(Col):- ignore(Col="&nbsp;").
slack_into_rows(List,Row):- append(List,Stf,Row),!,my_maplist(blank_stuff,Stf).
slack_into_rows(List,Row):- append(Row,_DropStf,List),!.

:- meta_predicate(as_html_encoded(0)).
as_html_encoded(Goal):- 
  html_stream_encoding(UTF8),
  with_enc(UTF8,Goal).

:- meta_predicate(with_enc(+,0)).
with_enc(Enc,Goal):-
 stream_property(current_output,encoding(Was)),
 setup_call_cleanup(current_prolog_flag(encoding,EncWas),
 (( ignore(catch(set_prolog_flag(encoding,Enc),_,true)),
    current_prolog_flag(encoding,EncNew),
     locally(set_prolog_flag(encoding,EncNew),
 setup_call_cleanup(
       set_stream_encoding(Enc),
   Goal,
       set_stream_encoding(Was))))),
       set_prolog_flag(encoding,EncWas)).

html_stream_encoding(octet).
%html_stream_encoding(utf8).

set_html_stream_encoding:- html_stream_encoding(UTF8),set_html_stream_encoding(UTF8).
set_html_stream_encoding(_UTF8):-!.
set_html_stream_encoding(UTF8):- 
    %'$set_encoding'(_,[UTF8]),
    set_url_encoding(_,UTF8), 
    set_prolog_flag(encoding,UTF8),
    ignore(catch(arc_set_stream(current_output,encoding(UTF8)),_,true)).

set_stream_encoding(Text):- ignore(catch(set_stream(current_output,encoding(Text)),_,true)).
 %set_prolog_flag(encoding,Text),
/*
 notrace((
 ignore(catch(arc_set_stream(current_output,encoding(Text)),_,true)),
 %ignore(catch(arc_set_stream(user_output,encoding(Text)),_,true)),
 ignore(catch(arc_set_stream(current_output,tty(true)),_,true)))),!.
*/
/*
subst_entity(S,SS):- string(S),atom_chars(S,Codes),subst_entity1(Codes,CS),sformat(SS,'~s',[CS]).
subst_entity1([],[]):-!.
subst_entity1(['\\','u',A,B,C,D|Codes],[S|SS]):- into_entity(A,B,C,D,S), subst_entity1(Codes,SS).
subst_entity1([C|Codes],[S|SS]):- subst_entity1(Codes,SS).
into_entity(A,B,C,D,S):- atom_codes('\u0Aaa',C)
*/


arc_session_vars:-   
 write_arc_start(Where),
 arc_session_vars_pt1,
 arc_session_vars_pt3,
  write_arc_end(Where),!.

arc_session_vars_pt1:-   
  show_console_info,
  show_indiv_filters('<br/>'),!,
  findall(N=V,luser_getval(N,V),List), sort_safe(List,SList), print_table_vars(SList), 
  arc_session_vars_pt2(SList).
arc_session_vars_pt2(ASList):- 
  findall(NG=V,(luser_getval_g(NG,N,V),\+ member(N=V,ASList)),List), sort_safe(List,SList),print_table_vars(SList).

arc_session_vars_pt3:-
  current_output(Out), 
  findall(N=V,(member(N,[Out,current_input,user_error]),stream_property(N,V)),SList2),
  print_table_vars(SList2).

print_table_vars(SList):- 
   with_tag_class(table,session_vars,
     forall(member(N=V,SList),
      (wots(NS,print(N)),
       wots(VS,ptcol_html_scrollable(V)),
       html_table_row([NS,write(VS)])))).

luser_getval_g(nb_current(N),N,V):- nb_current(N,V).
luser_getval_g(global_user(N),N,V):- arc_user_prop(global,N,V).
luser_getval_g(prolog_flag(N),N,V):- current_prolog_flag(N,V).

print_grid_html:- arc_grid(Grid),print_grid_html(_SH,_SV,_EH,_EV,Grid).
%print_grid_html(Grid):-print_grid_html(_SH,_SV,_EH,_EV,Grid),!.
%print_grid_html(Name,Grid):- !,print_table([[print_grid(A),print_grid(B)]])
%print_grid_html(Name,Grid):-print_grid(_OH,_OV,Name,Grid),!.
%print_grid_html(SH,SV,EH,EV,Grid):- print_grid_html_old(SH,SV,EH,EV,Grid),!.
%print_grid_html(SH,SV,EH,EV,Grid):- write('<pre>\n'),print_grid_ansi(SH,SV,EH,EV,Grid),!,write('\n').
print_grid_html(SH,SV,EH,EV,Grid):- 
(plain_var(EH) ->grid_size(Grid,EH,_) ; true),ignore(SH=1),
  (plain_var(EV) ->grid_size(Grid,_,EV) ; true),ignore(SV=1),
    bg_sym_ui(BGC),
     with_luser(alt_grid_dot,'@',print_grid_http_bound(BGC,SH,SV,EH,EV,Grid)),!.

has_content(Header):- nonvar(Header), Header\==[], Header\=='',Header\=="''&nbsp;",Header\=='\'\'&nbsp;', Header\=="".


print_ss_html(TitleColor,G1,N1,LW,G2,N2):- \+ wants_html,!,  print_side_by_side_ansi(TitleColor,G1,N1,LW,G2,N2),!.

print_ss_html(TitleColor,In,FIn,_LW,Out,FOut):-
 wots_vs(HIn, with_color_span(color(TitleColor),wqs_c(FIn))),
 wots_vs(HOut, with_color_span(color(TitleColor),wqs_c(FOut))),
 print_ss_html_h(In,HIn,Out,HOut).

/*
print_ss_html(TitleColor,G1,N1,_LW,G2,N2):-  
 into_nv_cmd(N1,N1Cmd),
 into_nv_cmd(N2,N2Cmd),
 (( print_ss_html_pair(TitleColor, 
    wqs(N1),navCmd(N1Cmd),N1,G1,wqs(N1),
    wqs(N2),navCmd(N2Cmd),N2,G2,wqs(N2)))).   
*/

print_ss_html_pair(TitleColor,
                    NameIn,    NameInCmd, ID1,In, LeftTitle,
                    TestAtom,TestAtomCmd, ID2,Out,RightTitle):-
  wots_vs(HIn, with_color_span(color(TitleColor),write_nav_cmd(NameIn,NameInCmd))),
  wots_vs(HOut,with_color_span(color(TitleColor),write_nav_cmd(TestAtom,TestAtomCmd))),
  once((data_type(In,S1), data_type(Out,S2))),
  wots_vs(FIn, format_footer(TitleColor, LeftTitle,S1)),
  wots_vs(FOut,format_footer(TitleColor,RightTitle,S2)),
 grid_size(In,H1,V1), grid_size(Out,H2,V2),
 grid_bgc(In,BGC1), grid_bgc(Out,BGC2),
 print_table([[HIn,'   ',HOut],[
   call(locally(nb_setval(grid_footer,FIn),with_other_grid(Out,print_grid_http_bound(ID1,BGC1,1,1,H1,V1,In)))),'-->',
   call(locally(nb_setval(grid_footer,FOut),with_other_grid(In,print_grid_http_bound(ID2,BGC2,1,1,H2,V2,Out))))]]).

into_nv_cmd(N2,N2Cmd):- into_attribute(N2,N2Cmd).


print_ss_html_h(In,HIn,Out,HOut):- 
  print_table([[HIn,'   ',HOut],
  [ call(locally(nb_setval(grid_footer,[]),with_other_grid(Out,print_grid(In)))),
    '-->',
    call(locally(nb_setval(grid_footer,[]),with_other_grid(In,print_grid(Out))))]]).
print_ss_html_h(In,HIn,Out,HOut):- 
 print_table([
  [call(locally(nb_setval(grid_footer,HIn),with_other_grid(Out,print_grid(In)))),
   '-->',
   call(locally(nb_setval(grid_footer,HOut),with_other_grid(In,print_grid(Out))))]]).

print_grid_http_bound(_BGC,SH,SV,EH,EV,Grid):- ansi_main,!,print_grid_ansi_real(SH,SV,EH,EV,Grid).

/*
print_grid_http_bound(BGC,SH,SV,EH,EV,Grid):-
  grid_to_task_pair(Grid,TaskIDSubTask),
  grid_to_image_oid(Grid,OID),
  format('<img id="~w" name="~w" title="grid(~w,~w)" src="(none)"/>',[TaskIDSubTask,OID,EH,EV]),
  format('<script> var var oid="~w"; name="~w" title="grid(~w,~w)" src="(none)"/>',[TaskIDSubTask,OID,EH,EV]),
*/

print_grid_http_bound(BGC,SH,SV,EH,EV,Grid):- 
  grid_to_task_pair(Grid,TaskIDSubTask),
  ignore(grid_to_image_oid(Grid,ID)),
  (should_shrink_grid_half(Grid,EH,EV)->(GridClass=grid_table_shrink,HM=8);(GridClass=grid_table,HM=16)),

  Width is EH-SH+1,
  TWidth is Width*HM,
  HGHT is EV-SV+2,
  THGHT is HGHT*HM,

 (nb_take_content(grid_title,Title)->true;Title=_),
   
     wots(GTitle, 
             (format('grid(~w,~w)',[EH,EV]),             
             (nonvar(Title)-> format(' ~w',[Title]) ; true),
             (nonvar(ID)-> format(' ~w',[ID]) ; true))),
  into_attribute(GTitle,GTitleAttr),

  format('<table style="max-width: ~wpx; max-height: ~wpx;" id="~w" class="~w selectable" title="~w">',
                      [TWidth,THGHT,TaskIDSubTask,GridClass,GTitleAttr]),

  ignore((nb_take_content(grid_header,Header),
    with_tag('tr',format('<th style="max-width: ~wpx" colspan="~w" class="wrappable">~w</th>',[TWidth,Width,Header])))),

  print_grid_http_bound(_,BGC,SH,SV,EH,EV,Grid),

  ignore((nb_take_content(grid_footer,Footer),
    with_tag('tr',format('<th style="max-width: ~wpx" colspan="~w" class="wrappable">~w</th>',[TWidth,Width,Footer])))),
  write('</table>'),

  ignore((cvtToIMG,
     ((grid_to_image_oid(Grid,ID),contains_data(ID))->prev_intoNamedImg(ID); cvtToIMG(TaskIDSubTask)))).

cvtToIMG:- false.
cvtToIMG(TaskIDSubTask):-
  if_t(contains_data(TaskIDSubTask),
    run_script('cvtToIMG(`~w`);',[TaskIDSubTask])),!.

prev_intoNamedImg(OID):-
 if_t(contains_data(OID),
  (asserta(did_prev_intoNamedImg(OID)),
   run_script_now(' top.intoNamedImg(true,true,prev,`~w`); ',[OID]))).

contains_data(OID):- atomic(OID),sformat(S,'~w',[OID]),atom_length(S,Len),Len>=4.

run_script(Fmt,Args):-
 gensym(script_id_,ScriptID),
 format('<script id="~w">
 var me = document.currentScript; 
 var prev = me.previousElementSibling;
 setTimeout(function(){ 
 ~@ 
 prev = me.previousElementSibling; alreadyRan(me,prev);}, 30000); 
</script>',[ScriptID,format(Fmt,Args)]).

run_script_now(Fmt,Args):-
 gensym(script_id_,ScriptID),
 format('<script id="~w">
 var me = document.currentScript; 
 var prev = me.previousElementSibling;
 ~@ 
 prev = me.previousElementSibling; alreadyRan(me,prev); 
</script>',[ScriptID,format(Fmt,Args)]).


:- dynamic(page_has_grid_out/2).
:- retractall(page_has_grid_out(_,_)).



into_gui_item(I,S):- wots(S,print_gui_item(I)).

current_output_page(Page):- current_output(Page).
print_gui_item(Obj):- ansi_main,!,draw_gui_item(Obj),!.
print_gui_item(Obj):- obj_to_oid(Obj,OID),current_output_page(Page),print_gui_item(Page,OID,Obj).

print_gui_item(Page,OID,_Obj):- page_has_grid_out(Page,OID),!,  
  with_output_to(Page,((
  format('<img src="https://via.placeholder.com/100" class="placeholder" name="~w"/>',[OID]),
  run_script('prev.setAttribute("src",top.milledImages["~w"]);',[OID])))).
print_gui_item(_Page,_OID,Obj):- draw_gui_item(Obj),!.
print_gui_item(Page,OID,Obj):- assert(page_has_grid_out(Page,OID)),
  with_output_to(Page,((draw_gui_item(Obj), prev_intoNamedImg(OID)))).
  %print_gui_item(Obj).

draw_gui_item(Obj):- is_object(Obj),!,
  ignore(loc2D(Obj,H,V)),
  obj_to_oid(Obj,OID),
  object_glyph(Obj,O),
  global_grid(Obj,Grid),!,
  with_tag_ats(div,[name(OID),title(Obj),class(selectable)],
    (print_grid(Grid),nl_if_needed,format('~w (~w,~w)',[O,H,V]))).
draw_gui_item(Obj):- is_gridoid(Obj),!,print_grid(Obj).
draw_gui_item(Obj):-
  global_grid(Obj,OID,Grid), numbervars(Grid,666,_,[attvars(bind),singletons(true)]), assert_grid_oid(Grid,OID),
  locally(nb_setval(grid_title,OID),print_grid(OID,Grid)),!.


nb_take_content(_Var,Footer):- nonvar(Footer),has_content(Footer),!.
nb_take_content(Name,Footer):- nb_has_content(Name,Footer),nb_setval(Name,[]).
nb_has_content(Name,Footer):- nb_current(Name,Footer),has_content(Footer).

grid_member(E,Grid):- is_grid(Grid), append(Grid,List),!,member(E,List).

should_shrink_half(EH,_EV,OH,_OV):- EH>OH*2,!.
should_shrink_half(_EH,EV,_OH,OV):- EV>OV*2,!.
should_shrink_half(EH,EV,OH,OV):- EH>=(OH*2),EV>=(OV*2).

should_shrink_grid_half(Grid,EH,EV):- is_grid(Grid), 
 \+ ( grid_member(E,Grid), compound(E)), !, 
 (EH>14 ; EV>14 ; 
   ((EH>=6,EV>=6),other_grid(Grid,Other),is_grid(Other),grid_size(Other,OH,OV),OH>0,OV>0,should_shrink_half(EH,EV,OH,OV))),!.


print_grid_http_bound(ID,BGC,SH,SV,EH,EV,Grid):-  

 


 (nb_take_content(grid_title,Title)->true;Title=_),

 ignore((nonvar(ID),

   
     wots(GTitle, 
             (format('grid(~w,~w)',[EH,EV]),             
             (nonvar(Title)-> format(' ~w',[Title]) ; true),
             (nonvar(ID)-> format(' ~w',[ID]) ; true))),
     into_attribute(GTitle,GTitleAttr),
   (should_shrink_grid_half(Grid,EH,EV)->GridClass=grid_table_shrink;GridClass=grid_table),
   format('<table id="~w" class="~w selectable" title="~w">',[ID,GridClass,GTitleAttr])

   )),

 forall(between(SV,EV,V),
    with_tag('tr',((
    forall(between(SH,EH,H),
     (( must_det_ll((once((hv_cg_value(Grid,CG,H,V);CG=BGC)), 
       only_color_data_or_atom(CG,Color),
       into_html_color(Color,HTMLColor))),
         (var(Title)-> (into_title_str_attr(CG,Str),format('<td bgcolor="~w" title="~w (~w,~w)">',[HTMLColor,Str,H/EH,V/EV]));
           format('<td bgcolor="~w">',[HTMLColor])),
          catch(print_hg1(CG),E,writeln(CG=E)),write('</td>')))))))),

  ignore((nonvar(ID),write('</table>'))),
  ignore((nonvar(ID),cvtToIMG(ID))),!.

into_title_str_attr(CG,Str):- into_title_str(CG,A), into_attribute(A,Str),!.


into_html_color(Color,'#333'):- plain_var(Color),!.
into_html_color(Color,'#060'):- var(Color),!.
into_html_color(bg,'#123').
into_html_color(wbg,'#321').
into_html_color(purple,'#A45EE5').
into_html_color(yellow,'#f1ff62').
into_html_color(orange,'#FFA500').
into_html_color(brown,'#8B4513').
into_html_color(fg,'#456').
into_html_color(wfg,'#654').
into_html_color(green,'#50D050').
into_html_color(Color,Color).

print_hg1(Plain):- plain_var(Plain), write('.').
print_hg1(Wbg):- Wbg==bg, write('&nbsp;').
print_hg1(Wbg):- Wbg==wbg, write('&nbsp;').
print_hg1(Wbg):- is_black(Wbg), write('&nbsp;').
print_hg1(Wbg):- Wbg==' ', write('&nbsp;').
print_hg1(Wbg):- is_fg_color(Wbg),is_real_color(Wbg), write('&nbsp;').
print_hg1(Wbg):- atom(Wbg),atom_concat('#',L,Wbg),L\=='', write(' ').
print_hg1(X):- print_g1(X),!.

/*
pri=======================nt_grid_http_bound(BGC,SH,SV,EH,EV,Grid):- 
  arc_html_format(`<code>tbody td:nth-of-type(odd){ background:rgba(255,255,136,0.5); }</code>`),
   output_html(table([ class([table, 'table-striped']), 
             style('width:auto; margin-left:2em') ],
           [ tr(th(colspan(EH), ['Table for ', 'This'])),
             \ mforeach(between(SV,EV,V),
                      html(tr([ \ mforeach((between(SH,EH,H),once(hv_cg_value(Grid,CG,H,V);CG=BGC), 
                         wots(Cell,(print_g1(cpwui0,CG)))),
                                     html(td([class('mc-10'),style('text-align:center; width:11px;')], html_echo(Cell) ))) ])))
           ])),!.

*/



write_ddm(Title,Goal):- 
 write('<li class="dropdown"><a href="javascript:void(0)" class="dropbtn">'),wqs(Title),
 write('</a><div class="dropdown-content">'),call_e_dmsg(Goal),write('</div>').
term_to_www_encoding(Goal,A):- with_output_to(string(S),writeq(Goal)),www_form_encode(S,A).

write_nav_cmd(Info,Cmd):- \+ compound(Cmd), !, write_nav_cmd(Info,navCmd(Cmd)).
write_nav_cmd(Info,navCmd(Cmd)):- !, %nonvar(Goal), %toplevel_pp(PP), %first_current_example_num(ExampleNum),
  %get_current_test_atom(TestAtom), %get_current_test(TestID), term_to_www_encoding(TestID,TestAtom), %in_pp(PP),  
  %term_to_www_encoding(Goal,CmdAtom),
  into_title_str(Info,Info1),
  into_attribute(Cmd,Attr),
  sformat(SO,'<a href="javascript:void(0)" onclick="top.navCmd(`~w`)">~w</a>~n',[Attr,Info1]),!,
  our_pengine_output(SO).
write_nav_cmd(Info,Cmd):- write_nav_cmd(Info,navCmd(Cmd)).

write_http_link(Info,Goal):- nonvar(Goal), %toplevel_pp(PP), %first_current_example_num(ExampleNum),
  get_current_test_atom(TestAtom), %get_current_test(TestID), term_to_www_encoding(TestID,TestAtom), %in_pp(PP),  
  term_to_www_encoding(Goal,CmdAtom),
  into_title_str(Info,Info1),
  sformat(SO,'<a href="?cmd=~w" target="_top">~w</a>~n',[CmdAtom,TestAtom,Info1]),!,
  our_pengine_output(SO),!.

arcproc_iframe(Request):- 
 start_arc_html,
 with_toplevel_pp(http,handler_arcproc_iframe(Request)),!.

start_arc_html:-
  %nodebug,notrace,
  format('Content-type: text/html~n~n',[]),!,
  set_stream(current_output,buffer(false)),
 % arc_set_stream(current_output,write_errors(ignore)),
  set_stream(current_output,tty(false)), !.
 % arc_set_stream(current_output,representation_errors(unicode)),
  %arc_set_stream(current_output,encoding(octet)), arc_set_stream(current_output,encoding(utf8)),


arcproc_main(Request):- % update_changed_files1,
 start_arc_html,
 with_toplevel_pp(http,handler_arcproc_iframe(Request)),!.


handler_arcproc_iframe(_Request):- Var = icmd,
  get_now_cmd(Var,Prolog),!, dmsg(call_current_arc_cmd(Var)=Prolog),
  ignore(invoke_arc_cmd(Prolog)),!.
handler_arcproc_iframe(Request):-
  wots(_,intern_arc_request_data(Request)),
  handler_arcproc_main(Request),!.

handler_arcproc_main(_Request):-
format_s(`<!DOCTYPE html>`),
nop(format_s(`<!--html
<head> 
<meta charset="utf-8">
<meta http-equiv="X-UA-Compatible" content="IE=edge">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<meta name="description" content="LOGICMOO ARC Solver Butterfly WebUI">
<meta name="author" content="Douglas R. Miles & Mounier Florian"> 
<title>LOGICMOO ARC Solver Butterfly WebUI</title>-->`)),
format_s(`
<link rel="shortcut icon" href="/static/images/favicon.png?v=ab6cc8e66e2c76decf54d9c3fa2b90345ce2df6bfcd7b546450cf2ddcc5a6311d4a76feaff55c28512c1688bc017478e24a7af1c0949c1da366417b44d6cbcf3" crossorigin="anonymous">
<script src="https://releases.jquery.com/git/jquery-3.x-git.js" crossorigin="anonymous"></script>
<script src="https://code.jquery.com/ui/1.13.1/jquery-ui.js" integrity="sha256-6XMVI0zB8cRzfZjqKcD01PBsAy3FlDASrlC8SxCpInY=" crossorigin="anonymous"></script>
<!--script src="https://code.jquery.com/ui/1.12.1/jquery-ui.js" crossorigin="anonymous"></script>
<script src="https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js"></script-->
<!--script src="https://www.kryogenix.org/code/browser/sorttable/sorttable.js" crossorigin="anonymous"></script-->

<link rel="stylesheet" type="text/css" href="/node_modules/datatables.net-dt/css/jquery.dataTables.css" crossorigin="anonymous">
<script type="text/javascript" charset="utf8" src="/node_modules/datatables.net/js/jquery.dataTables.js"></script>

<link href="https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC" crossorigin="anonymous">
<script src="https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/js/bootstrap.bundle.min.js" integrity="sha384-MrcW6ZMFYlzcLA8Nl+NtUVF0sA7MsXsP1UyJoMp4YLEuNSfAP+JcXn/tWtIaxVXM" crossorigin="anonymous"></script>
<script src="/node_modules/html-to-image/dist/html-to-image.js" crossorigin="anonymous"></script>
<!--script src="https://www.w3schools.com/lib/w3.js" crossorigin="anonymous"></script>
link href="https://www.w3schools.com/w3css/4/w3.css" rel="stylesheet" crossorigin="anonymous"-->

<script src="/node_modules/floating-scroll/dist/jquery.floatingscroll.min.js" crossorigin="anonymous"></script>
<link rel="stylesheet" type="text/css" href="/node_modules/floating-scroll/dist/jquery.floatingscroll.css" crossorigin="anonymous"/>

<script src="https://d3js.org/d3.v4.js"></script>
<script src="https://d3js.org/d3-geo-projection.v2.min.js"></script>
<script src="https://d3js.org/d3-scale-chromatic.v1.min.js"></script>	

<script src="/swish/muarc/kaggle_arc_ui_html.js"></script>
<script src="/swish/muarc/kaggle_arc_ui_html_d3.js"></script>
<script>
$(document).ready(function(){$(".spacious-container").floatingScroll();});
</script>`),
write_arc_start(Where),
format_s(`<body class="ignore-scroll" style="top: 0px; bottem: 0px; left: 0px; width: 100vw; height: 100vh; overflow: auto; padding: 0px;">
<div id="mouse_iframer_div" style="display:none;"><iframe id="mouse_iframer" name="lm_xref_two" height="300px" width="100%" title="Iframe Example" src="about:blank"></iframe></div>`),
nop(format_s(`<!--body-->`)),
format_s(`<script> top.clearMenu(); </script>`),
retractall(page_has_grid_out(_,_)),
format_s(`<div id="main_in_iframe" class="spacious-container frame_body , scrollable-nav">`),
format_s(`<div id="hidden_swish_app" style="display:none; visibility:hidden"><div id="swish_app"><header class="navbar navbar-default"><div class="container pull-left"><div class="navbar-header"><a href="/" class="pengine-logo">&nbsp;</a><a href="/" class="swish-logo">&nbsp;</a></div><nav id="navbar"></nav></div></header><div id="content" class="container"><div class="tile horizontal" data-split="60%"><div class="prolog-editor"></div><div class="tile vertical" data-split="70%"><div class="prolog-runners"></div><div class="prolog-query"></div></div></div></div></div></div>`),
format_s(`<div id="hidden_popups" style="display:none; visibility:hidden"><pre><textarea id="input-helper"></textarea></pre><div id="input-view" class="hidden"></div><div id="popup" class="hidden"></div></div>`),
%format_s(`<script src="https://logicmoo.org:17771/static/main.js"></script><script src="https://logicmoo.org:17771/static/ext.js"></script><pre><div id="app"></div><div id="packed"></div><div id="term"></div></pre>`),
nop((format_s(`
      <link rel="stylesheet" href="node_modules/xterm/css/xterm.css" />
      <script src="node_modules/xterm/lib/xterm.js"></script>
      <div id="terminal"></div>
      <script>
       // import { Terminal } from 'xterm';
        //import { WebLinksAddon } from 'xterm-addon-web-links';
        //import { SearchAddon } from 'xterm-addon-search';
      // Load WebLinksAddon on terminal, this is all that''s needed to get web links
      // working in the terminal.
      
        var term = new Terminal();
        /*term.loadAddon(new WebLinksAddon());
        const fitAddon = new FitAddon();
        term.loadAddon(fitAddon);        */
        term.open(document.getElementById('terminal'));
        term.write('Hello from \x1B[1;3;31mxterm.js\x1B[0m $ ')
        //fitAddon.fit();

      </script>
`))),

  flush_output,
  %sccs(true,with_toplevel_pp(http,arcproc_now_main), write_arc_end(Where)).
  sccs(true,arcproc_now_main, write_arc_end(Where)).

% ansi
arcproc_now_main :- call_current_arc_cmd(cmd),!.
arcproc_now_main :- show_selected_object,!.

arcproc_right(Request):- 
 start_arc_html,
  wots(_,intern_arc_request_data(Request)),
 with_toplevel_pp(http,handler_logicmoo_right),!.
handler_logicmoo_right:- 
  write_arc_start(Where),
  handler_logicmoo_menu,
  ignore(arc_http_nav_menu),  
  ignore(call_current_arc_cmds),
  write_arc_end(Where).

click_grid:- get_now_cmd(grid,TG),click_grid(TG),!,show_selected_object.
click_grid:- show_selected_object.

click_grid(G):- writeq(G),nl,fail.
click_grid(TG):- \+ is_grid(TG),into_grid(TG,G),G\==TG,!,click_grid(G).
click_grid(G):- is_grid(G),print_grid(G),set_current_test(G).


set_html_component(_Name,_Value):-  \+ is_cgi,!.
set_html_component(Name,Value):- 
  nop((write_arc_start(Where),
  format(Where,'<script> window.setUrlParam("~w","~w"); window.setComponent("~w","~w");</script>',[Name,Value,Name,Value]))).

add_tool_tips(_Name,_Value):-  \+ is_cgi,!.
add_tool_tips(Name,Value):- 
  must_det_ll((
   write_arc_start_script(Where),
   into_attribute(Value,SValue),
   format(Where,'<script> window.add_tool_tips("~w","~w");</script>',[Name,SValue]))).

get_context_atoms(TestAtom,PairName):- 
  ignore(get_current_test_atom(TestAtom)),
  get_current_test(TestID), foc_current_example_num(ExampleNum),
  ignore((TestID>ExampleNum) = PairName).

%show_selected_object :- print_test,!.
show_selected_object:- get_pair_mode(entire_suite),!, preview_suite.

show_selected_object:- get_pair_mode(whole_test),!, get_current_test(TestID),get_current_test_atom(TestAtom),
  w_section(title(["Task",TestAtom]),((preview_test(TestID),do_web_menu_key('e')))).
show_selected_object:- get_pair_mode(single_pair),!,   
  get_context_atoms(TestAtom,PairName),
  w_section(title(["Pair ",PairName,"of",TestAtom]),((preview_test(PairName),do_web_menu_key('e')))).
  

show_selected_object:-   
   ignore(show_console_info),   
   ignore(print_test),
   section_break,
   w_section(title(edit1term),call_e_dmsg(edit1term)),
   show_http_session.


make_session_checkbox(N,C,BR,TF):- nl_if_needed,session_checkbox(N,C,BR,TF).

show_indiv_filters(BR):- 
   must_run_html((
   findall(N,ui_option_checkbox(N,_,_),NList),
   list_to_set(NList,Set),
   forall(member(N,Set),once((ui_option_checkbox(N,C,TF),ignore(make_session_checkbox(N,C,BR,TF=='1'))))))).

ui_option_checkbox(doDiagonals,'doDiagonals','1').
ui_option_checkbox(Spyable,title(Spyable),Str):- vm_bool_opts(Spyable,Str).
ui_option_checkbox(Spyable,wants_output_for(Spyable),Str):- is_arc_spyable(Spyable),
  (wants_output_for(Spyable)->Str='1';Str='0').

:- dynamic(is_arc_spyable_deduced/1).
is_arc_spyable_known(NonAtom):- var(NonAtom),!,fail.
is_arc_spyable_known(Spyable):- is_arc_spyable_deduced(Spyable).
is_arc_spyable_known(Spyable):- arc_spyable_keyboard_key(Spyable,_).

is_arc_spyable(debug).  is_arc_spyable(info).
is_arc_spyable(Spyable):- is_arc_spyable_known(Spyable).
is_arc_spyable(Spyable):- luser_getval(Spyable,Show), once(Show==hide;Show==show), \+ is_arc_spyable_known(Spyable).

if_wants_output_for(Spyable, Goal):- wants_html,!,w_section(Spyable,Goal,Spyable,true).

if_wants_output_for(SpyableC, Goal):-
 copy_term(SpyableC,Spyable),
  catch_log(((wants_output_for(Spyable)->w_section(title(Spyable),Goal,Spyable) ; 
     w_section(title(Spyable),Goal,ran_on_expand)))).


if_extreme_debug(G):- nop(G).
wants_output_for(SpyableC):- is_cgi,!, copy_term(SpyableC,Spyable),
  if_extreme_debug((wants_output_for_1(Spyable)-> make_session_checkbox(Spyable,wants_output_for(Spyable),'<br/>',true) ;
    (make_session_checkbox(Spyable,wants_output_for(Spyable),'<br/>',false),!,fail))).

wants_output_for(_Spyable):- main_thread,!.
wants_output_for(SpyableC):- copy_term(SpyableC,Spyable),
   (wants_output_for_1(Spyable)-> pp(wqs(showing(Spyable)));
     ((arc_spyable_keyboard_key(Spyable,SpyableKey)->pp(skipping(Spyable,key(SpyableKey)));(pp(skipping(Spyable)))),!,fail)).

wants_output_for_1(Spyable):- nb_current(menu_key,_), arc_spyable_keyboard_key(Spyable,SpyableKey), menu_or_upper(SpyableKey),!.
wants_output_for_1(Spyable):- \+ \+ is_arc_spyable_known(Spyable), !, \+ luser_getval(Spyable, hide).
wants_output_for_1(Spyable):- \+ is_arc_spyable_known(Spyable), 
   assertz_if_new(is_arc_spyable_deduced(Spyable)), 
   luser_setval(Spyable,show),!,
   nop(wants_output_for_1(Spyable)).
   


vm_bool_opts(SpyableC,Str):- copy_term(SpyableC,Spyable),
 ((ensure_peek_vm(Machine),
  get_kov1(Spyable,Machine,Value))),into_0_or_1(Value,Str).

ensure_peek_vm(Machine):- peek_vm(Machine),!.
ensure_peek_vm(Machine):- rand_current_grid(X),into_fti(_ID,[complete],X,Machine),!,set_vm(Machine).
rand_current_grid(X):- get_current_grid(X).
rand_current_grid([[blue]]).


into_0_or_1(Value,Str):- is_bool_like(T,F),(Value==T->Str='1';(Value==F->Str='0';fail)),!.
is_bool_like(true,false). is_bool_like(yes,no). is_bool_like(on,off). 
is_bool_like(t,f). is_bool_like(t,nil). is_bool_like('T','NIL').
is_bool_like(1,0). is_bool_like('1','0').
is_bool_like('CHECKED','UNCHECKED'). 
is_bool_like('CHECKED',''). 
% is_bool_like(show,hide).
is_bool_like(always,never).
is_bool_like(ran_expanded,ran_on_expand).

is_bool_like(show,hide).
is_bool_like(shown,hidden).

is_amount_shown(toplevel). 
is_amount_shown(maybe).
is_amount_shown(UWH):- user_wants_header(UWH).
is_amount_shown(YN):- into_0_or_1(YN,_),!.

user_wants_header(ran_expanded). % 1
user_wants_header(ran_collapsed). % ... ?
user_wants_header(ran_on_expand). % 0




get_now_cmd(Cmd,Prolog):- get_param_req(Cmd,Call),url_decode_term(Call,Prolog).
%get_now_cmd(Cmd,Prolog):- get_http_current_request(Request), member(request_uri(List),Request),request_uri('/arc_web_interface.html?dump_from_pairmode),url_decode_term(Call,Prolog),!.

call_current_arc_cmds_pp:- with_toplevel_pp(http,call_current_arc_cmds).

%call_current_arc_cmds:- get_now_cmd('cmd',Prolog), dmsg(call_current_arc_cmds(cmd)=Prolog), trace, !,invoke_arc_cmd(Prolog).
call_current_arc_cmds:- luser_getval('cmd',Prolog), dmsg(call_current_arc_cmds(cmd)=Prolog), !,invoke_arc_cmd(Prolog).
call_current_arc_cmds:- print_test,!. %,print_all_info_for_test,do_web_menu_key('t'),!.
/*
call_current_arc_cmds:- 
 call_current_arc_cmd(cmd),
 call_current_arc_cmd(cmd2),
 call_current_arc_cmd(footer_cmd).
*/
call_current_arc_cmd(Var):-
   ignore((get_now_cmd(Var,Prolog),        
   dmsg(call_current_arc_cmd(Var)=Prolog),invoke_arc_cmd(Prolog))).



arc_http_nav_menu:- 
  with_li_pre((get_test_cmd(Prolog),
  print_menu_cmd1(prev_test),
  print_menu_cmd1(print_all_info_for_test),
  print_menu_cmd1((next_test)), 
  print_menu_cmd1(( Prolog)))),!.

pgo(N):- true, into_grid(N,O),!,print_grid(O).

show_console_info:-
  show_webui_call(in_pp(_)),
  show_webui_call(toplevel_pp(_)),
  show_webui_call(is_cgi),
  show_webui_call(wants_html),
  show_webui_call(arc_html),
  show_webui_call(em_html),!.

show_webui_call(G):- (call_e_dmsg(G)*->pp(webui_call_success(G));pp(webui_call_failed(G))).

/*

arc_script_header:- 
  use_module(library(xlisting/xlisting_web)),
  use_module(library(xlisting/xlisting_web_server)),
  arc_script_header_pt2.

arc_script_header_pt2:- 
  arc_html_format('<meta name="viewport" content="width=device-width, initial-scale=1.0">
<meta name="description" content="Prolog XListing for Logicmoo Code">
<meta name="author" content="logicmoo@gmail.com">
<meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1">
<link rel="stylesheet" type="text/css" href="/swish/css/menu.css">
<link rel="stylesheet" type="text/css" href="/swish/css/cliopatria.css">
<script src="https://code.jquery.com/jquery-3.6.0.min.js"></script>
<script type="text/javascript">window.name="lm_xref"; </script>  
<script data-main="/swish/js/swish" src="/node_modules/requirejs/require.js"></script>
<script type="text/javascript" src="/www/yui/2.7.0/build/utilities/utilities.js"></script>
<script type="text/javascript" src="https://ajax.googleapis.com/ajax/libs/jqueryui/1.12.1/jquery-ui.min.js"></script>
<script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/jquery.qrcode/1.0/jquery.qrcode.min.js"></script>
<link rel="stylesheet" type="text/css" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">
<link rel="stylesheet" type="text/css" href="https://fonts.googleapis.com/icon?family=Material+Icons">
<script type="text/javascript" src="/swish/lm_xref/pixmapx/popupmenu/scripts/Popup-plugin.js"></script>
<link rel="stylesheet" type="text/css" href="/swish/lm_xref/pixmapx/popupmenu/styles/Popup-plugin.css">
<script type="text/javascript" src="/swish/lm_xref/pixmapx/selected/js/social.selection.js"></script>
<link rel="stylesheet" type="text/css" href="/swish/lm_xref/pixmapx/selected/css/social.selection.css">
<script type="text/javascript" src="/swish/js/cliopatria.js"></script>
<link rel="stylesheet" type="text/css" href="/swish/css/butterfly_term.css">
<script type="text/javascript" href="/swish/js/butterfly_term.js"></script>
<link rel="stylesheet" type="text/css" href="/swish/css/term.css">
<script src="https://cdnjs.cloudflare.com/ajax/libs/json2html/2.1.0/json2html.min.js"></script>').
*/

:- dynamic(was_inline_to_bfly/0).

inline_to_bfly:- was_inline_to_bfly,!.
inline_to_bfly:- asserta(was_inline_to_bfly),inline_to_bfly_html.

inline_to_bfly_html:- toplevel_pp(swish),!,ensure_collapsable_styles.
inline_to_bfly_html:- catch_log(ensure_collapsable_styles),
 arc_html_format(
'<link rel="stylesheet" type="text/css" href="/swish/css/menu.css">
<link rel="stylesheet" type="text/css" href="/swish/css/cliopatria.css">
<script src="https://unpkg.com/gojs@2.2.15/release/go.js"></script>
<script src="https://code.jquery.com/jquery-3.6.0.min.js"></script>
<script type="text/javascript">window.name="lm_xref"; </script>
<script type="text/javascript" src="https://ajax.googleapis.com/ajax/libs/jqueryui/1.12.1/jquery-ui.min.js"></script>
<script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/jquery.qrcode/1.0/jquery.qrcode.min.js"></script>

<script type="text/javascript" src="/www/yui/2.7.0/build/utilities/utilities.js"></script>
<script type="text/javascript" src="/www/yui/2.7.0/build/datasource/datasource.js"></script>
<script type="text/javascript" src="/www/yui/2.7.0/build/autocomplete/autocomplete.js"></script>

<link rel="stylesheet" type="text/css" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">
<link rel="stylesheet" type="text/css" href="https://fonts.googleapis.com/icon?family=Material+Icons">
<script type="text/javascript" src="/swish/lm_xref/pixmapx/popupmenu/scripts/Popup-plugin.js"></script>
<link rel="stylesheet" type="text/css" href="/swish/lm_xref/pixmapx/popupmenu/styles/Popup-plugin.css">
<script type="text/javascript" src="/swish/lm_xref/pixmapx/selected/js/social.selection.js"></script>
<link rel="stylesheet" type="text/css" href="/swish/lm_xref/pixmapx/selected/css/social.selection.css">
<script type="text/javascript" src="/swish/js/cliopatria.js"></script>
<link rel="stylesheet" type="text/css" href="/swish/css/butterfly_term.css">
<script type="text/javascript" href="/swish/js/butterfly_term.js"></script>
<link rel="stylesheet" type="text/css" href="/swish/css/term.css">
<!-- -->
<script data-main="/swish/js/swish" src="/node_modules/requirejs/require.js"></script> 
').

arc_script_header2:- 
  arc_html_format((('<script src="https://code.jquery.com/jquery-3.6.0.min.js" crossorigin="anonymous"></script>
<script src="https://ajax.googleapis.com/ajax/libs/jqueryui/1.12.1/jquery-ui.min.js"></script> <!-- necessary for the "draggable" ui  -->
<script src="/swish/lm_xref/pixmapx/popupmenu/scripts/Popup-plugin.js"></script>
<script src="/swish/lm_xref/pixmapx/popupmenu/scripts/Example.js"></script>

<link rel="shortcut icon" href="/static/images/favicon.png?">
<link rel="stylesheet" type="text/css" href="/swish/css/menu.css">
<link rel="stylesheet" type="text/css" href="/swish/css/cliopatria.css">
<script type="text/javascript" src="/swish/js/cliopatria.js"></script>
<link type="text/css" rel="stylesheet" href="/swish/css/term.css">
<link type="text/css" rel="stylesheet" href="/swish/css/butterfly_term.css">

<link rel="stylesheet" type="text/css" href="/www/yui/2.7.0/build/autocomplete/assets/skins/sam/autocomplete.css">
<script type="text/javascript" src="/www/yui/2.7.0/build/utilities/utilities.js"></script>
<script type="text/javascript" src="/www/yui/2.7.0/build/datasource/datasource.js"></script>
<script type="text/javascript" src="/www/yui/2.7.0/build/autocomplete/autocomplete.js"></script>

<script src="https://cdnjs.cloudflare.com/ajax/libs/jquery.qrcode/1.0/jquery.qrcode.min.js"></script>
<script src="/swish/lm_xref/pixmapx/selected/js/social.selection.js"></script>
<link rel="stylesheet" type="text/css" href="/swish/lm_xref/pixmapx/selected/css/social.selection.css">
<link rel="stylesheet" type="text/css" href="/swish/lm_xref/pixmapx/selected/css/example.css">
<link rel="stylesheet" type="text/css" href="/swish/lm_xref/pixmapx/popupmenu/styles/Popup-plugin.css">
<link rel="stylesheet" type="text/css" href="/swish/lm_xref/pixmapx/popupmenu/styles/Example.css">
<!--Use either font-awesome icons or Google icons with these links. Other icons could also be used if preferred-->
<link rel="stylesheet" href="https://fonts.googleapis.com/icon?family=Material+Icons">
<script data-main="/swish/js/swish" src="/node_modules/requirejs/require.js"></script>'))).
%http_debug_console:- current_prolog_flag(gui_tracer,true),!,guitracer.



arc_weto(G):- call_e_dmsg(G).


%:- luser_default(cmd,print_test).
:- luser_default(cmd,ndividuator). 
:- luser_default(footer_cmd,statistics).

current_arc_cmd(Prolog):- current_arc_cmd('cmd',Prolog).
%current_arc_cmd(cmd,Prolog):- luser_getval(cmd,Prolog).
%current_arc_cmd(cmd,Prolog):- luser_getval(cmd,Prolog).
current_arc_cmd(V,Prolog):- luser_getval(V,Prolog).
%current_arc_cmd(footer_cmd,Prolog):- (\+ current_arc_cmd(cmd,menu) -> luser_getval(footer_cmd,Prolog,menu) ; luser_getval(footer_cmd,Prolog,edit1term)).


%muarc:test_arcui

 % our_pengine_output(`<script src="https://unpkg.com/gojs/release/go-debug.js"></script>`).

:-   ignore((predicate_property(phil:'$exported_op'(_,_,_),(discontiguous)),
  \+ predicate_property(phil:'$exported_op'(_,_,_),number_of_clauses(_)),
     abolish(phil:'$exported_op',3))),
  ignore((predicate_property(rdf11:'$exported_op'(_,_,_),(discontiguous)),
\+ predicate_property(rdf11:'$exported_op'(_,_,_),number_of_clauses(_)),
  abolish(rdf11:'$exported_op',3))),
     ignore((predicate_property(lemur:'$exported_op'(_,_,_),(discontiguous)),
  \+ predicate_property(lemur:'$exported_op'(_,_,_),number_of_clauses(_)),
     abolish(lemur:'$exported_op',3))).

:- include(kaggle_arc_ui_html_go1).
:- include(kaggle_arc_ui_html_go2).
/*

:- abolish(lemur:'$exported_op',3).
:- abolish(rdf11:'$exported_op',3).
*/

:- include(kaggle_arc_footer).

