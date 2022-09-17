/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/

% INSTRUCTIONS
% =swipl kaggle_arc.pl=
% =:- start_arc_server.=
%
% Then navigate to http://localhost:17666 in your browser

/*
:- module(kaggle_arc_ui_html,
  [ start_arc_server/0,
    stop_arc_server/0
  ]
).
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.

:- use_module(library(thread_pool)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_path)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/websocket)).


:- meta_predicate(noisey_debug(0)).
noisey_debug(Goal):- collapsible_section(debug,Goal).

:- meta_predicate(collapsible_section(0)).
collapsible_section(Goal):- collapsible_section(object,Goal).

:- meta_predicate(collapsible_section(+,0)).
collapsible_section(Type,Goal):-
  invent_header(Goal,Title),
  collapsible_section(Type,Title,true,Goal).

print_title(Var):- (var(Var);Var==[]),!.
print_title([L|List]):- is_list(List), !, print_title(L),write(' '),print_title(List).
print_title(Title):- trim_newlines(ppt(Title)).

:- meta_predicate(collapsible_section(+,+,0)).
collapsible_section(Type,Title,Goal):-
  collapsible_section(Type,Title,false,Goal).

:- meta_predicate(collapsible_section(+,+,+,0)).
/*
collapsible_section(Type,Title,true,Goal):-
  (nb_current('$collapsible_section',Was);Was=[]),
  length(Was,Depth),
  setup_call_cleanup(format('~N~@!mu~w! ~@ |~n',[dash_chars(Depth,' '), Type, print_title(Title)]),
                     locally(b_setval('$collapsible_section',[Type|Was]),wots(S,Goal)), 
                     format('~N~w~@¡mu~w¡~n',[S,dash_chars(Depth,' '), Type])).
*/
collapsible_section(Tag,Title,_,Goal):-
  once(nb_current('$collapsible_section',Was);Was=[]), length(Was,Depth),!,wots(Ident,dash_chars(Depth,' ')),
  setup_call_cleanup(format('~N~w!mu~w! ~@ |~n',[Ident, Tag, print_title(Title)]),
                     locally(b_setval('$collapsible_section',[c(Tag)|Was]),tabbed_print_im(Depth+2,Goal)), 
                     format('~N~w¡mu~w¡ ',[Ident, Tag])).

with_tagged(Tag,Goal):- 
  once(nb_current('$collapsible_section',Was);Was=[]), length(Was,Depth),!,wots(Ident,dash_chars(Depth,' ')),
  setup_call_cleanup(
    bfly_html_goal(format('~w<~w> ~N',[Ident,Tag])),
    locally(b_setval('$collapsible_section',[h(Tag)|Was]),tabbed_print_im(Depth+2,(Goal))),
    bfly_html_goal(format('~w</~w> ',[Ident,Tag]))).

tabbed_print_im(Tab,Goal):- Tab2 is Tab, tabbed_print(Tab2,Goal).

:- meta_predicate(trim_newlines(0)).
trim_newlines(Goal):- wots(S,Goal),trim_leading_trailing_whitespace(S,SS),write(SS).
trim_leading_trailing_whitespace(In,Out):-
  split_string(In, ", ", "\s\t\n",List), 
  atomics_to_string(List,' ',Out).

:- meta_predicate(invent_header(+,-)).
invent_header(Term,Title):- \+ compound(Term),!, Title = goal(Term).
invent_header(Term,Title):- compound_name_arity(Term,F,_),
  (( \+ dumb_functor(Term)) -> (maybe_ia(Term,E),Title=g(F,E));
     (header_arg(Term,E),invent_header(E,Title))).
header_arg(Term,E):- sub_term(E,Term), E\=@=Term, compound(E), \+ is_list(E).
maybe_ia(Term,DT):- header_arg(Term,E), !, \+ dumb_functor(E), data_type(E,DT),!.
maybe_ia(_Term,args).

dumb_functor(Term):- is_list(Term),!, \+ is_grid(Term).
dumb_functor(Term):- predicate_property(Term,meta_predicate(_)),!.
dumb_functor(Term):- compound_name_arity(Term,F,_),upcase_atom(F,UC),!,downcase_atom(F,UC).

test_collapsible_section:- 
  collapsible_section(info,
    forall(nth0(N,[a,b,c,d],E),writeln(N=E))).
     
     



:- multifile user:file_search_path/2.

%user:file_search_path(document_root,	'/srv/htdocs').
%user:file_search_path(document_root,  AbsolutePath):- arc_sub_path(arc_apps,AbsolutePath).

% user:file_search_path(arc_apps,  AbsolutePath):- arc_sub_path(arc_apps,AbsolutePath).

%:- http_handler('/ARC/', http_reply_from_files(arc_apps, []), [prefix]).

%:- http_handler('/swish/arc/', swish_arc, [prefix]).

user:file_search_path(arc,  AbsolutePath):- arc_sub_path('.',AbsolutePath).

:- http_handler('/swish/arc/', swish_arc, [prefix]).
:- http_handler('/swish/muarc/arcproc_right', arcproc_right, [prefix]).
:- http_handler('/swish/muarc/arcproc_left', arcproc_left, [prefix]).
:- http_handler('/swish/muarc/swish_config.json', swish_reply_config_root,[priority(200)]).


% http_handler docs: http://www.swi-prolog.org/pldoc/man?predicate=http_handler/3
% =http_handler(+Path, :Closure, +Options)=
%
% * root(.) indicates we're matching the root URL
% * We create a closure using =http_reply_from_files= to serve up files
%   in the local directory
% * The option =spawn= is used to spawn a thread to handle each new
%   request (not strictly necessary, but otherwise we can only handle one
%   client at a time since echo will block the thread)
%:- http_handler(('/swish/arc/'), http_reply_from_files(arc, []), [prefix]).
% * root(echo) indicates we're matching the echo path on the URL e.g.
%   localhost:17666/echo of the server
% * We create a closure using =http_upgrade_to_websocket=
% * The option =spawn= is used to spawn a thread to handle each new
%   request (not strictly necessary, but otherwise we can only handle one
%   client at a time since echo will block the thread)


:- http_handler('/swish/arcproc/web_socket_echo',
                http_upgrade_to_websocket(web_socket_echo, []),
                [spawn([])]).

start_arc_server :-
    default_port(Port),
    start_arc_server(Port),
    use_module(library(xlisting/xlisting_web)),
    use_module(library(xlisting/xlisting_web_server)),
    arc_http_server.

start_arc_server(Port) :-
    http_server(http_dispatch, [port(Port)]).

stop_arc_server :-
    default_port(Port),
    stop_arc_server(Port).
stop_arc_server(Port) :-
    http_stop_server(Port, []).

default_port(17666).

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

arc_http_server:- thread_property(ID,status(running)),ID=='http@17666',!.
arc_http_server:- http_server(http_dispatch, [port(17666)]),

 thread_pool_create(compute, 3,
                      [ local(20000), global(100000), trail(50000),
                        backlog(5)
                      ]),
 thread_pool_create(media, 30,
                      [ local(100), global(100), trail(100),
                        backlog(100)
                      ]),
 http_handler('/swish/arc/solve',     solve,     [spawn(compute)]),
 http_handler('/swish/arc/thumbnail', thumbnail, [spawn(media)]).

%:- http_handler('/swish/arc/user'/User), user(Method, User),[ method(Method), methods([get,post,put]) ]).


%:- http_handler('/favicon.ico', http_reply_file('favicon.ico', []), []).

%http:location(images,	root(images), []).

webui_tests:-
  test_print_tree,
  bfly_tests.


no_web_dbg:-
  unsetenv('DISPLAY'),
  no_xdbg_flags,
  no_x_flags,
  set_prolog_flag(xpce,true).

%:- no_web_dbg.

begin_arc_html(Request):- notrace(with_webui(begin_arc_html0(Request))).
begin_arc_html0(Request):-
  ignore((current_output(Out),
  set_stream(Out,buffer(false)))),
  /* set_stream(Out,close_on_exec(false)),
  set_stream(Out,close_on_abort(true)),
  set_stream(Out,encoding(octet)),
  set_stream(Out,write_errors(ignore)))),*/
  format('Content-type: text/html~n~n',[]),
  format('<!DOCTYPE html>',[]),
  %ignore(set_test_param),
  ignore((member(search(List),Request),member(task=Task,List),  
  atom_id(Task,ID), dmsg(Task->ID), set_current_test(ID))),
  ignore(if_arc_webui(intern_request_data(Request))),
  ignore(if_arc_webui(write_begin_html('ARC Solver'))),
  nop(ensure_readable_html).

set_test_param:-
  ignore((if_arc_webui((get_param_sess(task,Task), Task\=='',  Task\=="",
  atom_id(Task,ID), dmsg(Task->ID), set_current_test(ID))))),!.


swish_arc(Request):-   
  muarc_tmp:arc_directory(ARC_DIR),
  http_reply_from_files(ARC_DIR, [], Request).

%arcproc_left(Request):- xlisting_web:handler_logicmoo_cyclone(Request),!.
arcproc_left(Request):-  
  %no_web_dbg,
  notrace((begin_arc_html(Request),
  flush_output,
  with_webui((inline_html_format([
    ignore(handler_logicmoo_left),
    ignore(ensure_colapsable_script),
    ignore(write_end_html)]))))),!.

%arcproc_left(Request):- swish_arc(Request),!.
arcproc_left(Request):- 
  notrace((begin_arc_html(Request),
  with_webui((inline_html_format([
    handler_logicmoo_right,
    ensure_colapsable_script,
    write_end_html]))))).


with_webui(Goal):- ignore(if_arc_webui(with_http(Goal))).
%:- initialization arc_http_server.




% arc_find_tests(menu):- ignore(menu).
arc_find_tests(F):- find_tests(F).

:- dynamic(xlisting_whook:offer_testcase/1).
:- multifile(xlisting_whook:offer_testcase/1).
xlisting_whook:offer_testcase(F):- arc_find_tests(F).

handler_logicmoo_right:-   
 if_arc_webui(inline_html_format([
   ignore((get_http_current_request(Request))),write('<pre>'),
   print_tree(Request),offer_testcases,show_http_session,
   write('</pre>')])).

handler_logicmoo_arc:- if_arc_webui(inline_html_format([call(handler_logicmoo_left)])).
handler_logicmoo_left:- 
   if_arc_webui(inline_html_format( 
   [ `<pre>`,
   ignore(arc_nav_menu),
   flush_output,
   ignore(show_console_info),
   flush_output,
   ignore(call_current_arc_cmd),
   flush_output,
   invoke_arc_cmd(menu),
   flush_output,
   invoke_arc_cmd(edit1term),
   show_http_session,
    `</pre>`])), 
  !.

arc_nav_menu:- 
  current_arc_cmd(tc_cmd,Prolog),
  print_menu_cmd1((prev_test,Prolog)),
  print_menu_cmd1((next_test,Prolog)),
  print_menu_cmd1((Prolog)),!.
show_console_info:-
  in_pp(PP),pp(in_pp(PP)),!.

call_current_arc_cmd:- 
  call_current_arc_cmd(cmd),
 call_current_arc_cmd(footer_cmd).

call_current_arc_cmd(Var):-
   current_arc_cmd(Var,Prolog),        
   dmsg(Var=Prolog),invoke_arc_cmd(Prolog).


invoke_arc_cmd(Prolog):-
   nonvar(Prolog),
   asserta_new(xlisting_whook:offer_testcase(Prolog)), !,
   catch(weto(Prolog),E,wdmsg(E)),!.

:- luser_defval(cmd,print_test).
:- luser_defval(tc_cmd,ndividuatorO1).
:- luser_defval(footer_cmd,statistics).

current_arc_cmd(Prolog):- current_arc_cmd(cmd,Prolog).
current_arc_cmd(cmd,Prolog):- luser_getval(cmd,Prolog).
current_arc_cmd(tc_cmd,Prolog):- luser_getval(tc_cmd,Prolog).
current_arc_cmd(footer_cmd,Prolog):- luser_getval(footer_cmd,Prolog).
%current_arc_cmd(footer_cmd,Prolog):- (\+ current_arc_cmd(cmd,menu) -> luser_getval(footer_cmd,Prolog,menu) ; luser_getval(footer_cmd,Prolog,edit1term)).

/*
:- if(exists_source(library(logicmoo_webui))).
:- set_prolog_flag(no_sandbox,true).
:- use_module(library(logicmoo_webui)).
:- webui_start_swish_and_clio.
:- endif.
*/

:- fixup_exports.


