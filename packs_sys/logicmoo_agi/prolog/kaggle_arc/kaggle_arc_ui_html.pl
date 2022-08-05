/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/

% INSTRUCTIONS
% =swipl kaggle_arc.pl=
% =:- start_arc_server.=
%
% Then navigate to http://localhost:1777 in your browser

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

:- if(current_module(logicmoo_arc)).
:- use_module(library(xlisting/xlisting_web)).
:- endif.

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
%   localhost:1777/echo of the server
% * We create a closure using =http_upgrade_to_websocket=
% * The option =spawn= is used to spawn a thread to handle each new
%   request (not strictly necessary, but otherwise we can only handle one
%   client at a time since echo will block the thread)


:- http_handler('/swish/arcproc/web_socket_echo',
                http_upgrade_to_websocket(web_socket_echo, []),
                [spawn([])]).

start_arc_server :-
    default_port(Port),
    start_arc_server(Port).
start_arc_server(Port) :-
    http_server(http_dispatch, [port(Port)]).

stop_arc_server :-
    default_port(Port),
    stop_arc_server(Port).
stop_arc_server(Port) :-
    http_stop_server(Port, []).

default_port(1777).

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

arc_http_server:- thread_property(ID,status(running)),ID=='http@1777',!.
arc_http_server:- http_server(http_dispatch, [port(1777)]),

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


swish_arc(Request):- 
  muarc_tmp:arc_directory(ARC_DIR),
  http_reply_from_files(ARC_DIR, [], Request).

arcproc_right(_Request):- 
  ((format('Content-type: text/html~n~n',[]),
  format('<!DOCTYPE html><html><head></head><body><pre>~@</pre></body></html>~n~n',[(listing(arcproc_right/1),nop(www_dumpST))]),
  flush_output_safe)),!.
arcproc_right(Request):- swish_arc(Request),!.

arcproc_left(_Request):- 
  ((format('Content-type: text/html~n~n',[]),
  format('<!DOCTYPE html><html><head></head><body><pre>~@</pre></body></html>~n~n',[listing(arcproc_left/1)]),
  flush_output_safe)),!.

:- if(current_module(logicmoo_arc)).
arcproc_left(Request):- xlisting_web:handler_logicmoo_cyclone(Request),!.
:- else.
:- initialization arc_http_server.
:- endif.

arc_html(`



`).

:- fixup_exports.
