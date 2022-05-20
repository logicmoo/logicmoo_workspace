% INSTRUCTIONS
% =swipl kaggle_arc.pl=
% =:- start_arc_server.=
%
% Then navigate to http://localhost:1777 in your browser


:- module(kaggle_arc_ui_html,
  [ start_arc_server/0,
    stop_arc_server/0
  ]
).


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

% http_handler docs: http://www.swi-prolog.org/pldoc/man?predicate=http_handler/3
% =http_handler(+Path, :Closure, +Options)=
%
% * root(.) indicates we're matching the root URL
% * We create a closure using =http_reply_from_files= to serve up files
%   in the local directory
% * The option =spawn= is used to spawn a thread to handle each new
%   request (not strictly necessary, but otherwise we can only handle one
%   client at a time since echo will block the thread)
:- http_handler(root(.),
                http_reply_from_files('.', []),
                [prefix]).
% * root(echo) indicates we're matching the echo path on the URL e.g.
%   localhost:1777/echo of the server
% * We create a closure using =http_upgrade_to_websocket=
% * The option =spawn= is used to spawn a thread to handle each new
%   request (not strictly necessary, but otherwise we can only handle one
%   client at a time since echo will block the thread)
:- http_handler(root(echo),
                http_upgrade_to_websocket(echo, []),
                [spawn([])]).

start_arc_server :-
    default_port(Port),
    start_arc_server(Port).
start_arc_server(Port) :-
    http_server(http_dispatch, [port(Port)]).

stop_arc_server() :-
    default_port(Port),
    stop_arc_server(Port).
stop_arc_server(Port) :-
    http_stop_arc_server(Port, []).

default_port(1777).

%! echo(+WebSocket) is nondet.
% This predicate is used to read in a message via websockets and echo it
% back to the client
echo(WebSocket) :-
    ws_receive(WebSocket, Message, [format(json)]),
    ( Message.opcode == close
    -> true
    ; get_response(Message.data, Response),
      write("Response: "), writeln(Response),
      ws_send(WebSocket, json(Response)),
      echo(WebSocket)
    ).

%! get_response(+Message, -Response) is det.
% Pull the message content out of the JSON converted to a prolog dict
% then add the current time, then pass it back up to be sent to the
% client
get_response(Message, Response) :-
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
                      ]).

:- http_handler('/swish/ARC/solve',     solve,     [spawn(compute)]).
:- http_handler('/swish/ARC/thumbnail', thumbnail, [spawn(media)]).


:- http_handler(root(user/User), user(Method, User),
                [ method(Method),
                  methods([get,post,put])
                ]).


:- http_handler('/favicon.ico', http_reply_file('favicon.ico', []), []).

http:location(images,	root(images), []).

:- multifile user:file_search_path/2.

user:file_search_path(document_root,	'/srv/htdocs').

user:file_search_path(icons,		document_root(icons)).

:- http_handler(images(.), serve_files_in_directory(icons), [prefix]).

:- http_handler('/ARC/', http_reply_from_files('apps', []), [prefix]).
:- http_handler('/swish/arc/', swish_arc, [prefix]).
:- http_handler(root('swish/arc/prolog_right'), prolog_right, [prefix]).
:- http_handler(root('swish/arc/prolog_left'), prolog_left, [prefix]).

swish_arc(F):- wdmsg(swish_arc(F)).
prolog_right(F):- wdmsg(prolog_right(F)).
prolog_right(F):- wdmsg(prolog_right(F)).

:- initialization arc_http_server.


