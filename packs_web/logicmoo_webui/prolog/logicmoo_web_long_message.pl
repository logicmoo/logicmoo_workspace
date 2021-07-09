
:- module(web_long_message,
 [set_long_message_server/1,
  add_long_web_message/1,
  add_long_web_message/2,
  add_long_web_message/3,
  maybe_long_message_printer/1,
  maybe_long_message_printer/2,
  print_long_message/1,
  print_long_web_message/1]).

:- /*system:*/use_module(library(http/thread_httpd)).
:- /*system:*/use_module(thread_httpd:library(http/http_dispatch)).
%:- use_module(library(http/http_dispatch))
:- /*system:*/use_module(swi(library/http/html_head)).
:- /*system:*/use_module(library(http/http_dispatch)).
:- /*system:*/use_module(library(http/http_path)).
:- /*system:*/use_module(library(http/http_log)).
:- /*system:*/use_module(library(http/http_client)).
:- /*system:*/use_module(library(http/http_server_files)).
:- /*system:*/use_module(library(http/http_parameters)).

:- /*system:*/use_module(library(uri)).
:- /*system:*/use_module(library(http/http_openid)).
:- /*system:*/use_module(library(http/http_host)).
:- use_module(library(http/html_write)).
:- /*system:*/use_module(library(http/http_error)).

:- http_handler(root(swish/long_message),long_web_message,[chunked,methods([get,post,put])]).

:- dynamic(wlm:long_message_server/1).
:- dynamic(wlm:long_message_data/2).
:- volatile(wlm:long_message_server/1).
:- volatile(wlm:long_message_data/2).

wlm:long_message_server('https://logicmoo.org').
set_long_message_server(Server):- retractall(wlm:long_message_server(_)), asserta(wlm:long_message_server(Server)).


add_long_web_message(Message):-
 add_long_web_message(Message,_,HREF),
  echo_long_href(HREF).

add_long_web_message(Message,HREF):- var(HREF), !,
 add_long_web_message(Message,_,HREF),!.

add_long_web_message(Message,id(ID)):- !,
 add_long_web_message(Message,ID,_),!.

add_long_web_message(Message,href(HREF)):- !,
 add_long_web_message(Message,_,HREF),!.

add_long_web_message(Message,ID):- atom(ID),
 add_long_web_message(Message,ID,HREF),!,
  echo_long_href(HREF).
 
add_long_web_message(Message,IDREF):- parse_url(IDREF,Attrs),member(search([_=ID|_]),Attrs),!,
 add_long_web_message(Message,ID,HREF),
 echo_long_href(HREF).

echo_long_href(ID):- \+ atom_contains(ID,'='),id_to_href(ID,HREF),!, echo_long_href(HREF).
echo_long_href(HREF):- 
 wlm:long_message_server(Server),
 write(Server),write(HREF).

into_message_data(Message,S):- atomic(Message),S=Message.
into_message_data(Message,S):- with_output_to(string(S),print(Message)).

print_long_message(ID):-  forall(wlm:long_message_data(ID,Message), message_into_print(Message)).
message_into_print(Message):- atomic(Message)->format('~w',[Message]);format('~N~p~N',[Message]).

add_long_web_message(Message,ID,HREF):-
  (var(ID)->gensym(long_message_,ID);true),
  into_message_data(Message,S),
  assertz(wlm:long_message_data(ID,S)),
  id_to_href(ID,HREF).

id_to_href(ID,HREF):- http_link_to_id(long_web_message, [id(ID)], HREF).

long_web_message(Request) :-
  member(search(List),Request),
  member(_=ID,List),
  print_long_web_message(ID).

print_long_web_message(ID):- 
  %id_to_href(ID,HREF),
  with_output_to(string(S),print_long_message(ID)),
    phrase(html([
     %a([class(id), href(HREF)],HREF),
     html([head(''),body(pre(S))])]), Tokens),
     print_html(Tokens).


:- meta_predicate(maybe_long_message_printer_simple(+,0)).
maybe_long_message_printer_simple(Max,Goal):-
  with_output_to(string(S),Goal),
  atomic_list_concat(Lines,'\n',S),
  length(Lines,Vert),
  ((Vert > Max) -> 
    (add_long_web_message(S,href(HREF)),
      echo_long_href(HREF));
  write(S)),
  format('~N',[]).


begin_lmp(Max,ID):- 
 gensym(mlmp_,ID),
 flag(ID,_,Max).

end_lmp(ID):- 
  flag(ID,X, 0),
  (X > 0 
    -> destroy_long_message(ID) 
    ; echo_long_href(ID)).

:- meta_predicate(in_long_message_printer(+,+,0)).
in_long_message_printer(Prev,ID,Goal):-  
 with_output_to_predicate(long_message_printer(Prev,ID),Goal).

merge_in_newlines([],['\n']).
merge_in_newlines([Lines],[Lines]):- !.
merge_in_newlines([L|Lines],[L,' \n'|WNLs]):- 
  merge_in_newlines(Lines,WNLs).

long_message_printer(Prev,ID,S):- 
  atomic_list_concat(Lines,'\n',S),
  merge_in_newlines(Lines,WNLs),
  maplist(long_message_printer_e(Prev,ID),WNLs).

long_message_printer_e(O,ID, S):- flag(ID,X,X), X>0, write(O,S), fail.
long_message_printer_e(_,ID,' \n'):- flag(ID,X,X-1), fail.
long_message_printer_e(_,ID, S):- add_long_web_message(S,ID,_).


destroy_long_message(ID):- retractall(wlm:long_message_data(ID,_)).

:- meta_predicate(maybe_long_message_printer(+,0)).
maybe_long_message_printer(Max,Goal):-
  current_output(Prev),
  setup_call_cleanup(
    begin_lmp(Max,ID),
     in_long_message_printer(Prev,ID,Goal),
    end_lmp(ID)).


:- meta_predicate(maybe_long_message_printer(0)).
maybe_long_message_printer(Goal):-
  maybe_long_message_printer(4, Goal).

system:mlmp(G):- maybe_long_message_printer(0,G).
