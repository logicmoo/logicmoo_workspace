#!/usr/local/bin/swipl 
/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015-2016, VU University Amsterdam
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

:- module(swish_ide,
	  [ swish/0,
	    swish/1			% ?Port
	  ]).

:- set_prolog_flag(lm_no_autoload,false).
:- set_prolog_flag(lm_pfc_lean,false).


:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(www_browser)).
:- if(exists_source(library(uid))).
:- use_module(library(uid)).
:- endif.

:- multifile(owl2_model:datatype/2).
:- dynamic(owl2_model:datatype/2).


/** <module>

Open SWISH as an IDE for developing a remote application.
*/



:- meta_predicate(from_http(0)).
from_http(G):- with_output_to(main_error,G).

set_main_error:- stream_property(X,file_no(2)),
   stream_property(X,alias(Was)),
   set_stream(X,alias(main_error)),
   set_stream(X,alias(Was)).
    
%:- use_module(library(aleph),[]).


%:- use_module(library(must_trace)).

reexport_from(ReExporter,From:P):- 
    From:export(From:P),
    ReExporter:import(From:P),
    ReExporter:export(From:P).
   


:- multifile(pldoc_register:process_stored_comments/0).
:- dynamic(pldoc_register:process_stored_comments/0).
:- assert(pldoc_register:process_stored_comments).


		 /*******************************
		 *	       CONFIG		*
		 *******************************/

:- multifile
	swish_config:config/2,			% Name, Value
	swish_config:source_alias/2,		% Alias, Options
	swish_config:verify_write_access/3,	% Request, File, Options
	pengines:authentication_hook/3,		% Request, Application, User
	pengines:not_sandboxed/2,		% User, Application
        pengines:allowed/2,			% Peer, Application
	user:file_search_path/2.		% Alias, Path

is_symlink_to(File,AbsLink):- 
         read_link(File,Link,Link),
         absolute_file_name(Link,AbsLink,[access(read),file_errors(fail)]),
         absolute_file_name(File,AFile,[access(read),file_errors(fail)]), 
         AFile \== Link.

swish_home(AbsDir):- swish_home0(Dir),resolve_symlinks(Dir,AbsDir).

resolve_symlinks(File,Link):- is_symlink_to(File,AbsLink)-> resolve_symlinks(AbsLink,Link) ; File=Link.

swish_home0(Dir):- absolute_file_name(pack(swish),Dir,[file_type(directory),access(read),file_errors(fail)]).
swish_home0(Dir):- absolute_file_name(cpack(swish),Dir,[file_type(directory),access(read),file_errors(fail)]).
swish_home0(Dir):- is_symlink_to('./remote_ide.pl',To),file_directory_name(To,Dir).

:- discontiguous(user:file_search_path/2).
user:file_search_path('swish', Dir):-  swish_home(Dir),!.

:- dynamic(non_swish_file_search_path/2).
:- forall(retract(user:file_search_path(config_enabled, Was)),
          assert(non_swish_file_search_path(config_enabled, Was))).

user:file_search_path(project, '.').
user:file_search_path(config_enabled, 'config-enabled-swish').
user:file_search_path(config_enabled, swish('config-enabled')).

:- dynamic http:location/3.
:- multifile http:location/3.
http:location(root, '/', [priority(1100)]).
http:location(swish, root('swish'), [priority(500)]).
http:location(root, '/swish', []).

% http:location(swish, '/example', [priority(500)]).
%http:location(root, root('example'), [priority(500)]).
%http:location(example, root('example'), [priority(500)]).
% http:location(root, '/example', []).
:- http_handler(root(example), swish,[]).



nowdmsg(_).

% :- use_module(library(r/r_sandbox)).

:- if((false, exists_source(library(trill)))).

:- system:use_module(library(trill)).

:- reexport_from(system,trill:end_bdd/1),
   reexport_from(system,trill:add_var/5),
   reexport_from(system,trill:and/4),
   reexport_from(system,trill:em/8),
   reexport_from(system,trill:randomize/1),
   reexport_from(system,trill:rand_seed/1),
   reexport_from(system,trill:or/4),
   reexport_from(system,trill:ret_prob/3),
   reexport_from(system,trill:init_test/2),
   reexport_from(system,trill:end/1),
   reexport_from(system,trill:end_test/1),
   reexport_from(system,trill:bdd_not/3),
   reexport_from(system,trill:zero/2),
   reexport_from(system,trill:one/2),
   reexport_from(system,trill:equality/4),
   reexport_from(system,trill:init_bdd/2),
   reexport_from(system,trill:init/3).


:- use_module(library(pita),[]).

:- endif.
 
:- if(false).
:- use_module(library(cplint_r),[]).
:- use_module(library(mcintyre)).
:- use_module(library(slipcover)).
:- use_module(library(lemur),[]).
:- use_module(library(auc)).
:- use_module(library(matrix)).

%:- dynamic(user:db/1).
%:- thread_local(user:db/1).


:- use_module(library(clpr)).

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(nf_r:{_}).

:- endif.


/*

%prolog:prolog_load_file(library(swish/X),How):- trace, prolog:load_files([swish(lib/X)],How),!.
%prolog:prolog_load_file(swish(lib/swish/X),How):- prolog:load_files([swish(lib/X)],How),!.
*/

swish_config:config(show_beware,        false).
swish_config:config(community_examples, true).

swish_config:source_alias(project, [access(both), search('*.pl')]).
swish_config:source_alias(library, []).

swish_config:verify_write_access(Request, File, Options) :- currently_logged_in(swish_config:verify_write_access(Request, File, Options),_).

pengines:authentication_hook(Request, swish, User) :- 
   fail, currently_logged_in(pengines:authentication_hook(Request, swish, User),User),!.

:- multifile pengines:allowed/2.
:- dynamic pengines:allowed/2.

% dmiles runs in very well protected VM
pengines:not_sandboxed(_Maybe, _Application) :- gethostname(X),X=logicmoo.org.
pengines:not_sandboxed(Maybe, Application) :- currently_logged_in(pengines:not_sandboxed(Maybe, Application),_User),!.

current_user(User):- currently_logged_in(why,User).

currently_logged_in(_Why,D):- thread_self(main), ignore(D=default).
currently_logged_in(Why,User):- 
  from_http((http_session:
    (http_in_session(_SessionID),
     http_session_data(oauth2(OAuth, _)),
     http_session_data(user_info(OAuth, Info))),
   User=Info.name,!, 
    nop(wdmsg(currently_logged_in(Why,User=Info.name))))),!.

currently_logged_in(Why,User):- 
  from_http((http_session:
    (session_data(S,oauth2(OAuth, Y)),
      nop(wdmsg(currently_logged_in(User=Why,
      session_data(S,oauth2(OAuth, Y)))))))),!,
  ignore(User="guest1"),!.


currently_logged_in(Why,D):- http_session:http_in_session(SessionID),!,
   
  from_http(
  ((wdmsg(fail_dispite_http_in_session(SessionID,D,Why)),  
    http_session:http_in_session(SessionID),
    listing(http_session: session_data(SessionID,_Data))))),!,fail.



currently_logged_in(Why,D):- thread_self(S),wdmsg(fail_currently_logged_in(Why,S,D)),!,fail.


no_auth_needed(Request):- is_list(Request),memberchk(path_info(Path),Request),mimetype:file_mime_type(Path,Type),memberchk(Type,[image/_,_/javascript]),!.
no_auth_needed(Request):- is_list(Request),!,memberchk(path(Path),Request),no_auth_needed(Path).
no_auth_needed(X):- \+ atom(X),!,fail.
no_auth_needed(X):- atom_concat('/swish',XX,X),!,no_auth_needed(XX).
no_auth_needed('/chat').
%no_auth_needed('/login').
no_auth_needed('/').
no_auth_needed('').


:- multifile swish_config:authenticate/2.
:- dynamic swish_config:authenticate/2.

swish_config:authenticate(_Request, User) :- 
  http_session:
    (http_in_session(_SessionID),
     http_session_data(oauth2(OAuth, _)),
     http_session_data(user_info(OAuth, Info))),
   User=Info.name,!.

% swish_config:authenticate(Request, User) :- \+ http_session:http_in_session(_),current_user(User),with_output_to(current_error,nowdmsg((swish_config:authenticate(Request, User)))),!.

% :- use_module(pack('swish-with-filesystem-interaction'/swish)).

:- use_module(swish(swish)).

  
% swish_config:authenticate(Request, "bad_user") :- wdmsg(swish_config:authenticate(Request, "bad_user")),!.
swish_config:authenticate(Request, User) :-
        swish_http_authenticate:logged_in(Request, User), !.

  

:- maplist(  ( [F] >> (ensure_loaded('config-enabled-swish'/F))),
  [auth_google,  auth_stackoverflow,  data,   email,  hdt,    
          rlimit,  r_serve,  user_profile, % network,
   auth_unity,          debug,      logging,  notifications,  
    rpc , auth_http]).


%%	swish
%
%	Start the SWISH server and open the main page in your browser.

:- initialization(swish).

swish :-
	swish('0.0.0.0':3020).

swish(Port) :-
	http_server_property(Port, goal(swish_ide:http_dispatch)), !,
	open_browser(Port).
swish(_:Port) :-
	integer(Port),
	http_server_property(Port, goal(swish_ide:http_dispatch)), !,
	open_browser(Port).
swish(Port) :-
	http_server(http_dispatch,
		    [ port(Port),
		      workers(16)
		    ]),
	open_browser(Port).

open_browser(Port):- !, dmsg(open_browser(Port)).
open_browser(Address) :-
	host_port(Address, Host, Port),
	http_server_property(Port, scheme(Scheme)),
	http_absolute_location(root(.), Path, []),
	format(atom(URL), '~w://~w:~w~w', [Scheme, Host, Port, Path]),
	wdmsg(www_open_url(URL)).

host_port(Host:Port, Host, Port) :- !.
host_port(Port,Host, Port):- gethostname(Host),!.
host_port(Port,_, Port):-!.

:- [library(pengines)].

:- multifile pengines:allowed/2.
:- dynamic pengines:allowed/2.

:- asserta((
  pengines:allowed(Request, Application) :- 
  Application=swish-> true; 
   currently_logged_in(pengines:allowed(Request, Application),_User))).

% Or Sure we trust that our sandbox is good enough
% pengines:allowed(_,_). 

:- listing(pengines:allowed/2).


pet:- pengine_rpc("http://prologmoo.com:3020",
                       sin_table(X,Y),
                       [ src_text(':- dynamic(sin_table/2). sin_table(1,2).'),
                         application(swish)
                       ]),
   wdmsg(sin_table(X,Y)).


%:- debug.

:- swish.


user:file_search_path(What, Alias):- % maybe confirm this is not SWISH?
     non_swish_file_search_path(What, Alias).

:- listing(swish_config:authenticate/2).
:- listing(pengines:allowed/2).

:- debug(authenticate).
:- debug(chat(_)).
:- debug(cm(change)).
:- debug(dot).
:- debug(html(script)).
:- debug(http(authenticate)).
:- debug(http(error)).
:- debug(http(redirect)).
:- debug(http(request)).
:- debug(http_authenticate).
:- debug(http_path).
:- debug(http_session).
:- debug(hub(_)).
:- debug(login).
% :- debug(modules).
:- debug(notify(_)).
:- debug(notify).
:- debug(openid(_)).
:- debug(openid).
:- debug(openid_fake(_)).
:- debug(pack(mirror)).
:- debug(pengine(abort)).
:- debug(pldoc).
:- debug(plweb).
:- debug(predicate_options).
:- debug(setting).
:- debug(settings).
:- debug(stats).
:- debug(storage).
:- debug(swish(search)).
:- debug(websocket(_)).
% :- prolog_ide(debug_monitor).



% :- initialization(user:ensure_loaded(run_clio)).


end_of_file.

:- tdebug.
:- debug(pengine(_)).
:- debug(sandbox(_)).
:- debug(swish(_)).
:- debug(sldnf(_)).
:- debug(http_authenticate(_)).
:- debug(http(_)).
