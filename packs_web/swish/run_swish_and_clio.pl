
:- module(swish_ide,
	  [ start_swish_and_clio/0,
      start_swish_and_clio_real/0
      %remote_swish/0,
	    %remote_swish/1			% ?Port
	  ]).

/** <module> Utility LOGICMOO UTILS
This module starts Swish and Cliopatria. 

The purpose is to run Swish and Clio Together for LOGICMOO Web UI.   

- @author Douglas R. Miles
- @license LGPL


 Part of SWISH

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
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(www_browser)).
% set_prolog_flag(access_level,system), cls, forall(rtrace(file_search_path(X,Y)),writeln(file_search_path(X,Y))).
%:- set_prolog_flag(access_level,system).
%:- rtrace.
:- if(exists_source(library(uid))).
:- use_module(library(uid)).
:- endif.
%:- break.
:- user:use_module(library(semweb/rdf_library)).
:- system:use_module(library(semweb/rdf_library)).
:- system:use_module(library(settings)).


% So we dont get stuck in Console color snooping test!
:- system:use_module(library(console_input)).
:- initialization(system:use_module(library(console_input)),restore_state).
:- system:use_module(library(prolog_autoload)).
:- system:use_module(library(lists)).

:- if( \+ exists_source(library(sldnfdraw))).
:- attach_packs('/opt/logicmoo_workspace/packs_lib').
:- endif.


:- if( \+ exists_source(pack(plweb/pack_info))).
:- attach_packs('/opt/logicmoo_workspace/packs_web').
:- endif.

:- if( \+ exists_source(library(lps_syntax))).
:- attach_packs('/opt/logicmoo_workspace/packs_sys').
:- endif.

:- if( \+ exists_source(library(lps_syntax))).
:- attach_packs('/opt/logicmoo_workspace/packs_web').
:- endif.


/* not a * <module>

Open SWISH as an IDE for developing a local application.
*/


from_http(G):- 
 stream_property(Main_error, file_no(2)),
 flush_output(Main_error),
 call_cleanup(with_output_to(Main_error, G),
   flush_output(Main_error)).

:- meta_predicate(from_http(0)).

:- use_module(library(logicmoo_webui)).



		 /*******************************
		 *	       CONFIG		*
		 *******************************/
:- multifile pengines:allowed/2.
:- dynamic pengines:allowed/2.

:- multifile
	swish_config:config/2,			% Name, Value
	swish_config:source_alias/2,		% Alias, Options
	swish_config:verify_write_access/3,	% Request, File, Options
	pengines:authentication_hook/3,		% Request, Application, User
	pengines:not_sandboxed/2,		% User, Application
	user:file_search_path/2.		% Alias, Path

:- dynamic
	swish_config:config/2,			% Name, Value
	swish_config:source_alias/2,		% Alias, Options
	swish_config:verify_write_access/3,	% Request, File, Options
	pengines:authentication_hook/3,		% Request, Application, User
	pengines:not_sandboxed/2,		% User, Application
	user:file_search_path/2.		% Alias, Path

:- prolog_load_context(directory,Dir),asserta(user:file_search_path(swish, Dir)).

:- multifile(prolog:doc_directory/1).
:- dynamic(prolog:doc_directory/1).
prolog:doc_directory(_Dir).

user:file_search_path(project, '.').

:- dynamic http:location/3.
:- multifile http:location/3.
% http:location(root, '/remote', []).
http:location(root, '/', [priority(1100)]).
http:location(swish, root('swish'), [priority(500)]).

rsmsg(_):- !.
rsmsg(X):- compound(X),functor(X,currently_logged_in,_),!.
rsmsg(X):- wdmsg(X).                              

/*

%prolog:prolog_load_file(library(swish/X),How):- trace, prolog:load_files([swish(lib/X)],How),!.
%prolog:prolog_load_file(swish(lib/swish/X),How):- prolog:load_files([swish(lib/X)],How),!.
*/

:- if(false).

swish_config:config(show_beware,        false).
swish_config:config(community_examples, true).

swish_config:source_alias(project, [access(both), search('*.pl')]).
swish_config:source_alias(library, []).

swish_config:verify_write_access(Request, File, Options) :- currently_logged_in(swish_config:verify_write_access(Request, File, Options),_).

pengines:authentication_hook(Request, swish, User) :- fail, currently_logged_in(pengines:authentication_hook(Request, swish, User),User),!.

pengines:allowed(Request, Application) :- Application=swish-> true; currently_logged_in(pengines:allowed(Request, Application),_User),!.
pengines:allowed(_Request, _Application).


pengines:not_sandboxed(Maybe, Application) :- currently_logged_in(pengines:not_sandboxed(Maybe, Application),_User),!.
pengines:not_sandboxed(_Maybe, _Application) :- !.


currently_logged_in(_Why,User):- thread_self(main), ignore(User="guest").
currently_logged_in(Why,User):- 
  from_http((http_session:
    (http_in_session(_SessionID),
     http_session_data(oauth2(OAuth, _)),
     http_session_data(user_info(OAuth, Info))),
   User=Info.name,!,
   rsmsg(currently_logged_in(Why,User=Info.name)))),!.
currently_logged_in(Why,User):- 
  from_http((
  http_session:http_in_session(SessionID),
  http_session:session_data(SessionID, profile_id(Profile)),
  impl_profile_prolog_profile:profile_attribute(Profile, name, User))),!,
  rsmsg(currently_logged_in(User=Why)),!.
currently_logged_in(Why,User):- 
  from_http((
  http_session:session_data(_SessionID, profile_id(Profile)),
  impl_profile_prolog_profile:profile_attribute(Profile, name, User))),!,
  rsmsg(currently_logged_in(User=Why)),!.
currently_logged_in(Why,User):- 
  ignore(User="guest"),
  from_http((http_session:session_data(S,oauth2(OAuth, Y)),
     rsmsg(currently_logged_in(User=Why,session_data(S,oauth2(OAuth, Y)))))).  
currently_logged_in(no_auth_needed, User):- ignore(User="guest"),!.
currently_logged_in(Why,User):- 
  http_session:http_in_session(SessionID),  
  from_http(
  ((listing(http_session:session_data(SessionID,_Data)),
    rsmsg(fail_despite_http_in_session(SessionID,User,Why))))),!,fail.
currently_logged_in(Why,User):- thread_self(S),rsmsg(fail_currently_logged_in(Why,S,User)),!,fail.


no_auth_needed(Request):- is_list(Request),memberchk(path_info(Path),Request),mimetype:file_mime_type(Path,Type),memberchk(Type,[image/_,_/javascript]),!.
no_auth_needed(Request):- is_list(Request),!,memberchk(path(Path),Request),no_auth_needed(Path).
no_auth_needed(X):- \+ atom(X),!,fail.
no_auth_needed(X):- atom_concat('/swish',XX,X),!,no_auth_needed(XX).
no_auth_needed('/chat').
no_auth_needed('/login').
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

swish_config:authenticate(Request, User) :- 
  no_auth_needed(Request),!,
  ignore(currently_logged_in(no_auth_needed,User)),!,
  ignore(User="guest"),!.

%swish_config:authenticate(Request, User) :- swish_http_authenticate:logged_in(Request, User), !.

swish_config:authenticate(Request, User) :- http_session:http_in_session(_),
  currently_logged_in(request(Request),User),
  ignore(User="guest"),!.

% swish_config:authenticate(Request, "bad_user") :- rsmsg(swish_config:authenticate(Request, "bad_user")),!.
swish_config:authenticate(Request, User) :- 
   swish_http_authenticate:logged_in(Request, User), !.

swish_config:authenticate(Request, User) :- \+ http_session:http_in_session(_),
  currently_logged_in(authenticate(Request),User),
  from_http(rsmsg((swish_config:authenticate(authenticate(Request), User)))),!.

  
  

%%	swish
%
%	Start the SWISH server and open the main page in your browser.

remote_swish :-
	remote_swish('0.0.0.0':3020).

remote_swish(Port) :-
	http_server_property(Port, _), !,
	open_browser(Port).
remote_swish(_:Port) :-
	integer(Port),
	http_server_property(Port, _), !,
	open_browser(Port).
remote_swish(Port) :-
	catch(http_server(http_dispatch,
		    [ port(Port),
		      workers(16)
		    ]),_,fail),
	open_browser(Port).

open_browser(Address) :-
	host_port(Address, Host, Port),
	http_server_property(Port, scheme(Scheme)),
	http_absolute_location(root(swish), Path, []),
	format(atom(URL), '~w://~w:~w~w', [Scheme, Host, Port, Path]),
	rsmsg(www_open_url(URL)).

host_port(Host:Port, Host, Port) :- !.
host_port(Port,Host, Port):- gethostname(Host),!.
host_port(Port,_, Port):-!.

:- endif. % false



pet_test:- pengine_rpc("https://logicmoo.org:3020",
                       sin_table(X,Y),
                       [ src_text(':- dynamic(sin_table/2). sin_table(1,2).'),
                         application(swish)
                       ]),
   rsmsg(sin_table(X,Y)).


/*
:- if(\+ prolog_load_context(reload,true)).
:- abolish(prolog_stack:option/2).
:- multifile(prolog_stack:option/2).
:- thread_local(prolog_stack:option/2).
:- asserta(prolog_stack:option(1,2)).
:- retract(prolog_stack:option(1,2)).
:- prolog_stack:use_module(library(prolog_stack)).
:- endif.
*/


/*
:- if( current_prolog_flag(xpce,true) ).

% Debugging
:- prolog_ide(debug_monitor).
:- debug.
:- tdebug.
:- guitracer.
:- endif.


:- debug(dot).
:- debug(html(script)).
:- debug(hub(_)).
% :- debug(modules).
:- debug(pack(mirror)).
:- debug(pengine(abort)).
:- debug(pldoc).
:- debug(plweb).
:- debug(predicate_options).
:- debug(stats).
:- debug(storage).
:- debug(swish(search)).
:- debug(websocket(_)).
*/


some_debug:-
  debug(http(authenticate)),
  debug(http(error)),
  nodebug(http(redirect)),
  debug(http(request)),
  debug(http_authenticate),
  nodebug(http_path),
  debug(http_session),
  debug(setting),
  debug(settings),
  debug(login),
  debug(notify(_)),
  debug(notify),
  debug(openid(_)),
  debug(openid),
  debug(openid_fake(_)),
  debug(cm(tokens)),
  debug(authenticate),
  debug(chat(_)),
  debug(cm(change)),
  !.

:- multifile(cliopatria:menu_item/2).
:- dynamic(cliopatria:menu_item/2).      
:- asserta(cliopatria:menu_item(90=swish/swish, 'Swish Home')).
:- asserta(cliopatria:menu_item(300=query/swish, 'SWISH Prolog shell')).
/*
*/

%:- use_module(library(pengines)).


:- setting(listing:tab_distance, nonneg, 0,
           'Distance between tab-stops.  0 uses only spaces').


pengines_iri_hook(X,Y,Z):- '$rc':res_iri_hook(X,Y,Z).
:- register_iri_scheme(pengine, pengines_iri_hook, []).

swish_and_clio:is_module.

add_relative_search_path(Alias, Abs) :- fail,
	is_absolute_file_name(Abs), !,
	prolog_load_context(file, Here),
	relative_file_name(Abs, Here, Rel),
	assertz(user:file_search_path(Alias, Rel)).
add_relative_search_path(Alias, Rel) :-
	assertz(user:file_search_path(Alias, Rel)).

%user:file_search_path(cliopatria, '/opt/logicmoo_workspace/packs_web/ClioPatria').
%user:file_search_path(swish,      '/opt/logicmoo_workspace/packs_web/swish').
%user:file_search_path(lps_corner, '/opt/logicmoo_workspace/packs_web/lps_corner').

:- lmconfig:logicmoo_webui_dir(Dir),
   % trace,
   absolute_file_name('../../ClioPatria/',Run,[relative_to(Dir),file_type(directory),file_errors(fail)]),
   add_relative_search_path(cliopatria, Run).

/*
:- multifile(swish_authenticate:user_property/2).
:- dynamic(swish_authenticate:user_property/2).
:- swish_authenticate:export(swish_authenticate:user_property/2).
:- user_db:import(swish_authenticate:user_property/2).

:- multifile(user_db:user_property/2).
:- dynamic(user_db:user_property/2).
:- user_db:export(user_db:user_property/2).
:- swish_authenticate:import(user_db:user_property/2).
*/

% Use the ClioPatria help system.  May   be  commented to disable online
% help on the source-code.
:- user:use_module(cliopatria('applications/help/load')).

% Load ClioPatria itself.  Better keep this line.
:- user:use_module(cliopatria(cliopatria)).

% :- listing(cp_server:cp_server).
%:- listing(user_property/2).

:- if( \+ current_module(lps_server_UI) ).
%:- lps_corner:ensure_loaded(library(lps_corner)).
% :- ensure_loaded(lps_corner(swish/user_module_clio)).
:- endif.


:- swish:use_module(swish(swish)).


rt123:- rtrace(swish_highlight:codemirror_tokens([protocol(http),method(post),request_uri('/swish/cm/tokens'),
    path('/swish/cm/tokens'),http_version(1-1),host('logicmoo.org'),port(3020),
    connection('keep-alive'),cache_control('max-age=0'),peer(ip(192,168,88,1)),
    accept_encoding('gzip, deflate'),accept_language('en-US,en;q=0.9'),cookie(['_ga'='GA1.2.2901774.1587353525',
    swipl_session='2c10-e3c5-65e9-df9a.gitlab']),content_type(html)])).

% :- cpack_install([prov,amalgame,skos,cpack_repository,media_cache,'EDM','cloud',trill_on_swish,ecdemo,command,rdf_qa,waisda,jquery,accurator,pirates,cluster_search_ui,skos_browser,tag_matcher,statistics,opmv,vumix]).


% X = lps_visualization(_1422{groups:[_2144{content:"left(A)", id:"left/1", order:3, subgroupStack:"false"}, _2190{content:"right(A)", id:"right/1", order:3, subgroupStack:"false"}, _2238{content:"searching(A)", id:"searching/1", order:3, subgroupStack:"false"}, _2286{content:"Actions", id:"action", order:4}], items:[_1440{content:"0", end:2, group:"left/1", id:0, start:1, subgroup:"0", title:"Fluent left(0) initiated at 1<br/>and terminated at transition to 2"}, _1518{content:"5", end:4, group:"left/1", id:1, start:2, subgroup:"5", title:"Fluent left(5) initiated at 2<br/>and terminated at transition to 4"}, _1596{content:"7", end:21, group:"left/1", id:2, start:4, subgroup:"7", title:"Fluent left(7) initiated at 4<br/>and terminated at transition to 21"}, _1674{content:"7", end:21, group:"right/1", id:3, start:3, subgroup:"7", title:"Fluent right(7) initiated at 3<br/>and terminated at transition to 21"}, _1754{content:"9", end:3, group:"right/1", id:4, start:1, subgroup:"9", title:"Fluent right(9) initiated at 1<br/>and terminated at transition to 3"}, _872{content:"60", end:21, group:"searching/1", id:5, start:1, subgroup:"60", title:"Fluent searching(60) initiated at 1<br/>and terminated at transition to 21"}, _954{content:"sample(4)", group:"action", id:6, start:2, style:"color:green", title:"happens(sample(4),1,2)", type:"point"}, _1030{content:"sample(7)", group:"action", id:7, start:3, style:"color:green", title:"happens(sample(7),2,3)", type:"point"}, _1106{content:"sample(6)", group:"action", id:8, start:4, style:"color:green", title:"happens(sample(6),3,4)", type:"point"}]}, [])+dot(digraph([node([1], [label = 'left(0)\nright(9)\nsearching(60)', fillcolor = '#D7DCF5', style=filled, color=black]), node([3], [label = 'left(5)\nright(7)\nsearching(60)', fillcolor = '#D7DCF5', style=filled, color = '#D7DCF5']), node([2], [label = 'left(5)\nright(9)\nsearching(60)', fillcolor = '#D7DCF5', style=filled, color = '#D7DCF5']), node([4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20], [label = 'left(7)\nright(7)\nsearching(60)', fillcolor = '#D7DCF5', style=filled, color = '#D7DCF5']), edge(([1]->[2]), [label=sample(4), color=forestgreen]), edge(([2]->[3]), [label=sample(7), color=forestgreen]), edge(([3]->[4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]), [label=sample(6), color=forestgreen])])).
% X = dot(digraph([node([1], [label = 'left(0)\nright(9)\nsearching(60)', fillcolor = '#D7DCF5', style=filled, color=black]), node([3], [label = 'left(5)\nright(7)\nsearching(60)', fillcolor = '#D7DCF5', style=filled, color = '#D7DCF5']), node([2], [label = 'left(5)\nright(9)\nsearching(60)', fillcolor = '#D7DCF5', style=filled, color = '#D7DCF5']), node([4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20], [label = 'left(7)\nright(7)\nsearching(60)', fillcolor = '#D7DCF5', style=filled, color = '#D7DCF5']), edge(([1]->[2]), [label=sample(4), color=forestgreen]), edge(([2]->[3]), [label=sample(7), color=forestgreen]), edge(([3]->[4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]), [label=sample(6), color=forestgreen])])).

:- stream_property(X,file_no(2)),set_stream(X,alias(main_error)).


:- dynamic(did_start_swish_and_clio/0).
start_swish_and_clio:- did_start_swish_and_clio,!.
start_swish_and_clio:- !.
%start_swish_and_clio:- start_swish_and_clio_real.
start_swish_and_clio_real:- asserta(did_start_swish_and_clio),
 % :- cd('/opt/logicmoo_workspace/packs_web/ClioPatria').
   current_prolog_flag(argv,WasArgV),
   current_prolog_flag(os_argv,WasOSArgV),
   
   setup_call_cleanup((set_prolog_flag(argv,[]),set_prolog_flag(os_argv,[swipl])),
      cp_server:cp_server([]),
     (set_prolog_flag(argv,WasArgV),set_prolog_flag(os_argv,WasOSArgV))),
   nop(remote_swish),
   broadcast:broadcast(http(post_server_start)).

:- use_module(swish(lib/plugin/login)).


:- runtime_boot(start_swish_and_clio).




