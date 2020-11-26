#!/usr/local/bin/swipl

:- module(logicmoo_swish, [ ]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(broadcast)).
% :- user:use_module(library(logicmoo_utils)).

%:- baseKB:use_module(library(pfc_lib)).

% :- baseKB:use_module(library(logicmoo_clif)).

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(tlbugger:rtracing).
sandbox:safe_primitive(tlbugger:dont_skip_bugger).
sandbox:safe_primitive(tlbugger:skip_bugger).
sandbox:safe_primitive(tlbugger:skipMust).
:- forall(current_predicate(tlbugger:P),asserta(sandbox:safe_primitive(tlbugger:P))).


:- set_prolog_flag(no_sandbox, true).

% :- initialization(user:run_swish, program).
:- remove_undef_search.
%:- tdebug.
%:- guitracer.
:- add_history((repeat,sleep(10),make,join_threads,fail)).
% :- interactor.
run_logicmoo_swish:- 
    set_prolog_flag(toplevel_goal, prolog),
    current_prolog_flag(argv, Argv),
    argv_options(Argv, _, Options),
    option(port(Port), Options, 3020),
    (   option((public true), Options)
    ->  Address=Port
    ;   Address=localhost:Port
    ),
    broadcast(http(pre_server_start)),
	http_server(http_dispatch,
		    [ port(Address),
		      workers(6)
		    ]),
	broadcast(http(post_server_start)).


end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.

:- module(logicmoo_swish,
          [ swish/0,
            swish/1                     % ?Port
          ]).


/** <module>

Remote SWISH as an IDE for developing a Remote application.
*/

/*
:- use_module(library(sanity_must)).


:- if(\+ current_prolog_flag(lm_no_autoload,_)).
:- set_prolog_flag(lm_no_autoload,false).
:- dmsg("WARNING: PFC_AUTOLOAD").
:- endif.

%:- set_prolog_flag(lm_pfc_lean,false).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(www_browser)).

:- if(exists_source(library(uid))).
:- use_module(library(uid)).
:- endif.
*/
%:- multifile(owl2_model:datatype/2).
%:- dynamic(owl2_model:datatype/2).
                                  

/* < module>

Open SWISH as an IDE for developing a remote application.
*/



:- meta_predicate(from_http(0)).
from_http(G):- stream_property(Main_error,file_no(2)), with_output_to(Main_error, G).

:- ignore((stream_property(X,file_no(2)),
   stream_property(X,alias(Was)),
   set_stream(X,alias(main_error)),
   set_stream(X,alias(Was)))).

:- use_module(library(aleph),[]).


:- use_module(library(sanity_must)).

reexport_from(ReExporter,From:P):-
    From:export(From:P),
    ReExporter:import(From:P),
    ReExporter:export(From:P).



:- multifile(pldoc_register:process_stored_comments/0).
:- dynamic(pldoc_register:process_stored_comments/0).
:- assert(pldoc_register:process_stored_comments).


                 /*******************************
                 *             CONFIG           *
                 *******************************/

:- multifile
        swish_config:config/2,                  % Name, Value
        swish_config:source_alias/2,            % Alias, Options
        swish_config:verify_write_access/3,     % Request, File, Options
        pengines:authentication_hook/3,         % Request, Application, User
        pengines:not_sandboxed/2,               % User, Application
        pengines:allowed/2,                     % Peer, Application
        user:file_search_path/2.                % Alias, Path

is_symlink_to(File,AbsLink):-
         read_link(File,Link,Link),
         absolute_file_name(Link,AbsLink,[access(read),file_errors(fail)]),
         absolute_file_name(File,AFile,[access(read),file_errors(fail)]),
         AFile \== Link.

swish_home(AbsDir):- swish_home0(Dir),resolve_symlinks(Dir,AbsDir).

resolve_symlinks(File,Link):- is_symlink_to(File,AbsLink)-> resolve_symlinks(AbsLink,Link) ; File=Link.

swish_home0(Dir):- absolute_file_name(pack(swish),Dir,[file_type(directory),access(read),file_errors(fail)]),!.
swish_home0(Dir):- absolute_file_name(pack('swish-with-filesystem-interaction'),Dir,[file_type(directory),access(read),file_errors(fail)]).
swish_home0(Dir):- absolute_file_name(cpack(swish),Dir,[file_type(directory),access(read),file_errors(fail)]).
swish_home0(Dir):- is_symlink_to('./remote_ide.pl',To),file_directory_name(To,Dir).

:- discontiguous(user:file_search_path/2).
user:file_search_path('swish', Dir):-  swish_home(Dir),!.

:- nodebug.

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

:- asserta(sandbox:safe(_,_,_,_,_):-!).

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
pengines:not_sandboxed(_Maybe, _Application) :- gethostname(X),X=gitlab.
pengines:not_sandboxed(Maybe, Application) :- currently_logged_in(pengines:not_sandboxed(Maybe, Application),_User),!.

current_user(User):- currently_logged_in(why,User).

currently_logged_in(_Why,D):- thread_self(main), ignore(D=default).
currently_logged_in(Why,User):-
  from_http((http_session:
    (http_in_session(_SessionID),
     http_session_data(oauth2(OAuth, _)),
     http_session_data(user_info(OAuth, Info))),
   User=Info.name,!,
    nop(dmsg(currently_logged_in(Why,User=Info.name))))),!.

currently_logged_in(Why,User):-
  from_http((http_session:
    (session_data(S,oauth2(OAuth, Y)),
      nop(dmsg(currently_logged_in(User=Why,
      session_data(S,oauth2(OAuth, Y)))))))),!,
  ignore(User="guest1"),!.


currently_logged_in(Why,D):- http_session:http_in_session(SessionID),!,

  from_http(
  ((dmsg(fail_dispite_http_in_session(SessionID,D,Why)),
    http_session:http_in_session(SessionID),
    listing(http_session: session_data(SessionID,_Data))))),!,fail.



currently_logged_in(Why,D):- thread_self(S),dmsg(fail_currently_logged_in(Why,S,D)),!,fail.


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


% swish_config:authenticate(Request, "bad_user") :- dmsg(swish_config:authenticate(Request, "bad_user")),!.
swish_config:authenticate(Request, User) :-
        swish_http_authenticate:logged_in(Request, User), !.



:- maplist(  ( [F] >> (ensure_loaded('config-enabled-swish'/F))),
  [auth_google,  auth_stackoverflow,  data,   email,  hdt,
          % rlimit,
            % r_serve,
              user_profile, % network,
   auth_unity,          debug,      logging,  notifications,
    rpc , auth_http]).


%%      swish
%
%       Start the SWISH server and open the main page in your browser.

swish :-
        swish('0.0.0.0':3020).

swish(Port) :-
        http_server_property(Port, goal(logicmoo_swish:http_dispatch)), !,
        open_browser(Port).
swish(_:Port) :-
        integer(Port),
        http_server_property(Port, goal(_)), !,
        open_browser(Port).
swish(Port) :-
        http_server(logicmoo_swish:http_dispatch,
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
        dmsg(www_open_url(URL)).

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

%:- listing(pengines:allowed/2).


pet:- pengine_rpc("http://prologmoo.com:3020",
                       sin_table(X,Y),
                       [ src_text(':- dynamic(sin_table/2). sin_table(1,2).'),
                         application(swish)
                       ]),
   dmsg(sin_table(X,Y)).


user:file_search_path(What, Alias):- % maybe confirm this is not SWISH?
     non_swish_file_search_path(What, Alias).

   
:- multifile
	cp_menu:menu_item/2,
	cp_menu:menu_popup_order/2.
:- dynamic
	cp_menu:menu_item/2,
	cp_menu:menu_popup_order/2.

:- asserta(cp_menu:menu_item(400=places/swish,		'SWISH')).
:- asserta(cp_menu:menu_popup_order(swish,       550)).
:- asserta(cp_menu:menu_item(200=swish/swish,		'SWISH')).

%:- initialization(swish).
%:- swish.

end_of_file.

:- tdebug.
:- debug(pengine(_)).
:- debug(sandbox(_)).
:- debug(swish(_)).
:- debug(sldnf(_)).
:- debug(http_authenticate(_)).
:- debug(http(_)).

