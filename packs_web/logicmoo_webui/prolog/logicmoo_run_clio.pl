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


/* <module> xlisting_web
% Provides /logicmoo runtime preds browsing
%
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )). 
:- endif.
:- module(logicmoo_run_clio,[]).


% :- ensure_loaded(logicmoo_base).

% :- ensure_loaded(logicmoo(util/logicmoo_util_preddefs)).

:- kb_shared(mpred_online:semweb_startup/0).

:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.

:- if(if_defined(load_mud_www)).

:- kb_shared prolog:message/3.
% I WISH I COULD DO :- was_dynamic prolog:message/3.
%prolog:message(git(update_versions),A,A):-!.

:- was_dynamic(did_do_semweb_startup_late_once).
do_semweb_startup_late_once:-did_do_semweb_startup_late_once,!.
do_semweb_startup_late_once:-asserta(did_semweb_startup_late_once),forall(clause(semweb_startup_late,G),must(show_failure(why,G))).


% % % OFF :- system:use_module(user(user_db)).

:- was_dynamic http:location/3.
:- kb_shared http:location/3.

:- user:file_search_path(cliopatria,SP),
   exists_directory(SP),
   writeq(user:file_search_path(cliopatria,SP)),nl.
   %set_setting_default(cliopatria_binding:path, SP).
   %save_settings('moo_settings.db').
   %%setting(cliopatria_binding:path, atom, SP, 'Path to root of cliopatria install'),!.

% Load ClioPatria itself.  Better keep this line.
% % % OFF :- system:use_module(cliopatria(cliopatria)).
% % % OFF :- system:use_module(cliopatria('applications/help/load')).
% % % % OFF :- system:use_module(cliopatria(components/menu)).


:- if(not(current_predicate(user_db:grant_openid_server/2))).
user_db:grant_openid_server(_,_).
:- endif.

% Load package manager

% % % OFF :- system:use_module(library(cpack/cpack)).

% Load the remainder of the  configuration. The directory config-enabled
% can also be used to  load   additional  (plugin)  functionality.

% % % OFF :- system:use_module(library(conf_d)).

:- load_conf_d([ 'config-enabled' ], []).

:- nb_setval(pldoc_options,[ prefer(manual) ]).


%pre_http_location(hmud_files, '/hmud', [prefix,js(true)]).
:- http_handler('/hmud/', http_reply_from_files(pack(hMUD), []), [prefix]).
%user:file_search_path(hmud_files, '../pack/hMUD')).

% :- listing((pre_http_location(_, _, _))), retractall((pre_http_location(cliopatria, root('.'), []))), retractall((pre_http_location(_, root('.'), []))),!.
% doesn't descend from root because that's being moved for cliopatria
% http:location(cliopatria, root(cliopatria), [priority(100)]).

:- retractall((http:location(_, root('.'), []))).

:- retractall((http:location(cliopatria, _, _))).
:- http:asserta(http:location(cliopatria, root(cliopatria), [priority(100)])).

:- http:asserta((http:location(A,B,C):- (var(A),pre_http_location(A,B,C)))).
:- http:asserta((http:location(A,B,C):- (nonvar(A),pre_http_location(A,B,C),!))).

cliopatria_redir(Request):- with_all_dmsg((member(request_uri(URI),Request),fix_clio_atom(URI,NEWURI),http_redirect(moved,NEWURI,Request))).
fix_clio_atom(I,O):-atom(I),atom_concat('/cliopatria',O,I).
fix_clio_atom(IO,IO).

:- http_handler(root('cliopatria'),cliopatria_redir, [priority(100),prefix] ).



%pre_http_location(cliopatria, root('.'), [priority(100)]).
%pre_http_location(pldoc, root('.'), []).
pre_http_location(pldoc, root(pldoc), []).
pre_http_location(pldoc_man, pldoc(refman), []).
pre_http_location(pldoc_pkg, pldoc(package), []).
pre_http_location(pldoc_resource, A, []) :- call(http_dispatch:http_location_by_id,pldoc_resource, A).
pre_http_location(web, cliopatria(web), []).
pre_http_location(sesame, root(servlets), []).
pre_http_location(sparql, root(sparql), []).
pre_http_location(rdf_browser, cliopatria(browse), []).
pre_http_location(flint, cliopatria(flint), []).
pre_http_location(api, cliopatria(api), []).
pre_http_location(json, api(json), []).
pre_http_location(yasgui, cliopatria(yasgui), []).
pre_http_location(yasqe, cliopatria(yasqe), []).
pre_http_location(yasr, cliopatria(yasr), []).
pre_http_location(icons, root(icons), [priority(-100)]).
pre_http_location(css, root(css), [priority(-100)]).
pre_http_location(js, root(js), [priority(-100)]).
pre_http_location(openid, root(openid), [priority(-100)]).
pre_http_location(openid, root(openid), []).
pre_http_location(www, root(www), []).
pre_http_location(pldoc, root('help/source'), [priority(10)]).
pre_http_location(script, www(script), [js(true)]).
pre_http_location(yui, yui_base(build), [js(true)]).
pre_http_location(yui_examples, yui_base(examples), [js(true)]).
pre_http_location(yui_base, www('yui/2.7.0'), []).


/*
http_open:location(A, B) :-
        member(C, A),
        phrase(atom_field(location, B), C), !.
*/
semweb_startup_late:- cp_server:attach_account_info.

% :- asserta((user:file_search_path(A,B):-pre_file_search_path(A,B))).
 
%  % :- debug_logicmoo(_).

:- meta_predicate cpa_browse:when_true(0,//,?,?).
:- meta_predicate rdf_describe:rdf_include_labels(3,+,+).

% semweb_startup_late:- debug(http_request(_)),debug(cm(_)),debug(swish(_)),debug(storage).
% semweb_startup_late:- listing(pre_http_location/3).
% semweb_startup_late:- listing(http:location/3).
semweb_startup_late:- ensure_webserver.


% prolog:message(git(update_versions))  --> [ 'Updating GIT version stamps in the background.' ])).
% :-must(prolog:retract((message(git(update_versions,_,_):-_)))).

mpred_online:semweb_startup:- do_semweb_startup_late_once.

% :- ['../pack/swish/lib/authenticate'],swish_add_user(guru, 'top secret', []).

:- if_startup_script(do_semweb_startup_late_once).

:- during_boot(do_semweb_startup_late_once).

:- during_boot(ensure_webserver).

:- do_semweb_startup_late_once, ensure_webserver.

/*
:- on_x_debug(shell('wget http://localhost:3020/home')).
:- on_x_debug(shell('wget http://localhost:3020/help/source/doc_for?object=cliopatria:context_graph/3')).
:- on_x_debug(shell('wget http://localhost:3020/help/source/doc/devel/PrologMUD/pack/ClioPatria/hooks.pl')).
*/

% % % OFF :- system:use_module(library(semweb/rdf_http_plugin)).
% :- on_x_debug(rdf_load('http://prologmoo.com/downloads/mud.ttl',[format(trig),graph(foobar)])),!.
% :- on_x_debug(rdf_load('./mud.ttl',[format(trig),graph(foobar)])),!.
%:- on_x_log_throw(eggdrop:deregister_unsafe_preds).

:- endif.

end_of_file.


% :- pack_install(blog_core,[interactive(false)]).
load_blog_core% % % OFF :- system:use_module(library(arouter)),use_module(library(docstore)),use_module(library(bc/bc_main)),use_module(library(bc/bc_view)),
   thread_create(bc_main('site.docstore',[port(3080)]),_,[]).

%:- load_blog_core.

% :- debug(daemon).

% Do not run xpce in a thread. This disables forking. The problem here
% is that loading library(pce) starts the event dispatching thread. This
% should be handled lazily.
:- set_prolog_flag(xpce_threaded, false).
:- set_prolog_flag(message_ide,   false). % cause xpce to trap messages

% [Optionaly] Solve the Halting problem
% % % OFF :- system:use_module(library(process)).
% % % % OFF :- system:use_module(library(pce)).
%:- has_gui_debug -> true ; remove_pred(pce_principal,send,2).
%:- has_gui_debug -> true ; remove_pred(pce_principal,new,2).


% % % OFF :- system:use_module(library(http/thread_httpd)).
% % % OFF :- system:use_module(library(http/http_dispatch)).

pre_file_search_path(pldoc, library(pldoc)).
pre_file_search_path(package_documentation, swi('doc/packages')).
pre_file_search_path(pldoc, library(pldoc)).
pre_file_search_path(dtd, '.').
pre_file_search_path(dtd, swi('library/DTD')).
pre_file_search_path(demo, pce('prolog/demo')).
pre_file_search_path(contrib, pce('prolog/contrib')).
pre_file_search_path(image, pce(bitmaps)).
pre_file_search_path(rdfql, cliopatria(rdfql)).
pre_file_search_path(cpack, cliopatria(cpack)).
pre_file_search_path(web, web).
pre_file_search_path(cpacks, cliopatria('.')).
pre_file_search_path(library, cpacks(lib)).
pre_file_search_path(rdf, cpacks(rdf)).
pre_file_search_path(entailment, cpacks(entailment)).
pre_file_search_path(components, cpacks(components)).
pre_file_search_path(applications, cpacks(applications)).
pre_file_search_path(api, cpacks(api)).
pre_file_search_path(user, cpacks(user)).
pre_file_search_path(config_available, cpacks('config-available')).
pre_file_search_path(skin, cpacks(skin)).
pre_file_search_path(web, cpacks(web)).
pre_file_search_path(css, web(css)).
pre_file_search_path(icons, web(icons)).
pre_file_search_path(yui, web('yui/2.7.0')).
pre_file_search_path(js, web(js)).
pre_file_search_path(html, web(html)).
pre_file_search_path(help, web(help)).
pre_file_search_path(tutorial, web(tutorial)).
pre_file_search_path(flint, web('FlintSparqlEditor/sparql')).
pre_file_search_path(yasqe, web('yasqe/dist')).
pre_file_search_path(yasr, web('yasr/dist')).
pre_file_search_path(icons, library('http/web/icons')).
pre_file_search_path(css, library('http/web/css')).
pre_file_search_path(js, library('http/web/js')).
pre_file_search_path(library, cliopatria(lib)).
pre_file_search_path(bundle, library(bundles)).

pre_http_location(cliopatria, root('.'), [priority(100)]).
pre_http_location(pldoc, root('.'), []).
pre_http_location(pldoc_man, pldoc(refman), []).
pre_http_location(pldoc_pkg, pldoc(package), []).
pre_http_location(pldoc_resource, A, []) :- call(http_dispatch:http_location_by_id,pldoc_resource, A).
pre_http_location(web, cliopatria(web), []).
pre_http_location(sesame, root(servlets), []).
pre_http_location(sparql, root(sparql), []).
pre_http_location(rdf_browser, cliopatria(browse), []).
pre_http_location(flint, cliopatria(flint), []).
pre_http_location(api, cliopatria(api), []).
pre_http_location(json, api(json), []).
pre_http_location(yasgui, cliopatria(yasgui), []).
pre_http_location(yasqe, cliopatria(yasqe), []).
pre_http_location(yasr, cliopatria(yasr), []).
pre_http_location(icons, root(icons), [priority(-100)]).
pre_http_location(css, root(css), [priority(-100)]).
pre_http_location(js, root(js), [priority(-100)]).
pre_http_location(openid, root(openid), [priority(-100)]).
pre_http_location(openid, root(openid), []).
pre_http_location(www, root(www), []).
pre_http_location(pldoc, root('help/source'), [priority(10)]).
pre_http_location(script, www(script), [js(true)]).
pre_http_location(yui, yui_base(build), [js(true)]).
pre_http_location(yui_examples, yui_base(examples), [js(true)]).
pre_http_location(yui_base, www('yui/2.7.0'), []).



:- add_to_search_path_first(cliopatria, pack('ClioPatria')).
:- add_to_search_path_first(user, pack('ClioPatria/user')).

