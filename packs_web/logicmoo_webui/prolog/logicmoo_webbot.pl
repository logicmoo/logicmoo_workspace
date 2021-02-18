%#!/usr/bin/swipl 

:- module(logicmoo_webbot,[
 www_start/0,www_start/1]).

% ==============================================
% [Required] Load the Logicmoo Common Utils
% ==============================================
:- ensure_loaded(library(logicmoo_common)).


:- whenever_flag_permits(load_network,load_library_system(library(logicmoo_network))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dmsg("LOGICMOO WEBBOT").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

www_start:- app_argv_off('--www'),!.
www_start:- app_argv_off('--net'),!.
www_start:- www_start(3020).

:- use_module(library(http/thread_httpd)).
www_start(Port):- dmsg("WWW Server " = Port), http_server_property(Port, goal(_)),!.
www_start(Port):- http_server(http_dispatch,[ port(Port)]). % workers(16) 

app_argv_www(Flag):- app_argv_off(Flag),!,fail.
app_argv_www(Flag):- app_argv1(Flag),!.
app_argv_www(Flag):- app_argv_ok(Flag),(app_argv('--www');app_argv('--all')),!.


:- if(app_argv_www('--swish')).
:- dmsg("SWISH Server").
%:- user:load_library_system(logicmoo_swish).
:- endif.
%:- user:['lps_corner/swish/user_module_file.pl'].
%:- user:['swish/daemon.pl'].


:- if(app_argv_www('--cliop')).
%:- user:load_library_system(logicmoo_cliop).
:- endif.

:- if(app_argv_www('--plweb')).
:- dmsg("PLWEB Server").
:- user:load_library_system(logicmoo_plweb).
:- endif.

:- if(app_argv_www('--docs');app_argv_www('--pldoc')).
:- dmsg("PLDOC Server").
:- user:load_library_system(logicmoo_pldoc).
:- endif.


/*
:- if(app_argv_www('--www')).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(swi(library/http/html_write)).
:- endif.  % --www
*/


:- if(( false  , app_argv_www('--sigma'))).
:- dmsg("SIGMA-KE Server").
:- user:use_module(library(xlisting/xlisting_web)).
foo_broken.
:- listing(foo_broken).
:- break.
:- user:listing(baseKB:shared_hide_data/1).
:- set_fileAssertMt(baseKB).
foo_broken2.
:- listing(foo_broken2).
:- break.
%#:- set_current_module(baseKB).
:- endif.

:- if((app_argv('--irc'))).
:- if(exists_source(library(eggdrop))).
:- dmsg("Eggdrop Server").
:- user:use_module(library(eggdrop)).
%:- egg_go_fg.
:- endif.
:- endif.


:- if(app_argv('--www')).
%:- during_net_boot(www_start).
:- endif.

% :- break.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sanity tests that first run whenever a person stats the MUD to see if there are regressions in the system
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
%.
:- endif.


:- logicmoo_webbot:import(http_dispatch:http_dispatch/1).


%:- threads.

end_of_file.


