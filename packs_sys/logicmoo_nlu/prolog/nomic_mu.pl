/*
% NomicMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
% Bits and pieces:
%
%  LogicMOO, Inform7, FROLOG, Guncho, PrologMUD and Marty's Prolog Adventure Prototype
% 
% Copyright (C) 2004 Marty White under the GNU GPL 
% Sept 20,1999 - Douglas Miles
% July 10,1996 - John Eikenberry 
%
% Logicmoo Project changes:
%
% Main file.
%
*/

:- module(mu, [srv_mu/0, srv_mu/1, run_mu/0, mu_port/1, usage_mu/0]).

% nohup websocket_redir.sh dbutterfly 4004 &
:- use_module(library(logicmoo_common)).

:- multifile aXiom/1.
:- meta_predicate aXiom(+).
:- multifile aXiom/3.
:- meta_predicate aXiom(+,?,?).

%:- pack_install(dictoo).

%:- if(current_prolog_flag(argv,[])).
%  sudo -u prologmud_server gdb -x gdbinit -return-child-result -ex "set pagination off" -ex run -ex quit --args swipl -l run_mud_server.pl --all --world --repl --lisp --lispsock --sumo --planner --nonet --repl --noworld
:- if(\+ ((current_prolog_flag(argv,X),member(E,X),atom_concat('--',_,E)))).
/*
:- current_prolog_flag('argv',WasArgV),
   append(WasArgV,[         
   '--', % '--all',
   % '--pdt','--repl','--lisp','--lispsock','--swish','--docs','--plweb', '--cliop','--sigma','--www',
   % '--elfinder',  '--defaults'
   '--nonet', % we will start our own net servers in NomicMU
   '--irc',   
   %'--noworld',   
   %'--world',
   '--sumo', '--planner'], NewArgV),
   set_prolog_flag('argv',NewArgV).
:- current_prolog_flag('argv',Is),writeq(set_prolog_flag('argv',Is)),!,nl.
*/
:- endif.

%:- use_module(library(logicmoo_nlu)).
/*
% :- system:ensure_loaded(pack(logicmoo_nlu/prolog/logicmoo_nlu/parser_chat80)).
%:- system:ensure_loaded(pack(logicmoo_nlu/prolog/logicmoo_nlu/parser_pldata)).

:- use_module(library(logicmoo_nlu/parser_sharing)).
:- use_module(library(logicmoo_nlu/parser_tokenize)).
:- parser_all:ensure_loaded(library(logicmoo_nlu/parser_pldata)).
:- use_module(library(logicmoo_nlu/parser_chat80)).
:- use_module(library(logicmoo_nlu/parser_e2c)).

:- if(exists_source(library(logicmoo_nlu))).
% 
:- use_module(library(logicmoo_nlu)).
%:- use_module(library(logicmoo_nlu/parser_sharing)).
%:- use_module(library(logicmoo_nlu/parser_tokenize)).
%:- use_module(library(logicmoo_nlu/nl_pipeline)).
:- else.
% :- system:ensure_loaded(pack(logicmoo_nlu/prolog/logicmoo_nlu/parser_sharing)).
:- if(exists_source(pack(logicmoo_nlu/ext/pldata/nl_iface))).
:- ensure_loaded(pack(logicmoo_nlu/ext/pldata/nl_iface)).
:- else.
:- if(exists_source(library(nldata/nl_iface))).
% being in user is just to help debugging from console
%:- user:ensure_loaded(library(nldata/nl_iface)).
:- endif.
:- endif.
:- load_wordnet.
:- endif.

*/


:- ensure_loaded(library(episodic_memory/adv_main)).
:- ensure_loaded(library(episodic_memory/adv_telnet)).
:- if(\+ current_prolog_flag(ec_loader,false)).
%  :- use_module(library(ec_planner/ec_loader)).
:- endif.

%:- use_module(library(pfc_lib)).
% :- use_module(library(logicmoo_nlu)).

% :- use_module(library(logicmoo_mud)).

% :- use_module(library(dialect/ifprolog),except([time / 1])).
% :- use_module(./chat80).
% :- ensure_loaded('./adv_chat80'). 

:- dynamic(mu_tmp:no_autostart/0).
:- volatile(mu_tmp:no_autostart/0).

srv_mu(TwoSixSixSix) :-
  atom_concat('mu_',TwoSixSixSix,Alias),
  thread_property(_,alias(Alias)),!,  
  format('~NServer should be running on port ~w~n',[TwoSixSixSix]),
  threads, !.

srv_mu(TwoSixSixSix) :- 
  adv_server(TwoSixSixSix),
  asserta(mu_tmp:no_autostart),
  format('~NServer is starting on port ~w~n',[TwoSixSixSix]),
  threads,  
  !.
  
srv_mu:-
  mu_port(TwoSixSixSix),
  srv_mu(TwoSixSixSix),
  run_mu.
 
run_mu:- 
   setup_console,
   setup_call_cleanup(
      asserta(mu_tmp:no_autostart),
      once(must(mu:adventure)),
      retractall(mu_tmp:no_autostart)).

srv_mu_main:- mu_tmp:no_autostart,!.
srv_mu_main:- srv_mu.

mu_port(4004).

:- add_history((mu:srv_mu)).
   
usage_mu:- mu_port(Port), format('~N
You may start the server with:

 ?- mu:srv_mu(PortNum).
 
 
Or serve default port ~w:
 
 ?- mu:srv_mu.
 

For single-threaded debugging/devel
or to run as single player use:

 ?- mu:run_mu.
   

',[Port]).

:- fixup_exports.

%:- '$set_typein_module'(mu).

%:- runtime_boot(srv_mu_main, (main)).
:- initialization(usage_mu, program).
%:- initialization(usage_mu, (program)).

%:- listing(aXiom/3).
%:- srv_mu.
%:- break.
