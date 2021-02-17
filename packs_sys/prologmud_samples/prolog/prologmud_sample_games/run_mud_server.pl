#!/usr/bin/env swipl
/* * module  MUD server startup script in SWI-Prolog

?- 
 ignore(( \+ exists_source(library(prologmud_sample_games/run_mud_server)), 
     attach_packs('/opt/logicmoo_workspace/packs_sys'))),
 consult(library(prologmud_sample_games/run_mud_server)).

%  sudo -u prologmud_server gdb -x gdbinit -return-child-result -ex "set pagination off" -ex run -ex quit --args swipl -l run_mud_server.pl --all --nonet --noworld


C:\Users\logicmoo>rem C:\Windows\System32\runas.exe /savecred /user:remote-user 
 "\"C:\Program Files\swipl\bin\swipl.exe\" -f \"C:\Users\remote-user\AppData\Local\swi-prolog\pack\prologmud_samples\prolog\prologmud_sample_games\run_mud_server.pl\""


?- cd(library(prologmud_sample_games)).
?- [run_mud_server].


W:\opt\logicmoo_workspace\packs_sys\logicmoo_utils\prolog;W:\opt\logicmoo_workspace\packs_sys\logicmoo_base\prolog;W:\opt\logicmoo_workspace\packs_sys\pfc\prolog;W:\opt\logicmoo_workspace\packs_sys\logicmoo_nlu\prolog\logicmoo_nlu;W:\opt\logicmoo_workspace\packs_sys\prologmud\prolog;W:\opt\logicmoo_workspace\packs_sys\logicmoo_nlu\prolog\episodic_memory;W:\opt\logicmoo_workspace\packs_sys\logicmoo_nlu\ext\chat80;W:\opt\logicmoo_workspace\packs_sys\logicmoo_nlu\ext\ape


*/
:- discontiguous rdf11:'$exported_op'/3.
:- discontiguous phil:'$exported_op'/3.
:- discontiguous lemur:'$exported_op'/3. 

:- multifile rdf11:'$exported_op'/3.
:- multifile phil:'$exported_op'/3.
:- multifile lemur:'$exported_op'/3. 

:- multifile swish_help:help_files/1.


:- multifile(cp_label:rdf_link/4).
:- dynamic(cp_label:rdf_link/4).
:- multifile(swish_render_rdf:rdf_link/4).
:- dynamic(swish_render_rdf:rdf_link/4).

:- getenv('DISPLAY',_)->true;setenv('DISPLAY', '10.0.0.122:0.0').
%:- (notrace(gtrace),nodebug).
%:- set_prolog_flag(verbose_load,true).
:- set_prolog_flag(pfc_version,2.0).
:- set_prolog_flag(dmsg_level,always).

:- discontiguous rdf11:'$exported_op'/3. 
:- discontiguous phil:'$exported_op'/3.
:- discontiguous lemur:'$exported_op'/3.

attach_packs_relative(Rel):-
   once(((
    (working_directory(Dir,Dir);prolog_load_context(directory,Dir)),
    (absolute_file_name(Rel,PackDir,[relative_to(Dir),file_type(directory),file_errors(fail)]);
      absolute_file_name(Rel,PackDir,[file_type(directory),file_errors(fail)])),
    writeln(attach_packs(PackDir)),attach_packs(PackDir)));writeln(failed(attach_packs_relative_web(Rel)))).

load_package_dirs:-
  findall(PackDir,'$pack':pack(Pack, PackDir),Before),  
  ignore(( \+ exists_source(library(logicmoo_common)), attach_packs_relative('../../..'))),
  findall(PackDir,'$pack':pack(Pack, PackDir),After),
  (Before==After -> writeln(load_package_dirs(After)) ; true),
  ignore(catch(make_directory('/tmp/tempDir/pack'),_,true)),
  (user:file_search_path(pack,'/tmp/tempDir/pack') -> true ; asserta(user:file_search_path(pack,'/tmp/tempDir/pack'))),
  attach_packs('/tmp/tempDir/pack'),
  pack_install(logicmoo_utils,[upgrade(true),interactive(false)]),
  pack_list_installed,
  !.
 
:- initialization(load_package_dirs, now).
:- initialization(load_package_dirs, restore_state).


%:- dmsg("Ensure run_mud_server").
%:- rtrace(dmsg2("Ensure Run_MUD_SERVER1")).


%:- '$set_source_module'(baseKB).
%:- 'module'(baseKB).
%:- set_prolog_flag(runtime_speed,0). % 0 = dont care
:- set_prolog_flag(runtime_speed, 0). % 1 = default
:- set_prolog_flag(runtime_debug, 3). % 2 = important but dont sacrifice other features for it
:- set_prolog_flag(runtime_safety, 3).  % 3 = very important
:- set_prolog_flag(unsafe_speedups, false).
:- set_prolog_flag(logicmoo_message_hook,dumpst).
:- set_prolog_flag(encoding,text).
:- set_prolog_flag(lisp_repl_goal,true).

% :- initialization(shell('./PreStartMUD.sh'),program).

:- current_prolog_flag('argv',Is),writeq(current_prolog_flag('argv',Is)),!,nl.


:- use_module(library(prolog_deps)).
:- use_module(library(logicmoo_common)).

check_startup_flags:- 
   current_prolog_flag(argv,WasArgV),
   ignore((  
           \+ ((member(E,WasArgV), 
                atom_concat('--',_,E))),
   append(WasArgV,[
   '--',   

   '--mud', % Load MUD server
   '--world', % Load MUD server World
   %'--nonet' '--noworld',

   '--clif', % Support for CLIF
   '--sumo', % Support for SUMO
   '--nlkb', % Load CYC NL 
   '--cyckb', % Load CYC KB 
   '--tinykb', % Support for LarKC

   '--www', % https://logicmoo.org/*
   '--no-fork', '--workers=16', '--port=3020',
   %'--user=www-data',
   '--sigma', % Sigma Inference Engine Server  https://logicmoo.org/logicmoo/
   '--cliop',  % https://logicmoo.org/cliopatria/
   '--irc', % Launch IRC Eggdrop Client
   '--swish', % https://logicmoo.org/swish/
   '--docs', % https://logicmoo.org/pldoc/
   '--plweb',   % https://logicmoo.org/plweb/
   
   % '--lispsock', % '--wamcl', % is already implied by --lispsock

   '--logtalk', % Support Logtalk
   '--elfinder', % Support Filesystem Browser   https://logicmoo.org/ef/
   '--nopdt', % Prolog Development for Eclipse
   '--planner', % Load any planners

   '--all', % all default options (in case there are new ones!)
   '--defaults'
   ], NewArgV),
   set_prolog_flag('argv',NewArgV))),
   current_prolog_flag(argv,Is),
   asserta(lmconf:saved_app_argv(Is)),
   writeq(set_prolog_flag('argv',Is)),!,nl.

:- initialization(check_startup_flags, now).
:- initialization(check_startup_flags, restore_state).

system:set_modules(M) :- '$set_typein_module'(M),'$set_source_module'(M),module(M).
:- set_modules(baseKB).

% ==============================================
% WWW Support
% ==============================================

% :- cpack_install([prov,amalgame,skos,cpack_repository,media_cache,'EDM','cloud',trill_on_swish,ecdemo,command,rdf_qa,waisda,jquery,accurator,pirates,cluster_search_ui,skos_browser,tag_matcher,statistics,opmv,vumix]).


load_plweb :-  
 % set_prolog_flag(cant_qsave_logicmoo,true),
 %ignore(( \+ exists_source(pack(plweb/pack_info)), attach_packs('/opt/logicmoo_workspace/packs_web'))),
 % :- attach_packs('/opt/logicmoo_workspace/packs_web/plweb/packs').
 @((user:['/opt/logicmoo_workspace/packs_web/plweb/plweb.pl'],
  doc_enable(true),
  call(call,call(plweb:with_mutex(plweb_init, server_init)))),plweb).


% ==============================================
% ============= MUD SERVER CODE LOADING =============
% ==============================================


remove_undef_srch:- remove_undef_search.

:- before_boot(remove_undef_srch).

add_hist(X):- nop(add_history(X)).
:- baseKB:import(add_hist/1).

do_setup_history:-!.
do_setup_history:-  
 ((
  current_input(S),
  ignore(catch(prolog:history(S, load), _, true)),  
  logicmoo_startup:((
  add_hist([
  (mpred_why(mudIsa(iCoffeeCup7, tSpatialThing))),
  (make:make_no_trace),
  (update_changed_files),
  (shell('./PreStartMUD.sh')),
  ([pack(logicmoo_base/t/examples/fol/'einstein_simpler_03.pfc')]),
  ([pack(logicmoo_base/t/examples/fol/'einstein_simpler_04.pfc')]),
  (make:make_no_trace),
  (load_plweb),
  (help(match_regex/2)),
  (list_undefined),
  (listing(lmconf:at_restore_goal/2)),
  (statistics),        
  (make),        
  (mmake),
  (login_and_run),
  ignore((prolog_load_context(file,File),forall((source_file(Code,File),strip_module(Code,_,Atom),atom(Atom)),(Code)))),
  (loadSumo),
  (loadTinyKB),
  (threads),
  (run_pending_inits),
  % (use_module(library(sexpr_reader))),
  (input_to_forms("( #\\a #\\u0009 . #\\bell )",'$VAR'('O'),'$VAR'('Vs'))),
  (tstl),
  (qconsult_kb7166),
  (qsave_logicmoo),
  (start_all),
  (load_before_compile),
  (adventure),
  (start_all),
  (start_mud_telnet),
  (lar),
  (lst)]),
  
  maplist(add_hist, [    
   % rtrace,
   load_nomic_mu,% autoload_all([verbose(true)]), 
   import_some,
   expose_all,
   %baseKB:start_runtime_mud,
   run_setup_now,  
   %baseKB:start_mud_telnet, 
   % adventure,
   % lar,
   baseKB:listing(mudAtLoc)]),
   add_hist(try_zebra),
   add_hist(start_all),
   add_hist(qsave_logicmoo),
  add_hist((+ 1 = _Y)))))),
  !.

:- before_boot(do_setup_history).

zebra00:- [(pack(logicmoo_base/t/examples/fol/'einstein_simpler_04.pfc'))].
% :- kif_compile.
%:- load_library_system(library(logicmoo_nlu)).
:- set_prolog_flag(ec_loader,false).
%:- abolish(system:trace,0).
%:- asserta(system:trace).
%load_lpn :- prolog_load_context(directory,D), cd('/home/prologmud_server/lpn/www'),user:[server],cd(D).
try_zebra:- mpred_trace_all, zebra00, 
 forall(trait(P),listing(P)),
 add_hist(clif_show),
 add_hist(listing(person)),
 clif_show.

load_nomic_mu:- 
  % set_prolog_flag(cant_qsave_logicmoo,true),
  mu:ensure_loaded(library(nomic_mu)),
  add_hist(srv_mu_main),
  add_history(mu:srv_mu),
  !.

:- user:use_module(library(eggdrop)).

% ==============================================
% =========== LOGICMOO COMPILATION =============
% ==============================================
% :- prolog_load_context(directory,D),cd(D).
dont_export(_,F,_):- atom_concat('$',_,F),!.
dont_export(M,_,_):- atom_concat('$',_,M),!.
dont_export(_,attr_unify_hook,2).
dont_export(_,attribute_goals,3).
dont_export(_,project_attributes,2).
dont_export(_,attr_portray_hook,2).
dont_export(_,portray,1).
dont_export(_,term_expansion,_).
dont_export(_,rdf_rewrite,_).
dont_export(rdf_rewrite,_,_).
dont_export(utility_translation,_,_).
dont_export(_,goal_expansion,_).
dont_export(_,clause_expansion,_).


expose_all:- !.
expose_all:-
     forall((current_predicate(M:F/A),functor(P,F,A),
       (predicate_property(M:P,imported_from(RM))->true;RM=M),
       \+ dont_export(M,F,A)),
       (catch((RM:export(RM:F/A),system:import(RM:F/A)),E,nop(dmsg(E))))).


% test LPS is not broken yet
system:lps_0( '/opt/logicmoo_workspace/packs_sys/lps_corner/examples/binaryChop2.pl' ).

system:lps_2(M):- 
  lps_0(File), ignore(File = '/opt/logicmoo_workspace/packs_sys/lps_corner/examples/binaryChop2.pl'), 
  absolute_file_name(File,M), M:call(use_module(library(lps_corner))).

system:lps_3(M):- 
   interpreter:check_lps_program_module(M),
   M:consult(M),
   %listing(db:actions/1),
   %listing(interpreter:actions/1),
   interpreter:get_lps_program_module(M),
   notrace(elsewhere:listing(M:_)),
   M:golps(X),
   %listing(interpreter:lps_program_module/1),
   notrace(wdmsg(X)),
   abolish_module(M),!.

abolish_module(M):-
 notrace(forall(
   (current_predicate(M:F/A),functor(P,F,A), \+ predicate_property(M:P, imported_from(_)), \+ predicate_property(M:P, static)),
    retractall(M:P))),!,
   (exists_file(M) -> unload_file(M) ; true).


lps_1:- lps_2(_DB).

system:lps_2:- lps_2(M), lps_3(M).


:- multifile(rdf_rewrite:arity/2).
:- dynamic(rdf_rewrite:arity/2).                         
/*
 (1) * /usr/local/share/swi-prolog/pack
   (2)   /usr/share/swi-prolog/pack
   (3)   /var/lib/snapd/desktop/swi-prolog/pack
   (4)   /etc/xdg/swi-prolog/pack

*/
load_before_compile:- 
   %set_prolog_flag(verbose_file_search,true), 
   use_module(library(sandbox)),

   %use_module(library(lps_corner)),
   use_module(library(logicmoo_webui)),      
   %use_module(library(logicmoo_lps)),
   %set_prolog_flag(verbose_file_search,false),
   
   %:- use_module(library(logicmoo_nlu)).

   /*
   ignore(catch(pack_install(rocksdb),_,true)),
   ignore(catch(pack_install(sldnfdraw),_,true)),
   ignore(catch(pack_install(aleph),_,true)),
   ignore(catch(pack_install(phil),_,true)),
   ignore(catch(pack_install(cplint_r),_,true)),
   */
   ignore((
    \+ exists_directory('/tmp/tempDir/') -> catch(shell('./PreStartMUD.sh'),_,true))),
   %ignore(( exists_directory('/tmp/tempDir') -> cd('/tmp/tempDir'))),
   webui_load_swish_and_clio,   
   add_hist(start_network).

%start_network:- 
%   load_before_compile,!.

start_network:-       
   load_before_compile,
   egg_go,   
   webui_start_swish_and_clio,
   threads,statistics,
   !.

fdict:- dmsg:fmt90(lps_visualization(_22488{groups:[_22052{content:"left(A)",id:"left/1",order:3,subgroupStack:"false"},
   _22078{content:"right(A)",id:"right/1",order:3,subgroupStack:"false"},_22104{content:"searching(A)",id:"searching/1",order:3,
    subgroupStack:"false"},_22130{content:"Actions",id:"action",order:4}],items:[_22152{content:"0",end:2,group:"left/1",id:0,
    start:1,subgroup:"0",title:"Fluent left(0) initiated at 1<br/>and terminated at transition to 2"},_22190{content:"5",end:4,
    group:"left/1",id:1,start:2,subgroup:"5",title:"Fluent left(5) initiated at 2<br/>and terminated at transition to 4"},
    _22228{content:"7",end:21,group:"left/1",id:2,start:4,subgroup:"7",
    title:"Fluent left(7) initiated at 4<br/>and terminated at transition to 21"},_22266{content:"7",end:21,
    group:"right/1",id:3,start:3,subgroup:"7",title:"Fluent right(7) initiated at 3<br/>and terminated at transition to 21"},
    _22304{content:"9",end:3,group:"right/1",id:4,start:1,subgroup:"9",
    title:"Fluent right(9) initiated at 1<br/>and terminated at transition to 3"},
    _22342{content:"60",end:21,group:"searching/1",id:5,start:1,subgroup:"60",
     title:"Fluent searching(60) initiated at 1<br/>and terminated at transition to 21"},
    _22380{content:"sample(4)",group:"action",id:6,start:2,style:"color:green",title:"happens(sample(4),1,2)",type:"point"},
    _22418{content:"sample(7)",group:"action",id:7,start:3,style:"color:green",title:"happens(sample(7),2,3)",type:"point"},
    _22456{content:"sample(6)",group:"action",id:8,start:4,style:"color:green",title:"happens(sample(6),3,4)",type:"point"}]},[])).

load_rest:- 
   nodebug,
   load_nomic_mu,   
   load_before_compile,
   use_module(library(instant_prolog_docs)),
   baseKB:ensure_loaded(library(narsese)),
   add_history((mmake, autodoc_test)),
   load_rest2,
   !.

% for when dmiles is doing fast testing
% load_rest2:- gethostname('logicmoo.org'), !.
load_rest2:-
   locally(set_prolog_flag(verbose_load,true),load_rest3).

load_rest3:-
   set_modules(baseKB),
   baseKB:ensure_loaded(library(logicmoo_cg)),
   baseKB:ensure_loaded(library(logicmoo_ec)),
   baseKB:ensure_loaded(library(logicmoo_nlu)),
   baseKB:ensure_loaded(library(logicmoo_clif)),
   baseKB:ensure_loaded(library('logicmoo/common_logic/common_logic_sumo.pfc')),   
   add_hist(try_zebra),
   add_hist(start_all),
   add_hist(qsave_logicmoo),
   system:reexport(pldata(kb_0988)),
   (current_prolog_flag(gui_tracer,true)->noguitracer;true),
   % run_before_qsave,
   do_setup_history,
   nodebug,
   baseKB:ensure_loaded(library(logicmoo_mud)),
   finish_processing_world,
  !.

%:- add_hist(load_before_compile).

:- dynamic(lmconfig:has_import_module/2).
normalize_imports(M):- 
 forall(current_module(M),forall(import_module(M,Inherits),
  (assertz(lmconfig:has_import_module(M,Inherits)),format('mi(~q,~q).~n',[Inherits,M])))).

normalize_and_save_imports :- forall(current_module(M),normalize_imports(M)).


qsave_logicmoo :-   
   load_before_compile,
   set_prolog_flag(lisp_repl_goal,true),
   current_prolog_flag(stack_limit,Stack_limit),
   qsave_program(logicmoo_server,
     [ class(development), 
       verbose(true),
       stack_limit(Stack_limit),
       toplevel(prolog),
       goal(prolog),
       undefined(ignore), 
       op(save),
       % map('logicmoo_server.map'),
       foreign(no_save),
       autoload(true),       
       stand_alone(false)]),
   add_history(start_all),
   !.

import_some:- !.
import_some:- 
      forall((current_predicate(baseKB:F/A),M=baseKB,functor(P,F,A),
         (predicate_property(M:P,imported_from(RM))->true;RM=M)),
         (RM:export(RM:F/A),rtrace:import(RM:F/A))),   
      forall((current_predicate(M:F/A),M==baseKB,functor(P,F,A),
         (predicate_property(M:P,imported_from(RM))->true;RM=M)),
         (RM:export(RM:F/A),rtrace:import(RM:F/A))), !.

start_rest:-    
   load_rest,  
   % rtrace,
   %load_nomic_mu,% autoload_all([verbose(true)]), 
   import_some,
   expose_all,
   start_rest2,
   !.

% start_rest2:- \+ gethostname('logicmoo.org'), !.
start_rest2:- \+ current_predicate(baseKB:start_runtime_mud/0), !.
start_rest2:- 

   set_modules(baseKB),
   baseKB:start_runtime_mud,
   run_setup_now,  
   baseKB:start_mud_telnet, 
   % adventure,
   % lar,
   baseKB:listing(mudAtLoc),
   threads,
   !.

skip_sandboxing(F):- functor(P,F,1), 
  (SF1 = (sandbox:F/1)),
  sandbox:redefine_system_predicate(SF1),
  sandbox:multifile(SF1),
  sandbox:dynamic(SF1),
  sandbox:asserta((P:-!)).

skip_sandboxing:-
 maplist(skip_sandboxing,
  [safe_goal,
   safe_call,
   safe_directive,
   safe_meta_predicate,
   safe_source_directive,
   safe_load_file_option,
   safe_source_directive]).

:- skip_sandboxing.


start_all :- start_network, start_rest.

:- set_prolog_flag(no_sandbox, true).
% :- use_module(library(pfc_lib)).

:- load_before_compile.

lps_sanity:- Limit = 1000,
 catch(call_with_depth_limit(lps_2, Limit, R), E,(R=E)),
   format(user_error,"~N ~q~n",[lps_sanity=R]),
   ((integer(R),R<Limit)-> true; (dumpST,break,fail)),
   must(fdict).

:- lps_sanity.


:- noguitracer, tnodebug.

:- initialization(start_network,restore).
:- if( \+ compiling).
:- initialization(start_network,now).
:- endif.


:- set_prolog_flag(debug,true).
:- set_prolog_flag(access_level,system).


%:- abolish(user:prolog_load_file/2).
%:- dynamic(user:prolog_load_file/2).


:- load_rest.
:- initialization(start_rest,restore).
:- if( \+ compiling).
:- initialization(start_rest,now).
:- endif.
% :- initialization(qsave_logicmoo, main).
:- initialization(initialize,restore).
:- if( \+ compiling).
:- initialization(initialize,now).
:- endif.

:- volatile(http_log:log_stream/1).
:- volatile(http_log:log_stream/2).
:- volatile(prolog_listing:opened_source/3).
%:- abolish( yall:(?)/0 ).
%:- delete_import_module(user,pfc_lib).

:- meta_predicate aleph:abgen(*,*,*,0).
:- meta_predicate aleph:abgen(*,*,0).
:- meta_predicate aleph:abgen(*,0).
:- meta_predicate aleph:add_eqs(*,*,*,*,*,*,0).
:- meta_predicate aleph:add_eqs(*,*,*,*,*,0).
:- meta_predicate aleph:add_new_lit(*,*,*,*,*,*,0).
:- meta_predicate aleph:aleph_induce_theory(*,*,0).
:- meta_predicate aleph:aleph_induce_theory(*,0).
:- meta_predicate aleph:create_worker_pool(*,*,*,*,0).
:- meta_predicate aleph:cwinduce(0).
:- meta_predicate aleph:estimate_clauselength_distribution(*,*,*,*,0).
:- meta_predicate aleph:estimate_clauselength_scores(*,*,*,*,*,0).
:- meta_predicate aleph:evalfn(*,*,0).
:- meta_predicate aleph:execute_equality(0).
:- meta_predicate aleph:find_clause(*,*,0).
:- meta_predicate aleph:find_clause(*,0).
:- meta_predicate aleph:find_theory(*,*,0).
:- meta_predicate aleph:find_theory1(*,0).
:- meta_predicate aleph:flatten(*,*,*,*,0).
:- meta_predicate aleph:flatten_atom(*,*,*,*,*,*,*,0).
:- meta_predicate aleph:flatten_atoms(*,*,*,*,0).
:- meta_predicate aleph:flatten_lits(*,*,*,*,*,*,*,0).
:- meta_predicate aleph:gcws(*,*,*,*,0).
:- meta_predicate aleph:gcws(0).
:- meta_predicate aleph:gen_abduced_atoms(*,*,0).
:- meta_predicate aleph:get_atoms(*,*,*,*,*,0).
:- meta_predicate aleph:get_atoms1(*,*,*,*,*,0).
:- meta_predicate aleph:get_besthyp(*,0).
:- meta_predicate aleph:get_gain(*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,0).
:- meta_predicate aleph:get_gains(*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,0).
:- meta_predicate aleph:get_refine_gain(*,*,*,*,*,*,*,*,*,*,*,0).
:- meta_predicate aleph:get_sibgain(*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,0).
:- meta_predicate aleph:get_sibgains(*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,0).
:- meta_predicate aleph:get_theory_gain(*,*,*,*,*,*,*,*,*,*,0).
:- meta_predicate aleph:graphsearch(*,*,0).
:- meta_predicate aleph:insert_eqs(*,*,*,*,0).
:- meta_predicate aleph:reduce(*,*,0).
:- meta_predicate aleph:reduce(*,0).
:- meta_predicate aleph:rls_refine(*,*,*,0).
:- meta_predicate aleph:rls_search(*,*,*,*,*,0).
:- meta_predicate aleph:rls_thread(*,*,*,*,*,0).
:- meta_predicate aleph:rsat(*,0).
:- meta_predicate aleph:rsat(0).
:- meta_predicate aleph:sample_clauses(*,*,0).
:- meta_predicate aleph:sample_nclauses(*,*,*,0).
:- meta_predicate aleph:sat(*,*,0).
:- meta_predicate aleph:sat(*,0).
:- meta_predicate aleph:search(*,*,0).
:- meta_predicate aleph:sphyp(0).
:- meta_predicate aleph:theory_move(*,*,*,0).
:- meta_predicate aleph:theorysearch(*,*,0).
:- meta_predicate aleph:time(0,*,*).
:- meta_predicate aleph:time_loop(*,0,*).
:- meta_predicate aleph:tsearch(*,*,0).
:- meta_predicate aleph:work(*,*,0).
:- meta_predicate aleph:worker(*,*,0).
:- meta_predicate ape_utils:cpu_time(0,*).
:- meta_predicate baseKB:add_game_dir(*,0).
:- meta_predicate baseKB:agent_coerce_for(2,*,?,?,?).
:- meta_predicate baseKB:apply_cond(*,0).
:- meta_predicate baseKB:call_close_and_detatch(*,*,*,0).
:- meta_predicate baseKB:cycword_sem(*,*,0).
:- meta_predicate baseKB:dcgParse213(//,//,//,*,*).
:- meta_predicate baseKB:findall_set(?,0,*).
:- meta_predicate baseKB:freeze_safe(?,0).
:- meta_predicate baseKB:get_sorted_instances(?,*,3).
:- meta_predicate baseKB:hooked_random_instance(*,*,0).
:- meta_predicate baseKB:in_call(*,0,*,0).
:- meta_predicate baseKB:intersect(*,*,*,*,0,-).
:- meta_predicate baseKB:matcher_to_data_args(3,*,*,?,*,?).
:- meta_predicate baseKB:nonvar_must_be(*,0).
:- meta_predicate baseKB:now_try_game_dir(0).
:- meta_predicate baseKB:punless(0,0).
:- meta_predicate baseKB:random_instance_no_throw0(*,*,0).
:- meta_predicate baseKB:run_mud_test_clause(:,0).
:- meta_predicate baseKB:string_to_info(*,0).
:- meta_predicate baseKB:t(1,?).
:- meta_predicate baseKB:t(2,?,?).
:- meta_predicate baseKB:t(3,?,?,?).
:- meta_predicate baseKB:t(4,?,?,?,?).
:- meta_predicate baseKB:t(5,?,?,?,?,?).
:- meta_predicate baseKB:t(6,?,?,?,?,?,?).
:- meta_predicate baseKB:t(7,?,?,?,?,?,?,?).
:- meta_predicate baseKB:tdomcall(0).
:- meta_predicate baseKB:telnet_repl_writer(*,*,*,0).
:- meta_predicate baseKB:term_to_info(*,0).
:- meta_predicate baseKB:test_call0(0).
:- meta_predicate baseKB:thread_signal_blocked(*,0).
:- meta_predicate baseKB:time_as(*,0).
:- meta_predicate baseKB:time_as(0).
:- meta_predicate baseKB:trye(0).
:- meta_predicate baseKB:want_more_question(0).
:- meta_predicate baseKB:with_domain_preds(1).
:- meta_predicate baseKB:within_user(0).
:- meta_predicate common_logic_compiler:map_each_subterm_compound(2,*,*).
:- meta_predicate drs_to_coreace:conds_to_andlist(2,*,*).
:- meta_predicate ec_loader:must_or_dumpst(0).
:- meta_predicate ec_loader:only_dec_pl(0).
:- meta_predicate ec_loader:only_lps(0).
:- meta_predicate ec_nnf:if_dbg(0).
:- meta_predicate ec_nnf:thmust(0).
:- meta_predicate ec_reader:convert_e(1,+,+).
:- meta_predicate ec_reader:trans_e(*,*,1,?,+,*).
:- meta_predicate ec_reader:with_e_file_write2(1,?,+).
:- meta_predicate get_ape_results:call_ape(0).
:- meta_predicate grammar_words:try(0,*,*,*).
:- meta_predicate grammar_words:word(*,0,*,*).
:- meta_predicate grammar_words:word_initial(*,0,*,*).
:- meta_predicate grammar_words:word_noninitial(*,0,*,*).
:- meta_predicate grammar_words:words(*,0,*,*).
:- meta_predicate grammar_words:words_initial(*,0,*,*).
:- meta_predicate grammar_words:words_noninitial(*,0,*,*).
:- meta_predicate icl_int:ex(0,*,*).
:- meta_predicate icl_int:example_query(0).
:- meta_predicate icl_int:explain(0).
:- meta_predicate icl_int:explain(0,*).
:- meta_predicate icl_int:explain(0,*,*).
:- meta_predicate icl_int:prove(0,*,*,*,*,*,*,*).
:- meta_predicate icl_int:prove1(0,*,*,*,*,*,*,*).
:- meta_predicate icl_int:tprove(*,0,*,*,*,*,*,*,*).
:- meta_predicate logicmoo_ocl:tdomcall(0).
:- meta_predicate logicmoo_ocl:time_as(*,0).
:- meta_predicate logicmoo_ocl:time_as(0).
:- meta_predicate logicmoo_ocl:trye(0).
:- meta_predicate logicmoo_ocl:with_domain_preds(1).
:- meta_predicate logicmoo_startup:enotrace(0).
:- meta_predicate logicmoo_startup:with_abs_paths(1,?).
:- meta_predicate logicmoo_util_autocut:do_body(0).
:- meta_predicate logicmoo_util_autocut:do_body(0,*,*).
:- meta_predicate logicmoo_util_autocut:last_clause(0).
:- meta_predicate logicmoo_util_autocut:last_clause(0,*).
:- meta_predicate logicmoo_util_body_reorder:call_body_reorder_compare(*,*,0,0).
:- meta_predicate logicmoo_util_body_reorder:call_body_reorder_key(*,*,*,0,0).
:- meta_predicate logicmoo_util_body_reorder:reorder_if_var(*,0,0).
:- meta_predicate lps_server_UI:any_call(0).
:- meta_predicate mcintyre:take_a_sample(*,*,*,2,?).
%:- meta_predicate mpred_type_constraints:'__aux_maplist/2_freeze_rev+1'(*,0).
%:- meta_predicate mpred_type_constraints:'__aux_wrapper_594d82f1742fe8b6586d0fcc675e4bd8258e4541'(0).
:- meta_predicate mpred_type_constraints:freeze_rev(0,?).
:- meta_predicate mpred_type_constraints:lazy_1(0).
:- meta_predicate mu:api_invoke(+).
:- meta_predicate mu:api_invoke(+,?,?).
:- meta_predicate mu:apply_act(+,?,?).
:- meta_predicate mu:aXiom(+).
:- meta_predicate mu:aXiom(+,?,?).
:- meta_predicate mu:call_lf(?,0).
:- meta_predicate mu:call_z(1,?).
:- meta_predicate mu:eVent(*,+).
:- meta_predicate mu:eVent(*,+,*,?).
:- meta_predicate mu:map_apply_findall(+,?,?).
:- meta_predicate mu:munl_call(0).
:- meta_predicate mu:must_act(+,?,?).
:- meta_predicate mu:rapply_state(1,+,-,?).
:- meta_predicate mu:reframed_call(4,*,?).
:- meta_predicate mu:reframed_call(4,?,?,?,?).
:- meta_predicate mu:thread_create_adv(0,?,+).
:- meta_predicate parser_sharing:try_maybe_f(*,0,*).
:- meta_predicate psyntax:dumploaded(0,*).
:- meta_predicate rdf_describe:rdf_bounded_description(3,+,*,+,-).
:- meta_predicate rdf_describe:rdf_include_labels(3,+,+).
:- meta_predicate rsasak_forward_wa_star_h_add:replc_structure_vars(2,-).
:- meta_predicate rsasak_forward_wa_star_h_add:replc_structure_vars1(2,-).
:- meta_predicate rsasak_pddl_parser:dcgStructSetOpt(*,*,3,?,?).
:- meta_predicate rsasak_pddl_parser:dcgStructSetOptTraced(*,*,3,?,?).
:- meta_predicate rsasak_pddl_parser:effected_typed_list(3,*,?,*).
:- meta_predicate rsasak_pddl_parser:function_typed_list(3,*,?,*).
:- meta_predicate rsasak_pddl_parser:oneOrMore(3,*,?,?).
:- meta_predicate rsasak_pddl_parser:typed_list(3,*,?,*).
:- meta_predicate rsasak_pddl_parser:typed_list0(3,*,?,*).
:- meta_predicate rsasak_pddl_parser:typed_list_as_list(3,*,?,?).
:- meta_predicate rsasak_pddl_parser:typed_list_exact(3,*,?,*).
:- meta_predicate rsasak_pddl_parser:typed_list_keys(3,*,?,*).
:- meta_predicate rsasak_pddl_parser:zeroOrMore(3,*,?,?).
:- meta_predicate smtp:do_send_mail(*,*,*,1,*).
:- meta_predicate smtp:do_send_mail_cont(*,*,*,1,*,*).
:- meta_predicate smtp:error_cleanup(*,0).
:- meta_predicate states_explorer:my_ite(0,0,0).
:- meta_predicate swish_data_source:range(*,:,0).
:- meta_predicate swish_filesystems:catch_reply(0,?,0).
:- meta_predicate swish_svgtree:'__aux_maplist/3_filtered_tree+2'(*,*,3,+).
:- meta_predicate talkdb:erase_when(2,?,?).
:- meta_predicate talkdb:save_to_file(*,2,*).
:- meta_predicate utility_translation:time_goal(0,*).
:- meta_predicate utility_translation:timed_forall(0,0).
:- meta_predicate verbnet_iface:is_reloading(0).
:- meta_predicate xml_reader:error_catch(0,*,0).
:- meta_predicate xml_reader:immediateCall(*,0).

% swish_highlight:lazy_read_lines

:- noguitracer, tnodebug.

% :- qsave_logicmoo.
%:- setenv('DISPLAY', '192.168.88.1:0.0').
%:- (notrace(gtrace),nodebug).
%:- guitracer.

% :- mu:srv_mu.

:- add_history((mmake, autodoc_test)).

:- lps_sanity.

%:- prolog. 

end_of_file.


