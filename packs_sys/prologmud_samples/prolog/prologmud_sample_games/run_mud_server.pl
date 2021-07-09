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
%:- set_prolog_flag(xpce, false).

%:- reconsult('/opt/logicmoo_workspace/lib/swipl/xpce/prolog/lib/pce.pl').

% :- dynamic(pce_principal:send/2).
%:- lock_predicate(pce_principal:send/2).
% :- use_module(library(jpl)).

:- thread_local(t_l:squelch_message/0).

:- meta_predicate(wo_messages(0)).
wo_messages(G):- locally(t_l:squelch_message,G).

squeltch:squelch_message(_):- t_l:squelch_message.

push_msg(_) :- t_l:squelch_message, !.
push_msg(Term) :-
    nb_current('$inprint_message', Messages),
    !,
    \+ ( '$member'(Msg, Messages),
         Msg=@=Term
       ),
    b_setval('$inprint_message', [Term|Messages]).
push_msg(Term) :-
    b_setval('$inprint_message', [Term]).

pop_msg :- t_l:squelch_message, !.
pop_msg :-
    (   nb_current('$inprint_message', [_|Messages]),
        Messages\==[]
    ->  b_setval('$inprint_message', Messages)
    ;   nb_delete('$inprint_message'),
        b_setval('$inprint_message', [])
    ).

print_message_guarded(_,_) :- t_l:squelch_message, !.
print_message_guarded(Level, Term) :-
 '$messages':(
    (   must_print(Level, Term)
    ->  (   translate_message(Term, Lines, [])
        ->  (   nonvar(Term),
                (   notrace(user:thread_message_hook(Term, Level, Lines))
                ->  true
                ;   notrace(user:message_hook(Term, Level, Lines))
                )
            ->  true
            ;   '$inc_message_count'(Level),
                print_system_message(Term, Level, Lines),
                maybe_halt_on_error(Level)
            )
        )
    ;   true
    )).

:- multifile '$messages':print_message/2.
:- dynamic '$messages':print_message/2.
:- asserta(('$messages':print_message(A,B):- notrace((squeltch:squelch_message('$messages':print_message(A,B)))),!)).
:- dynamic message_hook/3.
:- multifile message_hook/3.
:- asserta((message_hook(A, B, C):- notrace((squeltch:squelch_message(message_hook(A, B, C)))),!)).
:- dynamic message_hook/1.
:- multifile message_hook/1.
:- asserta((message_hook(A):- notrace((squeltch:squelch_message(message_hook(A)))),!)).


pre_run_mud_server:-
 
 % volatile(http_log:log_stream/1),
 volatile(http_log:log_stream/2),
 volatile(prolog_listing:opened_source/3),
  %(current_prolog_flag(xpce, true) -> (noguitracer,tnodebug) ; true),
  discontiguous(rdf11:'$exported_op'/3),
  discontiguous(phil:'$exported_op'/3),
  discontiguous(lemur:'$exported_op'/3),
  multifile(rdf11:'$exported_op'/3),
  multifile(phil:'$exported_op'/3),
  multifile(lemur:'$exported_op'/3),

  multifile(swish_help:help_files/1),
  multifile(cp_label:rdf_link/4),
  dynamic(cp_label:rdf_link/4),
  
  multifile(swish_render_rdf:rdf_link/4),
  dynamic(swish_render_rdf:rdf_link/4),
  
  (getenv('DISPLAY',_)->true;setenv('DISPLAY','10.0.0.78:0.0')),
  %(notrace(gtrace),nodebug),
  %set_prolog_flag(verbose_load,true),
  set_prolog_flag(pfc_version,v(2,0,0)),
  set_prolog_flag(dmsg_level,always),
  
  discontiguous(rdf11:'$exported_op'/3),
  discontiguous(phil:'$exported_op'/3),
  discontiguous(lemur:'$exported_op'/3),
  multifile(rdf_rewrite:arity/2),
  dynamic(rdf_rewrite:arity/2),!.

never_catch:- 
   current_prolog_flag(access_level,Was),
   set_prolog_flag(access_level,system),
   redefine_system_predicate(system:catch/3),
   abolish(system:catch,3),
   meta_predicate(system:catch(0,?,0)),
   system:assert((catch(G,E,C):-mycatch(G,E,C))),   
   set_prolog_flag(access_level,Was).



:- initialization(pre_run_mud_server, now).
:- initialization(pre_run_mud_server, restore_state).


never_notrace:- 
   abolish_notrace,
   current_prolog_flag(access_level,Was),
   set_prolog_flag(access_level,system),
   redefine_system_predicate(system:notrace/1),
   abolish(system:notrace,1),
   meta_predicate(system:notrace(0)),
   system:assert((notrace(G):-once(G))),   
   set_prolog_flag(access_level,Was).
%:- never_notrace.

abolish_notrace:- redefine_system_predicate(system:notrace/0),abolish(system:notrace/0),asserta(system:notrace).

never_portray:- 
   current_prolog_flag(access_level,Was),
   set_prolog_flag(access_level,system),
   abolish(prolog:portray,1),dynamic(prolog:portray/1),
   abolish(user:portray,1),dynamic(user:portray/1),
   retractall(prolog:portray(_)),
   retractall(user:portray(_)),
   set_prolog_flag(access_level,Was).

attach_packs_relative(Rel):-
   once(((
    (working_directory(Dir,Dir);prolog_load_context(directory,Dir)),
    (absolute_file_name(Rel,PackDir,[relative_to(Dir),file_type(directory),solutions(all),file_errors(fail)]);
      absolute_file_name(Rel,PackDir,[file_type(directory),solutions(all),file_errors(fail)])),
    writeln(attach_packs(PackDir)),attach_packs(PackDir)));writeln(failed(attach_packs_relative_web(Rel)))).

load_package_dirs_0:-
  ignore(( \+ exists_source(library(logicmoo_common)), attach_packs_relative('../../..'))),
  attach_packs('/opt/logicmoo_workspace/packs_sys',[duplicate(keep)]),
  attach_packs('/opt/logicmoo_workspace/packs_lib',[duplicate(keep)]),
  attach_packs('/opt/logicmoo_workspace/packs_web',[duplicate(keep)]),
  !.

load_package_dirs_1:-
  ignore(catch(make_directory('/tmp/tempDir/pack'),_,true)),
  (user:file_search_path(pack,'/tmp/tempDir/pack') -> true ; asserta(user:file_search_path(pack,'/tmp/tempDir/pack'))),
  attach_packs('/tmp/tempDir/pack'),  
  % nop(pack_install(logicmoo_utils,[upgrade(true),interactive(false)])),
  !.

load_package_dirs:-
  findall(PackDir,'$pack':pack(Pack, PackDir),Before),  
  load_package_dirs_0,
  load_package_dirs_1,
  findall(PackDir,'$pack':pack(Pack, PackDir),After),
  (Before\==After -> writeln(load_package_dirs(After)) ; true),
  pack_list_installed,
  use_module(library(logicmoo_common)),
  % use_module(library(logicmoo_packs)).
  !.

:- initialization(load_package_dirs, now).
:- initialization(load_package_dirs, restore_state).


set_startup_flags:-
  set_prolog_flag(runtime_speed, 0), % 1 = default
  set_prolog_flag(runtime_debug, 3), % 2 = important but dont sacrifice other features for it
  set_prolog_flag(runtime_safety, 3),  % 3 = very important
  set_prolog_flag(unsafe_speedups, false),
  set_prolog_flag(logicmoo_message_hook,dumpst),
  set_prolog_flag(encoding,text),
  set_prolog_flag(lisp_repl_goal,prolog),
  current_prolog_flag('argv',Is),writeq(current_prolog_flag('argv',Is)),!,nl,
  !.

:- initialization(set_startup_flags, now).
:- initialization(set_startup_flags, restore_state).


:- use_module(library(prolog_deps)).
:- use_module(library(logicmoo_common)).

%:- bfly.

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
   '--sigma', % Sigma Inference Engine Server  https://logicmoo.org/swish/lm_xref/
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
system:set_modules_baseKB :-  nop(set_modules(baseKB)).

:- initialization(set_modules_baseKB, restore_state).
:- initialization(set_modules_baseKB, now).


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

do_setup_history:-!. % this comments out the next lines
do_setup_history:-  
 ((
  current_input(S),
  ignore(catch(prolog:history(S, load), _, true)),  
  logicmoo_startup:((
  maplist(add_hist,
[ (mpred_why(mudIsa(iCoffeeCup7, tSpatialThing))),
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



abolish_module(M):-
 notrace(forall(
   (current_predicate(M:F/A), functor(P,F,A), \+ predicate_property(M:P, imported_from(_))),
    (predicate_property(M:P, static) -> abolish(M:F/A) ; retractall(M:P)))),!,
   (exists_file(M) -> unload_file(M) ; true).




% test LPS is not broken yet
melee:- lps_sanity(lps_tests('Melee')).
system:lps_sanity:- lps_sanity(lps_tests('binaryChop2.pl' )).
restaurant:- lps_sanity(lps_tests('restaurant')).
goat:- lps_sanity(lps_tests('goat')).
ballot:- lps_sanity(lps_tests('Ballot')).

:- dynamic(user:file_search_path/2).
:- multifile(user:file_search_path/2).
user:file_search_path(lps_tests, Dir):-
 %absolute_file_name(library('.'),LibDir,[file_type(directory),solutions(all),access(exist),file_errors(fail)]),
 member(A,['../test/lps_planner/','../test/ec_planner/abdemo_test/','../test/lps_user_examples/','../examples/']),
 absolute_file_name(library(A),Dir,[ /*relative_to(LibDir),*/ file_type(directory),solutions(all),access(exist),file_errors(fail)]),
 exists_directory(Dir).


lps_demo_tests:- lps_sanity(lps_tests('lps_demo_tests')).
lps_demo_test_1:- lps_sanity(lps_tests('lps_demo_test_1')).
lps_demo_test_2:- lps_sanity(lps_tests('lps_demo_test_2.pl')).
lps_demo_test_3:- lps_sanity(lps_tests('lps_demo_test_3.pl')).
lps_demo_test_4:- lps_sanity(lps_tests('lps_demo_test_4.pl')).
lps_demo_test_5:- lps_sanity(lps_tests('lps_demo_test_5.pl')).
lps_demo_test_9:- lps_sanity(lps_tests('lps_demo_test_9.pl')).

lps_insanity(File):- 
   absolute_file_name(File,M,[access(read),extensions(['pl','P','lps','pfc.pl',''])]),
   M\==File,!,lps_insanity(M).

lps_insanity(M):-
   M:use_module(library(lps_corner)),
   interpreter:check_lps_program_module(M),  
   M:unload_file(M),
   M:consult(M),
   %listing(db:actions/1),
   %listing(interpreter:actions/1),
   interpreter:get_lps_program_module(M),
   notrace(from_elsewhere:listing(M:_)),
   wdmsg(running(M)),
   % M:golps(X),
   ignore((M:godc(X),
   %listing(interpreter:lps_program_module/1),
   notrace(print_tree(X)))),!,
   run_tests_from_file(M).

run_tests_from_file(File):- 
 forall((clause(ec:demo_test(Name, Type, Goal),Body,R),clause_property(R,source(File))),
  (forall(call(Body),
   (pprint_ecp_cmt(blue, do(demo_test(Name, Type))),  %Type \== slow, 
     abdemo(Goal))))).

lps_sanity(File):- Limit = 110580,
 catch(call_with_depth_limit(lps_insanity(File), Limit, R), E,(R=E)),
   format(user_error,"~N ~q~n",[lps_sanity=R]),
   ((integer(R),R<Limit)-> true; (dumpST,break,fail)).


baseKB:':-'(ConsqIn):- throw(':-'(ConsqIn)).
:- lock_predicate(baseKB:':-'/1).

% t:/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/ext/pldata/plkb7166/kb7166_pt7_constant_renames.pldata

/*
 (1) * /usr/local/share/swi-prolog/pack
   (2)   /usr/share/swi-prolog/pack
   (3)   /var/lib/snapd/desktop/swi-prolog/pack
   (4)   /etc/xdg/swi-prolog/pack

*/

:- multifile(html_write:html_meta/1).
:- dynamic(html_write:html_meta/1).

:-  use_module(library(prolog_autoload)).
:-  use_module(library(qsave)).

keep_user_module(Goal):- 
   setup_call_cleanup('$current_typein_module'(WasTIM), 
          setup_call_cleanup('$set_source_module'(Was,user), 
          Goal, 
          '$set_source_module'(_,Was)), 
   '$set_typein_module'(WasTIM)).
    
load_before_compile:- keep_user_module(load_before_compile_now).
load_before_compile_now:- 
   set_prolog_flag(ec_loader,false),
   skip_sandboxing,
   %set_prolog_flag(verbose_file_search,true), 
   use_module(library(sandbox)),
    use_module(library(logicmoo_webui)),
    webui_load_swish_and_clio,
    webui_start_swish_and_clio,
 % use_module(library(xlisting/xlisting_web)),

    use_module(library(logicmoo_lps)),
    use_module(library(logicmoo/butterfly_console)),
    use_module(library(logicmoo/pretty_clauses)),
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
   % ignore(( \+ exists_directory('/tmp/tempDir/') -> catch(shell('./PreStartMUD.sh'),_,true))),
   % ignore(( exists_directory('/tmp/tempDir') -> cd('/tmp/tempDir'))),
    use_module(library(pfc_lib)),
    use_module(library(xlisting/xlisting_web)),
    use_module(library(lsp_server)),
    baseKB:ensure_loaded(library(logicmoo_nlu)),!,
    load_before_compile_now2.


load_before_compile_now2:- 
    baseKB:ensure_loaded(library(logicmoo_mud)),
    baseKB:ensure_loaded(library(logicmoo_clif)),        
    %register_logicmoo_browser,
  % never_notrace,
  % bfly_set(butterfly,t),
    load_nomic_mu,
    baseKB:ensure_loaded(library(logicmoo_cg)),
    baseKB:ensure_loaded(library(logicmoo_ec)),        
    baseKB:ensure_loaded(library('logicmoo/common_logic/common_logic_sumo.pfc')),   
    %system:reexport(pldata(kb_0988)),
    %ensure_loaded(pldata(kb_0988)),        
    baseKB:ensure_loaded(library(narsese)),   
    use_module(library(instant_prolog_docs)),
    add_hist(start_network). 


%start_network:- 
%   load_before_compile,!.
call_safely([H|T]):- !, maplist(call_safely,[H|T]).
call_safely((G,!,G2)):- !, call_safely(G),!,call_safely(G2).
call_safely((G,!)):- !, call_safely(G),!.
call_safely((G,G2)):-!,call_safely(G),call_safely(G2).
call_safely(G):- ignore(must_or_rtrace(G)),
  nop(check_memory(G)).

only_runtime(G):- (current_prolog_flag(logicmoo_compiling,true);compiling)-> true; call(G).

start_lsp_server:-
 lsp_server:
   (set_prolog_flag(toplevel_prompt, ''),
    debug(server),
    debug(server, "Starting stdio client", []),
    current_input(In),
    set_stream(In, buffer(full)),
    set_stream(In, newline(posix)),
    set_stream(In, tty(false)),
    set_stream(In, representation_errors(error)),
    % handling UTF decoding in JSON parsing, but doing the auto-translation
    % causes Content-Length to be incorrect
    set_stream(In, encoding(octet)),
    current_output(Out),
    set_stream(Out, encoding(utf8)),
    stdio_handler(A-A, In)).

start_network:- only_runtime(keep_user_module(start_network_now)).
start_network_now:-  
  call_safely(
   [
   %load_before_compile,
   user:use_module(library(eggdrop)),
   egg_go,   
   webui_start_swish_and_clio,
   thread_create(start_lsp_server,_,[detached(true),alias(lsp_server)]),
   threads,statistics]),
   !.

load_rest:- keep_user_module(load_rest_now).
load_rest_now:- 
  call_safely(
  [
   nodebug,   
   add_history((mmake, autodoc_test)),
   load_rest2]),
   !.

% for when dmiles is doing fast testing
load_rest2:- gethostname('logicmoo.org'), !.
load_rest2:- locally(set_prolog_flag(verbose_load,true),load_rest3).

load_rest3:- keep_user_module(load_rest3_now).
load_rest3_now:-
  call_safely(
  [
   set_modules_baseKB,
   add_hist(try_zebra),
   add_hist(start_all),
   add_hist(qsave_logicmoo),
  % (current_prolog_flag(gui_tracer,true)->noguitracer;true),
   % run_before_qsave,
   do_setup_history,
   nodebug,
   finish_processing_world]),
  !.

%:- add_hist(load_before_compile).

:- dynamic(lmconfig:has_import_module/2).
normalize_imports(M):- 
 forall(current_module(M),forall(import_module(M,Inherits),
  (assertz(lmconfig:has_import_module(M,Inherits)),format('mi(~q,~q).~n',[Inherits,M])))).

normalize_and_save_imports :- forall(current_module(M),normalize_imports(M)).

check_memory(_):- current_prolog_flag(check_memory,false),!.
check_memory(_):- \+ current_prolog_flag(check_memory,true),!.
check_memory(G):-
  set_prolog_flag(debug,true),
  set_prolog_flag(report_error,true),
  set_prolog_flag(debug_on_error,true),
  prolog_load_context(file,Y),
  writeln(prolog_load_context(file,Y)),
  gensym(akill,X),
  qsave_program(X),
  catch(process_create(path(true), [], []),
    error(resource_error(no_memory),_),
     (dumpST,wdmsg(no_memory(after,G)),break)).

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
  keep_user_module((    
   %load_rest,  
   % rtrace,
   %load_nomic_mu,% autoload_all([verbose(true)]), 
   import_some,
   expose_all,
   only_runtime(start_rest2))),
   !.

start_rest2:- \+ gethostname('logicmoo.org'), !.
start_rest2:- \+ current_predicate(baseKB:start_runtime_mud/0), !.
start_rest2:- 
  keep_user_module((  
  call_safely(
  [
    set_modules_baseKB,
   baseKB:start_runtime_mud,
   run_setup_now,  
   baseKB:start_mud_telnet, 
   % adventure,
   % lar,
   baseKB:listing(mudAtLoc),
   threads]))),
   !.


skip_sandboxing(F):- functor(P,F,1), 
  (SF1 = (sandbox:F/1)),
  sandbox:redefine_system_predicate(SF1),
  sandbox:multifile(SF1),
  sandbox:dynamic(SF1),
  sandbox:asserta((P:-!)).

skip_sandboxing:-
 set_prolog_flag(no_sandbox, true),
 maplist(skip_sandboxing,
  [safe_goal,
   safe_call,
   safe_directive,
   safe_meta_predicate,
   safe_source_directive,
   safe_load_file_option,
   safe_source_directive]).

:- skip_sandboxing.

%:- break.



start_all :- keep_user_module((start_network, start_rest)).

% :- use_module(library(pfc_lib)).

:- keep_user_module((load_before_compile)).

%:- lps_sanity.
%:- goat.

:- if( current_prolog_flag(xpce, true) ).
%:- noguitracer, tnodebug.
:- endif.


un_used:- abolish(check:cross_module_call,2),  
   asserta((check:cross_module_call(_Callee, _Context):- fail)).
un_used:- abolish(error:permission_error,3),  
   asserta((
    error:permission_error(Operation, PermissionType, Culprit) :-
    wdmsg((throw(error(permission_error(Operation,
                                 PermissionType,
                                 Culprit),
                _)))))).



%:- set_prolog_flag(debug,true).
:- set_prolog_flag(access_level,system).


%:- abolish(user:prolog_load_file/2).
%:- dynamic(user:prolog_load_file/2).
:- initialization(start_network,restore).
:- if( \+ current_prolog_flag(logicmoo_compiling,false)).
:- initialization(start_network,now).
:- endif.


:- load_rest.
:- initialization(start_rest,restore).
:- if( \+ current_prolog_flag(logicmoo_compiling,false)).
:- initialization(start_rest,now).
:- endif.
% :- initialization(qsave_logicmoo, main).
:- initialization(keep_user_module(initialize),restore).
:- if( \+ current_prolog_flag(logicmoo_compiling,false)).
:- initialization(keep_user_module(initialize),now).
:- endif.

%:- abolish( yall:(?)/0 ).
%:- delete_import_module(user,pfc_lib).

% swish_highlight:lazy_read_lines

:- if( current_prolog_flag(xpce, true) ).
%:- noguitracer, tnodebug.
:- endif.


% :- qsave_logicmoo.
%:- setenv('DISPLAY', '192.168.88.1:0.0').
%:- (notrace(gtrace),nodebug).
%:- guitracer.

% :- mu:srv_mu.

:- add_history((mmake, autodoc_test)).

swi_ide:- \+ current_prolog_flag(xpce, true), !.
swi_ide:- use_module(library(swi_ide)),
 ( getenv('DISPLAY',_)
   -> prolog_ide(thread_monitor)
    ;true).

:- add_history(swi_ide).
:- add_history([run_mud_server]).
:- add_history(forall(chat80(X),run_pipeline(X))).
:- add_history(run_pipeline("is there a man who becomes the greatest tenor?")).
:- add_history(test_chat80).
:- add_history(forall(((ape_test(_,X);fracas(X);e2c(X)),!);chat80(X),run_pipeline(X))).
:- add_history(never_notrace).
:- add_history(bfly_set(butterfly,t)).
:- add_history(bfly_tests).
:- add_history(test_pp).
:- add_history(x123).
:- add_history(search4term).
:- add_history(edit1term).
%:- add_history(never_catch).

:- meta_predicate(system:mycatch(0,?,0)).
system:mycatch(G,_E,_C):- call(G),!.

%:- break.

%:- autoload_all.
:- tdebug.
:- tnodebug.
:- gui_tracer:guitracer.
:- gui_tracer:noguitracer.
%:- bfly.

%:- autoload_all.
%:- never_notrace.
%:- never_catch.

%:- lps_sanity.

% :- prolog. 

end_of_file.


