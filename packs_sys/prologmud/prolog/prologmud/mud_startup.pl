/* * module * 
% This file loads the world (world.pl), the map of the world, 
% the agents and their definitions.
% This file is used as a configuation file and a startup script.
%
% July 10,1996
% John Eikenberry
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

%:-pfc_untrace.
%:-pfc_no_spy_all.

use_baseKB :- '$set_typein_module'(baseKB),'$set_source_module'(baseKB),module(baseKB).
:- use_baseKB.

% ==============================================
% ============= MUD SERVER CODE LOADED =============
% ==============================================

:- with_mpred_trace_exec(ain(isLoaded(iSourceCode7))).

%:- flag_call(runtime_debug=3).

:- if((gethostname(ubuntu),fail)). % INFO this fail is so we can start faster
:- show_entry(gripe_time(40, doall(baseKB:regression_test))).
:- endif.


% ==============================================
% [Optional] Creates or suppliments a world
% ==============================================
set_default_sample_games:- user:file_search_path(sample_games,_Dir),!.
set_default_sample_games:- 
   must((catch(absolute_file_name(library('prologmud_sample_games/'),Dir,[file_type(directory), access(read)]),_,true),
   ignore((nonvar(Dir),asserta(user:file_search_path(sample_games,Dir)))))).

%:- if( \+ user:file_search_path(sample_games,_Dir)).
:- set_default_sample_games.
%:- sanity(user:file_search_path(sample_games,_Dir)).
%:- endif.

:- dynamic(lmconf:eachRule_Preconditional/1).
:- dynamic(lmconf:eachFact_Preconditional/1).
:- assert_setting01(lmconf:eachRule_Preconditional(true)).
:- assert_setting01(lmconf:eachFact_Preconditional(true)).

:- if(functorDeclares(mobExplorer)).

:- sanity(functorDeclares(tSourceData)).
:- sanity(functorDeclares(mobExplorer)).


==>((tCol(tLivingRoom),
 tSet(tRegion),
 tSet(tLivingRoom),

 tSet(mobExplorer),
 genls(tLivingRoom,tRegion),
 genls(tOfficeRoom,tRegion),


genlsFwd(tLivingRoom,tRegion),
genlsFwd(tOfficeRoom,tRegion),

% create some seats
mobExplorer(iExplorer1),
mobExplorer(iExplorer2),
mobExplorer(iExplorer3),
mobExplorer(iExplorer4),
mobExplorer(iExplorer5),
mobExplorer(iExplorer6),

(tHumanBody(skRelationAllExistsFn)==>{trace_or_throw(tHumanBody(skRelationAllExistsFn))}),

genls(mobExplorer,tHominid))).

:- endif.


% ==============================================
% [Required] isRuntime Hook
% ==============================================
(((localityOfObject(P,_),isRuntime)==>{if_defined(put_in_world(P))})).


:- set_prolog_flag_until_eof(do_renames,term_expansion).



% ==============================================
% [Optional] Creates or suppliments a world
% ==============================================
:- if( \+ app_argv('--noworld')).
:- if( \+ tRegion(_)).

==> prologHybrid(mudAreaConnected(tRegion,tRegion),rtSymmetricBinaryPredicate).
==> rtArgsVerbatum(mudAreaConnected).

==>((
tRegion(iLivingRoom7),
tRegion(iOfficeRoom7),

mobExplorer(iExplorer7),
wearsClothing(iExplorer7,'iBoots773'),
wearsClothing(iExplorer7,'iCommBadge774'),
wearsClothing(iExplorer7,'iGoldUniform775'),
mudStowing(iExplorer7,'iPhaser776'))).

:- kb_shared(baseKB:tCol/1).
:- kb_shared(baseKB:ttCoercable/1).
% :- add_import_module(mpred_type_isa,baseKB,end).
onSpawn(localityOfObject(iExplorer7,tLivingRoom)).

==>((
pddlSomethingIsa('iBoots773',['tBoots','ProtectiveAttire','PortableObject','tWearAble']),
pddlSomethingIsa('iCommBadge774',['tCommBadge','ProtectiveAttire','PortableObject','tNecklace']),
pddlSomethingIsa('iGoldUniform775',['tGoldUniform','ProtectiveAttire','PortableObject','tWearAble']),
pddlSomethingIsa('iPhaser776',['tPhaser','Handgun',tWeapon,'LightingDevice','PortableObject','Device-SingleUser','tWearAble']),

mobMonster(iCommanderdata66),
mobExplorer(iCommanderdata66),
mudDescription(iCommanderdata66,txtFormatFn("Very scary looking monster named ~w",[iCommanderdata66])),
tAgent(iCommanderdata66),
tHominid(iCommanderdata66),
wearsClothing(iCommanderdata66,'iBoots673'),
wearsClothing(iCommanderdata66,'iCommBadge674'),
wearsClothing(iCommanderdata66,'iGoldUniform675'),
mudStowing(iCommanderdata66,'iPhaser676'),

pddlSomethingIsa('iBoots673',['tBoots','ProtectiveAttire','PortableObject','tWearAble']),
pddlSomethingIsa('iCommBadge674',['tCommBadge','ProtectiveAttire','PortableObject','tNecklace']),
pddlSomethingIsa('iGoldUniform675',['tGoldUniform','ProtectiveAttire','PortableObject','tWearAble']),
pddlSomethingIsa('iPhaser676',['tPhaser','Handgun',tWeapon,'LightingDevice','PortableObject','Device-SingleUser','tWearAble']))).


onSpawn(localityOfObject(iCommanderdata66,tOfficeRoom)).
onSpawn(mudAreaConnected(tLivingRoom,tOfficeRoom)).
:- endif.
:- endif.

:- if( \+ is_startup_script(_) ).
%:- init_why("run_mud_server").
:- endif.


%:- set_prolog_flag(access_level,system).
%:- debug.



start_mud_server:-  
  on_x_log_cont((call(call,start_mud_telnet))).

% ==============================================
% [Optionaly] Start the telent server % iCommanderdata66
% ==============================================
%:- if( \+ app_argv('--nonet')).
:- after_boot(start_mud_server).
% :- assert_setting01(lmconf:eachFact_Preconditional(isRuntime)).
%:- endif.

% [Manditory] This loads the game and initializes so test can be ran
:- baseKB:ensure_loaded(sample_games('src_game_nani/objs_misc_household.pfc')).
:- baseKB:ensure_loaded(sample_games('src_game_nani/a_nani_household.pfc')).

% isa(starTrek,mtHybrid).
%lst :- !.
lst :- baseKB:ensure_loaded(sample_games('src_game_startrek/?*.pfc*')).
lstr :- forall((baseKB:how_virtualize_file(heads,F,0), \+ mpred_unload_option(F, never)), baseKB:ensure_loaded(F)).
lstra :- forall(baseKB:how_virtualize_file(_,F,0),baseKB:ensure_loaded(F)).

% ==============================================
% [Optional] the following game files though can be loaded separate instead
% ==============================================
:- declare_load_dbase(sample_games('src_game_nani/?*.pfc*')).

% ==============================================
% [Optional] the following worlds are in version control in examples
% ==============================================
% :- add_game_dir(sample_games('src_game_wumpus'),prolog_repl).
% :- add_game_dir(sample_games('src_game_sims'),prolog_repl).
% :- add_game_dir(sample_games('src_game_nani'),prolog_repl).
%:- add_game_dir(sample_games('src_game_startrek'),prolog_repl).
%:- declare_load_dbase(sample_games('src_game_startrek/?*.pfc*')).

%:- check_clause_counts.

:- sanity(argIsa(genlPreds,2,_)).

:- test_runtime_boot(argIsa(genlPreds,2,_)).


% ==============================================
% Sanity tests
% ==============================================
:- if( \+ app_argv('--noworld')).
sanity_test(ifood_rez):- ignore((
     %user:ensure_loaded(init_mud_server),
     % mpred_notrace_exec,
     % flag_call(runtime_debug>true),
     ain(isa(iFoodRez2,tFood)),must(isa(iFoodRez2,tEatAble)))),
    must((call(call,parseIsa_Call(tEatAble,O,["food"],Rest)),O=iFoodRez2,Rest=[])).

:- test_runtime_boot((dmsg(sanity_test_ifood_rez))).


sanity_test(s_direction):- gripe_time(1.0,must(coerce("s",vtDirection,_))).
sanity_test(l_not_a_direction):- gripe_time(2.0,must( \+ coerce(l,vtDirection,_))).
%:- test_runtime_boot().
%:- test_runtime_boot().
:- endif.
:- test_runtime_boot((statistics)).
:- test_runtime_boot(check_clause_counts).


% ==============================================
% [Required/Optional]  Ensures...
% ==============================================

% :- after_boot(set_prolog_flag(runtime_debug,0)).
:- before_boot(set_prolog_flag(unsafe_speedups,false)).

:- if( \+ app_argv('--noworld')).
:- if(app_argv('--world')).
% :- lst.
:- dmsg("Dont forget to ?- lst. ").
:- add_history(lst).
:- endif.
:- retractall(t_l:disable_px).
%:- xlisting('/mnt/sde1/packs_sys/logicmoo_base/prolog/logicmoo/pfc/system_basic.pfc.pl').
%:- break.
%:- lstr.
:- endif.

lar0 :- app_argv('--repl'),!,dmsg("Ctrl-D to start MUD"),prolog,lar.
lar0 :- lar.

:- add_history(lar).
lar :- % set_prolog_flag(dmsg_level,never),
     start_runtime_mud,
       if_defined(login_and_run,wdmsg("MUD code not loaded")).


%:- after_boot(qsave_lm(lm_init_mud)).
%:- after_boot(lar0).


:- after_boot((statistics,dmsg("Type lar.<enter> at the '?-' prompt to start the MUD (a shortcut for ?- login_and_run. )"))).

:- if(gethostname(gitlab)).                                            

:- set_prolog_flag(runtime_debug,3).
:- set_prolog_flag(runtime_safety,3).
:- set_prolog_flag(runtime_speed,0).

:- else.


:- set_prolog_flag(runtime_debug,1).
:- set_prolog_flag(runtime_safety,1).
:- set_prolog_flag(runtime_speed,1).

:- endif.


:- before_boot(use_baseKB).
:- during_boot(use_baseKB).
:- during_boot(ain(tSourceData(iWorldData8))).

start_runtime_mud:- 
   use_baseKB,
   ain(isLoaded(iWorldData8)),
   dmsg(call(listing(feature_test))),
   dmsg(call(listing(sanity_test))),
   dmsg(call(listing(regression_test))),
   with_mpred_trace_exec(ain(isRuntime)).

:- after_boot(start_runtime_mud).

%:- setenv('DISPLAY', '').

:- add_history(profile(ain(tAgent(foofy)))).
:- add_history(listing(inRegion)).
:- add_history(listing(localityOfObject)).                  
:- add_history(listing(mudAtLoc)).
:- add_history(baseKB:lst).
:- add_history(logicmoo_i_cyc_xform).

:- fixup_exports.

