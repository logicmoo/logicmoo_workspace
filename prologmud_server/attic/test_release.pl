#!/usr/bin/env swipl
/* * module * Logicmoo_base sanity test script to be ran before a release

*/


% ==============================
% Setup Testing Options
% ==============================

:- asserta(baseKB:load_mud_www).
:- asserta(t_l:verify_side_effect_buffer).
:- asserta(baseKB:skip_el_assertions).


% ==============================
% Setup Runtime paths
% ==============================

:- user:ensure_loaded(setup_paths).

:- user:test_for_release(setup_paths).

% ==============================
% Load logicmoo REPL Base
% (and Default Daemons/inference engine)
% ==============================

:- user:test_for_release(logicmoo_repl).


% ==============================
% Release tests
% ==============================

% :- statistics(globallimit,G),statistics(locallimit,L),statistics(traillimit,T),qsave_program(logicmoo_repl,[map('logicmoo_repl.sav'),global(G),trail(T),local(L)]).


:- tell(blalla).

:-listing(side_effect_buffer/3).

:-forall(t_l:actual_side_effect(H,B),(nl,portray_clause(H:-B))).

:-listing(side_effect_buffer/3).

:- told.


% [Required] Load the Logicmoo Backchaining Inference System
:- gripe_time(40,with_no_mpred_expansions(if_file_exists(user:test_for_release(library(logicmoo_user))))).


:- asserta(baseKB:skip_el_assertions).
:- dynamic  el_assertions:el_holds/4.
:- dynamic  el_assertions:el_holds/10.
:- dynamic  el_assertions:el_holds/11.
:- dynamic  el_assertions:el_holds/12.
:- dynamic  el_assertions:el_holds/13.
:- dynamic  el_assertions:el_holds/14.



:- user:test_for_release(library(logicmoo_plarkc)).
% :- user:test_for_release(library(logicmoo_planner)).
%:- user:load_library_system(library(logicmoo_planner)).
%:- user:test_for_release(library(parser_all)).
:- user:test_for_release(init_mud_server).


% [Never] saves about a 3 minute compilation time (for when not runing mud)
:- if((fail,gethostname(titan),fail)).
:- if_startup_script( finish_processing_world).
:- enqueue_agent_action("rez crackers").

:- endif.


%:- flag_call(runtime_debug=false).

%:- set_prolog_flag(trace_gc,false).
%:- set_prolog_flag(backtrace_depth,400).


% [Manditory] This loads the game and initializes so test can be ran
:- if_startup_script(finish_processing_world).

% sanity_test:- rescan_pfc.

%:- rescan_pfc. 
:-dmsg("About to run Sanity").


:- show_entry(gripe_time(40,if_startup_script(doall(sanity_test)))).


feature_testp1:- forall(parserTest(Where,String),assert_text(Where,String)).

:- if((fail,gethostname(titan))).

% :-feature_testp1.

% [Optionaly] Run a battery of tests
% :- if_startup_script( doall(now_run_local_tests_dbg)).

% [Optionaly] Run a battery of tests
% :- if_startup_script( doall(baseKB:regression_test)).


sanity_test0a:- enqueue_agent_action("hide").

sanity_test0b:- enqueue_agent_action(actWho).
:-sanity_test0b.

sanity_test1:-   
   enqueue_agent_action("rez crackers"),
   enqueue_agent_action("drop crackers"),
   enqueue_agent_action('look'),
   enqueue_agent_action("take crackers"),
   enqueue_agent_action("eat crackers"),
   enqueue_agent_action('look').
:-sanity_test1.

sanity_test2:- enqueue_agent_action("rez pants"),
   enqueue_agent_action("wear pants"),
   enqueue_agent_action("tp to closet"),
   enqueue_agent_action("take shirt"),
   enqueue_agent_action("inventory").

:-sanity_test2.


% [Optionaly] Tell the NPCs to do something every 60 seconds (instead of 90 seconds)
% :- register_timer_thread(npc_ticker,60,npc_tick).

:- show_entry(gripe_time(40,test_for_release(prologmud(mud_startup)))).


:- pce_show_profile.

:-endif.  % MUD TESTS



:- multifile(push_env_ctx/0).
:- dynamic(push_env_ctx/0).

push_env_ctx:-!,fail.
push_env_ctx:-!.

:- user:test_for_release(debug_mud_game).


% :- statistics(globallimit,G),statistics(locallimit,L),statistics(traillimit,T),qsave_program(logicmoo_repl,[map('logicmoo_repl.sav'),global(G),trail(T),local(L)]).



% [Mostly Required] Load the UPV Curry System
%:- time(test_for_release(library(upv_curry/main))).





