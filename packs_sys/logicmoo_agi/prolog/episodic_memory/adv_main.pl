/*
% NomicMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
% Bits and pieces:
%
% LogicMOO, Inform7, FROLOG, Guncho, PrologMUD and Marty's Prolog Adventure Prototype
%
% Copyright (C) 2004 Marty White under the GNU GPL
% Sept 20, 1999 - Douglas Miles
% July 10, 1996 - John Eikenberry
%
% Logicmoo Project changes:
%
% Main file.
%
*/
%:- use_module(library(pfc)).

:- use_module(library(logicmoo_common)).
:- '$set_source_module'(mu).


/*
:- if(\+ exists_source(library(poor_bugger))).
:- prolog_load_context(file, File),
 absolute_file_name('..', X, [relative_to(File), file_type(directory)]),
 asserta(user:file_search_path(library, X)).
:- endif.
*/

:- multifile aXiom/1.
:- meta_predicate aXiom(+).
:- multifile aXiom/3.
:- meta_predicate aXiom(+, ?, ?).

must_security_of(Doer, Level):- security_of(Doer, Level).

security_of(_, _Wiz).
admin :- true. % Potential security hazzard.
wizard :- true. % Potential to really muck up game.
extra :- true. % Fuller, but questionable if needed yet.

:- op(200, fx, '$').

% :- user:ensure_loaded(library(parser_sharing)).


:- ensure_loaded(adv_debug).
:- ensure_loaded(adv_help).
:- ensure_loaded(adv_util).
:- ensure_loaded(adv_io).

:- ensure_loaded(adv_model).
:- ensure_loaded(adv_percept).

:- ensure_loaded(adv_inst).
:- ensure_loaded(adv_edit).

:- ensure_loaded(adv_behaviour_tree).

:- ensure_loaded(adv_axiom).
:- ensure_loaded(adv_implies).

%:- ensure_loaded(adv_abdemo).

:- ensure_loaded(adv_examine).
:- ensure_loaded(adv_action).
:- ensure_loaded(adv_agent).
:- ensure_loaded(adv_floyd).
:- ensure_loaded(adv_physics).
:- ensure_loaded(adv_plan).

:- ensure_loaded(adv_functors).
:- ensure_loaded(adv_eng2txt).
:- ensure_loaded(adv_log2eng).
:- ensure_loaded(adv_eng2cmd).

%:- ensure_loaded(adv_lexicon).

:- ensure_loaded(adv_quasiquote).

:- ensure_loaded(adv_state).

:- ensure_loaded(adv_portray).
:- ensure_loaded(adv_data).

:- ensure_loaded(adv_plugins).

%:- ensure_loaded(adv_test).
%:- ensure_loaded(adv_telnet).
mindi:- reconsult(library(episodic_memory/adv_mindi)),call(testsit_all).
mindi2:- reconsult(library(episodic_memory/adv_mindi2)).


adventure_reset :-
 must_mw1((
 test_ordering, !,
 init_logging, !,
 get_state_context(Before),
 player_format('=============================================~n', []),
 player_format('RESET STATE~n', []),
 clear_state_context(Before),
 copy_prolog_sim(istate,Before),
 player_format('=============================================~n', []))).

adventure_ensure_inited :-  
 set_state_context(advstate_db),
 get_advstate(S0), \+ in_model(h(_,_,_,_), S0), 
 !, adventure_reset.
adventure_ensure_inited.

adventure_init :-
 set_state_context(advstate_db),
 adventure_ensure_inited, !,
 get_advstate(S1),
   player_format('=============================================~n', []),
   player_format('INIT STATE~n', []),
   player_format('=============================================~n', []),
 printable_state(S1, SP),
 pprint(SP, state), !.

thread_create_adv(_, _, C):-member(alias(Alias), C), thread_property(X, _),
  (Alias=X;(thread_property(X, alias(Y)), Alias=Y)),
  thread_property(X, status(Running)),
  (Running == running-> ! ; (join_threads, fail)).
thread_create_adv(A, B, C):-thread_create(A, B, C).

adventure:-
 adventure_init,
 player_format('=============================================~n', []),
 player_format('Welcome to Marty\'s Prolog Adventure Prototype~n', []),
 player_format('=============================================~n', []),
 setup_player_console,!,
 % start_missing_threads,
 mainloop,
 !.
 % start_player_console.

start_missing_threads:-
 thread_create_adv(mainloop, _, [detached(true), alias(adv_mainloop)]),
 !.

setup_player_console:-
  mu_current_agent(Agent), current_input(InStream), current_output(OutStream),
  asserta(mu_global:console_io_player(InStream, OutStream, Agent)), !.

run_player_console:-
  mu_current_agent(Agent), current_input(InStream), current_output(OutStream),
  adventure_client_process(Agent, Agent, InStream, OutStream, 'Host', 'Peer'), !.


mainloop :-
 repeat,
 call_cleanup(once(main_once), set_prolog_flag(gc, true)),
 (get_advstate(S1)->declared(quit, S1)),
 stop_logging,
 !. % Don't allow future failure to redo mainloop.

flush_output_s(S):- notrace(ignore(catch(flush_output(S), _, true))).
my_ttyflush:-
 notrace((
  ttyflush,
   ignore((stream_property(User_Out, file_no(1)), flush_output_s(User_Out))),
   ignore((stream_property(User_Err, file_no(2)), flush_output_s(User_Err))),
   flush_output_s(user_error),
   flush_output_s(user_output),
   flush_output_s(current_output),
   flush_output_s(current_error),
   !)).

each_pre_clean_up:-
  nop(( my_ttyflush,
   sleep(0.005),
   set_prolog_flag(gc, true),
   % gc_heap,
   garbage_collect_atoms,
   garbage_collect_clauses,
   garbage_collect,
   set_prolog_flag(gc, false),
   my_ttyflush)).

main_once:-
   each_pre_clean_up,
   update_network_connections,
   get_live_agents(LiveAgents),
   my_ttyflush, !,
   % ignore((LiveAgents\=[_], dbug1(liveAgents = LiveAgents))),
   with_agents(run_perceptq, LiveAgents),
   with_agents(decide_action, LiveAgents),
   with_agents( invoke_intents, LiveAgents),
   %notrace((set_advstate(S9))),
   !. % Don't allow future failure to redo main.
main_once:-
 dbug(general, 'main_once FAILED~n').

:- dynamic(mu_temp:needs_agent_conn/4).
:- volatile(mu_temp:needs_agent_conn/4).


update_network_connections :-
  with_mutex(get_advstate,
    update_network_connections_in_mutex).

update_network_connections_in_mutex:-
 forall(retract(mu_temp:needs_agent_conn(Agent, Named, _Alias, Info)),
        create_agent_conn(Agent, Named, Info)).

% create_agent_conn(Agent, _Named, _Info, S0, S0) :- declared(agent(Agent, t), S0), !.
 %create_new_unlocated('watch', Watch),
    %create_new_unlocated('bag', Bag),
    %create_new_unlocated('coins', Coins),
     % h(Spatially, worn_by, Watch, Agent),
    %h(Spatially, in, Bag, Coins),
    %h(Spatially, held_by, Bag, Agent),
create_agent_conn(Agent, Named, Info):-
  update_running(props(Agent,
        [name(Named), inherit(telnet, t), inherit(humanoid, t), inherit(player, t), info(Info)])),
  update_running(h(spatial, in, Agent, kitchen)),
  get_advstate(S0),
  mu_create_object(Agent, S0, S9),
  set_advstate(S9).


:- dynamic(mu_global:console_tokens/2).
telnet_decide_action(Agent, Mem0, Mem0):-
 % If actions are queued, no further thinking required.
 thought_check(Agent, intent(Agent, [Action|_]), Mem0),
 agent_somewhere(Agent,Here),
 dbug(telnet, '~w @ ~w telnet: Already about to: ~w~n', [Agent, Here, Action]).

telnet_decide_action(Agent, Mem0, Mem1) :-
 %must_mw1(thought(Agent, timestamp(T0), Mem0)),
 retract(mu_global:console_tokens(Agent, Words)), !,
 must_mw1((eng2cmd(Agent, Words, Action, Mem0),
 if_tracing(dbug(telnet, 'Telnet TODO ~p~n', [Agent: Words->Action])),
 add_intent( Agent, Action, Mem0, Mem1))), !.
telnet_decide_action(Agent, Mem, Mem) :-
 nop(dbug(telnet, '~w: Can\'t think of anything to do.~n', [Agent])),
 fail.


:- use_module(library(instant_prolog_docs),[autodoc_file/1]).
%:- if(\+ prolog_load_context(reloading, t)).
:- autodoc_file(library(episodic_memory/'*.pl')).
%:- endif.

/*main_loop(State) :-
 declared(quit, State), !.
main_loop(State) :-
 declared(undo, State),
 mu_current_agent(Player),
 retract(undO(Player, [_, Prev|Tail]))),
 assertz(undo(Player, Tail)),
 !,
 main_loop(Prev).
main_loop(S0) :-
 %repeat,
 mu_current_agent(Player),
 retract(undO(Player, [U1, U2, U3, U4, U5, U6|_]))),
 assertz(undO(Player, [S0, U1, U2, U3, U4, U5, U6]))),
 run_agent(Player, S0, S4),
 run_agent(floyd, S4, S5),
 %user_interact(S3, S4), !,
 %automate_agent(floyd, S4, S5),
 !,
 main_loop(S5).
main_loop(_) :-
 dbug(general, 'main_loop() FAILED!~n').
*/

:- fixup_exports.
