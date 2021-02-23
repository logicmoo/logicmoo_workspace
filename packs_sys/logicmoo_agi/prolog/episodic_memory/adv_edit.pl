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
:- '$set_source_module'(mu).

%:- nop(ensure_loaded('adv_chat80')).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_main_commands')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%printable_state(L, S):- sort(L, S).
printable_state(S, S).


include_functor(List, P):- compound(P), safe_functor(P, F, _), member(F, List), !.
include_string(Search, P):- compound(P), sformat(S,"~p",[P]), sub_string(S,_,_,_,Search).

% invoke_metacmd(Doer, Action, S0, S1)
:- discontiguous invoke_metacmd/4. 
:- add_help(quit, "Quits the game.").

:- defn_state_setter( invoke_metacmd( agent, action)).

find_world(World, W0, W1, S0):- wdmsg(warn(find_world(World, W0, W1, S0))), W0=S0.

save_world(World, W1, S0, S1):- wdmsg(warn(save_world(World, W1, S0, S1))), S1=W1.

invoke_metacmd(_Doer, in_world(Agent, World, Cmd), S0, S1):- !, find_world(World, W0, W1, S0),
 invoke_metacmd(Agent, Cmd, W0, W1), !,
  save_world(World, W1, S0, S1).

invoke_metacmd(Doer, attempts(Agent, Command)) --> !, {Doer\==Agent},
  invoke_metacmd(Agent, Command).

invoke_metacmd(Doer, quit(Agent)) -->
 redeclare(wishes(Agent, quit)),
 {player_format(Doer, 'logging off~w ~n', [Agent]),
  player_format(Agent, 'Bye! (from ~w)~n', [Doer])}.

invoke_metacmd(Doer, help, S0, S0) :- !,
 with_agent_console(Doer, listing(mu_global:cmd_help)).

:- add_help(english, "english <level>: turn on paraphrase generation.").
invoke_metacmd(Doer, [english], S0, S0) :- must_security_of(Doer, admin),
 flag(english, Was, Was),
 player_format(Doer, '~w=~q~n', [english, Was]).
invoke_metacmd(Doer, english(N0), S0, S0) :- must_security_of(Doer, admin),
 any_to_number(N0, N),
 flag(english, _Was, N),
 flag(english, New, New),
 player_format(Doer, '~w=~q~n', [english, N]).


:- add_help(rtrace, "Debbuging: Start the non-interactive tracer.").
invoke_metacmd(Doer, rtrace, S0, S0) :- must_security_of(Doer, admin), rtrace.

:- add_help(nortrace, "Debbuging: Stop the non-interactive tracer.").
invoke_metacmd(Doer, nortrace, S0, S0) :- must_security_of(Doer, admin), nortrace.

:- add_help(trace, "Debbuging: Start the interactive tracer.").
invoke_metacmd(Doer, trace, S0, S0) :- must_security_of(Doer, admin), trace.

:- add_help(notrace, "Debbuging: Stop the interactive tracer.").
invoke_metacmd(Doer, notrace, S0, S0) :- must_security_of(Doer, admin), notrace.

:- add_help(spy(+pred), "Put a spy point on all predicates meeting the predicate specs").
invoke_metacmd(Doer, spy(Pred), S0, S0) :- must_security_of(Doer, admin), spy(Pred).

:- add_help(nospy(+pred), "Remove spy point on all predicates meeting the predicate specs").
invoke_metacmd(Doer, nospy(Pred), S0, S0) :- must_security_of(Doer, admin), nospy(Pred).

:- add_help(possess(agent), "Take possession of a character").
invoke_metacmd(Doer, possess(NewAgent), S0, S0) :-
 must_security_of(Doer, wizard),
 mu_current_agent(OldAgent),
 current_input(InputConsole),
 retractall(console_controls_agent(_, OldAgent)),
 retractall(console_controls_agent(_, NewAgent)),
 asserta(console_controls_agent(InputConsole, NewAgent)).


invoke_metacmd(Doer, Cmd, S0, S9) :-
 command_to_list(Cmd,List),
 invoke_metacmd_l(Doer, List, S0, S9).
 
command_to_list(Atom,[Atom]):- atom(Atom),!.
command_to_list(List,List):- is_list(List),!.
command_to_list(Cmpd,[Cmd|List]):- compound(Cmpd), compound_name_arguments(Cmpd,Cmd,Args),flatten(Args,List),!.

show_state_of(Doer, [], S0):- !,
  printable_state(S0, S),
  reverse(S, SR),
  player_pprint(Doer, SR, always),
  maybe_pause(Doer).
show_state_of(Doer, ['.'|List], S0):- !, show_state_of(Doer, List, S0).
show_state_of(Doer, ['"',Atom,'"'|Rest], S0):- atom_string(Atom,Str),
  show_state_of(Doer, [Str|Rest], S0).
show_state_of(Doer, [Atom|Rest], S0):- string(Atom),!,
  printable_state(S0, S),
  include(include_string(Atom), S, SP),
  show_state_of(Doer, Rest, SP).
show_state_of(Doer, ['props'|Rest], S0):-
  printable_state(S0, S),!,
  include(include_functor([props,h]), S, SP),
  show_state_of(Doer, Rest, SP).
show_state_of(Doer, ['types'|Rest], S0):-
  printable_state(S0, S),!,
  include(include_functor([type_props]), S, SP),
  show_state_of(Doer, Rest, SP).
show_state_of(Doer, ['mems'|Rest], S0):- 
  printable_state(S0, S),!,
  include(@>=(props(_, _)), S, SP),
  show_state_of(Doer, Rest, SP).
show_state_of(Doer, [Atom|Rest], S0):- atom(Atom),!,
  printable_state(S0, S),
  include(include_functor(Atom), S, SP),
  show_state_of(Doer, Rest, SP).

invoke_metacmd_l(Doer, [echo|List], S0, S0):- !, player_format(Doer, '~w~n', [List]).
invoke_metacmd_l(Doer, [state|List], S, S) :- !, show_state_of(Doer, List, S).
invoke_metacmd_l(Doer, [PPT|List], S, S) :- member(PPT,[props,perceptq,types,mems]),!,
 show_state_of(Doer,[PPT|List], S),!.
invoke_metacmd_l(Doer, [model|List], S0, S0) :-
 agent_thought_model(Doer, Model, S0),
 show_state_of(Doer, List, Model).
invoke_metacmd_l(Doer, [mem|List], S0, S0) :-
 declared(memories(Doer, Memory), S0),
 show_state_of(Doer, List, Memory).

invoke_metacmd(Doer, make, S0, S0) :-
 must_security_of(Doer, wizard),
 thread_signal(main, make),
 ensure_has_prompt(Doer).

invoke_metacmd(Doer, prolog, S0, S0) :-
 must_security_of(Doer, wizard),
 '$current_typein_module'(Was),
 setup_call_cleanup('$set_typein_module'(mu), prolog, '$set_typein_module'(Was)),
 ensure_has_prompt(Doer).

invoke_metacmd(Doer, CLS, S0, S9) :- must_security_of(Doer, wizard),
 current_predicate(_, CLS),
 set_advstate(S0),
 (is_main_console -> catch(CLS, E, (dbug1(CLS:- throw(E)), fail)) ;
    (redirect_error_to_string(catch(CLS, E, (dbug1(CLS:- throw(E)), fail)), Str), !, write(Str))), !,
 ensure_has_prompt(Doer),
 get_advstate(S9).

invoke_metacmd(Doer, inspect(Self, NamedProperty, Target), S0, S1) :-
 invoke_metacmd(Doer, inspect(Self, getprop(Target, NamedProperty)), S0, S1).

invoke_metacmd(Doer, inspect(Self, getprop(Target, NamedProperty)), S0, S0) :-
 must_security_of(Doer, wizard),
 pred_holder(NamedProperty, PropertyPred),
 PropAndData=..[PropertyPred, Data],
 findall(Data, getprop(Target, PropAndData, S0), DataList),
 player_pprint(Self, DataList, always),
 maybe_pause(Doer).

invoke_metacmd(Doer, rez(Type), S0, S9) :-
 must_security_of(Doer, wizard),
 must_mw1((mu_current_agent(Agent),
 g_h(spatial, Prep, Agent, Here, S0),
 create_new_unlocated(Type, Object, S0, S1),
 declare(h(spatial, Prep, Object, Here), S1, S9),
 player_format(Doer, 'You now see a ~w.~n', [Object]))).

invoke_metacmd(Doer, derez(Object), S0, S1) :-
 must_security_of(Doer, wizard),
 undeclare(h(spatial, _, Object, _), S0, S1),
 player_format(Doer, 'It vanishes instantly.~n', []).

invoke_metacmd(Doer, PropCmd, S0, S1) :-
 action_verb_agent_args(PropCmd, setprop, _, [Object | Args]), Prop =.. Args,
 must_security_of(Doer, wizard),
 setprop(Object, Prop, S0, S1),
 player_format(Doer, 'Set ~p ~p.~n', [Object, Prop]),
 invoke_metacmd(Doer, properties(Object), S1, S1).

invoke_metacmd(Doer, PropCmd, S0, S1) :-
 action_verb_agent_args(PropCmd, updateprop, _, [Object | Args]), Prop =.. Args,
 must_security_of(Doer, wizard),
 updateprop(Object, Prop, S0, S1),
 player_format(Doer, 'Update ~p ~p.~n', [Object, Prop]),
 invoke_metacmd(Doer, properties(Object), S1, S1).

invoke_metacmd(Doer, DelProp, S0, S1) :-
 action_verb_agent_args(DelProp, delprop, _, [Object | Args]), Prop =.. Args,
 must_security_of(Doer, wizard),
 delprop(Object, Prop, S0, S1),
 player_format(Doer, 'Deleted ~p ~p.~n', [Object, Prop]),
 invoke_metacmd(Doer, properties(Object), S1, S1).

invoke_metacmd(Doer, Properties, S0, S0) :-
 action_verb_agent_thing(Properties, properties, _, Object), % Prop =.. Args,
 must_security_of(Doer, wizard),
 (declared(props(Object, PropList), S0);declared(type_props(Object, PropList), S0)), !,
 player_format(Doer, 'Properties now: ~@~n', [pprint(props(Object, PropList))]).

invoke_metacmd(Doer, undo, S0, S1) :-
 declare(wants(Doer, undo), S0, S1),
 player_format(Doer, 'undo...OK~nKO...odnu~n', []).

invoke_metacmd(_Doer, save(Basename), S0, S0) :-
 atom_concat(Basename, '.adv', Filename),
 save_term_exists(Filename, S0).

invoke_metacmd(Doer, WA, S0, S1) :-
 (( cmd_workarround(WA, WB) -> WB\==WA)), !, invoke_metacmd(Doer, WB, S0, S1).


pred_holder(memory, memories).
pred_holder(percepts, perceptq).
pred_holder(X, X).


