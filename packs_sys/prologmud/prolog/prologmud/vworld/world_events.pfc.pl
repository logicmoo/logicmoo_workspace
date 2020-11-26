/* * <module> File used to implement in_world_events
% like Talking, Appearing, falling rocks..
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% :-swi_module(world_events,[]).

:- include(prologmud(mud_header)).


asInvoked(Cmd,[L|Ist]):-stack_check,once(append([L|Ist],[],Foo)),Foo\==[L|Ist],!,asInvoked(Cmd,Foo).
asInvoked(Cmd,[L|Ist]):-atom(L),not(bad_functor(L)),!, Cmd=..[L|Ist].
asInvoked(Cmd,[L|Ist]):-!,Cmd=..[asInvoked,L|Ist].
asInvoked(Cmd,Cmd):-!.

prologBuiltin(mudObjNearLoc(tObj,tObj)).
mudObjNearLoc(Whom,Where):-nonvar(Where),!,findall(Whom,atlocNear0(Whom,Where),List),list_to_set(List,Set),!,member(Whom,Set).
mudObjNearLoc(Whom,Where):-nonvar(Whom),!,findall(Where,atlocNear0(Whom,Where),List),list_to_set(List,Set),!,member(Where,Set).
mudObjNearLoc(Whom,Where):-findall(Whom+Where,atlocNear0(Whom,Where),List),list_to_set(List,Set),!,member(Whom+Where,Set).

atlocNear0(Whom,Where):-!,mudNearbyLocs(Where,LOC),clause_asserted(mudAtLoc(Whom,LOC)).
atlocNear0(Whom,Where):-mudNearbyLocs(Where,LOC),is_asserted(mudAtLoc(Whom,LOC)).


:-export(raise_location_event/2).
raise_location_event(Where,Event):- forall(no_repeats(Whom,(no_repeats(tAgent(Whom)),mudObjNearLoc(Whom,Where))),
   deliver_event(Whom,Event)).
deliver_event(Whom,Event):- quietly(doall(call_no_cuts(baseKB:deliver_event_hooks(Whom,Event)))).


% :-export(mudDeliverableLocationEvents/3).
:-dynamic(baseKB:mudDeliverableLocationEvents/3).
prologHybrid(mudDeliverableLocationEvents(tAgent,tRegion,ftTerm)).

