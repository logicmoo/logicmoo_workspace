%%%
%%% Command line interface for controlling character
%%% Forces characters to perform actions using the do/1 predicate.
%%%

:- public do/1.
do(goto(Location)) :-
   !,
   concern(C, command_line),
   C/priority:Priority,
   assert(C/location_bids/Location:Priority).
do(Command) :-
   concern(C, command_line),
   !,
   assert(C/command:Command).
do(_) :-
   throw(error("No command_line concern is runnning.")).

propose_action(Command, command_line, C) :-
   C/command:Command.

score_action(Command, command_line, C, Priority) :-
   C/priority:Priority,
   C/command:Command.

on_event(Command, command_line, C, retract(C/command)) :-
   C/command:X,
   X=Command.

on_event(arrived_at(Location), command_line, C, retract(C/location_bids/Location)).
