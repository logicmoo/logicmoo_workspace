%%%
%%% Command line interface for controlling character
%%% Forces characters to perform actions using the do/1 predicate.
%%%

do(Command) :-
   concern(C, command_line),
   !,
   assert(C/command:Command).
do(_) :-
   throw(error("No command_line concern is runnning.")).