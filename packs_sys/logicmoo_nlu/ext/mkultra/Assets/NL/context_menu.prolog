%% menu_item(+GameObject, -String, -DialogAct)
%  DialogAct is an option to display in the context menu for GameObject
%  with the english surface form of String.
:- public menu_item/3.
menu_item(GameObject, String, Command) :-
   menu_dialog_act(GameObject, Command),
   once(generate_text_for_menu(Command, String)).

%% menu_dialog_act(+GameObject, -DialogAct)
%  DialogAct should be displayed in the context menu for GameObject.
menu_dialog_act(GameObject, command(player, $pc, Action)) :-
   menu_action(GameObject, Action).
menu_dialog_act($pc, show_status(player, $pc, notebook)).
menu_dialog_act($pc, show_status(player, $pc, inventory)).
menu_dialog_act($pc, show_status(player, $pc, vocabulary)).
menu_dialog_act(Character, DialogAct) :-
   character(Character),
   in_conversation_with(Character),
   menu_dialog(Character, DialogAct).

%% menu_dialog(+Character, -DialogAct)
%  DialogAct should be listed in the context menu for Character when in
%  conversation with them.
:- external menu_assertion/2, menu_question/2, menu_hypno_command/2, menu_command/2.
menu_dialog(Character, question($pc, Character, LF, present, simple)) :-
   menu_question(Character, LF).
menu_dialog(Character, assertion($pc, Character, LF, present, simple)) :-
   menu_assertion(Character, LF).
menu_dialog(Character, hypno_command($pc, Character, LF, present, simple)) :-
   menu_hypno_command(Character, LF).
menu_dialog(Character, command($pc, Character, Action)) :-
   menu_command(Character, Action).

menu_action($pc, stop($pc)) :-
   everyday_life_task_busy.

menu_action(X, go($pc, X)) :-
   X \= $pc,
   \+ (docked_with(X) ; in_conversation_with(X)).
menu_action(X, talk($pc, X, _)) :-
   character(X),
   X \= $pc,
   \+ in_conversation_with(X).

menu_action(X, sleep($pc, X)) :-
   is_a(X, layable).
menu_action(X, turn_on($pc, X)) :-
   once(is_a(X, appliance)),
   \+ prop_activated(X).
menu_action(X, turn_off($pc, X)) :-
   once(is_a(X, appliance)),
   prop_activated(X).
