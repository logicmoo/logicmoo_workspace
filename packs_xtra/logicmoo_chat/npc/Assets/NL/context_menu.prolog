%% menu_item(+MetaverseObject, -String, -DialogAct)
%  DialogAct is an option to display in the context menu for MetaverseObject
%  with the english surface form of String.
:- public menu_item/3.
menu_item(MetaverseObject, String, Command) :-
   menu_dialog_act(MetaverseObject, Command),
   once(generate_text_for_menu(Command, String)).

%% menu_dialog_act(+MetaverseObject, -DialogAct)
%  DialogAct should be displayed in the context menu for MetaverseObject.
menu_dialog_act(MetaverseObject, command($user, $pc, Action)) :-
   menu_action(MetaverseObject, Action).
menu_dialog_act($pc, show_status($user, $pc, notebook)).
menu_dialog_act($pc, show_status($user, $pc, inventory)).
menu_dialog_act($pc, show_status($user, $pc, vocabulary)).
menu_dialog_act(Character, DialogAct) :-
   character(Character),
   in_conversation_with(Character),
   menu_dialog(Character, DialogAct).

%% menu_dialog(+Character, -DialogAct)
%  DialogAct should be listed in the context menu for Character when in
%  conversation with them.
:- external menu_assertion/2, menu_question/2, menu_automa_command/2, menu_command/2.
menu_dialog(Character, question($pc, Character, LF, present, simple)) :-
   menu_question(Character, LF).
menu_dialog(Character, assertion($pc, Character, LF, present, simple)) :-
   menu_assertion(Character, LF).
menu_dialog(Character, automa_command($pc, Character, LF, present, simple)) :-
   menu_automa_command(Character, LF).
menu_dialog(Character, command($pc, Character, Action)) :-
   menu_command(Character, Action).



%% menu_action( ?X, ?Pc) is semidet.
%
% Menu Action.
%
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
   iz_a(X, debugable).
menu_action(X, switch($pc, X , power,on)) :-
   once(iz_a(X, appliance)),
   \+ prop_activated(X).
menu_action(X, switch($pc, X, power,on)) :-
   once(iz_a(X, appliance)),
   prop_activated(X).
