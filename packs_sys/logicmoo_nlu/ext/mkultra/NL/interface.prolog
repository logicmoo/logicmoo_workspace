:- public generate_text/2, input_completion/3.
:- indexical input_from_player=false,
   generating_nl=false,
   discourse_variables=null.

%% generate_text(?SpeechAct, ?Text)
%  The string Text is a possible realization of SpeechAct.
generate_text(SpeechAct, Text) :-
   bind_dialog_indexicals_for_output(SpeechAct),
   step_limit(10000),
   randomize(utterance(SpeechAct, Words, [ ])),
   contracted_form(Words, Contracted),
   word_list(Text, Contracted).

%% input_completion(+InputText, -CompletionText, -SpeechAct)
%  InputText followed by CompletionText is a possible realization of SpeechAct.
input_completion(InputText, CompletionText, SpeechAct) :-
   bind_dialog_indexicals_for_input,
   word_list(InputText, InputWords),
   contracted_form(InputUncontracted, InputWords),
   append(InputUncontracted, CompletionUncontracted, AllWords),
   %call_with_step_limit(10000, randomize(utterance(SpeechAct, AllWords, []))),
   step_limit(10000),
   randomize(utterance(SpeechAct, AllWords, [])),
   contracted_form(CompletionUncontracted, CompletionWords),
   word_list(CompletionText, CompletionWords).

:- public well_formed_dialog_act/1.
well_formed_dialog_act(general_help(_,_)).
well_formed_dialog_act(how_do_i(_,_,_)).
well_formed_dialog_act(objective_query(_,_)).
well_formed_dialog_act(color_query(_,_,_)).
well_formed_dialog_act(question(_, _, LF, _, _)) :-
   (LF = _:Y) ->
      well_typed(Y, condition)
      ;
      well_typed(LF, condition).
well_formed_dialog_act(command(_, _, LF)) :-
   well_typed(LF, action).
well_formed_dialog_act(assertion(_, _, LF, _, _)) :-
   well_typed(LF, condition).

bind_dialog_indexicals_for_input :-
   in_conversation_with_npc(NPC),
   !,
   bind(input_from_player, true),
   bind(speaker, $me),
   bind(addressee, NPC),
   bind(dialog_group, $me).
bind_dialog_indexicals_for_input :-
   bind(input_from_player, true),
   bind(speaker, player),
   bind(addressee, $me),
   bind(dialog_group, $me).

bind_indexicals_for_addressing_character_named(Name) :-
   proper_name(Character, [Name]),
   character(Character),
   bind_indexicals_for_addressing_character(Character).

bind_indexicals_for_addressing_character($me) :-
   !.
bind_indexicals_for_addressing_character(Character) :-
   bind(speaker, $me),
   bind(addressee, Character).

in_conversation_with_npc(NPC) :-
   concern(C),
   C/partner/NPC,
   NPC \= player.

bind_dialog_indexicals_for_output(SpeechAct) :-
   bind(generating_nl, true),
   bind(discourse_variables, null),
   agent(SpeechAct, A),
   patient(SpeechAct, P),
   bind(speaker, A),
   bind(addressee, P).

generating_nl :-
   X = $generating_nl, X.

input_from_player :-
   X = $input_from_player, X.

%% player_idle_time(-Time)
%  Time is the number of seconds of game time since the player
%  last did something (i.e. typed).
:- public player_idle_time/1.

player_idle_time(Time) :-
   $global_root/last_player_activity:Last,
   Time is $now-Last.

player_idle_for_at_least(Time) :-
   player_idle_time(T),
   T >= Time.