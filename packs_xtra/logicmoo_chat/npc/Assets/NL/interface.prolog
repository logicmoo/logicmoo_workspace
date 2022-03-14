:- public generate_text/2, input_completion/3.
:- indexical input_from_player=false,
   generating_nl=false,
   discourse_variables=[].

%% generate_text(?SpeechAct, ?Text)
%  The string Text is a possible realization of SpeechAct.
generate_text(SpeechAct, Text) :-
   bind_dialog_indexicals_for_output(SpeechAct,
   (step_limit(10000),
    randomize(utterance(SpeechAct, Words, [ ])),
    contracted_form(Words, Contracted),
    word_list(Text, Contracted))).



%=autodoc
%% generate_text_for_menu( ?SpeechAct, ?Text) is semidet.
%
% Generate Text For Menu.
%
generate_text_for_menu(SpeechAct, Text) :-
   bind_dialog_indexicals_for_output(SpeechAct,
   (step_limit(10000),
    utterance(SpeechAct, Words, [ ]),
    contracted_form(Words, Contracted),
    word_list(Text, Contracted))).


%% input_completion(+InputText, -CompletionText, -SpeechAct)
%  InputText followed by CompletionText is a possible realization of SpeechAct.
input_completion(InputText, CompletionText, SpeechAct) :-
  bind_dialog_indexicals_for_input(
  (word_list(InputText, InputWords),
   contracted_form(InputUncontracted, InputWords),
   append(InputUncontracted, CompletionUncontracted, AllWords),
   %call_with_step_limit(10000, randomize(utterance(SpeechAct, AllWords, []))),
   step_limit(10000),
   randomize(utterance(SpeechAct, AllWords, [])),
   contracted_form(CompletionUncontracted, CompletionWords),
   word_list(CompletionText, CompletionWords))).

:- public well_formed_dialog_act/1.


%=autodoc
%% well_formed_dialog_act( ?ARG1) is semidet.
%
% Well Formed Dialog Single Doer Action.
%
well_formed_dialog_act(acceptance(_, _, _, _)).
well_formed_dialog_act(general_help(_,_)).
well_formed_dialog_act(how_do_i(_,_,_)).
well_formed_dialog_act(objective_query(_,_)).
well_formed_dialog_act(color_query(_,_,_)).
well_formed_dialog_act(question(_, _, LF, _, _)) :-
   call(LF = _:Y) ->
      well_typed(Y, condition)
      ;
      well_typed(LF, condition).
well_formed_dialog_act(command(_, _, LF)) :-
   well_typed(LF, action).
well_formed_dialog_act(assertion(_, _, LF, _, _)) :-
   well_typed(LF, condition).
well_formed_dialog_act(offer(S, A, SAction, AAction)) :-
   agent(SAction, S),
   agent(AAction, A),
   well_typed(SAction, action),
   well_typed(AAction, action).
well_formed_dialog_act(inaction(S, A, SAction, AAction)) :-
   agent(SAction, S),
   agent(AAction, A),
   well_typed(SAction, action),
   well_typed(AAction, action).
well_formed_dialog_act(automa_command(_, Target, _, _, _)) :-
   Target \= $me.
well_formed_dialog_act(show_status(_,_,_)).

   


%=autodoc
%% bind_dialog_indexicals_for_input( ?NPC, ?G) is semidet.
%
% Bind Dialog Indexicals For Input.
%
bind_dialog_indexicals_for_input(NPC, G):-
   with_bind(input_from_player, true,
    with_bind(speaker, $me,
     with_bind(addressee, NPC,
      with_bind(dialog_group, $me, G)))).



%=autodoc
%% bind_dialog_indexicals_for_input( ?G) is semidet.
%
% Bind Dialog Indexicals For Input.
%
bind_dialog_indexicals_for_input(G):-
  in_conversation_with_npc(NPC),!, 
   bind_dialog_indexicals_for_input(NPC, G).
bind_dialog_indexicals_for_input(G) :-
   with_bind(input_from_player, true,
    with_bind(speaker, $user,
     with_bind(addressee, $me,
      with_bind(dialog_group, $me, G)))).



%=autodoc
%% bind_indexicals_for_addressing_character_named( ?ARG1, ?ARG2) is semidet.
%
% Bind Indexicals For Addressing Character Named.
%
bind_indexicals_for_addressing_character_named(Name, G) --> 
   {proper_name(Character, [Name]),
    character(Character)},
    bind_indexicals_for_addressing_character(Character, G).



%=autodoc
%% bind_indexicals_for_addressing_character( ?ARG1, ?ARG2) is semidet.
%
% Bind Indexicals For Addressing Character.
%
bind_indexicals_for_addressing_character($me, G) --> G.
bind_indexicals_for_addressing_character(Character, G) -->
   with_bind(speaker, $me,
    with_bind(addressee, Character, G)).



%=autodoc
%% with_bind( ?N, ?V, ?DCGDCG, ?S, ?E) is semidet.
%
% Using Bind.
%
with_bind(N, V, DCG, S, E):- with_bind(N, V, phrase(DCG, S, E)).



%=autodoc
%% in_conversation_with_npc( ?NPC) is semidet.
%
% In Conversation Using Automatic Character.
%
in_conversation_with_npc(NPC) :-
   qud(C),
   C/partner/NPC,
   NPC \= $user.



%=autodoc
%% bind_dialog_indexicals_for_output( ?SpeechAct, ?G) is semidet.
%
% Bind Dialog Indexicals For Output.
%
bind_dialog_indexicals_for_output(SpeechAct, G) :-
   with_bind(generating_nl, true,
    with_bind(discourse_variables, [],
    (agent(SpeechAct, A),
     patient(SpeechAct, P),
     with_bind(speaker, A,
      with_bind(addressee, P, G))))).



%=autodoc
%% generating_nl is semidet.
%
% Generating Nl.
%
generating_nl :-
   X = $generating_nl, X.



%=autodoc
%% input_from_player is semidet.
%
% Input Converted From User.
%
input_from_player :-
   X = $input_from_player, X.



%=autodoc
%% while_parsing( ?G) is semidet.
%
% While Parsing.
%
while_parsing(G) :-
 with_bind(input_from_player=true,G).


%=autodoc
%% while_generating( ?G) is semidet.
%
% While Generating.
%
while_generating(G) :-
 with_bind(generating_nl=true,G).


%=autodoc
%% while_completing( ?G) is semidet.
%
% While Completing.
%
while_completing(G) :-
 bind_dialog_indexicals_for_input(G).

%% player_idle_time(-Time)
%  Time is the number of seconds of metaverse time since the $user
%  last did something (i.e. typed).
:- public player_idle_time/1.

player_idle_time(Time) :-
   $global_root/last_player_activity:Last,
   Time is $now-Last.



%=autodoc
%% player_idle_for_at_least( ?Time) is semidet.
%
% User Idle For When Least.
%
player_idle_for_at_least(Time) :-
   player_idle_time(T),
   T >= Time.

%% caption(+MetaverseObject, -Caption) is det
% Generates caption for the desired object.
:- public caption/2.
caption($pc, "That's me!").
caption(MetaverseObject, Name) :-
   proper_name(MetaverseObject, Words),
   word_list(Name, Words).
caption(MetaverseObject, Description) :-
    t(caption, MetaverseObject, Description).
caption(MetaverseObject, Description) :-
   iz_a(MetaverseObject, module),
   base_kind(MetaverseObject, Kind),
   kind_noun(Kind, singular, Words, []),
   word_list(Description, Words).
caption(MetaverseObject, Description) :-
   base_kind(MetaverseObject, Kind),
   kind_noun(Kind, singular, Words, []),
   contracted_form([a | Words], Contracted),
   word_list(Description, Contracted).
caption(_, "???").