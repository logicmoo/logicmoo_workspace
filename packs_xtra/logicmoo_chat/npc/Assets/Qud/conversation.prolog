%%%
%%% Conversation quds
%%% These listen for dialog acts from the conversational partner and
%%% And spawn tasks to respond to them.
%%%
%%% Important state information
%%%    Qud/partner/Partner		Who the conversation is with
%%%    Qud/initial_history/Event	What happened before this qud
%%%					was launched.
%%% Subquds
%%%    Task: respond_to_dialog_act(DA)
%%%

%% launch_conversation(+Parent, +Partner, +Event)
%  IMPERATIVE
%  Creates a subqud of Parent to handle conversation with Partner.
%  Event is the initial event that is triggering the conversation.
launch_conversation(Parent, Partner, Event) :- 
  assert(/social_state/talking_to/Partner), 
  begin_child_qud( Parent, 
    conversation, 1, Child, 
    [ Child/partner/Partner, 
      Child/status_text:"[chat]":0.1, 
      Child/initial_history/Event]), 
  Partner\=player->assert(Child/location_bids/Partner:0.5);true.

on_stop(conversation, C) :-  
  forall(C/partner/P, retract(/social_state/talking_to/P)).


%% conversation_handler_task(+ConversationQud, Task)
%  IMPERATIVE
%  Launches child task to run Task.
conversation_handler_task(Qud, Input) :- 
  stop_children(Qud), 
  ignore(retract(Qud/location_bids)), 
  Qud/partner/P, 
  start_task(Qud, Input, 100, T, [T/partner/P]).

%% conversation_is_idle(+ConversationQud)
%  ConversationQud is idle, i.e. neither party is speaking.
conversation_is_idle(C) :-
   \+ ( still_speaking_to_partner(C) ; partner_still_speaking_to_me(C) ).

%% still_speaking_to_partner(+ConversationQud)
%  The conversation qud is in the process of saying something.
still_speaking_to_partner(C) :-
   C/quds/_.

%% partner_still_speaking_to_me(ConversationQud)
%  Whoever my partner is in ConversationQud is currently trying
%  to say something to me.
partner_still_speaking_to_me(Conversation) :-
   Conversation/partner/Partner,
   % Read the Partner's mind: find *their* conversation qud
   % and look to see what it's doing.
   elroot(Partner, Root),
   descendant_qud_of(Root, C),
   C/type:conversation,
   C/partner/ $me,
   C/quds/_.

% The conversation was just started up.
on_enter_state(start, conversation, C) :- 
  C/initial_history/Event, 
  normalize_dialog_act(Event, Normalized), 
  ( Normalized =  
      greet($me, _) ->  
      assert(C/greeted) ; conversation_handler_task(C, respond_to_dialog_act(Normalized))) .

% They just walked away from us!
on_event(exit_conversational_space(Partner),
	 conversation,
	 C,
	 stop_qud(C)) :-
   C/partner/Partner.

% They just left :(
on_event(departed(Partner),
	 conversation,
	 C,
	 stop_qud(C)) :-
   C/partner/Partner.

% KLUGE: when polling for actions, check if the conversation is idle,
% and if so try to say something.
%
% This is especially a kluge because it introduces a side-effect into
% a predicate that's not supposed to be imperative.
propose_action(_, conversation, C) :-
   conversation_is_idle(C),
   C/partner/Who,
   conversation_idle_task(Who, Task),
   conversation_handler_task(C, Task),
   fail.

%% currently_in_conversation
%  I'm currently talking to someone.
currently_in_conversation :-
   once(qud(_, conversation)).

%% in_conversation_with(+Character)
%  True if there is a running conversation qud with Character.
in_conversation_with(Person) :-
   /social_state/talking_to/Person.

%% Display debug data.
character_debug_display(Character, line("Talking to:\t", Partner)) :-
   Character::in_conversation_with(Partner).
