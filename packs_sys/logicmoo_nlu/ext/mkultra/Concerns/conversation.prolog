%%%
%%% Conversation concerns
%%% These listen for dialog acts from the conversational partner and
%%% And spawn tasks to respond to them.
%%%
%%% Important state information
%%%    Concern/partner/Partner		Who the conversation is with
%%%    Concern/initial_history/Event	What happened before this concern
%%%					was launched.
%%% Subconcerns
%%%    Task: respond_to_dialog_act(DA)
%%%

%% launch_conversation(+Parent, +Partner, +Event)
%  IMPERATIVE
%  Creates a subconcern of Parent to handle conversation with Partner.
%  Event is the initial event that is triggering the conversation.
launch_conversation(Parent, Partner, Event) :-
   begin_child_concern(Parent, conversation, 1, Child,
		       [ Child/partner/Partner,
			 Child/initial_history/Event ]),
   (Partner \= player -> assert(Child/location_bids/Partner:20);true).

%% conversation_handler_task(+ConversationConcern, Task)
%  IMPERATIVE
%  Launches child task to run Task.
conversation_handler_task(Concern, Input) :-
   kill_children(Concern),
   ignore(retract(Concern/location_bids)),
   Concern/partner/P,
   start_task(Concern, Input, 100, T, [T/partner/P]).

%% conversation_is_idle(+ConversationConcern)
%  ConversationConcern is idle, i.e. neither party is speaking.
conversation_is_idle(C) :-
   \+ ( still_speaking_to_partner(C) ; partner_still_speaking_to_me(C) ).

%% still_speaking_to_partner(+ConversationConcern)
%  The conversation concern is in the process of saying something.
still_speaking_to_partner(C) :-
   C/concerns/_.

%% partner_still_speaking_to_me(ConversationConcern)
%  Whoever my partner is in ConversationConcern is currently trying
%  to say something to me.
partner_still_speaking_to_me(Conversation) :-
   Conversation/partner/Partner,
   % Read the Partner's mind: find *their* conversation concern
   % and look to see what it's doing.
   elroot(Partner, Root),
   descendant_concern_of(Root, C),
   C/type:conversation,
   C/partner/ $me,
   C/concerns/_.

% The conversation was just started up.
on_enter_state(start, conversation, C) :-
   C/initial_history/Event,
   normalize_dialog_act(Event, Normalized),
   ( Event = greet($me, _) ->
       assert(C/greeted)
       ;
       conversation_handler_task(C, respond_to_dialog_act(Normalized)) ).

% They just walked away from us!
on_event(exit_conversational_space(Partner),
	 conversation,
	 C,
	 kill_concern(C)) :-
   C/partner/Partner.

% They just died :(
on_event(die(Partner),
	 conversation,
	 C,
	 kill_concern(C)) :-
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
   once(concern(_, conversation)).

%% in_conversation_with(+Character)
%  True if there is a running conversation concern with Character.
in_conversation_with(Person) :-
   once( ( concern(C, conversation),
	   C/partner/Person )).

%% Display debug data.
character_debug_display(Character, line("Talking to:\t", Partner)) :-
   Character::in_conversation_with(Partner).