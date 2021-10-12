%%
%% Top-level strategies for responding to different kinds of dialog acts
%%

strategy(respond_to_dialog_act(parting(Them, $me)),
	 begin(assert(Parent/generated_parting),
	       parting($me, Them),
	       sleep(1),
	       call(kill_concern(Parent)))) :-
   parent_concern_of($task, Parent),
   \+ Parent/generated_parting.

strategy(respond_to_dialog_act(parting(_Them, $me)),
	 call(kill_concern(Parent))) :-
   parent_concern_of($task, Parent),
   Parent/generated_parting.

%%
%% Uninterpretable inputs
%%

default_strategy(respond_to_dialog_act(Act),
		 speech(["huh?"])) :-
   asserta($global::not_understood($me, Act)).

%%
%% Greetings and closings
%%

strategy(respond_to_dialog_act(greet($addressee, $me)),
	 (assert(Conversation/greeted), greet($me, $addressee))) :-
   parent_concern_of($task, Conversation),
   \+ Conversation/greeted.
strategy(respond_to_dialog_act(greet($addressee, $me)),
	 null) :-
   parent_concern_of($task, Conversation),
   Conversation/greeted.

%%
%% Discourse increments
%%

strategy(respond_to_dialog_act(discourse_increment(_Sender, _Receiver, [ ])),
	 null).
strategy(respond_to_dialog_act(discourse_increment(Sender, Receiver,
						   [ Act | Acts])),
	 begin(respond_to_increment(Sender, Receiver, Act),
	       respond_to_dialog_act(discourse_increment(Sender, Receiver, Acts)))).

default_strategy(respond_to_increment(_, _, _),
		 null).
strategy(respond_to_increment(Speaker, Addressee, s(LF)),
	 respond_to_dialog_act(assertion(Speaker, Addressee, LF, present, simple))).

%%
%% Agreement/disagreement
%%

strategy(respond_to_dialog_act(agree(_, _, _)),
	 null).
strategy(respond_to_dialog_act(disagree(_, _, _)),
	 null).

%%
%% Hypnotic commands
%%

strategy(respond_to_dialog_act(hypno_command(_, $me, LF, present, simple)),
	 do_hypnotically_believe(LF)).

strategy(do_hypnotically_believe(LF),
	 begin(flash(Yellow, Green, 0.3, 1.5),
	       assertion($me, Partner, LF, present, simple))) :-
   hypnotically_believe(LF),
   Yellow is $'Color'.yellow,
   Green is $'Color'.green,
   $task/partner/Partner.

default_strategy(do_hypnotically_believe(_LF),
		 % No effect
		 null).