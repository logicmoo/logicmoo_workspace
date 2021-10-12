%%
%% Responding to imperatives
%%

strategy(respond_to_dialog_act(command(Requestor, $me, Task)),
	 follow_command(Requestor, Task, RequestStatus)) :-
   request_status(Requestor, Task, RequestStatus).

request_status(_Requestor, Task, immoral) :-
   @immoral(Task),
   !.
request_status(_Requestor, Task, non_normative) :-
   \+ well_typed(Task, action, _),
   !.
request_status(_Requestor, Task, unachievable(Reason)) :-
   \+ have_strategy(Task),
   once(diagnose(Task, Reason)),
   !.
request_status(Requestor, Task, incriminating(P)) :-
   guard_condition(Task, P),
   pretend_truth_value(Requestor, P, Value),
   Value \= true,
   !.
request_status(_Requestor, _Task, normal).

strategy(follow_command(Requestor, Task, normal),
	 if(dialog_task(Task),
	    Task,
	    call(add_pending_task(on_behalf_of(Requestor, Task))))).

:- public dialog_task/1.
dialog_task(tell_about(_,_,_)).

strategy(follow_command(_, _, immoral),
	 say_string("That would be immoral.")).
strategy(follow_command(_, _, non_normative),
	 say_string("That would be weird.")).
strategy(follow_command(_, _, unachievable(Reason)),
	 explain_failure(Reason)).
strategy(follow_command(_, _, incriminating(_)),
	 say_string("Sorry, I can't.")).

diagnose(Task, ~Precondition) :-
   unsatisfied_task_precondition(Task, Precondition).

default_strategy(explain_failure(_),
		 say_string("I don't know how.")).
strategy(explain_failure(~know(X:location(Object, X))),
	 speech(["I don't know where", np(Object), "is"])).

strategy(tell_about($me, _, Topic),
	 describe(Topic, general, null)).

normalize_task(go($me, Location),
	       goto(Location)).
normalize_task(take($me, Patient, _),
	       pickup(Patient)).
normalize_task(put($me, Patient, Destination),
	      move($me, Patient, Destination)) :-
   nonvar(Destination).

strategy(talk($me, $addressee, Topic),
	 describe(Topic, introduction, null)) :-
   nonvar(Topic).

strategy(talk($me, ConversationalPartner, Topic),
	 add_conversation_topic(ConversationalPartner, Topic)) :-
   ConversationalPartner \= $addressee.

strategy(add_conversation_topic(Person, Topic),
	 assert(/pending_conversation_topics/Person/ask_about($me,
							      Person,
							      Topic))) :-
   var(Topic) -> Topic = Person ; true.

strategy(end_game(_,_), end_game(null)).
