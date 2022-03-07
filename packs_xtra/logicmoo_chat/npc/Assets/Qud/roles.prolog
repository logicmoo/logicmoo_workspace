:- public agent/2, patient/2.

%% agent(+Event, ?Who) is det
%  True if Who is the agent of Event.
%  Assumes agent is first argument.
agent(player_input(_), _) :-
   !,
   fail.
agent(Event, Who) :-
    must_be(compound,Event),arg(1, Event, Who).

%% patient(+Event, ?What) is det
%  True if What is the parient of Event.
%  Assumes the patient is the second argument.
patient(Event, What) :-
    must_be(compound,Event),arg(2, Event, What).