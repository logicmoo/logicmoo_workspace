%% agent(+Event, ?Who) is det
%  True is Who is the agent of Event.
%  Assumes agent is first argument.
agent(Event, Who) :-
    arg(1, Event, Who).

%% patient(+Event, ?What) is det
%  True if What is the parient of Event.
%  Assumes the patient is the second argument.