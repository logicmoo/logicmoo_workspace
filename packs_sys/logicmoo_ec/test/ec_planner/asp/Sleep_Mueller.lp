timepoint(0..5).
agent(john).
fluent(awake(X)) :- agent(X).
event(wake_up(X)) :- agent(X).
event(fall_asleep(X)) :- agent(X).
initiates(wake_up(A),awake(A),T) :- agent(A),timepoint(T).
terminates(fall_asleep(A),awake(A),T) :- agent(A),timepoint(T).
holds_at(awake(john),0).
-released_at(F,0) :- fluent(F).
happens(fall_asleep(john),1).
happens(wake_up(john),2).

% End of file.
