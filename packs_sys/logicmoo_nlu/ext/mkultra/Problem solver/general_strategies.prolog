%%
%% General strategies
%%

strategy(achieve(runnable(Action)), Strategy) :-
   % If there's an unsatisfied precondition, try to satisfy it.
   % Otherwise, it's already runnable
   blocking(Action, Blocker) ->
      (Strategy = achieve(Blocker))
      ; (Strategy = null).

strategy(achieve(P), wait_condition(P)) :-
   self_achieving(P).

strategy(sleep(Seconds), wait_condition(after_time(Time))) :-
   Time is $now + Seconds.

after_time(Time) :-
   $now > Time.

self_achieving(/perception/nobody_speaking).

