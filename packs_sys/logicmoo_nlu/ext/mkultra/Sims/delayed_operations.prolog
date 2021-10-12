%%%
%%% Delayed operations
%%% Supports delay_until/2, delay_for/2.
%%%

delay_until(Time, Code) :-
   assert(/delayed/queue/Time:Code),
   % Recompute deadline
   if(/delayed/next_deadline:T,
      when(Time < T,
	   assert(/delayed/next_deadline:Time)),
      assert(/delayed/next_deadline:Time)).

delay_for(Seconds, Code) :-
   Time is $now + Seconds,
   delay_until(Time, Code).

run_delayed_operations :-
   /delayed/next_deadline:Time,
   Time < $now,
   forall(( /delayed/queue/Time:Code,
	    Time =< $now ),
	  begin(ignore(Code),
		retract(/delayed/queue/Time))),
   if(/delayed/queue/_,
      % Recompute deadline
      begin(minimum(T, /delayed/queue/T),
	    assert(/delayed/next_deadline:T)),
      retract(/delayed/next_deadline)).
run_delayed_operations.

minimum(Variable, Goal) :-
   arg_min(Variable, Variable, Goal).
      
   

