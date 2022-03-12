%%%
%%% Delayed operations
%%% Supports delay_until/2, delay_for/2.
%%%

:- higher_order delay_until(0, 1).


%=autodoc
%% delay_until( ?Time, ?Code) is semidet.
%
% Delay Until.
%
delay_until(Time, Code) :-
   assert(/delayed/queue/Time:Code),
   % Recompute deadline
   if(/delayed/next_deadline:T,
      when(Time < T,
	   assert(/delayed/next_deadline:Time)),
      assert(/delayed/next_deadline:Time)).

:- higher_order delay_for(0, 1).
:- public delay_for/2.


%=autodoc
%% delay_for( ?Seconds, ?Code) is semidet.
%
% Delay For.
%
delay_for(Seconds, Code) :-
   Time is $now + Seconds,
   delay_until(Time, Code).



%=autodoc
%% run_delayed_operations is semidet.
%
% Run Delayed Operations.
%
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



%=autodoc
%% minimum( ?Variable, :GoalGoal) is semidet.
%
% Minimum.
%
minimum(Variable, Goal) :-
   arg_min(Variable, Variable, Goal).
      
   

