
% Concurrent candidate actions dispatch(john, 10), dispatch(mary, 15).
% Both actions should be selected at time 2 for execution.


initial_state( [stock(30)] ).


reactive_rule( [ happens(order(C, Q), T1, T2)], 
	       [happens(dispatch(C, Q), T3, T4), tc(T2 =< T3)] ).


l_timeless(lps_less(X, Y), [X < Y]).
l_timeless(lps_subtract(X, Y, Z), [Z is X - Y]).
l_timeless(lps_add(X, Y, Z), [Z is X + Y]).

d_pre([happens(dispatch(C, Q), T1, T2), holds(stock(S), T1), lps_less(S, Q) ]).


terminated( happens(dispatch(C, Q), T1, T2), stock(X), [holds(stock(X), T1)] ).

initiated( happens(dispatch(C, Q), T1, T2), stock(Z), 
	[holds(stock(X), T1), lps_subtract(X,Q,Z)]).


terminated( happens(restock(Q), T1, T2), stock(X), [holds(stock(X), T1)] ).

initiated( happens(restock(Q), T1, T2), stock(Z), 
	[holds(stock(X), T1), lps_add(X,Q,Z)]).


observe([order(john, 10), order(mary, 15)], 1).
observe([], 2).
observe([], 3).
observe([restock(20)], 4).
observe([], 5).
observe([], 6).



fluent(stock(_)).

action( order(_, _) ).
action( dispatch(_, _) ).

event(order(C, Q)).
