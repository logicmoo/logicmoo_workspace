
:- expects_dialect(lps).

maxTime(10).
fluents locked(_), trash(_), bin(_).
actions dispose(_,_), keep(_).

initially bin(bucket).

% A convoluted story on the effect of implicit time variables..:

if true then
	keep(uhuh) from 5,
	terminate bin(bucket) to T,
	% we need to make T explicit here, otherwise the if could start earlier than intended...
	% because Finish is an explicit time var, the whole if-then-else is assumed to be fully
	% specified timewise, and the IF subgoal would NOT be assumed adjacent to the terminate action above
	% moral: if in doubt, be explicit!
	(if bin(Container) at T then 
		dispose(garbage, Container) else keep(garbage) to Finish ), 
	% This is actually the start our little story. Any Prolog system predicate can be used as 
	% an action,  as long as situated in some time... by doing that, our developer has
	% made time explicit, and so this action's start time is NOT assumed adjacent to the end of
	% the if-then-else. 
	% Dangerous corollary: 
	% if you remove all time variables from this rule, it will work as expected (with writeln executed as a "timeless predicate"):
	writeln(uhuhuh) from Finish.
