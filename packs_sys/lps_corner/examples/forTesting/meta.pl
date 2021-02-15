
:- expects_dialect(lps).

% Using an "event meta-call" in a precondition to avoid enumerating all forbidden atomic events
% Building an arbitrary intensional fluent out of a state subset
maxTime(6).

events foo, bar, blah.
observe foo from 1 to 2.
observe bar from 2 to 3.
observe blah from 3 to 4.
false happens(E,_,_), not member(E,[foo,writeln(_)]).


fluents a,b,temperature(_).
initially a,b,temperature(21).

uberFuent(F) at T if holds(F,T), not system_fluent(F).

if uberFuent(F) at T then writeln(F) from T.
