
:- expects_dialect(lps).

% Exemplify external events defined as (polled) Prolog predicates
% TODO: We need more control over this, to specify predicates and their calling patterns
maxTime(5).
prolog_events myEvent(2,_).
if myEvent(X,Y) from _ to _ then writeln(X-Y) from T2.

% no facts here please, otherwise they'll be taken as macro action clauses:
myEvent(1,hello) :- true.
myEvent(2,ola) :- true.
