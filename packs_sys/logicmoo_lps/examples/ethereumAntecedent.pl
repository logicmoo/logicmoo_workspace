
:- expects_dialect(lps).

too_little_wei(_).
actions organize(_).

% system fluents below will take a while to succeed, so avoid using maxTime(Cycles):
maxRealTime(3). % 3 seconds should do

% Reactive rule antecedents cannot execute actions, they consume events. 
% These can be generated with a prolog_events declaration, but this wouldn't suit this example,
% with a conjunction of system "event" calls; so instead we hack our way out of this using system fluents...
% ... still better than a plain imperative Prolog call, as we get our external world state samples timestamped:
if e(accounts(L)) at T1, member(A, L), 
   e(getBalance(A,latest,_),MID) at T1, e_available(MID) at T2,
   e_result(MID,Balance) at T2, 
   too_little_wei(Balance)
then organize(A) from T2 .
/** <examples> 
?- go(Timeline).
*/
