
:- expects_dialect(lps).

% see diningPhilosphers-client.pl
maxRealTime(180).
minCycleTime(0.01).

fluents		available(_).
events		pickup(_,_), putdown(_,_).

initially	available(fork1),
		available(fork2),
		available(fork3),
		available(fork4),
		available(fork5).
pickup(P, F)	terminates	available(F).
putdown(P, F)	initiates	available(F).

false	pickup(P, F),    not available(F).
false	pickup(P1, F),  pickup(P2, F), P1 \= P2.

/* If you want to be notified just before this server program finishes, uncomment this:
if real_time_beginning(B), maxRealTime(MRT), InTen is B+MRT-10
then 
	real_time(Now) at _T, Now>=InTen,
	lps_send_email(['youremail@xxx.com'], 'LPS table is closing soon!',
        'The philosophers table will close in ~w seconds!'-[10]).
*/
/** <examples>
?- serve(ID).
*/