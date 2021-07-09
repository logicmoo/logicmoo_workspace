
:- expects_dialect(lps).

/* Showing if-then-else originating an iteration like behavior, inspired on RAK's:
	An agent goes to a place if 
	whenever the agent is not at the place then the agent takes one step towards the place
	and when the agent is at the place then the agent stops.
*/

actions step_towards(_Agent,_Place).
fluents location(_Agent,_Place).

goto(Agent,Place) from _T1 to T2 if 
	(if not location(Agent,Place) at T then step_towards(Agent,Place) from T),
	location(Agent,Place) at T2.

initially location(miguel,lisboa).

next(lisboa,santarem).
next(santarem,coimbra).
next(coimbra,porto).

step_towards(A,P) updates OldP to NewP in location(A,OldP) if P\==OldP, next(OldP,NewP).

if true then goto(miguel,porto).

/** <examples> 
?- go(Timeline).
*/
