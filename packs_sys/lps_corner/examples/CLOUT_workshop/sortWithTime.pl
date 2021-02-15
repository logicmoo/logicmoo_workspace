
:- expects_dialect(lps).

/* Shortest sorting algorithm:-) A single logical implication, universally quantified on the antecedent.
Maps the ordeable feature into time. Just a cute programming pearl, don't do anything serious with this. */

if member(X,[4,1,3,2]) then writeln(X) from X.

/* From https://en.m.wikipedia.org/wiki/List_of_European_Cup_and_UEFA_Champions_League_finals :*/

won('Benfica',2).
won('Porto',2).
won('Real Madrid',13).
won('Barcelona',5).
won('Marseille',1).
won('Chelsea',1).
won('Manchester United',3).
won('Nottingham Forest',2).
won('Aston Villa',1).
won('Celtic',1).
won('AC Milan',7).
won('Bayern Munich',5).
won('Ajax',4).
won('Inter Milan',3).
won('Juventus',2).
won('Hamburg',1).
won('Steaua Bucuresti',1).
won('Borussia Dortmund',1).
won('Feyenoord',1).
won('PSV Eindhoven',1).
won('Red Star Belgrade',1).

/* Print these 5 times later, after the first example's output: */
if won(Team,Wins) then format("~w won ~w times~n",[Team,Wins]) from Wins+5.

/* Yet another variant, building a nunch of sorted fluent tuples: */
fluents sorted(_Team,_NumberOfWins).

if won(Team,Wins) then initiate sorted(Team,Wins) from Wins.

/** <examples> 
?- go(Timeline).
*/
