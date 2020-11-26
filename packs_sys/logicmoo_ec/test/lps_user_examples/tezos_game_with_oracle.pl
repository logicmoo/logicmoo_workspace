:- expects_dialect(lps).

% From http://www.liquidity-lang.org/doc/tutorial/game.html, single player
% Simple version, not on blockchain
 
events play(_Player, _Number, _Bet), fund(_Amount).
actions pay(_Player,_Prize), lost(_Player, _Number, _Bet).

fluents played(_Player,_Number,_Bet), balance(_Amount).
initially balance(0).

fund(Amount) updates Old to New in balance(Old) if New is Old+Amount.

false fund(Amount), Amount=<0. % redundant with blockchain's own constraints

play(Player,N,Bet) initiates played(Player,N,Bet).

false play(_,_,_), played(_,_,_). % one play at a time
false play(_,N,_), not between(0,100,N).
false play(_,_,Bet), balance(Balance), Bet>Balance. % not enough prior funds to allow play

pay(_,Prize) updates Old to New in balance(Old) if New is Old-Prize.
false pay(_,Prize), balance(Balance), Prize>Balance. % superflous wrt blockchain constraints , but nice to have

finish(Random_number) from T1 to T2 if
	played(P,Number,Bet), Number>Random_number,
	lost(P,Number,Bet) from T1 to T2,
	terminate played(P,Number,Bet) from T1 to T2.
finish(Random_number) from T1 to T2 if
	played(P,Number,Bet) at T1, Number=<Random_number,
	Prize is Bet+Bet*Number/100,
	pay(P,Prize) from T1 to T2,
	terminate played(P,Number,Bet) from T1 to T2.

% Oracle:
:- use_module(library(random)).
if played(_Player,_N,_Bet) then random_between(0,100,R) at T1, finish(R) from T1.

observe play(bob,65,50) from 1.
observe fund(100) from 2.
observe play(bob,66,50) from 3.
observe play(miguel,13,20) from 4.
observe play(bob,40,40) from 4.
observe play(fariba,50,1) from 5. 
observe play(miguel,101,1) from 7.


/** <examples> 
?- go(Timeline).
*/
