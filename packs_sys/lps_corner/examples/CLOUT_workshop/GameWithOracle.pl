
:- expects_dialect(lps).

% Based on http://www.liquidity-lang.org/doc/tutorial/game.html, a single player game
% Simple standalone version, not on Tezos blockchain
 
events play(_Number, _Player, _Bet), fund(_Amount).
actions pay(_Player,_Prize), lost(_Player, _Number, _Bet).

fluents game(_Number,_Bet,_Player), balance(_Amount).
initially balance(0).

fund(Amount) updates Old to New in balance(Old) if New is Old+Amount.

false fund(Amount), Amount=<0. % redundant with blockchain's own constraints

play(N,Player,Bet) initiates game(N,Bet,Player).

play(_,_,Bet) updates Old to New in balance(Old) if New is Old+Bet.

false play(_,_,_), game(_,_,_). % one game at a time
false play(N,_,_), not between(0,100,N).
false play(_,_,Bet), balance(Balance), Bet>Balance. % not enough prior funds to allow play

pay(_,Prize) updates Old to New in balance(Old) if New is Old-Prize.
false pay(_,Prize), balance(Balance), Prize>Balance. % superflous wrt blockchain constraints , but nice to have

finish(Random_number) from T1 to T2 if
	game(Number,Bet,P), Number>Random_number,
	lost(P,Number,Bet) from T1 to T2,
	terminate game(Number,Bet,P) from T1 to T2.
finish(Random_number) from T1 to T2 if
	game(Number,Bet,P) at T1, Number=<Random_number,
	Prize is Bet+Bet*Number/100,
	pay(P,Prize) from T1 to T2,
	terminate game(Number,Bet,P) from T1 to T2.

% Oracle:
:- use_module(library(random)).
if game(_N,_Bet,_Player) then random_between(0,100,R) at T1, finish(R) from T1.

% Example sequence of events:
observe play(65,bob,50) from 1.
observe fund(100) from 2.
observe play(66,bob,50) from 3.
observe play(13,miguel,20) from 4.
observe play(40,bob,40) from 4.
observe play(50,fariba,1) from 5. 
observe play(101,miguel,1) from 7.


/** <examples> 
?- go(Timeline).
*/

