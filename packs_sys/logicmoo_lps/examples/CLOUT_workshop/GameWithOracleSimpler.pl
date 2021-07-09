
:- expects_dialect(lps).

% Inspired on http://www.liquidity-lang.org/doc/tutorial/game.html, a single player game
% Simple standalone version, not on Tezos blockchain
maxTime(10).

events play(_Number, _Player, _Bet), fund(_Amount).
actions pay(_Player,_Prize), lost(_Player,_Bet).

fluents balance(_Amount).
initially balance(0).

fund(Amount) updates Old to New in balance(Old) if New is Old+Amount.

false fund(Amount), Amount=<0. % redundant with blockchain's own constraints

play(_,_,Bet) updates Old to New in balance(Old) if New is Old+Bet.

false play(N,_,_), not between(0,100,N).
false play(_,_,Bet), balance(Balance), Bet>Balance. % not enough prior funds to allow play
false play(_,A,_), play(_,B,_), A\==B.

pay(_,Prize) updates Old to New in balance(Old) if New is Old-Prize.
false pay(_,Prize), balance(Balance), Prize>Balance. % superflous wrt blockchain constraints , but nice to have

% Conjecture: Deterministic randomness is possible in the blockchain:-)
:- use_module(library(random)).
if play(N,Player,Bet) then 
	random_between(0,100,R) at T, 
	(if N>R then lost(Player,Bet) from T else Prize is Bet+Bet*N/100, pay(Player,Prize) from T   ).

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

