:- expects_dialect(lps).

% Tezos Game
maxTime(10). % Simulate over 10 cycles

events play(Player, Number, Bet), addfunds(Sender, Amount), observeRandom(Number). 
fluents game(Player, Number, Bet), balance(Amount).
actions failwith(Agent, Message), transfer(Receiver, Amount), send(Sender, Number).

% Here the transfer action only affects the balance in the smart contract.
% The funds in the recipients account are updated in the external environment.
transfer(_, Amount) updates Old to New in balance(Old)
if New is Old - Amount.

% Funds can be added to the smart contract as an external event:
addfunds(Sender, Amount) updates Old to New in balance(Old)
if New is Old + Amount.

initially balance(100).

oracle(myaddress).

% simulate input events:
observe play(miguel, 52, 90) from 1 to 2.
observe observeRandom(51) from 2 to 3. 

% Ensure that the number is a valid choice, i.e. is between 0 and 100.
% Here playFails/4 is an intensional predicate.
playFails(Player, Number, Bet, 'number must be <= 100') at T
if Number > 100 at T.

% Ensure that the contract has enough funds to pay the player in case she wins. 
% % The Liquidity version assumes the player’s bet has already been added to the balance.
% But if the bet fails, the bet should be returned. 
% I have modified the Liquidity version to avoid this.
 playFails(Player, Number, Bet, 'I do not have enough money for this bet') at T
if  balance(Amount) at T, Bet > Amount.

% Ensure there is not another game currently in play.
playFails(Player, Number, Bet, 'Game already started with g????')  at T
if game(_,_,_) at T.

% If an attempted play fails, send a message (to the Player?).
if play(Player, Number, Bet) from T1 to T2, playFails(Player, Number, Bet, Message) at T1
then failwith(Player, Message) from T2.

% Start the game.
play(Player, Number, Bet) initiates game(Player, Number, Bet)
if not playFails(Player, Number, Bet, Message).

% Add the Bet to the balance.
play(Player, Number, Bet) updates Old to New in balance(Old)
if not playFails(Player, Number, Bet, Message), New is Old + Bet.

% For inputs sent by the random number oracle.
% Assume for simplicity, the number is already between 0 and 100.
% Here oracleFails/2 is an intensional predicate.

oracleFails(Sender, 'No game already started') at T
if not game(_,_,_) at T.

oracleFails(Sender, 'Random numbers can not be generated') at T
if oracle(Address), Sender \= Address.

% If the oracle fails, send a message to ????. Player?
if send(Sender, Random) to T, oracleFails(Sender, Message) at T, 
game(Player, Number, Bet)
then failwith(Player, Message) from T.

% The player loses if her number is greater than the random number. In this case, she forfeits her % bet amount and the game smart contract is reset (the money stays on the game smart contract).
%
if send(Sender, Random) to T, not oracleFails(Sender,_) at T, 
game(Player, Number, Bet) at T, balance(Amount) at T,
Number > Random
then terminate game(Player, Number, Bet) from T.

% The player wins if her number n is smaller or equal to r.
%  In this case, she gets back her initial bet b 
%  plus a reward which is proportional to her bet and her chosen number b * n / 100. 
%
if send(Sender, Random) to T, not oracleFails(Sender,_) at T, 
game(Player, Number, Bet) at T, balance(Amount) at T,
Number =< Random, RepayandReward is Bet + (Bet*Number)/100
then  transfer(Player, RepayandReward) from NextT, terminate game(Player, Number, Bet) from T.

% This simulates the oracle, which monitors events and fluents,
% then generates a random number and sends it to the smart contract.
% Logically, a random number can be understood either as an argument of a fluent
% or as an argument of an action or event. 
% But in the current implementation 
% if it is an event it has to be in the antecedent of the rule.
% 
if play(Player, Number1, Bet) to T1, 
game(Player, Number1, Bet) at T1,
observeRandom(Number2) from T1 to T2
then send(myaddress, Number2) from T2.

/** <examples>
?- go(Timeline).
?- dumpen.
?- go.
*/
