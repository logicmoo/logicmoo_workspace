:- expects_dialect(lps).

% Tezos Game
maxTime(10). % Simulate over 10 cycles

events play(Player, Number, Bet), addfunds(Sender, Amount), observeRandom(Number). 

fluents game(Player, Number, Bet), balance(Amount),  failed(Agent, Message).

actions failwith(Agent, Message), transfer(Receiver, Amount), send(Sender, Number), 
gameOver(Player, Number, Bet).

/*
 * Here is an alternative approach, in which the action initiates the fluent.
 * However in the version below, the fluent generates the action.
 * Perhaps better if both were simultaneous.
 * Or better if failed were a macroevent which holds at the same time as the external event play.
 * 
failwith(Agent, Message) initiates failed(Agent, Message).
*/

% Here the transfer action only affects the balance in the smart contract.
% The funds in the recipients account are updated in the external environment.

transfer(_, Amount) updates Old to New in balance(Old)
if New is Old - Amount.

% Funds can be added to the smart contract as an external event:

addfunds(Sender, Amount) updates Old to New in balance(Old)
if New is Old + Amount.

% gameOver(Player, Number, Bet) terminates game(Player, Number, Bet).

initially balance(100).

oracle(myaddress).

% simulate input events:
observe play(miguel, 52, 75) from 1 to 2.
% observe observeRandom(51) from 2 to 3. % need a delay for this to work.
observe observeRandom(51) from 3 to 4.


% Ensure that the number is a valid choice, i.e. is between 0 and 100
% If not, a fail message is sent to (or about) the Player.
% The Liquidity version does not have the Player as a parameter of the failure message.

play(Player, Number, Bet) initiates failed(Player, 'number must be <= 100')
if Number > 100.

% Ensure that the contract has enough funds to pay the player in case she wins. 
% The Liquidity version assumes the player’s bet has already been added to the balance.
% But if the bet fails, the bet should be returned. I have modified the Liquidity version to avoid this.
% It might be more intuitive if failed were a macroevent, which holds at the same time as play.

play(Player, Number, Bet) initiates failed(Player,'I do not have enough money for this bet') 
if balance(Amount) at T, Bet > Amount.

% Ensure that the contract has enough funds to pay the player in case she wins. 

play(Player, Number, Bet) initiates failed(Player,'Game already started with g????') % What is g?
if game(_,_,_).

if play(Player, Number, Bet) to T, failed(Player, Message) at T
then failwith(Player, Message).


%  Delay of one cycle before game starts.
%  Not ideal.
%  It might be more natural if game is initiated by play.
%  
if play(Player, Number, Bet) to T,   not failed(Player,_) at T
then initiate game(Player, Number, Bet) from T.

 %  Delay of one cycle before game starts.
 %  
if play(Player, Number, Bet) to T,   not failed(Player,_) at T, 
balance(Old) at T, New is Old + Bet
then initiate balance(New) from T, terminate balance(Old) from T.

% For inputs from the random number generator.
% Assume for simplicity, the number is already between 0 and 100.

send(Sender, Number) initiates
failed(Sender, 'No game already started')
if not game(_,_,_).

send(Sender, Number) initiates
failed(Sender, 'Random numbers can not be generated')
if oracle(Address), Sender \= Address.

% The player loses if her number is greater than the random number. In this case, she forfeits her % bet amount and the game smart contract is reset (the money stays on the game smart contract).

if send(Sender, Random) to T, not failed(Sender,_) at T, 
game(Player, Number, Bet) at T, balance(Amount) at T,
Number > Random
then terminate game(Player, Number, Bet) from T.

% The player wins if her number n is smaller or equal to r.
%  In this case, she gets back her initial bet b 
%  plus a reward which is proportional to her bet and her chosen number b * n / 100. 
%
if send(Sender, Random) to T, not failed(Sender,_) at T, 
game(Player, Number, Bet) at T, balance(Amount) at T,
Number =< Random, RepayandReward is Bet + (Bet*Number)/100
then  transfer(Player, RepayandReward) from NextT, terminate game(Player, Number, Bet) from T.

% The version below doesn't work.
% Probably because of the negated action not failwith.
% However, it would probably be easy to extend the intepreteer.
% Or at least give a syntax error.
% 
/*
% We must also make sure that a game is currently being played otherwise this random number is quite useless.
%
if send(Sender, Number) to T, not game(_,_,_) at T
then failwith(Sender, 'No game already started') from T.

% For sake of security, check the address of the sender of the number is registered as an oracle.
% 
if send(Sender, Number) to T, oracle(Address), Sender \= Address
then failwith(Sender, 'Random numbers can not be generated') from T.

% The player loses if her number is greater than the random number. In this case, she forfeits her % bet amount and the game smart contract is reset (the money stays on the game smart contract).

if send(Sender, Random) to T, game(Player, Number, Bet) at T, balance(Amount) at T,
not failwith(Sender,_) from T to NextT, Number > Random
then terminate game(Player, Number, Bet) from NextT.

% The player wins if her number n is smaller or equal to r.
%  In this case, she gets back her initial bet b 
%  plus a reward which is proportional to her bet and her chosen number b * n / 100. 
%
if send(Sender, Random) to T, game(Player, Number, Bet) at T, balance(Amount) at T,
not failwith(Sender,_) from T to NextT, Number =< Random, 
RepayandReward is Bet + (Bet*Number)/100
then  transfer(Player, RepayandReward) from NextT, gameOver(Player, Number, Bet) from NextT.
*/

% This simulates the oracle, which monitors events and fluents,
% then generates a random number and sends it to the smart contract.
% Logically, a random number can be understood either as an argument of a fluent
% or as an argument of an action or event. 
% But in the current implementation 
% if it is an event it has to be in the antecedent of the rule.
% 
if play(Player, Number1, Bet) to T1, 
game(Player, Number1, Bet) at T2,
% game(Player, Number1, Bet) at T
% % this doesn't work, because of the delay between play and game.
 observeRandom(Number2) from T2 to T3
then send(myaddress, Number2) from T3.

/** <examples>
?- go(Timeline).
?- dumpen.
?- go.
*/
