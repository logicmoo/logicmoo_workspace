
:- expects_dialect(lps).

% these could be inferred:
events player_input(_Sender,_Choice,_Value).
actions send(_Winner,_Prize). 
fluents reward(_), played(_Sender,_Choice).

% These or others could be suggested:
observe player_input(miguel,paper,1000) from 2 to 3.
observe player_input(fariba,rock,1000) from 2 to 3. 
observe player_input(bob,rock,1000) from 3 to 4. 

beats(scissors, paper). % scissors beats paper, ....
beats(paper, rock).
beats(rock, scissors).

initially reward(0). % initially the reward is 0

% when a player ....
player_input(Sender,Choice,_Value) initiates played(Sender,Choice).
% a play with a value increases the reward by that value
player_input(_Sender,_Choice,Value) updates Old to New in reward(Old) if New is Old+Value.

% to get the number of players collect all plays and count them
num_players(N) at T if findall(P,played(P,_),L), length(L,N).

% Players must bet a positive ammount
false player_input(_Sender,_Choice,Value), Value=<0. 
% If a player has played, he can not play again:
false player_input(Sender,_Choice,_Value), played(Sender,_). 
% a play is forbidden if the number of players becomes greater than 2:
false player_input(Sender,Choice,Value) from T1 to T2, num_players(N) at T2, N>2. 

% if a player's choice beats the other's, send him the whole reward
if played(P0,Choice0) at T1, played(P1,Choice1) at T1, P0\==P1, beats(Choice0,Choice1), reward(R) at T1, R\==0
then send(P0,R) from T1.

% if both players choose the same choice, send half of the reward to each
if played(P0,Choice) at T1, played(P1,Choice) at T1, P0\==P1, reward(R) at T1, R\==0
then Prize is R/2, send(P0,Prize) from T1,  send(P1,Prize) from T1.

% After sending prizes terminate the contract
if send(_,_Prize) to T2 
then lps_terminate from T2.

% When sending a prize subtract it from the reward
send(_Player,Prize) updates Old to New in reward(Old) if New is Old-Prize.

% after sending a prize the reward must not become negative
false send(_,Prize) from T1 to T2, reward(V) at T2, V<0.
% It is impossible to send zero
false send(_,0).

/** <examples>
?- godc(Timeline).
*/