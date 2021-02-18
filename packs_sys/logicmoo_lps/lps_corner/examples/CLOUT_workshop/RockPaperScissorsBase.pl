
:- expects_dialect(lps).

% Rock, Paper, Scissors
maxTime(5). % Simulate over 5 cycles

beats(scissors, paper). 
beats(paper, rock).
beats(rock, scissors).

events transaction_from(_From,_Input,_Ammount).
fluents played(_Player,_Choice),reward(_Total), gameOver.
actions pay(_Player,_Prize).

initially reward(0).

% simulate input events:
observe transaction_from(miguel,rock,1000) from 1 to 2.
observe transaction_from(bob,paper,1000) from 1 to 2.
observe transaction_from(alex,paper,1000) from 2 to 3. % one player too many!

transaction_from(From,Input,Wei) initiates played(From,Input).

false transaction_from(_From,_Input,Wei), Wei=<0.
false transaction_from(From,_Input,_Wei), played(From,_).

num_players(N) at T if 
	findall(P, played(P,_) at T, L), length(L,N).

false num_players(N), N>2. 

transaction_from(_Player,_,X) updates Old to New in reward(Old) if 
	New is Old+X.

pay(_,Prize) updates Old to New in reward(Old) if New is Old-Prize.

if  played(P0,Choice0) at T1, played(P1,Choice1) at T1, P0\==P1, beats(Choice0,Choice1), not gameOver at T1
then initiate gameOver from T1, reward(Prize) at T1, pay(P0,Prize) from T1 to T2.

if played(P0,Choice) at T1, played(P1,Choice) at T1, P0 @> P1, not gameOver at T1
then initiate gameOver from T1, reward(Prize) at T1, Half is Prize/2, pay(P0,Half) from T1, pay(P1,Half) from T1.

/** <examples>
?- go(Timeline).
?- dumpen.
?- why(happens(pay(bob,2000),2,3)).
*/