
:- expects_dialect(lps).

/* From "Step by Step Towards Creating a Safe Smart Contract: Lessons and Insights from a Cryptocurrency Lab",
Delmolino et.al. */
% Approximates version in Fig.3

% maxRealTime(60).
maxTime(15).

observe init from 1 to 2.
observe player_input(miguel,rock,1000) from 2 to 3.
observe player_input(fariba,rock,1000) from 2 to 3. 
% Bob's play will actually be refused because of the num_players precondition below...:
observe player_input(bob,rock,1000) from 5 to 6. 

% check_winner(+Player0Choice,+Player1Choice,-Winner) Winner is integer or 'draw'
check_winner(paper,scissors,1).
check_winner(scissors,paper,0).
check_winner(rock,scissors,0).
check_winner(scissors,rock,1).
check_winner(rock,paper,1).
check_winner(paper,rock,0).
check_winner(C,C,draw).

events init, player_input(_Sender,_Choice,_Value), send(_Recipient,_Prize).
actions send(_Winner,_Prize). % actions are serialisable by default
% unserializable send(_Winner,_Prize).

fluents reward(_), played(_Sender,_Choice).

init initiates reward(0).
false init, reward(_).

player_input(Sender,Choice,_Value) initiates played(Sender,Choice).
player_input(_Sender,_Choice,Value) updates Old to New in reward(Old) if New is Old+Value.

num_players(N) at T if findall(P,played(P,_),L), length(L,N).

false player_input(_Sender,_Choice,Value), Value\==1000. 
false player_input(Sender,Choice,Value), played(Sender,_).
false player_input(Sender,Choice,Value) from T1 to T2, num_players(N) at T2, N>2.

if num_players(2), reward(R), R>0 % Removing R>0 would still produce the same actions, but new goals ad eternu,
then finalize from _.

finalize if 
	played(P0,Choice0) at T1, played(P1,Choice1) at T1, P0\==P1, P0 @< P1,
    check_winner(Choice0,Choice1,Winner),
    reward(Now) at T1, % specifying T1 is important, otherwise new goals will keep being added....
    send_rewards(Winner,P0,P1) from T1 to T2.
    %lps_terminate from T2. % no more players
finalize if reward(0).

send_rewards(0,P0,_P1) from T1 to T2 if reward(R) at T1, send(P0,R) from T1 to T2.
send_rewards(1,_P0,P1) from T1 to T2 if reward(R) at T1, send(P1,R) from T1 to T2.
send_rewards(draw,P0,P1) if 
	reward(R) at T1, num_players(N) at T1, Prize is R/2, 
	send(P0,Prize) from T1 to T2, send(P1,Prize) from T1 to T2.

send(_,Value) updates Old to New in reward(Old) if New is Old-Value.

false send(_,Prize) from T1 to T2, reward(V) at T1, Prize>V.
false send(_,0).

/** <examples>
?- godc(Timeline).
*/