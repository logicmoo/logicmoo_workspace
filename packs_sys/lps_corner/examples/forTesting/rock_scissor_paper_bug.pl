
:- expects_dialect(lps).

actions send/2.
events receive_message/3.
observe receive_message(bob, scissors, 1000), receive_message(fariba, paper, 1000) from 1 to 2.

check_winner(Player1, Choice, Player2, Choice, draw).

check_winner(Player1, Choice1, Player2, Choice2, Player1) :-
	beats(Choice1, Choice2).
check_winner(Player1, Choice1, Player2, Choice2, Player2) :-
	beats(Choice2, Choice1).

beats(scissors, paper).
beats(paper, rock).
beats(rock, scissors).

if receive_message(Player1, Choice1, 1000) from T1 to T2, 
receive_message(Player2, Choice2, 1000) from T1 to T2, Player1 \= Player2
then reward_winner(Player1, Choice1, Player2, Choice2, Player) from T2 to T3.
 
reward_winner(Player1, Choice1, Player2, Choice2, Player) from T2 to T3
if check_winner(Player1, Choice1, Player2, Choice2, Player), Player \= draw, 
send(Player, 2000) from T2 to T3.
 
reward_winner(Player1, Choice1, Player2, Choice2, Player) from T2 to T3
if check_winner(Player1, Choice1, Player2, Choice2, draw), 
send(Player1, 1000) from T2 to T3,
send(Player2, 1000) from T2 to T3.
