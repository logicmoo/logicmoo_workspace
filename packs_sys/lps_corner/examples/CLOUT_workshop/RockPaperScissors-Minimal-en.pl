
:- expects_dialect(lps).

% Formal English specification of the game:
en("
the events are:
   a player inputs a choice and a value.

the actions are:
   a prize is sent to a winner.

the fluents are:
   the reward is an amount, known as reward,
   a player has played a choice, known as played.

the observations are:
  miguel inputs paper and 1000 from 2 to 3,
  fariba inputs rock and 1000 from 2 to 3,
  bob inputs rock and 1000 from 3 to 4.

scissors beats paper.
paper beats rock.
rock beats scissors.

initially: the reward is 0.

When a player inputs a choice and a value then the player has played the choice.

When a player inputs a choice and a value
then the reward that is a number becomes the number plus the value.

When a prize is sent to a player
then the reward that is a number becomes the number minus the prize.

<!-- buggy, generating l_timeless instead of l_int: 
the players are a number, known as num_players, at a time if
  the number at the time is
     the sum of all
        a player has played a value at the time. -->

It must not be true that a player inputs a choice and a value
   and the value is equal or less than 0.

<!-- buggy, missing time variable binding: 
It must not be true that a player inputs a first choice and a value
   and the player has played a second choice. -->

It must not be true that
   a prize is sent to some body from a first time to a second time
   and the reward is a number at the second time
   and the number is less than 0.

It must not be true that
a player inputs a first choice and a value from a first time to a second time
and the players are a number, known as num_players, at the second time
and the number is greater than 2.

If a first player has played a first choice at a first time
and a second player has played a second choice at the first time
and the first player is not the same as the second player
and the first choice beats the second choice
and the reward is a number at the first time
and the number is not the same as 0
then the number is sent to the first player from the first time to a second time.

If a first player has played a choice at a first time
and a second player has played the choice at the first time
and the first player is not the same as the second player
and the reward is a first number at the first time
and the first number is not the same as 0
then a second number is half the first number
and the second number is sent to the first player from the first time to a second time
and the second number is sent to the second player from the first time to a third time.

If a number is sent to some body from a first time to a second time
then lps_terminate from the second time to a third time.

It must not be true that 0 is sent to some body.
    ").

% LPS explicit clauses to complement the above

false inputs(Sender,_Choice,_Value) from T1 to T2, played(Sender,_) at T1. 

num_players(N) at T if findall(P,played(P,_) at T,L), length(L,N).

/** <examples>
?- godc(Timeline).
?- dumplps.
?- dump.
*/