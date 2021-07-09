
:- expects_dialect(lps).

en("

the maximum time is 120.

the prolog events are:
    e_transaction a time a first address an input an amount a second address.

the fluents are:
   the game is over, known as gameOver,
   a player has played a choice, known as played.

scissors beats paper.
paper beats rock.
rock beats scissors.

When e_transaction latest a first address an input an amount a second address
and lps_my_account the second address
and the amount is greater than 0
and it is not the case that the first address has played a second choice
then the first address has played the input.

the reward is a number, known as reward, at a time if
    lps_my_account an address
and e_getBalance the address latest a value at the time
and the number is approximately 0.9 times the value.

the players are a number, known as num_players, at a time if
  the number at the time is
     the sum of all
        a player has played a value at the time.

It must not be true that
    the players are a number, known as num_players, at a time
and the number is greater than 2.

a player pays a prize, known as pay, from a first time to a second time if
    lps_my_account an address
and e_sendTransaction the address the player the prize a payment from the first time to a third time
and e_existsTransactionReceipt the payment at the second time.

If a first player has played a first choice at a first time
and a second player has played a second choice at the first time
and the first player is different from the second player
and the first choice beats the second choice
and it is not the case that the game is over at the first time
then initiate the game is over from the first time to a second time
and the reward is a prize, known as reward, at the first time
and the first player pays the prize, called pay, from the first time to a second time.

If a first player has played a first choice at a first time
and a second player has played a second choice at the first time
and the first player is different from the second player
and the first choice is the same as the second choice
and it is not the case that the game is over at the first time
then initiate the game is over from the first time to a second time
and the reward is a prize, known as reward, at the first time
and a number is half the prize
and the first player pays the number, called pay, from the first time to a second time
and the second player pays the number, called pay, from the first time to the second time.

").

/** <examples>
?- serve_ethereum(Address).
*/
