
:- expects_dialect(lps).

% Rock, Paper, Scissors "gambling" on Ethereum
maxRealTime(300). % 5 minutes game lifetime

beats(scissors, paper). 
beats(paper, rock).
beats(rock, scissors).

prolog_events e_transaction(latest,_From,_Input,_Wei,_To). % Generate events from the blockchain

e_transaction(latest,From,Input,Wei,To) initiates played(From,Input,Wei) if 
	lps_my_account(To), Wei>0, not played(From,_,_).

fluents played(_Player,_Choice,_Value), gameOver.

reward(R) at T if 
	balance(V) at T, 
	R is round(V*0.9). % keep 10% :-)

balance(B) at T if
	findall(V,played(_,_,V) at T,L), sum_list(L,B).

num_players(N) at T if 
	findall(P, played(P,_,_) at T, L), length(L,N).

false num_players(N), N>2. 

pay(Player,Prize) from T1 to T3 if   % plan / macro action on the blockchain
    lps_my_account(Us),
	e_sendTransaction(Us,Player,Prize,PaymentTx) from T1 to T2,
	e_existsTransactionReceipt(PaymentTx) at T3. 

if played(P0,Choice0,_) at T1, played(P1,Choice1,_) at T1, P0\==P1, beats(Choice0,Choice1), not gameOver at T1
then initiate gameOver from T1, reward(Prize) at T1, pay(P0,Prize) from T1 to T2.

if played(P0,Choice,_) at T1, played(P1,Choice,_) at T1, P0 @> P1, not gameOver at T1
then initiate gameOver from T1, reward(Prize) at T1, Half is Prize/2, pay(P0,Half) from T1, pay(P1,Half) from T1.

/** <examples>
?- Bet1=0.1, Choice1=rock, Bet2=0.1, Choice2=rock,
% Edit above; bets are in Eth 
serve_ethereum(Address), Address = lpsServer(HOUSE), 
grab_test_accounts(2,0.02,[Player1,Player2]),
EthToWei=1000000000000000000, Wei1 is round(Bet1*EthToWei), Wei2 is round(Bet2*EthToWei), 
e_sendTransactionWithAtom(Player1, HOUSE, Wei1, Choice1, PaymentTx1),  
e_sendTransactionWithAtom(Player2, HOUSE, Wei2, Choice2, PaymentTx2).
*/
