
:- expects_dialect(lps).

/* From section 2.2 in http://csp2017.mimuw.edu.pl/data/uploads/papers/CSP2017_paper_24.pdf :
1. Either the Buyer or the Seller begins a transaction. Buyer and Seller agree to the terms of the transaction via Escrow.com.
2. The Buyer deposits the payment to the secure Escrow account.
3. Escrow.com verifies the payment and notifies the Seller that funds have been
secured in Escrow.
4. The Seller ships the goods to the Buyer - Upon payment verification, the
Seller is authorised to send the goods and submit tracking information.
Escrow.com verifies that the Buyer receives the goods.
5. The Buyer has a set number of days to inspect the goods and the option
to accept or reject it. If the Buyer is satisfied, he authorises the Escrow to pay the Seller. 
Escrow.com then pays the Seller. However, if the Buyer is not satisfied, 
in addition to notifying the escrow, he returns the goods to the Seller. 
When the Seller notifies the escrow about the goods being received back, 
the Escrow refunds the deposit to the Buyer with a set number of days.

VERSION BELOW STILL INCOMPLETE, MISSING:
- What happens if the buyer receives the goods, and doesn't accept them or reject them within the designated period?
- Follow up to the return of goods
- Ethereum version
*/
maxTime(10).
events create_escrow(Buyer,Seller,Value), deposit(Buyer,Value), received_goods(Buyer), accept_goods(Buyer), reject_goods(Buyer).
fluents escrow(_Buyer,_Seller,_Value), balance(_Entity,_Value).
actions authorise_shipping, pay(_Seller,_Value).

observe create_escrow(bob,miguel,1600) from 1.
observe deposit(bob,1600) from 2.
observe received_goods(bob) from 5.
% observe accept_goods(bob) from 7.
observe reject_goods(bob) from 7.

create_escrow(Buyer,Seller,Value) initiates escrow(Buyer,Seller,Value) if not escrow(_,_,_).

deposit(B,V) initiates balance(B,V) if escrow(B,_,V).

false deposit(B,_), balance(B,_).
false deposit(B1,_), escrow(B2,_,_), B1 \== B2.
false deposit(B,_), not escrow(B,_,_).

if deposit(_Buyer,_Value) 
then authorise_shipping. % send secure message... or initiate a "unique authorisation token" fluent

if received_goods(Buyer), accept_goods(Buyer) to T, escrow(Buyer,Seller,Value) at T
then pay(Seller,Value) from T.

if received_goods(Buyer), reject_goods(Buyer) to T1, escrow(Buyer,Seller,Value) at T1
then real_date('some days later...') at T2, pay(Buyer,Value) from T2.

pay(_Seller,_Value) terminates balance(_,_) if balance(_,Value).

false pay(_,Value), not balance(_,Value).

real_date(_) at _.

/** <examples> 
?- go(Timeline).
*/
