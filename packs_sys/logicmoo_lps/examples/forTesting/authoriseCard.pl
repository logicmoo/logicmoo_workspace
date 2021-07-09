
:- expects_dialect(lps).

% Bob's April 3, 2020 email

maxTime(12).
fluents  entered_at(Card, Time), authorised(Card, Purchase).
actions enter_card(Card), authorise(Card, Purchase).

authorise(Card, Purchase) initiates authorised(Card, Purchase).
enter_card(Card) from T1 to T2 initiates entered_at(Card, T2).
  
have_card(001).
have_card(002).

% This simulates an external agent.
% if enter_card(002) to T then authorise(002, All) from T+1.
if enter_card(001) to T, T > 8 then authorise(001, All) from T.

checkout(Purchase) from T1 to T3 if 
	have_card(Card),
	enter_card(Card) from T1 to T2,
	authorised(Card, Purchase) at T3, T3 =< T2+3.

if true then checkout(my_new_laptop).

false enter_card(Card1), enter_card(Card2), Card1 \= Card2.
false enter_card(Card1) to T1, entered_at(Card2, T0), T0 < T1, T1 =<T0+3.


/** <examples>
?- go(Timeline,[more_actions]).
?- go(T).
?- go.
*/

