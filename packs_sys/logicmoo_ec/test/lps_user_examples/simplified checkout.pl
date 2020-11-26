:- expects_dialect(lps).

fluents approved/1.
maxTime(5). 
actions enter/1, approve/1.
approve(X) initiates approved(X).

fluents card/1.
initially card(1), card(2).

% card(1).
% card(2).

% There are two ways of solving this goal, depending on card(X).
% without the constraint, it executes both enter actions simultaneously.
% But even with the simple constraint, it executes enter(2) without waiting for 
% approved(1).
if true then 
	% card(X), 
	card(X) at T, 
	enter(X) from T, approved(X) at T+2.

% This constraint is necessary to avoid executing both enter actions simultaneously.
false enter(Card1), enter(Card2), Card1 \= Card2.

% This simulates an external agent.
if enter(2) to T then approve(2) from T.


/** <examples>
?- gov.
?- go(T, [more_actions]).
?- go(T).
*/
