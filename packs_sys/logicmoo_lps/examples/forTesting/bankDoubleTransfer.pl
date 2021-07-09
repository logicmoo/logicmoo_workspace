
:- expects_dialect(lps).

maxTime(5).
actions		transfer(From, To, Amount).
% following commented out: all actions are serialised, producing cumulative effects IF using updates
% unserializable transfer(From, To, Amount). 
fluents		balance(Person, Amount).

initially	balance(bob, 0).
/* These are now rejected by LPS: no observations at time 1 if there prospective preconditions, such as the one below
observe		transfer(fariba, bob, 10) 	from 0 to 1.
observe transfer(fariba, bob, 50) from 0 to 1.
*/
observe		transfer(fariba, bob, 10) 	from 1 to 2.
observe transfer(fariba, bob, 50) from 1 to 2.


transfer(F,T,A) updates Old to New in balance(T, Old) if New is Old + A.
transfer(F,T,A) updates Old to New in balance(F, Old) if New is Old - A.

% Prospective precondition: since no events are involved, fluent is checked agains the NEXT state.
% This will prevent repeated balances, when using unserializable above; unnecessary otherwise
false balance(P,X), balance(P,Y), X\==Y.