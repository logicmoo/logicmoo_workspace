:- expects_dialect(lps).

/* This example is based on the example in Bench-Capon, 
Persuasion in practical argument using value-based argumentation
frameworks. Journal of Logic and Computation, 13(3):429–448, 2003.
https://academic.oup.com/logcom/article-pdf/13/3/429/4241286/130429.pdf
*/

fluents has/2, need/2, diabetic/1, paid/3.
events highBloodSugar/1.
actions takeFrom/3, use/2, pay/3.

initially  has(dave, nsulin), has(carla, insulin), diabetic(dave).

% Here 1 and 2 are "cycle times" during which "time stands still". So cycle time is really a state index.
% Events and actions take place between cycles, and fluents hold during cycles.
%
observe highBloodSugar(bob) from 1 to 2.

% Here T2 is after T1. But the interpreter prefers solving goals as soon as possible.
% So in practice this means that T3 is as soon as possible after T2,
% This rule is a goal, which generates actions to make itself true:
%
if 	highBloodSugar(Person) from T1 to T2 
then 	obtain(Person, insulin) from T3 to T4, 
	use(Person, insulin) from T5 to T6, 
	T2 < T3, T4 =< T5.

% The order of the clauses can be used to give different priorities to different ways of satisfying the goal:
% obtain/2 is a macroaction/plan:
%
obtain(Person, Object) from T to T if has(Person, Object) at T.
obtain(Person1, Object) from T1 to T2 if has(Person2, Object) at T1, takeFrom(Person1, Person2, Object) from T1 to T2.
/*
 * obtain(Person1, Object) from T1 if 
	has(Person2, Object) at T1, 
	pay(Person1, Person2, Object) from T2 to T3, T1 < T2,
	takeFrom(Person1, Person2, Object) from T4 to T5, T1 < T4.
*/

% Event calculus state transitions, but with destructive updates.
%
takeFrom(Person1, Person2, Object) updates Person2 to Person1 in has(Person2, Object)
if has(Person2, Object).

pay(Person1, Person2, Object) initiates paid(Person1, Person2, Object).

/* Alternative "hard" goals encode different "values":
 if takeFrom(Person1, Person2, Object) 
 then paid(Person1, Person2, Object).
 
%
% Never take anything from anyone unless you have paid for it.
% The fluent paid is necessary here, because the current implementation does not cater for negations of event predicates.
%
false takeFrom(Person1, Person2, Object), not paid(Person1, Person2, Object).

% Never take anything from anyone who needs that thing:
false takeFrom(Person1, Person2, Object), need(Person2, Object).

need(Person, insulin) if diabetic(Person). 
*/

false takeFrom(Person1, Person2, Object), 
	has(Person1, Object).

false takeFrom(Person1, Person2, Object), 
	takeFrom(Person1, Person3, Object), 
	Person2 \= Person3.



