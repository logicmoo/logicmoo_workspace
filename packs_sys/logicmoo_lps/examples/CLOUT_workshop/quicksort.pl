
:- expects_dialect(lps).

% Prolog programs can be called from LPS.

maxTime(3).
events	request(_).
actions	announce(_).

observe  request(sort([2,1,4,3])) from 1 to 2.


if	request(sort(X)) from T1 to T2 
then	quicksort(X, Y), announce(sorted(Y)) from T2 to T3.

quicksort([X|Xs],Ys) :-
  	partition(Xs,X,Left,Right),
  	quicksort(Left,Ls),
  	quicksort(Right,Rs),
  	append(Ls,[X|Rs],Ys). % for XSB use instead basics:append(Ls,[X|Rs],Ys)

quicksort([],[]).


partition([X|Xs],Y,[X|Ls],Rs) :-
  	X =< Y, partition(Xs,Y,Ls,Rs).

partition([X|Xs],Y,Ls,[X|Rs]) :-
  	X > Y, partition(Xs,Y,Ls,Rs).

partition([],Y,[],[]).

/** <examples>
?- go(Timeline).
*/