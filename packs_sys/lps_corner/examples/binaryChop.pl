
:- expects_dialect(lps).

maxTime(9).

fluents left/1, right/1, searching/1, middle/1.
actions	initiate/1, terminate/1, update/1, result/2.

location(a, 0).
location(b, 1).
location(c, 2).
location(d, 3).

if true
then find(c) from T1 to T2.

find(Content) from T1 to T2 if initiate(left(0)) from T1 to T2 , initiate(right(4)) from T1 to T2 ,
initiate(searching(Content)) from T1 to T2 .

if searching(Content), left(L), right(R), L < R, middle(M), location(Item, M) , Item @< Content
then NewL is M +1, update(left(NewL)).

if searching(Content), left(L), right(R), L < R, middle(M), location(Item, M), not Item @< Content
then update(right(M)).

if searching(Content), left(L), right(R), R =< L
then terminate(searching(Content)), terminate(left(L)), terminate(right(R)),
result(Content, L).

middle(M) at T if right(R) at T, left(L) at T, M is round((R+L)/2).


update(left(Fluent)) initiates left(Fluent).
update(right(Fluent)) initiates right(Fluent).
initiate(left(Fluent)) initiates left(Fluent).
initiate(right(Fluent)) initiates right(Fluent).
terminate(right(_)) terminates right(_).
terminate(left(_)) terminates left(_).
initiate(searching(Content)) initiates searching(Content).
terminate(searching(Content)) terminates searching(Content).


update(left(_)) terminates left(_).
update(right(_)) terminates right(_).





