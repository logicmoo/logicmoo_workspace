
:- expects_dialect(lps).

maxTime(5).
fluents on(_,_).
actions place(_,_).
events place(_,_).

row(R):- 	member(R, [1,2,3]).
column(C) :- 	member(C,[1,2,3]).
diagonal([(1,1), (2,2), (3,3)]).
diagonal([(3,1), (2,2), (1,3)]).

% slot((R,C)) :- 	member(R, [1,2,3]), member(C,[1,2,3]).
fluents slot(_).

slot((R,C)) if 	member(R, [1,2,3]), member(C,[1,2,3]).
free_slot(S) at T if 	slot(S), not on(A, S) at T.

observe place(x, (1,2)) from 1 to 2.
place(P, S) initiates on(P, S).

if 	place(x, Slot) from _ to T2, not win(x) at T2 
then 	make_move(o) from T2 to _.

if 	place(o, Slot) from _ to T2, not win(o) at T2 
then 	make_move(x) from T2 to _.

make_move(P) from T to T if not free_slot(_) at T. 
make_move(P) from T1 to T2 
if 	find_slot(P, S) at T1, place(P, S) from T1 to T2.

win(P) at T if all_diagonal(P) at T.
win(P) at T if all_row(P) at T.    
win(P) at T if all_column(P) at T.

all_row(P) at T 
if 	row(R), on(P, (R,1)) at T, on(P, (R,2)) at T, on(P, (R,3)) at T.
all_column(P) at T 
if 	column(C), on(P, (1,C)) at T, on(P, (2,C)) at T, on(P, (3,C)) at T.
all_diagonal(P) at T 
if 	on(P, (1,1)) at T, on(P, (2,2)) at T, on(P, (3,3)) at T.
all_diagonal(P) at T 
if 	on(P, (1,3)) at T, on(P, (2,2)) at T, on(P, (3,1)) at T.


find_slot(P, S) at T 
if 	free_slot(S) at T, extra_consideration(P,S) at T.

extra_consideration(P,S) at T 		if toWin(P,S) at T.
extra_consideration(P,S) at T 		if toDefend(P,S) at T.
extra_consideration(P,S) at _T 	if true.

toWin(P,S) at T 	if 	twoFilled(P, S) at T.
toDefend(P,S) at T 	if twoFilled(Q, S) at T, not P=Q.


twoFilled(P,(R3,C)) at T  
if	on(P, (R1, C)) at T,  on(P, (R2, C)) at T, R1 \=R2.
twoFilled(P,(R,C3)) at T 
if 	on(P, (R, C1)) at T, on(P, (R, C2)) at T, C1 \= C2.
% twoFilled(P,D3) at T if diagonal(Ds), member(D1,Ds), member(D2, Ds), member(D3, Ds),D1 \= D2, on(P, D1) at T, on(P, D2) at T.

