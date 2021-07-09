/* 
A kid goes into a grocery store and buys four items. The cashier charges $7.11. 
The kid pays and is about to leave when the cashier calls the kid back, and says 
"Hold on, I multiplied the four items instead of adding them; I'll try again... 
Gosh, with adding them the price still comes to $7.11"! What were the prices of 
the four items?
*/
go:-
        cputime(X),
	Vars=[A,B,C,D],
	Vars in 1..711,
	A+B+C+D #= 711,
	A*B #= T1,
        C*D #= T2,
        T1*T2 #= 711*100*100*100,
	labeling(Vars),
        cputime(Y),
	T is Y-X,
	write(Vars), write(' found in '), write(T), write(' milliseconds'),nl.


