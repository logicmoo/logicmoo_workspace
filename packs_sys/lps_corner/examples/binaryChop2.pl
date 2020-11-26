
:- expects_dialect(lps).

fluents left(_Position), right(_Position), searching(_Content).
initially left(0), right(9), searching(60).
actions sample(_Position).
events do_sample/0.

%a(Position,Content)
a(0,10). a(1,12). a(2,20). a(3,25). a(4,30). 
a(5,31). a(6,35). a(7,60). a(8,65). a(9,500).

found(I) at T if left(I) at T, right(I) at T.

do_sample if 
	left(L), right(R), Mid is (R+L) div 2, sample(Mid).

if not found(_) then do_sample.

sample(Pos) initiates left(Mid1) if 
	searching(X), a(Pos,AX), AX<X, Mid1 is Pos+1.
sample(Pos) terminates left(_) if 
	searching(X), a(Pos,AX), AX<X.
sample(Pos) initiates right(Pos) if 
	searching(X), a(Pos,AX), AX>=X.
sample(Pos) terminates right(_) if 
	searching(X), a(Pos,AX), AX>=X.

