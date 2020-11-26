#domain time(Time), offset(Offset), dir(Dir;Dir1;Dir2).

time(0..maxstep).
offset(1..3).

dir(left).
dir(right).


fluent(oscillating).
fluent(at(Dir)).

event(hold).
event(swing).


:- holdsAt(at(Dir1),Time), holdsAt(at(Dir2),Time), Dir1!=Dir2.

initiates(swing,oscillating,Time).

releases(swing,at(Dir),Time).

initiates(hold,at(Dir),Time) :- holdsAt(at(Dir),Time).

terminates(hold,oscillating,Time).

trajectory(oscillating,Time,at(right),Offset) :- holdsAt(at(left),Time), Offset mod 2!=0.

trajectory(oscillating,Time,at(left),Offset) :- holdsAt(at(right),Time), Offset mod 2!=0.

trajectory(oscillating,Time,at(left),Offset) :- holdsAt(at(left),Time), Offset mod 2==0.

trajectory(oscillating,Time,at(right),Offset) :- holdsAt(at(right),Time), Offset mod 2==0.

:- holdsAt(oscillating,0).

holdsAt(at(left),0).

happens(swing,0).

happens(hold,3).

:- releasedAt(F,0), fluent(F).

hide.

show holdsAt(F,T), releasedAt(F,T), happens(E,T).
