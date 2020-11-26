#domain time(Time), offset(Offset), height(Height;Height1;Height2).

time(0..25).
height(0..15).
offset(1..15).



fluent(filling).
fluent(height(Height)).
fluent(spilling).

event(tapOn).
event(tapOff).
event(overflow).



initiates(tapOn,filling,Time).

terminates(tapOff,filling,Time).

releases(tapOn,height(Height),Time).

initiates(tapOff,height(Height),Time) :- holdsAt(height(Height),Time).

terminates(overflow,filling,Time).

initiates(overflow,height(Height),Time) :- holdsAt(height(Height),Time).

initiates(overflow,spilling,Time).

trajectory(filling,Time,height(Height2),Offset) :- holdsAt(height(Height1),Time), Height2==Height1+Offset.

:- holdsAt(height(Height1),Time), holdsAt(height(Height2),Time), Height1!=Height2.

happens(overflow,Time) :- holdsAt(height(15),Time), holdsAt(filling,Time).

holdsAt(height(0),0).

:- holdsAt(filling,0).

:- holdsAt(spilling,0).

happens(tapOn,5).

:- releasedAt(filling,0).

:- releasedAt(height(Height),0).

:- releasedAt(spilling,0).

hide.

show holdsAt(F,T), happens(E,T), happens3(E,T,T2).
