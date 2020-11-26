#domain time(Time), offset(Offset), height(Height;Height1;Height2).

time(0..maxstep).
height(0..20).
offset(1..20).



fluent(filling).
fluent(hasHeight(Height)).
fluent(spilling).

event(tapOn).
event(tapOff).
event(overflow).


initiates(tapOn,filling,Time).

terminates(tapOff,filling,Time).

releases(tapOn,hasHeight(Height),Time).

initiates(tapOff,hasHeight(Height),Time) :- holdsAt(hasHeight(Height),Time).

terminates(overflow,filling,Time).

initiates(overflow,hasHeight(Height),Time) :- holdsAt(hasHeight(Height),Time).

initiates(overflow,spilling,Time).

trajectory(filling,Time,hasHeight(Height2),Offset) :- holdsAt(hasHeight(Height1),Time), Height2=Height1+Offset.

:- holdsAt(hasHeight(Height1),Time), holdsAt(hasHeight(Height2),Time), Height1!=Height2.

happens(overflow,Time) :- holdsAt(hasHeight(10),Time), holdsAt(filling,Time).

holdsAt(hasHeight(0),0).

:- holdsAt(filling,0).

:- holdsAt(spilling,0).

happens(tapOn,5).

:- releasedAt(F,0), fluent(F).

hide.

show holdsAt(F,T), releasedAt(F,T), happens(E,T).
