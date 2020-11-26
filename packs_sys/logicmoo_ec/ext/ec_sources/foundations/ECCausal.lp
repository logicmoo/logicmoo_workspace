#domain fluent(Fluent), event(Event), time(Time).

% CC1
started(Fluent,Time) :- holdsAt(Fluent,Time).
started(Fluent,Time) :- {not happens(Event,Time)}0, {not initiates(Event,Fluent,Time)}0.

%CC2
stopped(Fluent,Time) :- not holdsAt(Fluent,Time).
stopped(Fluent,Time) :- {not happens(Event,Time)}0, {not terminates(Event,Fluent,Time)}0.

%CC3
terminated2(Fluent,Time) :- happens(Event,Time), terminates(Event,Fluent,Time).
initiated(Fluent,Time) :- started(Fluent,Time), not terminated2(Fluent,Time).

%CC4
initiated2(Fluent,Time) :- happens(Event,Time), initiates(Event,Fluent,Time).
terminated(Fluent,Time) :- stopped(Fluent,Time), not initiated2(Fluent,Time).

hide.

show holdsAt(F,T).
show happens(E,T).
show happens3(E,T,T2).

