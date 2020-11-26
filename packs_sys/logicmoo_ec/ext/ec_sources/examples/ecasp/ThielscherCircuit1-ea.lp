#domain time(Time).

time(0..1).
offset(1..1).



fluent(light).
fluent(switch1).
fluent(switch2).
fluent(switch3).
fluent(relay).

event(lightOn).
event(close1).
event(open2).
event(closeRelay).



happens(lightOn,Time) :- stopped(light,Time), initiated(switch1,Time), initiated(switch2,Time).

happens(open2,Time) :- started(switch2,Time), initiated(relay,Time).

happens(closeRelay,Time) :- stopped(relay,Time), initiated(switch1,Time), initiated(switch3,Time).

initiates(lightOn,light,Time).

terminates(open2,switch2,Time).

initiates(closeRelay,relay,Time).

initiates(close1,switch1,Time).

:- releasedAt(light,0).

:- releasedAt(switch1,0).

:- releasedAt(switch2,0).

:- releasedAt(switch3,0).

:- releasedAt(relay,0).

:- holdsAt(switch1,0).

holdsAt(switch2,0).

holdsAt(switch3,0).

:- holdsAt(relay,0).

:- holdsAt(light,0).

happens(close1,0).

hide.

show holdsAt(F,T), happens(E,T), happens3(E,T,T2).
