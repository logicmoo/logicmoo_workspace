; ThielscherCircuit1-ea.e
option renaming off

load foundations/Root.e
load foundations/EC.e
load foundations/ECCausal.e

event LightOn()
event Close1()
event Open2()
event CloseRelay()

fluent Light()
fluent Switch1()
fluent Switch2()
fluent Switch3()
fluent Relay()

[time]
(Stopped(Light(),time) &
Initiated(Switch1(),time) &
Initiated(Switch2(),time) ->
Happens(LightOn(),time)).

[time]
(Started(Switch2(),time) &
Initiated(Relay(),time) ->
Happens(Open2(),time)).

[time]
(Stopped(Relay(),time) &
Initiated(Switch1(),time) &
Initiated(Switch3(),time) ->
Happens(CloseRelay(),time)).

[time]Initiates(LightOn(),Light(),time).

[time]Terminates(Open2(),Switch2(),time).

[time]Initiates(CloseRelay(),Relay(),time).

[time]Initiates(Close1(),Switch1(),time).


!ReleasedAt(Light(),0).
!ReleasedAt(Switch1(),0).
!ReleasedAt(Switch2(),0).
!ReleasedAt(Switch3(),0).
!ReleasedAt(Relay(),0).


!HoldsAt(Switch1(),0).
HoldsAt(Switch2(),0).
HoldsAt(Switch3(),0).
!HoldsAt(Relay(),0).
!HoldsAt(Light(),0).

Happens(Close1(),0).

completion Happens

range time 0 1
range offset 1 1

; End of file.
