; Supermarket40-ea.e

load foundations/Root.e
load foundations/EC.e

event Push()
event Pull()
fluent Forwards()
fluent Backwards()
fluent Spinning()

[time]
(!Happens(Pull(), time) ->
Initiates(Push(), Forwards(), time)).

[time]
(!Happens(Pull(), time) ->
Terminates(Push(), Backwards(), time)).

[time]
(!Happens(Push(), time) ->
Initiates(Pull(), Backwards(), time)).

[time]
(!Happens(Push(), time) ->
Terminates(Pull(), Forwards(), time)).

[time]
(Happens(Push(), time) ->
Initiates(Pull(), Spinning(), time)).

[time]
(Happens(Push(), time) ->
Terminates(Pull(), Forwards(), time)).

[time]
(Happens(Push(), time) ->
Terminates(Pull(), Backwards(), time)).

[time]
(!Happens(Pull(), time) ->
Terminates(Push(), Spinning(), time)).

[time]
(!Happens(Push(), time) ->
Terminates(Pull(), Spinning(), time)).


!ReleasedAt(Forwards(),0).
!ReleasedAt(Backwards(),0).
!ReleasedAt(Spinning(),0).


!HoldsAt(Forwards(), 0).
!HoldsAt(Backwards(), 0).
!HoldsAt(Spinning(), 0).

Happens(Push(), 15).
Happens(Pull(), 15).
Happens(Pull(), 35).
Happens(Push(), 35).

completion Happens

range time 0 40
range offset 1 1

; End of file.
