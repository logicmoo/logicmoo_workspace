; Telephone40-ea.e
load foundations/Root.e
load foundations/EC.e

sort agent
sort phone

agent Agent1, Agent2
phone Phone1, Phone2

fluent Ringing(phone,phone)
fluent DialTone(phone)
fluent BusySignal(phone)
fluent Idle(phone)
fluent Connected(phone,phone)
fluent Disconnected(phone)

event PickUp(agent,phone)
event SetDown(agent,phone)
event Dial(agent,phone,phone)

; Sigma

[agent,phone,time]
(HoldsAt(Idle(phone),time) ->
Initiates(PickUp(agent,phone),DialTone(phone),time)).

[agent,phone,time]
(HoldsAt(Idle(phone),time) ->
Terminates(PickUp(agent,phone),Idle(phone),time)).

[agent,phone,time]
(HoldsAt(DialTone(phone),time) ->
Initiates(SetDown(agent,phone),Idle(phone),time)).

[agent,phone,time]
(HoldsAt(DialTone(phone),time) ->
Terminates(SetDown(agent,phone),DialTone(phone),time)).

[agent,phone1,phone2,time]
(HoldsAt(DialTone(phone1),time) &
HoldsAt(Idle(phone2),time) ->
Initiates(Dial(agent,phone1,phone2),Ringing(phone1,phone2),time)).

[agent,phone1,phone2,time]
(HoldsAt(DialTone(phone1),time) &
HoldsAt(Idle(phone2),time) ->
Terminates(Dial(agent,phone1,phone2),DialTone(phone1),time)).

[agent,phone1,phone2,time]
(HoldsAt(DialTone(phone1),time) &
HoldsAt(Idle(phone2),time) ->
Terminates(Dial(agent,phone1,phone2),Idle(phone2),time)).

[agent,phone1,phone2,time]
(HoldsAt(DialTone(phone1),time) &
!HoldsAt(Idle(phone2),time) ->
Initiates(Dial(agent,phone1,phone2),BusySignal(phone1),time)).

[agent,phone1,phone2,time]
(HoldsAt(DialTone(phone1),time) &
!HoldsAt(Idle(phone2),time) ->
Terminates(Dial(agent,phone1,phone2),DialTone(phone1),time)).

[agent,phone,time]
(HoldsAt(BusySignal(phone),time) ->
Initiates(SetDown(agent,phone),Idle(phone),time)).

[agent,phone,time]
(HoldsAt(BusySignal(phone),time) ->
Terminates(SetDown(agent,phone),BusySignal(phone),time)).

[agent,phone1,phone2,time]
(HoldsAt(Ringing(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Idle(phone1),time)).

[agent,phone1,phone2,time]
(HoldsAt(Ringing(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Idle(phone2),time)).

[agent,phone1,phone2,time]
(HoldsAt(Ringing(phone1,phone2),time) ->
Terminates(SetDown(agent,phone1),Ringing(phone1,phone2),time)).

[agent,phone1,phone2,time]
(HoldsAt(Ringing(phone1,phone2),time) ->
Initiates(PickUp(agent,phone2),Connected(phone1,phone2),time)).

[agent,phone1,phone2,time]
(HoldsAt(Ringing(phone1,phone2),time) ->
Terminates(PickUp(agent,phone2),Ringing(phone1,phone2),time)).

[agent,phone1,phone2,time]
(HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Idle(phone1),time)).

[agent,phone1,phone2,time]
(HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Disconnected(phone2),time)).

[agent,phone1,phone2,time]
(HoldsAt(Connected(phone1,phone2),time) ->
Terminates(SetDown(agent,phone1),Connected(phone1,phone2),time)).

[agent,phone1,phone2,time]
(HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone2),Idle(phone2),time)).

[agent,phone1,phone2,time]
(HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone2),Disconnected(phone1),time)).

[agent,phone1,phone2,time]
(HoldsAt(Connected(phone1,phone2),time) ->
Terminates(SetDown(agent,phone2),Connected(phone1,phone2),time)).

[agent,phone,time]
(HoldsAt(Disconnected(phone),time) ->
Initiates(SetDown(agent,phone),Idle(phone),time)).

[agent,phone,time]
(HoldsAt(Disconnected(phone),time) ->
Terminates(SetDown(agent,phone),Disconnected(phone),time)).

; Delta

Happens(PickUp(Agent1,Phone1),0).
Happens(Dial(Agent1,Phone1,Phone2),8).
Happens(PickUp(Agent2,Phone2),32).

; Gamma

[phone] HoldsAt(Idle(phone),0).
[phone] !HoldsAt(DialTone(phone),0).
[phone] !HoldsAt(BusySignal(phone),0).
[phone1,phone2] !HoldsAt(Ringing(phone1,phone2),0).
[phone1,phone2] !HoldsAt(Connected(phone1,phone2),0).
[phone] !HoldsAt(Disconnected(phone),0).


[phone1, phone2]!ReleasedAt(Ringing(phone1, phone2),0).
[phone]!ReleasedAt(DialTone(phone),0).
[phone]!ReleasedAt(BusySignal(phone),0).
[phone]!ReleasedAt(Idle(phone),0).
[phone1, phone2]!ReleasedAt(Connected(phone1,phone2),0).
[phone]!ReleasedAt(Disconnected(phone),0).

completion Happens

range time 0 40
range offset 1 1


; End of file.
