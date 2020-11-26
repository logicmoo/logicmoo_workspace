; Happy2-ea.e

load foundations/Root.e
load foundations/EC.e

sort person
person Fred
event Feed(person)
event Clothe(person)
fluent Happy(person)
fluent Hungry(person)
fluent Cold(person)

[person,time]
(HoldsAt(Happy(person),time) <->
!HoldsAt(Hungry(person),time) &
!HoldsAt(Cold(person),time)).

[person,time]
(Terminates(Feed(person),Hungry(person),time)).

[person,time]
(Terminates(Clothe(person),Cold(person),time)).


;noninertial Happy
[person,time]
(ReleasedAt(Happy(person),time)).

[person](!ReleasedAt(Hungry(person),0)).
[person](!ReleasedAt(Cold(person),0)).


HoldsAt(Hungry(Fred),0).
!HoldsAt(Cold(Fred),0).
Happens(Feed(Fred),1).

completion Happens

range time 0 2
range offset 1 1

; End of file.
