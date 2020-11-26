; CoinToss4-ea.e

load foundations/Root.e
load foundations/EC.e

event Toss()
fluent ItsHeads()
fluent Heads()

[time] 
(HoldsAt(ItsHeads(),time) -> Initiates(Toss(),Heads(),time)).

[time] 
(!HoldsAt(ItsHeads(),time) -> Terminates(Toss(),Heads(),time)).


;noninertial ItsHeads
[time]
(ReleasedAt(ItsHeads(),time)).

!ReleasedAt(Heads(),0).


HoldsAt(Heads(),0).
Happens(Toss(),1).
Happens(Toss(),2).
Happens(Toss(),3).

; prune models irrelevant to example:
HoldsAt(ItsHeads(),0).
HoldsAt(ItsHeads(),4).

completion Happens

range time 0 4
range offset 1 1

; End of file.
