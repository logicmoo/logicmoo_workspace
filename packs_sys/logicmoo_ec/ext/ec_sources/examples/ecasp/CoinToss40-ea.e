; CoinToss40-ea.e

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
Happens(Toss(),10).
Happens(Toss(),20).
Happens(Toss(),30).

; prune models irrelevant to example:
HoldsAt(ItsHeads(),0).
HoldsAt(ItsHeads(),40).

completion Happens

range time 0 40
range offset 1 1

; End of file.
