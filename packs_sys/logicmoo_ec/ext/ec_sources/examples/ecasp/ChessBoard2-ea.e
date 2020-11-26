; ChessBoard2-ea.e

load foundations/Root.e
load foundations/EC.e

event Throw()
fluent ItsBlack()
fluent ItsWhite()
fluent OnBlack()
fluent OnWhite()

[time]
(HoldsAt(ItsWhite(),time) ->
Initiates(Throw(),OnWhite(),time)).

[time]
(HoldsAt(ItsBlack(),time) ->
Initiates(Throw(),OnBlack(),time)).

[time] 
(HoldsAt(ItsWhite(),time) | HoldsAt(ItsBlack(),time)).


;noninertial ItsBlack, ItsWhite
[time]
(ReleasedAt(ItsBlack(),time)).

[time]
(ReleasedAt(ItsWhite(),time)).


!ReleasedAt(OnBlack(),0).
!ReleasedAt(OnWhite(),0).


!HoldsAt(OnWhite(),0).
!HoldsAt(OnBlack(),0).
Happens(Throw(),1).

; prune models irrelevant to example:
HoldsAt(ItsWhite(),0).
HoldsAt(ItsBlack(),0).
HoldsAt(ItsWhite(),2).
HoldsAt(ItsBlack(),2).

completion Happens

range time 0 2
range offset 1 1

; End of file.
