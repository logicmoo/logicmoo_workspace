; BusRide2-ea.e

load foundations/Root.e
load foundations/EC.e

fluent HasTicket()
fluent OnRed()
fluent OnYellow()
event Buy()
event Board()
event BoardRed()
event BoardYellow()

[time]
(Happens(Board(),time) -> 
 Happens(BoardRed(),time) | Happens(BoardYellow(),time)).

[time] 
(Initiates(Buy(),HasTicket(),time)).

[time]
(HoldsAt(HasTicket(),time) -> 
 Initiates(BoardRed(),OnRed(),time)).

[time] 
(HoldsAt(HasTicket(),time) -> 
 Initiates(BoardYellow(),OnYellow(),time)).

[time] 
(HoldsAt(OnRed(),time) -> 
 !HoldsAt(OnYellow(),time)).

[time] 
(HoldsAt(OnYellow(),time) -> 
 !HoldsAt(OnRed(),time)).

[time] 
(HoldsAt(OnRed(),time) -> 
 HoldsAt(HasTicket(),time)).

[time] 
(HoldsAt(OnYellow(),time) -> 
 HoldsAt(HasTicket(),time)).


!ReleasedAt(HasTicket(),0).
!ReleasedAt(OnRed(),0).
!ReleasedAt(OnYellow(),0).

HoldsAt(OnRed(),2).

!HoldsAt(HasTicket(),0).
Happens(Buy(),0).
Happens(Board(),1).
; ABDUCED Happens(BoardRed(), 1).

completion Happens

range time 0 2
range offset 1 1

; End of file.
