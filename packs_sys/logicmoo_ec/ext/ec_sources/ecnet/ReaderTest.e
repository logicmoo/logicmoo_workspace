[physobj1,physobj2]
!(physobj1=Pen1 & physobj2=Desk1) &
!(physobj1=Paper1 & physobj2=Desk1) ->
!HoldsAt(On(physobj1, physobj2),0).


[agent,book,page,time]
Happens(BookTurnPageTo(agent,book,page),time) ->
HoldsAt(Awake(agent),time) &
({page1} page1 != page & HoldsAt(BookIsOpenTo(book,page1),time)) &
HoldsAt(Holding(agent,book),time).

[book,time]
HoldsAt(BookClosed(book),time) ->
! {page} HoldsAt(BookIsOpenTo1(book,page),time).


[book,time]
HoldsAt(BookClosed(book),time) ->
{page} ! HoldsAt(BookIsOpenTo2(book,page),time).
