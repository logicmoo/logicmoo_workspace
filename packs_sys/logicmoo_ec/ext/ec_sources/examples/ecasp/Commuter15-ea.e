;Commuter15-ea.e

load foundations/Root.e
load foundations/EC.e

sort place
place Work, Home

sort station: place
station HerneHill, Victoria, SouthKen

fluent At(place)
fluent Train(station, station)
event WalkTo(place)
event TrainTo(station)
event GoToWork()

[place, time]
(Initiates(WalkTo(place), At(place), time)).

[place1, place2, time]
( place1!=place2 & HoldsAt(At(place1),time) ->
  Terminates(WalkTo(place2), At(place1), time) ).

[station1, station2, time]
( HoldsAt(Train(station1, station2), time) &
  HoldsAt(At(station1), time) ->
  Initiates(TrainTo(station2), At(station2), time) ).

[station1, station2, time]
( HoldsAt(Train(station1, station2), time) &
  HoldsAt(At(station1), time) ->
  Terminates(TrainTo(station2), At(station1), time) ).

[time](Initiates(GoToWork(), At(Work), time)).

[place1, time]
(HoldsAt(At(place1),time) & place1!=Work
->
Terminates(GoToWork(), At(place1), time)).


[time1, time2, time3, time4]
( 
  Happens3(WalkTo(HerneHill), time1, time1) &
  Happens3(TrainTo(Victoria), time2, time2) &
  Happens3(TrainTo(SouthKen), time3, time3) &
  Happens3(WalkTo(Work), time4, time4) & 
  time1<time2 & time2<time3 & time3<time4 &
  !Clipped(time1, At(HerneHill), time2) &
  !Clipped(time2, At(Victoria), time3) &
  !Clipped(time3, At(SouthKen), time4)
->
   Happens3(GoToWork(), time1, time4)
).



[time](HoldsAt(Train(HerneHill, Victoria),time)).
[time](HoldsAt(Train(Victoria, SouthKen),time)).


[place](!ReleasedAt(At(place), 0)).

[station1, station2]
(!ReleasedAt(Train(station1, station2), 0)).

[place1, place2, time]
(HoldsAt(At(place1), time) & HoldsAt(At(place2), time)
 -> place1=place2).

[station1, station2, time]
(HoldsAt(Train(station1, station2), time) -> station1!=station2).


[station1, station2, time]
(station1!=HerneHill & station1!=Victoria
-> !HoldsAt(Train(station1,station2),time)).

[station1, station2, time]
(station2!=Victoria & station2!=SouthKen
-> !HoldsAt(Train(station1,station2),time)).

[station1, station2, time]
(HoldsAt(Train(station1, station2),time) & 
 station1=HerneHill
 -> station2!=SouthKen).


HoldsAt(At(Home),0).
Happens3(WalkTo(HerneHill), 1, 1).
Happens3(TrainTo(Victoria), 6, 6).
Happens3(TrainTo(SouthKen), 10, 10).
Happens3(WalkTo(Work), 12, 12).


completion Happens

range time 0 15
range offset 1 1

; End of File
