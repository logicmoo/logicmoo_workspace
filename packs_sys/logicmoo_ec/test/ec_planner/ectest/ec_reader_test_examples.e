


; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2004a/Holding.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Mueller:2004a,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "Event calculus reasoning through satisfiability",
;   journal = "Journal of Logic and Computation",
;   volume = "14",
;   number = "5",
;   pages = "703--730",
; }
;

option encoding 3

load foundations/Root.e
load foundations/EC.e

sort person
sort object

event Hold(person,object)
fluent Holding(person,object)

person P1
object O1

Happens(Hold(P1,O1),0).

[person,object,time]
Initiates(Hold(person,object),Holding(person,object),time).

!HoldsAt(Holding(P1,O1),0).
;;; AUTO !ReleasedAt(Holding(P1,O1),0).

completion Happens

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2004a/Leaf.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Mueller:2004a,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "Event calculus reasoning through satisfiability",
;   journal = "Journal of Logic and Computation",
;   volume = "14",
;   number = "5",
;   pages = "703--730",
; }
;

option trajectory on

load foundations/Root.e
load foundations/EC.e

sort object
sort height: integer

fluent Height(object,height)
fluent Falling(object)
event StartFalling(object)
event HitsGround(object)

[object,height1,height2,time]
HoldsAt(Height(object,height1),time) &
HoldsAt(Height(object,height2),time) ->
height1=height2.

[object,time]
Initiates(StartFalling(object),Falling(object),time).

[object,height,time]
Releases(StartFalling(object),Height(object,height),time).

[object,height1,height2,offset,time]
HoldsAt(Height(object,height1),time) &
height2=height1-offset*offset ->
Trajectory(Falling(object),time,Height(object,height2),offset).

[object,time]
HoldsAt(Falling(object),time) &
HoldsAt(Height(object,0),time) ->
Happens(HitsGround(object),time).

;[object,height1,height2,time]
;HoldsAt(Height(object,height1),time) &
;height1 != height2 ->
;Terminates(HitsGround(object),Height(object,height2),time).

[object,height,time]
HoldsAt(Height(object,height),time) ->
Initiates(HitsGround(object),Height(object,height),time).

[object,time]
Terminates(HitsGround(object),Falling(object),time).

object Leaf

!HoldsAt(Falling(Leaf),0).
HoldsAt(Height(Leaf,9),0).
Happens(StartFalling(Leaf),0).

completion Happens

range time 0 4
range offset 1 9
range height 0 9

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Cassimatis2002/PolySpace.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @phdthesis{Cassimatis:2002,
;   author = "Nicholas L. Cassimatis",
;   year = "2002",
;   title = "Polyscheme: A Cognitive Architecture for Integrating Multiple Representation and Inference Schemes",
;   address = "Cambridge, MA",
;   school = "Program in Media Arts and Sciences, School of Architecture and Planning, Massachusetts Institute of Technology",
; }
;

; sorts
sort object
sort xcoord: integer
sort ycoord: integer
sort grid
sort shape
sort color

; constants
shape Round,Square
color Red,Green

; predicates, fluents, and events
predicate Equal(object,object)
predicate Shape(object,shape)
predicate Color(object,color)
fluent Location(grid,object,xcoord,ycoord)
event Move(grid,object,xcoord,ycoord,xcoord,ycoord)

; axioms

[object1,object2] Equal(object1,object2) -> Equal(object2,object1).

; objects have unique shape
[object,shape1,shape2]
Shape(object,shape1) & Shape(object,shape2) ->
shape1=shape2.

; objects have unique color
[object,color1,color2]
Color(object,color1) & Color(object,color2) ->
color1=color2.

; if objects are the same, they have the same shape
[object1,object2]
Equal(object1,object2) ->
({shape} Shape(object1,shape) & Shape(object2,shape)).

; if objects are the same, they have the same color
[object1,object2]
Equal(object1,object2) ->
({color} Color(object1,color) & Color(object2,color)).

; if objects are the same, they have the same location
[grid,object1,object2,xcoord1,ycoord1,xcoord2,ycoord2,time]
Equal(object1,object2) ->
(HoldsAt(Location(grid,object1,xcoord1,ycoord1),time) &
 HoldsAt(Location(grid,object2,xcoord2,ycoord2),time) ->
 xcoord1=xcoord2 & ycoord1=ycoord2).

; object in one location at a time
[grid,object,xcoord1,ycoord1,xcoord2,ycoord2,time]
HoldsAt(Location(grid,object,xcoord1,ycoord1),time) &
HoldsAt(Location(grid,object,xcoord2,ycoord2),time) ->
xcoord1=xcoord2 & ycoord1=ycoord2.

; objects have locations
[grid,object,time]
({xcoord,ycoord} HoldsAt(Location(grid,object,xcoord,ycoord),time)).

; different objects are not at same location
[grid,object1,object2,xcoord1,ycoord1,time]
HoldsAt(Location(grid,object1,xcoord1,ycoord1),time) &
HoldsAt(Location(grid,object2,xcoord1,ycoord1),time) ->
Equal(object1,object2).

; moving to a location causes an object to be at that location
[grid,object,xcoord1,ycoord1,xcoord2,ycoord2,time]
Initiates(Move(grid,object,xcoord1,ycoord1,xcoord2,ycoord2),
          Location(grid,object,xcoord2,ycoord2),
          time).

; moving to a location causes the object no longer to be at its previous
; location
[grid,object,xcoord1,ycoord1,xcoord2,ycoord2,time]
Terminates(Move(grid,object,xcoord1,ycoord1,xcoord2,ycoord2),
           Location(grid,object,xcoord1,ycoord1),
           time).

;; allow diagonal movements
;[grid,object,xcoord1,ycoord1,xcoord2,ycoord2,time]
;Happens(Move(grid,object,xcoord1,ycoord1,xcoord2,ycoord2),time) ->
;HoldsAt(Location(grid,object,xcoord1,ycoord1),time) &
;(xcoord1=xcoord2 |
; xcoord1=xcoord2+1 |
; xcoord1=xcoord2-1) &
;(ycoord1=ycoord2 |
; ycoord1=ycoord2+1 |
; ycoord1=ycoord2-1).

; only allow right angle movements
[grid,object,xcoord1,ycoord1,xcoord2,ycoord2,time]
Happens(Move(grid,object,xcoord1,ycoord1,xcoord2,ycoord2),time) ->
HoldsAt(Location(grid,object,xcoord1,ycoord1),time) &
((xcoord1=xcoord2 & (ycoord1=ycoord2+1 | ycoord1=ycoord2-1)) |
 (ycoord1=ycoord2 & (xcoord1=xcoord2+1 | xcoord1=xcoord2-1))).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Cassimatis2002/TwoScreens.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @phdthesis{Cassimatis:2002,
;   author = "Nicholas L. Cassimatis",
;   year = "2002",
;   title = "Polyscheme: A Cognitive Architecture for Integrating Multiple Representation and Inference Schemes",
;   address = "Cambridge, MA",
;   school = "Program in Media Arts and Sciences, School of Architecture and Planning, Massachusetts Institute of Technology",
; }
;

load foundations/Root.e
load foundations/EC.e
load examples/Cassimatis2002/PolySpace.e

grid G1
object X,Y,Screen1,Screen2

; perceptions:
Shape(X,Round).
Color(X,Red).
Shape(Y,Round).
Color(Y,Red).
Shape(Screen1,Square).
Color(Screen1,Green).
Shape(Screen2,Square).
Color(Screen2,Green).
[time] HoldsAt(Location(G1,Screen1,2,0),time).
[time] HoldsAt(Location(G1,Screen2,4,0),time).
HoldsAt(Location(G1,X,1,1),0).
HoldsAt(Location(G1,Y,5,1),4).

[xcoord,ycoord,time]
xcoord!=2 & xcoord!=4 & !(xcoord=1 & ycoord=1 & time=0) ->
!HoldsAt(Location(G1,X,xcoord,ycoord),time) |
xcoord=5 & ycoord=1 & time=4 & Equal(X,Y).

[xcoord,ycoord,time]
xcoord!=2 & xcoord!=4 & !(xcoord=5 & ycoord=1 & time=4) ->
!HoldsAt(Location(G1,Y,xcoord,ycoord),time) |
xcoord=1 & ycoord=1 & time=0 & Equal(X,Y).

range time 0 4
range xcoord 0 5
range ycoord 0 1
range offset 0 0

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Cassimatis2002/OneScreen.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @phdthesis{Cassimatis:2002,
;   author = "Nicholas L. Cassimatis",
;   year = "2002",
;   title = "Polyscheme: A Cognitive Architecture for Integrating Multiple Representation and Inference Schemes",
;   address = "Cambridge, MA",
;   school = "Program in Media Arts and Sciences, School of Architecture and Planning, Massachusetts Institute of Technology",
; }
;

load foundations/Root.e
load foundations/EC.e
load examples/Cassimatis2002/PolySpace.e

grid G1
object X,Y,Screen

; perceptions:
Shape(X,Round).
Color(X,Red).
Shape(Y,Round).
Color(Y,Red).
Shape(Screen,Square).
Color(Screen,Green).
[time] HoldsAt(Location(G1,Screen,2,0),time).
HoldsAt(Location(G1,X,1,1),0).
HoldsAt(Location(G1,Y,3,1),2).

[xcoord,ycoord,time]
xcoord!=2 & !(xcoord=1 & ycoord=1 & time=0) ->
!HoldsAt(Location(G1,X,xcoord,ycoord),time) |
xcoord=3 & ycoord=1 & time=2 & Equal(X,Y).

[xcoord,ycoord,time]
xcoord!=2 & !(xcoord=3 & ycoord=1 & time=2) ->
!HoldsAt(Location(G1,Y,xcoord,ycoord),time) |
xcoord=1 & ycoord=1 & time=0 & Equal(X,Y).

range time 0 2
range xcoord 0 4
range ycoord 0 2
range offset 0 0

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/BrewkaDixKonolige1997/Wine.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; reasoning by cases
; \fullciteA[p. 45]{BrewkaDixKonolige:1997}
;
; @book{BrewkaDixKonolige:1997,
;   author = "Gerhard Brewka and J{\"{u}}rgen Dix and Kurt Konolige",
;   year = "1997",
;   title = "Nonmonotonic Reasoning: An Overview",
;   address = "Stanford, CA",
;   publisher = "CSLI",
; }
;

load foundations/Root.e
load foundations/EC.e

sort x
x Person

predicate LikesWine(x)
predicate Italian(x)
predicate French(x)
predicate Ab1(x)
predicate Ab2(x)

[x] Italian(x) & !Ab1(x) -> LikesWine(x).
[x] French(x) & !Ab2(x) -> LikesWine(x).
[x] Italian(x) -> !French(x).

Italian(Person) | French(Person).

range time 0 0
range offset 1 1

completion Theta Ab1
completion Theta Ab2

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Shanahan1997/Yale.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{HanksMcDermott:1987,
;   author = "Steve Hanks and Drew V. McDermott",
;   year = "1987",
;   title = "Nonmonotonic logic and temporal projection",
;   journal = "Artificial Intelligence",
;   volume = "33",
;   number = "3",
;   pages = "379--412",
; }
;
; \fullciteA[pp. 322--323]{Shanahan:1997}
;
; @book{Shanahan:1997,
;   author = "Murray Shanahan",
;   year = "1997",
;   title = "Solving the Frame Problem",
;   address = "Cambridge, MA",
;   publisher = "MIT Press",
; }
;
; deduction
;
; modifications from Shanahan's formulation:
; InitiallyP -> HoldsAt
; timestamps
; added [time] Terminates(Shoot(),Loaded(),time).
;

option showpred off

load foundations/Root.e
load foundations/EC.e

event Load()
event Shoot()
event Sneeze()
fluent Loaded()
fluent Alive()

[time] Initiates(Load(),Loaded(),time).
[time] HoldsAt(Loaded(),time) -> Terminates(Shoot(),Alive(),time).
[time] Terminates(Shoot(),Loaded(),time).

HoldsAt(Alive(),0).
!HoldsAt(Loaded(),0).
Happens(Load(),0).
Happens(Sneeze(),1).
Happens(Shoot(),2).

completion Happens

range time 0 3
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Shanahan1997/StuffyRoom.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{GinsbergSmith:1988a,
;   author = "Matthew L. Ginsberg and David E. Smith",
;   year = "1988",
;   title = "Reasoning about action \uppercase{I}: \uppercase{A} possible worlds approach",
;   journal = "Artificial Intelligence",
;   volume = "35",
;   number = "2",
;   pages = "165--195",
; }
;
; \fullciteA[pp. 288--289]{Shanahan:1997}
;
; @book{Shanahan:1997,
;   author = "Murray Shanahan",
;   year = "1997",
;   title = "Solving the Frame Problem",
;   address = "Cambridge, MA",
;   publisher = "MIT Press",
; }
;
; deduction
;
; modifications from Shanahan's formulation:
; timestamps
; added:
; !HoldsAt(Blocked1(),0).
; !HoldsAt(Blocked2(),0).
;

load foundations/Root.e
load foundations/EC.e

event Close1()
event Close2()
event Start()
fluent Blocked1()
fluent Blocked2()
fluent Stuffy()
noninertial Stuffy

[time] Initiates(Close1(),Blocked1(),time).
[time] Initiates(Close2(),Blocked2(),time).

[time]
HoldsAt(Stuffy(),time) <->
HoldsAt(Blocked1(),time)&HoldsAt(Blocked2(),time).

[time] Initiates(Start(),Blocked1(),time).
[time] Terminates(Start(),Blocked2(),time).

!HoldsAt(Blocked1(),0).
!HoldsAt(Blocked2(),0).
Happens(Start(),0).
Happens(Close2(),1).

completion Happens

range time 0 2
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Shanahan1997/BusRide.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Kartha:1994,
;   author = "G. Neelakantan Kartha",
;   year = "1994",
;   title = "Two counterexamples related to \uppercase{B}aker's approach to the frame problem",
;   journal = "Artificial Intelligence",
;   volume = "69",
;   number = "1--2",
;   pages = "379--391",
; }
;
; \fullciteA[pp. 359--361]{Shanahan:1997}
;
; @book{Shanahan:1997,
;   author = "Murray Shanahan",
;   year = "1997",
;   title = "Solving the Frame Problem",
;   address = "Cambridge, MA",
;   publisher = "MIT Press",
; }
;
; modifications from Shanahan's formulation:
; InitiallyN -> !HoldsAt
; timestamps
;

load foundations/Root.e
load foundations/EC.e

fluent HasTicket()
fluent OnRed()
fluent OnYellow()
event Buy()
event Board()
event BoardRed()
event BoardYellow()

[time] Happens(Board(),time) -> Happens(BoardRed(),time) | Happens(BoardYellow(),time).

[time] Initiates(Buy(),HasTicket(),time).
[time] HoldsAt(HasTicket(),time) -> Initiates(BoardRed(),OnRed(),time).
[time] HoldsAt(HasTicket(),time) -> Initiates(BoardYellow(),OnYellow(),time).

[time] !(HoldsAt(OnRed(),time) & HoldsAt(OnYellow(),time)).
[time] HoldsAt(OnRed(),time) -> HoldsAt(HasTicket(),time).
[time] HoldsAt(OnYellow(),time) -> HoldsAt(HasTicket(),time).

HoldsAt(OnRed(),2).

!HoldsAt(HasTicket(),0).
Happens(Buy(),0).
Happens(Board(),1).
; ABDUCED Happens(BoardRed(), 1).

completion Happens

range time 0 2
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Shanahan1997/DeadOrAlive.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; \fullciteA[p. 324]{Shanahan:1997}
;
; @book{Shanahan:1997,
;   author = "Murray Shanahan",
;   year = "1997",
;   title = "Solving the Frame Problem",
;   address = "Cambridge, MA",
;   publisher = "MIT Press",
; }
;
; deduction
;
; modifications from Shanahan's formulation:
; InitiallyP -> HoldsAt
; timestamps
; added [time] Terminates(Shoot(),Loaded(),time).
;

load foundations/Root.e
load foundations/EC.e

event Load()
event Shoot()
event Sneeze()
fluent Loaded()
fluent Alive()
fluent Dead()
noninertial Dead

[time] Initiates(Load(),Loaded(),time).
[time] HoldsAt(Loaded(),time) -> Terminates(Shoot(),Alive(),time).
[time] Terminates(Shoot(),Loaded(),time).
[time] HoldsAt(Dead(),time) <-> !HoldsAt(Alive(),time).

HoldsAt(Alive(),0).
!HoldsAt(Loaded(),0).
Happens(Load(),0).
Happens(Sneeze(),1).
Happens(Shoot(),2).

completion Happens

range time 0 3
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Shanahan1997/Supermarket.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; \fullciteA[pp. 302--304]{Shanahan:1997}
;
; @book{Shanahan:1997,
;   author = "Murray Shanahan",
;   year = "1997",
;   title = "Solving the Frame Problem",
;   address = "Cambridge, MA",
;   publisher = "MIT Press",
; }
;
; deduction
;
; modifications from Shanahan's formulation:
; reformulated using the method of \fullciteA[pp. 460--461]{MillerShanahan:2002}
;
; @incollection{MillerShanahan:2002,
;   author = "Rob Miller and Murray Shanahan",
;   year = "2002",
;   title = "Some alternative formulations of the event calculus",
;   editor = "Antonis C. Kakas and Fariba Sadri",
;   booktitle = "Computational Logic: Logic Programming and Beyond: Essays in Honour of \uppercase{R}obert \uppercase{A}. \uppercase{K}owalski, Part \uppercase{II}",
;   series = "Lecture Notes in Computer Science",
;   volume = "2408",
;   pages = "452--490",
;   address = "Berlin",
;   publisher = "Springer",
; }
;
; added:
; !HoldsAt(Forwards(), 0).
; !HoldsAt(Backwards(), 0).
; !HoldsAt(Spinning(), 0).
;

load foundations/Root.e
load foundations/EC.e

event Push()
event Pull()
fluent Forwards()
fluent Backwards()
fluent Spinning()

[time]
!Happens(Pull(), time) ->
Initiates(Push(), Forwards(), time).

[time]
!Happens(Pull(), time) ->
Terminates(Push(), Backwards(), time).

[time]
!Happens(Push(), time) ->
Initiates(Pull(), Backwards(), time).

[time]
!Happens(Push(), time) ->
Terminates(Pull(), Forwards(), time).

[time]
Happens(Push(), time) ->
Initiates(Pull(), Spinning(), time).

[time]
Happens(Push(), time) ->
Terminates(Pull(), Forwards(), time).

[time]
Happens(Push(), time) ->
Terminates(Pull(), Backwards(), time).

[time]
!Happens(Pull(), time) ->
Terminates(Push(), Spinning(), time).

[time]
!Happens(Push(), time) ->
Terminates(Pull(), Spinning(), time).

!HoldsAt(Forwards(), 0).
!HoldsAt(Backwards(), 0).
!HoldsAt(Spinning(), 0).

Happens(Push(), 5).
Happens(Pull(), 5).
Happens(Pull(), 10).
Happens(Push(), 10).

completion Happens

range time 0 12
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Shanahan1997/StolenCar.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @inproceedings{Kautz:1986,
;   author = "Henry A. Kautz",
;   year = "1986",
;   title = "The Logic of Persistence",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{F}ifth \uppercase{N}ational \uppercase{C}onference on \uppercase{A}rtificial \uppercase{I}ntelligence",
;   pages = "401--405",
;   address = "Los Altos, CA",
;   publisher = "Morgan Kaufmann",
; }
;
; \fullciteA[p. 359]{Shanahan:1997}
;
; @book{Shanahan:1997,
;   author = "Murray Shanahan",
;   year = "1997",
;   title = "Solving the Frame Problem",
;   address = "Cambridge, MA",
;   publisher = "MIT Press",
; }
;
; abduction
;
; modifications from Shanahan's formulation:
; timestamps
; added !HoldsAt(CarParked(),0).
;

load foundations/Root.e
load foundations/EC.e

event Park()
event Steal()
fluent CarParked()

[time] Initiates(Park(),CarParked(),time).
[time] Terminates(Steal(),CarParked(),time).

!HoldsAt(CarParked(),0).
Happens(Park(),0).
; ABDUCED Happens(Steal(), 1).
!HoldsAt(CarParked(),2).

range time 0 2
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/MillerShanahan2002/Bowl.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; \fullciteA[p. 461]{MillerShanahan:2002}
;
; @incollection{MillerShanahan:2002,
;   author = "Rob Miller and Murray Shanahan",
;   year = "2002",
;   title = "Some alternative formulations of the event calculus",
;   editor = "Antonis C. Kakas and Fariba Sadri",
;   booktitle = "Computational Logic: Logic Programming and Beyond: Essays in Honour of \uppercase{R}obert \uppercase{A}. \uppercase{K}owalski, Part \uppercase{II}",
;   series = "Lecture Notes in Computer Science",
;   volume = "2408",
;   pages = "452--490",
;   address = "Berlin",
;   publisher = "Springer",
; }
;

load foundations/Root.e
load foundations/EC.e

event LiftLeft()
event LiftRight()
fluent Spilt()
fluent Raised()

[time]
!Happens(LiftRight(), time) ->
Initiates(LiftLeft(), Spilt(), time).

[time]
!Happens(LiftLeft(), time) ->
Initiates(LiftRight(), Spilt(), time).

[time]
Happens(LiftLeft(), time) ->
Initiates(LiftRight(), Raised(), time).

!HoldsAt(Spilt(), 0).
!HoldsAt(Raised(), 0).
Happens(LiftLeft(), 2).
Happens(LiftRight(), 2).

completion Happens

range time 0 3
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/ReiterCriscuolo1981/NixonDiamond1.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; conflicting defaults: showing that inconsistency results
; without a cancellation rule
; \fullciteA[p. 274]{ReiterCriscuolo:1981}
; \fullciteA[pp. 98--99]{McCarthy:1986}
;
; @inproceedings{ReiterCriscuolo:1981,
;   author = "Raymond Reiter and Giovanni Criscuolo",
;   year = "1981",
;   title = "On interacting defaults",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventh \uppercase{I}nternational \uppercase{J}oint \uppercase{C}onference on \uppercase{A}rtificial \uppercase{I}ntelligence",
;   volume = "1",
;   pages = "270--276",
;   address = "Los Altos, CA",
;   publisher = "William Kaufmann",
; }
;
; @article{McCarthy:1986,
;   author = "John McCarthy",
;   year = "1986",
;   title = "Applications of circumscription to formalizing common-sense knowledge",
;   journal = "Artificial Intelligence",
;   volume = "28",
;   pages = "89--116".
; }
;

load foundations/Root.e
load foundations/EC.e

sort x

predicate Republican(x)
predicate Quaker(x)
predicate Pacifist(x)
predicate Ab1(x)
predicate Ab2(x)

x John

Republican(John).
Quaker(John).

[x] Republican(x) & !Ab1(x) -> !Pacifist(x).
[x] Quaker(x) & !Ab2(x) -> Pacifist(x).

range time 0 0
range offset 1 1

completion Theta Ab1
completion Theta Ab2

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/ReiterCriscuolo1981/NixonDiamond2.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; conflicting defaults: method (D)
; \fullciteA[p. 274]{ReiterCriscuolo:1981}
; \fullciteA[pp. 98--99]{McCarthy:1986}
; \fullciteA[p. 18]{BrewkaDixKonolige:1997}
;
; @inproceedings{ReiterCriscuolo:1981,
;   author = "Raymond Reiter and Giovanni Criscuolo",
;   year = "1981",
;   title = "On interacting defaults",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventh \uppercase{I}nternational \uppercase{J}oint \uppercase{C}onference on \uppercase{A}rtificial \uppercase{I}ntelligence",
;   volume = "1",
;   pages = "270--276",
;   address = "Los Altos, CA",
;   publisher = "William Kaufmann",
; }
;
; @article{McCarthy:1986,
;   author = "John McCarthy",
;   year = "1986",
;   title = "Applications of circumscription to formalizing common-sense knowledge",
;   journal = "Artificial Intelligence",
;   volume = "28",
;   pages = "89--116".
; }
;
; @book{BrewkaDixKonolige:1997,
;   author = "Gerhard Brewka and J{\"{u}}rgen Dix and Kurt Konolige",
;   year = "1997",
;   title = "Nonmonotonic Reasoning: An Overview",
;   address = "Stanford, CA",
;   publisher = "CSLI",
; }
;

load foundations/Root.e
load foundations/EC.e

sort x

predicate Republican(x)
predicate Quaker(x)
predicate Pacifist(x)
predicate Ab1(x)
predicate Ab2(x)

x John

Republican(John).
Quaker(John).

[x] Republican(x) & !Ab1(x) -> !Pacifist(x).
[x] Quaker(x) & !Ab2(x) -> Pacifist(x).
Theta: [x] Republican(x) -> Ab2(x).

range time 0 0
range offset 1 1

completion Theta Ab1
completion Theta Ab2

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter2/Sleep2.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent

agent Nathan

fluent Awake(agent)

event WakeUp(agent)
event FallAsleep(agent)

; Sigma

[agent,time] Initiates(WakeUp(agent),Awake(agent),time).
[agent,time] Terminates(FallAsleep(agent),Awake(agent),time).

; Gamma

!HoldsAt(Awake(Nathan),0).
HoldsAt(Awake(Nathan),1).

; abduced:
; Happens(WakeUp(Nathan),0).

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter2/Sleep1.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent

agent Nathan

fluent Awake(agent)

event WakeUp(agent)
event FallAsleep(agent)

; Sigma

[agent,time] Initiates(WakeUp(agent),Awake(agent),time).
[agent,time] Terminates(FallAsleep(agent),Awake(agent),time).

; Delta

Happens(WakeUp(Nathan),1).

; Gamma

!HoldsAt(Awake(Nathan),0).

; entailed:
; HoldsAt(Awake(Nathan),3).

completion Happens

range time 0 3
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter2/Sleep3.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent

agent Nathan

fluent Awake(agent)

event WakeUp(agent)
event FallAsleep(agent)

; Sigma

[agent,time] Initiates(WakeUp(agent),Awake(agent),time).
[agent,time] Terminates(FallAsleep(agent),Awake(agent),time).

; Delta

[agent,time]
Happens(WakeUp(agent),time) ->
!HoldsAt(Awake(agent),time).

Happens(WakeUp(Nathan),0).

; Gamma

HoldsAt(Awake(Nathan),1).

; inferred:
; !HoldsAt(Awake(Nathan),0).

completion Happens

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter2/Inconsistency3.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object
object O1

fluent F(object)

event E(object)

[object,time] Releases(E(object),F(object),time).
[object,time] Terminates(E(object),F(object),time).

Happens(E(O1),0).

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter2/Sleep4.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent

agent Nathan

fluent Awake(agent)

event WakeUp(agent)
event FallAsleep(agent)

; Sigma

[agent,time] Initiates(WakeUp(agent),Awake(agent),time).
[agent,time] Terminates(FallAsleep(agent),Awake(agent),time).

; Delta

Happens(WakeUp(Nathan),1).

; entailed:
; HoldsAt(Awake(Nathan),3).

completion Happens

range time 0 3
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter2/Inconsistency4.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object
object O1

event E(object)

fluent F1(object)
fluent F2(object)

[object,time]
Initiates(E(object),F1(object),time).

[object,time]
HoldsAt(F1(object),time) <-> HoldsAt(F2(object),time).

!HoldsAt(F2(O1),0).
Happens(E(O1),0).

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter2/Inconsistency1.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object
object O1

fluent F(object)

event E(object)

[object,time] Initiates(E(object),F(object),time).
[object,time] Terminates(E(object),F(object),time).

Happens(E(O1),0).

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter2/Inconsistency2.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object
object O1

fluent F(object)

event E(object)

[object,time] Releases(E(object),F(object),time).
[object,time] Initiates(E(object),F(object),time).

Happens(E(O1),0).

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter8/CameraWithFlash.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort camera

camera Camera1

fluent ProperlyExposedPicture(camera)
fluent ImproperlyExposedPicture(camera)

event ReleaseShutter(camera)
event TriggerFlash(camera)

; Sigma

[camera,time]
Happens(TriggerFlash(camera),time) ->
Initiates(ReleaseShutter(camera),ProperlyExposedPicture(camera),time).

[camera,time]
!Happens(TriggerFlash(camera),time) ->
Initiates(ReleaseShutter(camera),ImproperlyExposedPicture(camera),time).

; Delta

Delta: Happens(ReleaseShutter(Camera1),0).
Delta: Happens(TriggerFlash(Camera1),1).
Delta: Happens(ReleaseShutter(Camera1),1).

; added:
[camera] !HoldsAt(ImproperlyExposedPicture(camera),0).
[camera] !HoldsAt(ProperlyExposedPicture(camera),0).

completion Delta Happens

range time 0 2
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter8/MovingRobot.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @inproceedings{Shanahan:1996,
;   author = "Murray Shanahan",
;   year = "1996",
;   title = "Robotics and the common sense informatic situation",
;   editor = "Wolfgang Wahlster",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{T}welfth \uppercase{E}uropean \uppercase{C}onference on \uppercase{A}rtificial \uppercase{I}ntelligence",
;   pages = "684--688",
;   address = "Chichester, UK",
;   publisher = "John Wiley",
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option renaming off

load foundations/Root.e
load foundations/EC.e

sort coord: integer

sort direction: integer
; 0 -> 0, 1 -> 90, 2 -> 180, 3 -> 370

sort robot

robot Robot1

function Sin(direction): coord
function Cos(direction): coord

Sin(0)=0.
Sin(1)=1.
Sin(2)=2.
Sin(3)=3.

Cos(0)=1.
Cos(1)=2.
Cos(2)=3.
Cos(3)=4.

fluent Direction(robot,direction)
fluent Location(robot,coord,coord)

event MoveLeftWheel(robot)
event MoveRightWheel(robot)

; Sigma

[robot,direction1,direction2,time]
!Happens(MoveRightWheel(robot),time) &
HoldsAt(Direction(robot,direction1),time) &
direction2 = (direction1-1)->
Initiates(MoveLeftWheel(robot),Direction(robot,direction2),time).

[robot,direction,time]
!Happens(MoveRightWheel(robot),time) &
HoldsAt(Direction(robot,direction),time) ->
Terminates(MoveLeftWheel(robot),Direction(robot,direction),time).

[robot,direction1,direction2,time]
!Happens(MoveLeftWheel(robot),time) &
HoldsAt(Direction(robot,direction1),time) &
direction2 = (direction1+1)->
Initiates(MoveRightWheel(robot),Direction(robot,direction2),time).

[robot,direction,time]
!Happens(MoveLeftWheel(robot),time) &
HoldsAt(Direction(robot,direction),time) ->
Terminates(MoveRightWheel(robot),Direction(robot,direction),time).

[robot,direction,coord1,coord2,coord3,coord4,time]
Happens(MoveLeftWheel(robot),time) &
HoldsAt(Location(robot,coord1,coord2),time) &
HoldsAt(Direction(robot,direction),time) &
coord3 = coord1+Cos(direction) &
coord4 = coord2+Sin(direction) ->
Initiates(MoveRightWheel(robot),
          Location(robot,coord3,coord4),
          time).

[robot,coord1,coord2,time]
Happens(MoveLeftWheel(robot),time) &
HoldsAt(Location(robot,coord1,coord2),time) ->
; FIX: Direction not needed!!
; HoldsAt(Direction(robot,direction),time) ->
Terminates(MoveRightWheel(robot),Location(robot,coord1,coord2),time).

; Delta

Happens(MoveRightWheel(Robot1),0).
Happens(MoveLeftWheel(Robot1),1).
Happens(MoveRightWheel(Robot1),1).

; Psi


[robot,coord1,coord2,coord3,coord4,time]
HoldsAt(Location(robot,coord1,coord2),time) &
HoldsAt(Location(robot,coord3,coord4),time) ->
coord1=coord3 &
coord2=coord4.

[robot,direction1,direction2,time]
HoldsAt(Direction(robot,direction1),time) &
HoldsAt(Direction(robot,direction2),time) ->
direction1=direction2.

; Gamma

HoldsAt(Location(Robot1,0,0),0).
HoldsAt(Direction(Robot1,0),0).

completion Happens

range time 0 3
range coord 0 3
range direction 0 3
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter8/PatHeadRubStomach.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent

event PatHead(agent)
event RubStomach(agent)

agent Nathan

; Delta

[agent,time]
Happens(PatHead(agent),time) ->
!Happens(RubStomach(agent),time).

Happens(PatHead(Nathan),0) & Happens(RubStomach(Nathan),0).

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter10/MovingNewspaperAndBox.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object
sort agent: object
sort physobj: object
sort room: object

fluent IN(object,object)
fluent INROOM(object,room)
noninertial INROOM

event MOVE(agent,object,object,object)

agent Lisa
physobj Box, Newspaper
room Kitchen, LivingRoom

; Sigma

; RS10
[agent,physobj1,physobj2,room,time]
HoldsAt(IN(agent,room),time) &
HoldsAt(IN(physobj1,room),time) &
HoldsAt(INROOM(physobj2,room),time) ->
Initiates(MOVE(agent,physobj1,room,physobj2),IN(physobj1,physobj2),time).

; RS11
[agent,physobj1,physobj2,room,time]
HoldsAt(IN(agent,room),time) &
HoldsAt(IN(physobj1,room),time) &
HoldsAt(INROOM(physobj2,room),time) ->
Terminates(MOVE(agent,physobj1,room,physobj2),IN(physobj1,room),time).

; RS12
[agent,physobj1,physobj2,room,time]
HoldsAt(IN(agent,room),time) ->
Initiates(MOVE(agent,physobj1,physobj2,room),IN(physobj1,room),time).

; RS13
[agent,physobj1,physobj2,room,time]
HoldsAt(IN(agent,room),time) ->
Terminates(MOVE(agent,physobj1,physobj2,room),IN(physobj1,physobj2),time).

; RS14
[agent,room1,room2,time]
HoldsAt(IN(agent,room1),time) ->
Initiates(MOVE(agent,agent,room1,room2),IN(agent,room2),time).

; RS15
[agent,room1,room2,time]
HoldsAt(IN(agent,room1),time) ->
Terminates(MOVE(agent,agent,room1,room2),IN(agent,room1),time).

; RS16
[agent,physobj,room,time]
HoldsAt(IN(agent,room),time) &
HoldsAt(IN(physobj,room),time) ->
Initiates(MOVE(agent,physobj,room,agent),IN(physobj,agent),time).

; RS17
[agent,physobj,room,time]
HoldsAt(IN(agent,room),time) &
HoldsAt(IN(physobj,room),time) ->
Terminates(MOVE(agent,physobj,room,agent),IN(physobj,room),time).

; RS18
[agent,physobj,room,time]
HoldsAt(IN(physobj,agent),time) &
HoldsAt(IN(agent,room),time) ->
Initiates(MOVE(agent,physobj,agent,room),IN(physobj,room),time).

; RS19
[agent,physobj,room,time]
HoldsAt(IN(physobj,agent),time) &
HoldsAt(IN(agent,room),time) ->
Terminates(MOVE(agent,physobj,agent,room),IN(physobj,agent),time).

; Delta

Happens(MOVE(Lisa,Newspaper,LivingRoom,Box),0).
Happens(MOVE(Lisa,Box,LivingRoom,Lisa),1).
Happens(MOVE(Lisa,Lisa,LivingRoom,Kitchen),2).
Happens(MOVE(Lisa,Box,Lisa,Kitchen),3).
Happens(MOVE(Lisa,Lisa,Kitchen,LivingRoom),4).

; Psi

; RS1
[object,time] !HoldsAt(IN(object,object),time).

; RS2
[object1,object2,time]
HoldsAt(IN(object1,object2),time) ->
!HoldsAt(IN(object2,object1),time).

; RS3
[object1,object2,object3,time]
HoldsAt(IN(object1,object2),time) &
HoldsAt(IN(object2,object3),time) ->
!HoldsAt(IN(object1,object3),time).

; RS4
[object,object1,object2,time]
HoldsAt(IN(object,object1),time) &
HoldsAt(IN(object,object2),time) ->
object1=object2.

; RS7
[object,room,time]
HoldsAt(IN(object,room),time) ->
HoldsAt(INROOM(object,room),time).

; RS8
[object1,object2,room,time]
HoldsAt(IN(object1,object2),time) &
HoldsAt(INROOM(object2,room),time) ->
HoldsAt(INROOM(object1,room),time).

; RS9
[object,room1,room2,time]
HoldsAt(INROOM(object,room1),time) &
HoldsAt(INROOM(object,room2),time) ->
room1=room2.

; Gamma

HoldsAt(IN(Lisa,LivingRoom),0).
HoldsAt(IN(Newspaper,LivingRoom),0).
HoldsAt(IN(Box,LivingRoom),0).

; added:
[room1,room2,time] !HoldsAt(INROOM(room1,room2),time).
[room,object,time] !HoldsAt(IN(room,object),time).

; entailed:
; HoldsAt(IN(Lisa,LivingRoom),5).
; HoldsAt(IN(Box,Kitchen),5).
; HoldsAt(INROOM(Newspaper,Kitchen),5).

completion Happens

range time 0 5
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter10/TwoScreens.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @phdthesis{Cassimatis:2002,
;   author = "Nicholas L. Cassimatis",
;   year = "2002",
;   title = "Polyscheme: A Cognitive Architecture for Integrating Multiple Representation and Inference Schemes",
;   address = "Cambridge, MA",
;   school = "Program in Media Arts and Sciences, School of Architecture and Planning, Massachusetts Institute of Technology",
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object
sort location

object O1, O2
location L1, L2, L3, L4, L5

predicate Adjacent(location,location)
predicate Equal(object,object)

fluent At(object,location)
event Move(object,location,location)

; Sigma

[object,location1,location2,time]
HoldsAt(At(object,location1),time) &
Adjacent(location1,location2) ->
Initiates(Move(object,location1,location2),At(object,location2),time).

[object,location1,location2,time]
HoldsAt(At(object,location1),time) &
Adjacent(location1,location2) ->
Terminates(Move(object,location1,location2),At(object,location1),time).

; Psi

[object,location1,location2,time]
HoldsAt(At(object,location1),time) &
HoldsAt(At(object,location2),time) ->
location1=location2.

[object,time]
{location} HoldsAt(At(object,location),time).

[object1,object2,location,time]
HoldsAt(At(object1,location),time) &
HoldsAt(At(object2,location),time) ->
Equal(object1,object2).

[location1, location2]
Adjacent(location1,location2) <->
Adjacent(location2,location1).

[object1,object2]
Equal(object1,object2) <->
Equal(object2,object1).

; Gamma

[location1,location2]
Adjacent(location1,location2) <->
(location1=L1 & location2=L2) |
(location1=L2 & location2=L1) |
(location1=L2 & location2=L3) |
(location1=L3 & location2=L2) |
(location1=L3 & location2=L4) |
(location1=L4 & location2=L3) |
(location1=L4 & location2=L5) |
(location1=L5 & location2=L4).

HoldsAt(At(O1,L1),0).
[object] !HoldsAt(At(object,L5),0).

HoldsAt(At(O2,L5),4).
[object] !HoldsAt(At(object,L1),4).

[object,time] !HoldsAt(At(object,L3),time).

; ADDED:
[object,location1,location2,time]
Happens(Move(object,location1,location2),time) ->
HoldsAt(At(object,location1),time) &
Adjacent(location1,location2).

[object1,object2,location1,location2,time]
Equal(object1,object2) &
Happens(Move(object1,location1,location2),time) ->
Happens(Move(object2,location1,location2),time).

; entailed: !Equal(O1,O2).

range time 0 4
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter10/OneScreen.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @phdthesis{Cassimatis:2002,
;   author = "Nicholas L. Cassimatis",
;   year = "2002",
;   title = "Polyscheme: A Cognitive Architecture for Integrating Multiple Representation and Inference Schemes",
;   address = "Cambridge, MA",
;   school = "Program in Media Arts and Sciences, School of Architecture and Planning, Massachusetts Institute of Technology",
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object
sort location

object O1, O2
location L1, L2, L3

predicate Adjacent(location,location)
predicate Equal(object,object)

fluent At(object,location)
event Move(object,location,location)

; Sigma

[object,location1,location2,time]
HoldsAt(At(object,location1),time) &
Adjacent(location1,location2) ->
Initiates(Move(object,location1,location2),At(object,location2),time).

[object,location1,location2,time]
HoldsAt(At(object,location1),time) &
Adjacent(location1,location2) ->
Terminates(Move(object,location1,location2),At(object,location1),time).

; Psi

[object,location1,location2,time]
HoldsAt(At(object,location1),time) &
HoldsAt(At(object,location2),time) ->
location1=location2.

[object,time]
{location} HoldsAt(At(object,location),time).

[object1,object2,location,time]
HoldsAt(At(object1,location),time) &
HoldsAt(At(object2,location),time) ->
Equal(object1,object2).

[location1, location2]
Adjacent(location1,location2) <->
Adjacent(location2,location1).

[object1,object2]
Equal(object1,object2) <->
Equal(object2,object1).

; Gamma

[location1,location2]
Adjacent(location1,location2) <->
(location1=L1 & location2=L2) |
(location1=L2 & location2=L1) |
(location1=L2 & location2=L3) |
(location1=L3 & location2=L2).

HoldsAt(At(O1,L1),0).
[object] !HoldsAt(At(object,L3),0).

[object] !HoldsAt(At(object,L1),1).
[object] !HoldsAt(At(object,L3),1).

HoldsAt(At(O2,L3),2).
[object] !HoldsAt(At(object,L1),2).

; ADDED:
[object,location1,location2,time]
Happens(Move(object,location1,location2),time) ->
HoldsAt(At(object,location1),time) &
Adjacent(location1,location2).

[object1,object2,location1,location2,time]
Equal(object1,object2) &
Happens(Move(object1,location1,location2),time) ->
Happens(Move(object2,location1,location2),time).

range time 0 2
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter9/RunningAndDriving.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent
sort location

agent James
location Bookstore

fluent Tired(agent)

event Go(agent,location)
event Run(agent,location)
event Drive(agent,location)

[agent,location,time]
Happens(Go(agent,location),time) ->
Happens(Run(agent,location),time) | Happens(Drive(agent,location),time).

xor Run, Drive

[agent,location,time] Initiates(Run(agent,location),Tired(agent),time).

!HoldsAt(Tired(James),0).
Happens(Go(James,Bookstore),0).
HoldsAt(Tired(James),1).

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter9/RouletteWheel.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option modeldiff on

load foundations/Root.e
load foundations/EC.e

sort dealer
sort wheel
sort value: integer

wheel Wheel1
dealer Dealer1

fluent WheelNumberDeterminer(wheel,value)
fluent WheelNumber(wheel,value)
noninertial WheelNumberDeterminer

event Spin(dealer,wheel)
event Reset(dealer,wheel)

[wheel,time]
{value}
HoldsAt(WheelNumberDeterminer(wheel,value),time).

[wheel,value1,value2,time]
HoldsAt(WheelNumberDeterminer(wheel,value1),time) &
HoldsAt(WheelNumberDeterminer(wheel,value2),time) ->
value1=value2.

[dealer,wheel,value,time]
HoldsAt(WheelNumberDeterminer(wheel,value),time) ->
Initiates(Spin(dealer,wheel),WheelNumber(wheel,value),time).

[dealer,wheel,value1,value2,time]
HoldsAt(WheelNumber(wheel,value1),time) &
HoldsAt(WheelNumberDeterminer(wheel,value2),time) &
value1!=value2 ->
Terminates(Spin(dealer,wheel),WheelNumber(wheel,value1),time).

[dealer,wheel,value,time]
Terminates(Reset(dealer,wheel),WheelNumber(wheel,value),time).

[wheel,value1,value2,time]
HoldsAt(WheelNumber(wheel,value1),time) &
HoldsAt(WheelNumber(wheel,value2),time) ->
value1=value2.

[value] !HoldsAt(WheelNumber(Wheel1,value),0).

Happens(Spin(Dealer1,Wheel1),0).
;Happens(Reset(Dealer1,Wheel1),1).

; added to prune models
HoldsAt(WheelNumberDeterminer(Wheel1, 1),1).

completion Happens

range value 1 3
range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter14/NetBill1.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @inproceedings{SirbuTygar:1995,
;   author = "Marvin A. Sirbu and J. D. Tygar",
;   year = "1995",
;   title = "Net\uppercase{B}ill: An \uppercase{I}nternet commerce system optimized for network delivered services",
;   editor = "
;   booktitle = "40th \uppercase{IEEE} \uppercase{C}omputer \uppercase{S}ociety \uppercase{I}nternational \uppercase{C}onference",
;   pages = "20--25",
;   publisher = "
;   address = "
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option modeldiff on

load foundations/Root.e
load foundations/EC.e

sort agent
agent MusicStore, Jen

sort product
product BritneyCD

sort f
f PurchaseRequestedJenMusicStoreBritneyCD1
f DeliveredMusicStoreJenBritneyCD
f EPOSentJenMusicStore1

sort amount: integer

fluent C(agent,agent,f)
fluent CC(agent,agent,f,f)

event CreateC(agent,agent,f)
event CreateCC(agent,agent,f,f)
event DischargeC(agent,agent,f)
event DischargeCC(agent,agent,f,f)

fluent QuoteSent(agent,agent,product,amount)
fluent PurchaseRequested(agent,agent,product,amount)
fluent Delivered(agent,agent,product)
fluent EPOSent(agent,agent,amount)

event SendQuote(agent,agent,product,amount)
event RequestPurchase(agent,agent,product,amount)
event Deliver(agent,agent,product)
event SendEPO(agent,agent,amount)

; Sigma

[agent1,agent2,f,time]
Initiates(CreateC(agent1,agent2,f),C(agent1,agent2,f),time).

[agent1,agent2,f1,f2,time]
Initiates(CreateCC(agent1,agent2,f1,f2),CC(agent1,agent2,f1,f2),time).

[agent1,agent2,f,time]
Terminates(DischargeC(agent1,agent2,f),C(agent1,agent2,f),time).

[agent1,agent2,f1,f2,time]
Terminates(DischargeCC(agent1,agent2,f1,f2),CC(agent1,agent2,f1,f2),time).

[agent1,agent2,product,amount,time]
Initiates(SendQuote(agent1,agent2,product,amount),
          QuoteSent(agent1,agent2,product,amount),
          time).

[agent1,agent2,product,amount,time]
Initiates(RequestPurchase(agent1,agent2,product,amount),
          PurchaseRequested(agent1,agent2,product,amount),
          time).

[agent1,agent2,product,time]
Initiates(Deliver(agent1,agent2,product),
          Delivered(agent1,agent2,product),
          time).

[agent1,agent2,amount,time]
Initiates(SendEPO(agent1,agent2,amount),
          EPOSent(agent1,agent2,amount),
          time).

[agent1,agent2,product,amount,f1,f2,time]
agent1=MusicStore &
agent2=Jen &
product=BritneyCD &
amount=1 &
f1=PurchaseRequestedJenMusicStoreBritneyCD1 &
f2=DeliveredMusicStoreJenBritneyCD ->
Initiates(SendQuote(agent1,agent2,product,amount),
          CC(agent1,agent2,f1,f2),
          time).

[agent1,agent2,product,amount,f1,f2,time]
agent1=Jen &
agent2=MusicStore &
product=BritneyCD &
amount=1 &
f1=DeliveredMusicStoreJenBritneyCD &
f2=EPOSentJenMusicStore1 &
!HoldsAt(Delivered(agent2,agent1,product),time) ->
Initiates(RequestPurchase(agent1,agent2,product,amount),
          CC(agent1,agent2,f1,f2),
          time).

; Delta

Delta: [time]
HoldsAt(CC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time) &
HoldsAt(PurchaseRequested(Jen,MusicStore,BritneyCD,1),time) ->
Happens(CreateC(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time).

Delta: [time]
HoldsAt(CC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time) &
HoldsAt(PurchaseRequested(Jen, MusicStore, BritneyCD, 1),time) ->
Happens(DischargeCC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time).

Delta: [time]
HoldsAt(CC(Jen, MusicStore, DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time) &
HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
Happens(CreateC(Jen,MusicStore,EPOSentJenMusicStore1),time).

Delta: [time]
HoldsAt(CC(Jen, MusicStore, DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time) &
HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
Happens(DischargeCC(Jen,MusicStore,DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time).

Delta: [time]
HoldsAt(C(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time) &
HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
Happens(DischargeC(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time).

Delta: [time]
HoldsAt(C(Jen,MusicStore,EPOSentJenMusicStore1),time) &
HoldsAt(EPOSent(Jen,MusicStore,1),time) ->
Happens(DischargeC(Jen,MusicStore,EPOSentJenMusicStore1),time).

Delta: Happens(SendQuote(MusicStore,Jen,BritneyCD,1),0).
Delta: Happens(RequestPurchase(Jen,MusicStore,BritneyCD,1),1).
Delta: Happens(Deliver(MusicStore,Jen,BritneyCD),3).
Delta: Happens(SendEPO(Jen,MusicStore,1),5).

; Gamma

[agent1,agent2,product,amount]
!HoldsAt(QuoteSent(agent1,agent2,product,amount),0).

[agent1,agent2,product,amount]
!HoldsAt(PurchaseRequested(agent1,agent2,product,amount),0).

[agent1,agent2,product]
!HoldsAt(Delivered(agent1,agent2,product),0).

[agent1,agent2,f]
!HoldsAt(C(agent1,agent2,f),0).

[agent1,agent2,f1,f2]
!HoldsAt(CC(agent1,agent2,f1,f2),0).

[agent1,agent2,amount]
!HoldsAt(EPOSent(agent1,agent2,amount),0).

completion Delta Happens

range time 0 7
range offset 1 1
range amount 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter14/NetBill3.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @inproceedings{SirbuTygar:1995,
;   author = "Marvin A. Sirbu and J. D. Tygar",
;   year = "1995",
;   title = "Net\uppercase{B}ill: An \uppercase{I}nternet commerce system optimized for network delivered services",
;   editor = "
;   booktitle = "40th \uppercase{IEEE} \uppercase{C}omputer \uppercase{S}ociety \uppercase{I}nternational \uppercase{C}onference",
;   pages = "20--25",
;   publisher = "
;   address = "
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option modeldiff on

load foundations/Root.e
load foundations/EC.e

sort agent
agent MusicStore, Jen

sort product
product BritneyCD

sort f
f PurchaseRequestedJenMusicStoreBritneyCD1
f DeliveredMusicStoreJenBritneyCD
f EPOSentJenMusicStore1

sort amount: integer

fluent C(agent,agent,f)
fluent CC(agent,agent,f,f)

event CreateC(agent,agent,f)
event CreateCC(agent,agent,f,f)
event DischargeC(agent,agent,f)
event DischargeCC(agent,agent,f,f)

fluent QuoteSent(agent,agent,product,amount)
fluent PurchaseRequested(agent,agent,product,amount)
fluent Delivered(agent,agent,product)
fluent EPOSent(agent,agent,amount)

event SendQuote(agent,agent,product,amount)
event RequestPurchase(agent,agent,product,amount)
event Deliver(agent,agent,product)
event SendEPO(agent,agent,amount)

; Sigma

[agent1,agent2,f,time]
Initiates(CreateC(agent1,agent2,f),C(agent1,agent2,f),time).

[agent1,agent2,f1,f2,time]
Initiates(CreateCC(agent1,agent2,f1,f2),CC(agent1,agent2,f1,f2),time).

[agent1,agent2,f,time]
Terminates(DischargeC(agent1,agent2,f),C(agent1,agent2,f),time).

[agent1,agent2,f1,f2,time]
Terminates(DischargeCC(agent1,agent2,f1,f2),CC(agent1,agent2,f1,f2),time).

[agent1,agent2,product,amount,time]
Initiates(SendQuote(agent1,agent2,product,amount),
          QuoteSent(agent1,agent2,product,amount),
          time).

[agent1,agent2,product,amount,time]
Initiates(RequestPurchase(agent1,agent2,product,amount),
          PurchaseRequested(agent1,agent2,product,amount),
          time).

[agent1,agent2,product,time]
Initiates(Deliver(agent1,agent2,product),
          Delivered(agent1,agent2,product),
          time).

[agent1,agent2,amount,time]
Initiates(SendEPO(agent1,agent2,amount),
          EPOSent(agent1,agent2,amount),
          time).

[agent1,agent2,product,amount,f1,f2,time]
agent1=MusicStore &
agent2=Jen &
product=BritneyCD &
amount=1 &
f1=PurchaseRequestedJenMusicStoreBritneyCD1 &
f2=DeliveredMusicStoreJenBritneyCD ->
Initiates(SendQuote(agent1,agent2,product,amount),
          CC(agent1,agent2,f1,f2),
          time).

[agent1,agent2,product,amount,f1,f2,time]
agent1=Jen &
agent2=MusicStore &
product=BritneyCD &
amount=1 &
f1=DeliveredMusicStoreJenBritneyCD &
f2=EPOSentJenMusicStore1 &
!HoldsAt(Delivered(agent2,agent1,product),time) ->
Initiates(RequestPurchase(agent1,agent2,product,amount),
          CC(agent1,agent2,f1,f2),
          time).

; Delta

Delta: [time]
HoldsAt(CC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time) &
HoldsAt(PurchaseRequested(Jen,MusicStore,BritneyCD,1),time) ->
Happens(CreateC(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time).

Delta: [time]
HoldsAt(CC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time) &
HoldsAt(PurchaseRequested(Jen, MusicStore, BritneyCD, 1),time) ->
Happens(DischargeCC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time).

Delta: [time]
HoldsAt(CC(Jen, MusicStore, DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time) &
HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
Happens(CreateC(Jen,MusicStore,EPOSentJenMusicStore1),time).

Delta: [time]
HoldsAt(CC(Jen, MusicStore, DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time) &
HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
Happens(DischargeCC(Jen,MusicStore,DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time).

Delta: [time]
HoldsAt(C(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time) &
HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
Happens(DischargeC(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time).

Delta: [time]
HoldsAt(C(Jen,MusicStore,EPOSentJenMusicStore1),time) &
HoldsAt(EPOSent(Jen,MusicStore,1),time) ->
Happens(DischargeC(Jen,MusicStore,EPOSentJenMusicStore1),time).

Delta: Happens(Deliver(MusicStore,Jen,BritneyCD),0).
Delta: Happens(SendEPO(Jen,MusicStore,1),2).

; Gamma

[agent1,agent2,product,amount]
!HoldsAt(QuoteSent(agent1,agent2,product,amount),0).

[agent1,agent2,product,amount]
!HoldsAt(PurchaseRequested(agent1,agent2,product,amount),0).

[agent1,agent2,product]
!HoldsAt(Delivered(agent1,agent2,product),0).

[agent1,agent2,f]
!HoldsAt(C(agent1,agent2,f),0).

[agent1,agent2,f1,f2]
!HoldsAt(CC(agent1,agent2,f1,f2),0).

[agent1,agent2,amount]
!HoldsAt(EPOSent(agent1,agent2,amount),0).

completion Delta Happens

range time 0 4
range offset 1 1
range amount 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter14/NetBill2.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @inproceedings{SirbuTygar:1995,
;   author = "Marvin A. Sirbu and J. D. Tygar",
;   year = "1995",
;   title = "Net\uppercase{B}ill: An \uppercase{I}nternet commerce system optimized for network delivered services",
;   editor = "
;   booktitle = "40th \uppercase{IEEE} \uppercase{C}omputer \uppercase{S}ociety \uppercase{I}nternational \uppercase{C}onference",
;   pages = "20--25",
;   publisher = "
;   address = "
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option modeldiff on

load foundations/Root.e
load foundations/EC.e

sort agent
agent MusicStore, Jen

sort product
product BritneyCD

sort f
f PurchaseRequestedJenMusicStoreBritneyCD1
f DeliveredMusicStoreJenBritneyCD
f EPOSentJenMusicStore1

sort amount: integer

fluent C(agent,agent,f)
fluent CC(agent,agent,f,f)

event CreateC(agent,agent,f)
event CreateCC(agent,agent,f,f)
event DischargeC(agent,agent,f)
event DischargeCC(agent,agent,f,f)

fluent QuoteSent(agent,agent,product,amount)
fluent PurchaseRequested(agent,agent,product,amount)
fluent Delivered(agent,agent,product)
fluent EPOSent(agent,agent,amount)

event SendQuote(agent,agent,product,amount)
event RequestPurchase(agent,agent,product,amount)
event Deliver(agent,agent,product)
event SendEPO(agent,agent,amount)

; Sigma

[agent1,agent2,f,time]
Initiates(CreateC(agent1,agent2,f),C(agent1,agent2,f),time).

[agent1,agent2,f1,f2,time]
Initiates(CreateCC(agent1,agent2,f1,f2),CC(agent1,agent2,f1,f2),time).

[agent1,agent2,f,time]
Terminates(DischargeC(agent1,agent2,f),C(agent1,agent2,f),time).

[agent1,agent2,f1,f2,time]
Terminates(DischargeCC(agent1,agent2,f1,f2),CC(agent1,agent2,f1,f2),time).

[agent1,agent2,product,amount,time]
Initiates(SendQuote(agent1,agent2,product,amount),
          QuoteSent(agent1,agent2,product,amount),
          time).

[agent1,agent2,product,amount,time]
Initiates(RequestPurchase(agent1,agent2,product,amount),
          PurchaseRequested(agent1,agent2,product,amount),
          time).

[agent1,agent2,product,time]
Initiates(Deliver(agent1,agent2,product),
          Delivered(agent1,agent2,product),
          time).

[agent1,agent2,amount,time]
Initiates(SendEPO(agent1,agent2,amount),
          EPOSent(agent1,agent2,amount),
          time).

[agent1,agent2,product,amount,f1,f2,time]
agent1=MusicStore &
agent2=Jen &
product=BritneyCD &
amount=1 &
f1=PurchaseRequestedJenMusicStoreBritneyCD1 &
f2=DeliveredMusicStoreJenBritneyCD ->
Initiates(SendQuote(agent1,agent2,product,amount),
          CC(agent1,agent2,f1,f2),
          time).

[agent1,agent2,product,amount,f1,f2,time]
agent1=Jen &
agent2=MusicStore &
product=BritneyCD &
amount=1 &
f1=DeliveredMusicStoreJenBritneyCD &
f2=EPOSentJenMusicStore1 &
!HoldsAt(Delivered(agent2,agent1,product),time) ->
Initiates(RequestPurchase(agent1,agent2,product,amount),
          CC(agent1,agent2,f1,f2),
          time).

; Delta

Delta: [time]
HoldsAt(CC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time) &
HoldsAt(PurchaseRequested(Jen,MusicStore,BritneyCD,1),time) ->
Happens(CreateC(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time).

Delta: [time]
HoldsAt(CC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time) &
HoldsAt(PurchaseRequested(Jen, MusicStore, BritneyCD, 1),time) ->
Happens(DischargeCC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time).

Delta: [time]
HoldsAt(CC(Jen, MusicStore, DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time) &
HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
Happens(CreateC(Jen,MusicStore,EPOSentJenMusicStore1),time).

Delta: [time]
HoldsAt(CC(Jen, MusicStore, DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time) &
HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
Happens(DischargeCC(Jen,MusicStore,DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time).

Delta: [time]
HoldsAt(C(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time) &
HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
Happens(DischargeC(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time).

Delta: [time]
HoldsAt(C(Jen,MusicStore,EPOSentJenMusicStore1),time) &
HoldsAt(EPOSent(Jen,MusicStore,1),time) ->
Happens(DischargeC(Jen,MusicStore,EPOSentJenMusicStore1),time).

Delta: Happens(RequestPurchase(Jen,MusicStore,BritneyCD,1),0).
Delta: Happens(Deliver(MusicStore,Jen,BritneyCD),2).
Delta: Happens(SendEPO(Jen,MusicStore,1),4).

; Gamma

[agent1,agent2,product,amount]
!HoldsAt(QuoteSent(agent1,agent2,product,amount),0).

[agent1,agent2,product,amount]
!HoldsAt(PurchaseRequested(agent1,agent2,product,amount),0).

[agent1,agent2,product]
!HoldsAt(Delivered(agent1,agent2,product),0).

[agent1,agent2,f]
!HoldsAt(C(agent1,agent2,f),0).

[agent1,agent2,f1,f2]
!HoldsAt(CC(agent1,agent2,f1,f2),0).

[agent1,agent2,amount]
!HoldsAt(EPOSent(agent1,agent2,amount),0).

completion Delta Happens

range time 0 6
range offset 1 1
range amount 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter14/Vision.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @inproceedings{ShanahanRandell:2004,
;   author = "Murray Shanahan and David A. Randell",
;   year = "2004",
;   title = "A logic-based formulation of active visual perception",
;   editor = "Didier Dubois and Christopher A. Welty and Mary-Anne Williams",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{N}inth \uppercase{I}nternational \uppercase{C}onference on \uppercase{P}rinciples of \uppercase{K}nowledge \uppercase{R}epresentation and \uppercase{R}easoning",
;   pages = "64--72",
;   address = "Menlo Park, CA",
;   publisher = "AAAI Press",
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option modeldiff on

load foundations/Root.e
load foundations/EC.e

sort object
sort shape
sort aspect

object Object1
aspect Aspect1, Aspect2, Aspect3
shape Shape1, Shape2

predicate Shape(object,shape)
predicate Arc(shape,aspect,aspect)
fluent Aspect(object,aspect)
event Change(object,aspect,aspect)

; Sigma

[object,aspect1,aspect2,shape,time]
HoldsAt(Aspect(object,aspect1),time) &
Shape(object,shape) &
(Arc(shape,aspect1,aspect2) |
 Arc(shape,aspect2,aspect1)) ->
Initiates(Change(object,aspect1,aspect2),Aspect(object,aspect2),time).

[object,aspect1,aspect2,shape,time]
HoldsAt(Aspect(object,aspect1),time) &
Shape(object,shape) &
(Arc(shape,aspect1,aspect2) |
 Arc(shape,aspect2,aspect1)) ->
Terminates(Change(object,aspect1,aspect2),Aspect(object,aspect1),time).

; preconditions (added)

[object,aspect1,aspect2,time]
Happens(Change(object,aspect1,aspect2),time) ->
HoldsAt(Aspect(object,aspect1),time).

[object,aspect1,aspect2,aspect3,time]
Happens(Change(object,aspect1,aspect2),time) &
Happens(Change(object,aspect1,aspect3),time) ->
aspect2=aspect3.

; Psi

[object,shape1,shape2]
Shape(object,shape1) &
Shape(object,shape2) ->
shape1=shape2.

[object,aspect1,aspect2,time]
HoldsAt(Aspect(object,aspect1),time) &
HoldsAt(Aspect(object,aspect2),time) ->
aspect1=aspect2.

[aspect1,aspect2]
Arc(Shape1,aspect1,aspect2) <->
(aspect1=Aspect1 & aspect2=Aspect2).

[aspect1,aspect2]
Arc(Shape2,aspect1,aspect2) <->
((aspect1=Aspect1 & aspect2=Aspect3) |
 (aspect1=Aspect3 & aspect2=Aspect2)).

; Gamma

HoldsAt(Aspect(Object1,Aspect1),0).
HoldsAt(Aspect(Object1,Aspect2),1).

;completion Delta Happens

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter14/Workflow.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @incollection{CicekliYildirim:2000,
;   author = "Nihan Kesim Cicekli and Yakup Yildirim",
;   year = "2000",
;   title = "Formalizing workflows using the event calculus",
;   editor = "Mohamed T. Ibrahim and Josef K{\"{u}}ng and Norman Revell",
;   booktitle = "Database and Expert Systems Applications",
;   series = "Lecture Notes in Computer Science",
;   volume = "1873",
;   pages = "222--231",
;   address = "Berlin",
;   publisher = "Springer",
; }
;
; @unpublished{WFMC:1999,
;   author = "{Workflow Management Coalition}",
;   year = "1999",
;   title = "\uppercase{W}orkflow \uppercase{M}anagement \uppercase{C}oalition Terminology \& Glossary",
;   howpublished = "Document Number WFMC-TC-1011, Document Status -- Issue 3.0, Workflow Management Coalition, Winchester, UK",
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option modeldiff on

load foundations/Root.e
load foundations/EC.e

sort activity
sort condition
activity A, B, C1, C2, C3, D, E1, E2, E3, F, G
condition E1C, E2C, E3C, FC

fluent Active(activity)
fluent Completed(activity)
fluent Condition(condition)
noninertial Condition

event Start(activity)
event End(activity)

; Sigma

[activity,time]
Initiates(Start(activity),Active(activity),time).

[activity,time]
Terminates(Start(activity),Completed(activity),time).

[activity,time]
Initiates(End(activity),Completed(activity),time).

[activity,time]
Terminates(End(activity),Active(activity),time).

; Delta

; A; B
Delta: [time]
!HoldsAt(Active(B),time) &
!HoldsAt(Completed(A),time-1) &
HoldsAt(Completed(A),time) ->
Happens(Start(B),time).

; B; AND-split C1, C2, C3
Delta: [time]
!HoldsAt(Active(C1),time) &
!HoldsAt(Completed(B),time-1) &
HoldsAt(Completed(B),time) ->
Happens(Start(C1),time).

Delta: [time]
!HoldsAt(Active(C2),time) &
!HoldsAt(Completed(B),time-1) &
HoldsAt(Completed(B),time) ->
Happens(Start(C2),time).

Delta: [time]
!HoldsAt(Active(C3),time) &
!HoldsAt(Completed(B),time-1) &
HoldsAt(Completed(B),time) ->
Happens(Start(C3),time).

; AND-join C1, C2, C3; D
Delta: [time]
!HoldsAt(Active(D),time) &
((!HoldsAt(Completed(C1),time-1) & HoldsAt(Completed(C1),time))|
 (!HoldsAt(Completed(C2),time-1) & HoldsAt(Completed(C2),time))|
 (!HoldsAt(Completed(C3),time-1) & HoldsAt(Completed(C3),time))) &
HoldsAt(Completed(C1),time) &
HoldsAt(Completed(C2),time) &
HoldsAt(Completed(C3),time) ->
Happens(Start(D),time).

; D; XOR-split E1, E2, E3
Delta: [time]
!HoldsAt(Active(E1),time) &
!HoldsAt(Completed(D),time-1) &
HoldsAt(Completed(D),time) &
HoldsAt(Condition(E1C),time) ->
Happens(Start(E1),time).

Delta: [time]
!HoldsAt(Active(E2),time) &
!HoldsAt(Completed(D),time-1) &
HoldsAt(Completed(D),time) &
HoldsAt(Condition(E2C),time) ->
Happens(Start(E2),time).

Delta: [time]
!HoldsAt(Active(E3),time) &
!HoldsAt(Completed(D),time-1) &
HoldsAt(Completed(D),time) &
HoldsAt(Condition(E3C),time) ->
Happens(Start(E3),time).

; XOR-join E1, E2, E3; F
Delta: [time]
!HoldsAt(Active(F),time) &
((!HoldsAt(Completed(E1),time-1) & HoldsAt(Completed(E1),time))|
 (!HoldsAt(Completed(E2),time-1) & HoldsAt(Completed(E2),time))|
 (!HoldsAt(Completed(E3),time-1) & HoldsAt(Completed(E3),time))) ->
Happens(Start(F),time).

; while (FC) F; G
Delta: [time]
!HoldsAt(Active(F),time) &
!HoldsAt(Completed(F),time-1) &
HoldsAt(Completed(F),time) &
HoldsAt(Condition(FC),time) ->
Happens(Start(F),time).

Delta: [time]
!HoldsAt(Active(G),time) &
!HoldsAt(Completed(F),time-1) &
HoldsAt(Completed(F),time) &
!HoldsAt(Condition(FC),time) ->
Happens(Start(G),time).

Delta: Happens(Start(A),0).
Delta: Happens(End(A),1).
Delta: Happens(End(B),3).
Delta: Happens(End(C1),5).
Delta: Happens(End(C2),6).
Delta: Happens(End(C3),7).
Delta: Happens(End(D),9).
Delta: Happens(End(E2),11).
Delta: Happens(End(F),13).
Delta: Happens(End(F),15).

; Gamma

[activity] !HoldsAt(Active(activity),0).
[activity] !HoldsAt(Completed(activity),0).
[time] time=14 <-> HoldsAt(Condition(FC),time).
[time] !HoldsAt(Condition(E1C),time).
[time] time=10 <-> HoldsAt(Condition(E2C),time).
[time] !HoldsAt(Condition(E3C),time).

completion Delta Happens

range time 0 18
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter6/ThielscherCircuit1.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Thielscher:1997,
;   author = "Michael Thielscher",
;   year = "1997",
;   title = "Ramification and causality",
;   journal = "Artificial Intelligence",
;   volume = "89",
;   pages = "317--364",
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e
load foundations/ECCausal.e

sort switch
sort relay
sort light

switch S1, S2, S3
relay R
light L

event Light(light)
event Close(switch)
event Open(switch)
event Activate(relay)

fluent Lit(light)
fluent Closed(switch)
fluent Activated(relay)

[time]
Stopped(Lit(L),time) &
Initiated(Closed(S1),time) &
Initiated(Closed(S2),time) ->
Happens(Light(L),time).

[time]
Started(Closed(S2),time) &
Initiated(Activated(R),time) ->
Happens(Open(S2),time).

[time]
Stopped(Activated(R),time) &
Initiated(Closed(S1),time) &
Initiated(Closed(S3),time) ->
Happens(Activate(R),time).

[switch,time] Initiates(Close(switch),Closed(switch),time).
[switch,time] Terminates(Open(switch),Closed(switch),time).
[relay,time] Initiates(Activate(relay),Activated(relay),time).
[light,time] Initiates(Light(light),Lit(light),time).

!HoldsAt(Closed(S1),0).
HoldsAt(Closed(S2),0).
HoldsAt(Closed(S3),0).
!HoldsAt(Activated(R),0).
!HoldsAt(Lit(L),0).

Happens(Close(S1),0).

completion Happens

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter6/CarryingABook1.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; Example: Carrying a Book (Effect Axioms)
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;
;

option modeldiff on

load foundations/Root.e
load foundations/EC.e

sort object
sort agent: object
sort room

object Book
agent Nathan
room LivingRoom, Kitchen

event LetGoOf(agent,object)
event PickUp(agent,object)
event Walk(agent,room,room)

fluent InRoom(object,room)
fluent Holding(agent,object)

; Sigma

[agent,room1,room2,time]
Initiates(Walk(agent,room1,room2),InRoom(agent,room2),time).

[agent,room1,room2,time]
room1!=room2 ->
Terminates(Walk(agent,room1,room2),InRoom(agent,room1),time).

[agent,object,room,time]
HoldsAt(InRoom(agent,room),time) &
HoldsAt(InRoom(object,room),time) ->
Initiates(PickUp(agent,object),Holding(agent,object),time).

[agent,object,time]
HoldsAt(Holding(agent,object),time) ->
Terminates(LetGoOf(agent,object),Holding(agent,object),time).

[agent,object,room1,room2,time]
HoldsAt(Holding(agent,object),time) ->
Initiates(Walk(agent,room1,room2),InRoom(object,room2),time).

[agent,object,room1,room2,time]
HoldsAt(Holding(agent,object),time) &
room1!=room2 ->
Terminates(Walk(agent,room1,room2),InRoom(object,room1),time).

; Delta

Happens(PickUp(Nathan,Book),0).
Happens(Walk(Nathan,LivingRoom,Kitchen),1).

; Psi

[object,room1,room2,time]
HoldsAt(InRoom(object,room1),time) &
HoldsAt(InRoom(object,room2),time) ->
room1=room2.

; Gamma

HoldsAt(InRoom(Nathan,LivingRoom),0).
HoldsAt(InRoom(Book,LivingRoom),0).

; added:
!HoldsAt(Holding(Nathan,Book),0).
[agent,time] !HoldsAt(Holding(agent,agent),time).

completion Happens

range time 0 2
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter6/ThielscherCircuit2.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Thielscher:1997,
;   author = "Michael Thielscher",
;   year = "1997",
;   title = "Ramification and causality",
;   journal = "Artificial Intelligence",
;   volume = "89",
;   pages = "317--364",
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort switch
sort relay
sort light

switch S1, S2, S3
relay R
light L

event Light(light)
event Unlight(light)
event Close(switch)
event Open(switch)
event Activate(relay)

fluent Lit(light)
fluent Closed(switch)
fluent Activated(relay)

[time]
!HoldsAt(Lit(L),time) &
HoldsAt(Closed(S1),time) &
HoldsAt(Closed(S2),time) ->
Happens(Light(L),time).

[time]
HoldsAt(Lit(L),time) &
(!HoldsAt(Closed(S1),time) | !HoldsAt(Closed(S2),time)) ->
Happens(Unlight(L),time).

[time]
HoldsAt(Closed(S2),time) &
HoldsAt(Activated(R),time) ->
Happens(Open(S2),time).

[time]
!HoldsAt(Activated(R),time) &
HoldsAt(Closed(S1),time) &
HoldsAt(Closed(S3),time) ->
Happens(Activate(R),time).

[switch,time] Initiates(Close(switch),Closed(switch),time).
[switch,time] Terminates(Open(switch),Closed(switch),time).
[relay,time] Initiates(Activate(relay),Activated(relay),time).
[light,time] Initiates(Light(light),Lit(light),time).
[light,time] Terminates(Unlight(light),Lit(light),time).

!HoldsAt(Closed(S1),0).
HoldsAt(Closed(S2),0).
HoldsAt(Closed(S3),0).
!HoldsAt(Activated(R),0).
!HoldsAt(Lit(L),0).

Happens(Close(S1),0).

completion Happens

range time 0 4
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter6/ShanahanCircuit.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @inproceedings{Shanahan:1999a,
;   author = "Murray Shanahan",
;   year = "1999",
;   title = "The ramification problem in the event calculus",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}ixteenth \uppercase{I}nternational \uppercase{J}oint \uppercase{C}onference on \uppercase{A}rtificial \uppercase{I}ntelligence",
;   pages = "140--146",
;   address = "San Mateo, CA",
;   publisher = "Morgan Kaufmann",
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort switch
sort relay
sort light

switch S1, S2, S3
relay R
light L

event Light(light)
event Unlight(light)
event Close(switch)
event Open(switch)
event Activate(relay)
event Deactivate(relay)

fluent Lit(light)
fluent Closed(switch)
fluent Activated(relay)

[time]
!HoldsAt(Lit(L),time) &
HoldsAt(Closed(S1),time) &
HoldsAt(Closed(S2),time) ->
Happens(Light(L),time).

[time]
HoldsAt(Lit(L),time) &
(!HoldsAt(Closed(S1),time) | !HoldsAt(Closed(S2),time)) ->
Happens(Unlight(L),time).

[time]
HoldsAt(Closed(S2),time) &
HoldsAt(Activated(R),time) ->
Happens(Open(S2),time).

[time]
!HoldsAt(Activated(R),time) &
HoldsAt(Closed(S1),time) &
HoldsAt(Closed(S2),time) &
HoldsAt(Closed(S3),time) ->
Happens(Activate(R),time).

[time]
HoldsAt(Activated(R),time) &
(!HoldsAt(Closed(S1),time) |
 !HoldsAt(Closed(S2),time) |
 !HoldsAt(Closed(S3),time)) ->
Happens(Deactivate(R),time).

[switch,time] Initiates(Close(switch),Closed(switch),time).
[switch,time] Terminates(Open(switch),Closed(switch),time).
[relay,time] Initiates(Activate(relay),Activated(relay),time).
[relay,time] Terminates(Deactivate(relay),Activated(relay),time).
[light,time] Initiates(Light(light),Lit(light),time).
[light,time] Terminates(Unlight(light),Lit(light),time).

!HoldsAt(Closed(S1),0).
HoldsAt(Closed(S2),0).
HoldsAt(Closed(S3),0).
!HoldsAt(Activated(R),0).
!HoldsAt(Lit(L),0).

Happens(Close(S1),0).

completion Happens

range time 0 4
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter6/CarryingABook2.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; Example: Carrying a Book (Release Axioms and State Constraints)
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object
sort agent: object
sort room

object Book
agent Nathan
room LivingRoom, Kitchen

event LetGoOf(agent,object)
event PickUp(agent,object)
event Walk(agent,room,room)

fluent InRoom(object,room)
fluent Holding(agent,object)

; Sigma

[agent,room1,room2,time]
Initiates(Walk(agent,room1,room2),InRoom(agent,room2),time).

[agent,room1,room2,time]
room1!=room2 ->
Terminates(Walk(agent,room1,room2),InRoom(agent,room1),time).

[agent,object,room,time]
HoldsAt(InRoom(agent,room),time) &
HoldsAt(InRoom(object,room),time) ->
Initiates(PickUp(agent,object),Holding(agent,object),time).

[agent,object,time]
HoldsAt(Holding(agent,object),time) ->
Terminates(LetGoOf(agent,object),Holding(agent,object),time).

[agent,object,room,time]
Releases(PickUp(agent,object),InRoom(object,room),time).

[agent,object,room,time]
HoldsAt(InRoom(agent,room),time) ->
Initiates(LetGoOf(agent,object),InRoom(object,room),time).

; Delta

Happens(PickUp(Nathan,Book),0).
Happens(Walk(Nathan,LivingRoom,Kitchen),1).

; Psi

[object,room1,room2,time]
HoldsAt(InRoom(object,room1),time) &
HoldsAt(InRoom(object,room2),time) ->
room1=room2.

[agent,object,room,time]
HoldsAt(Holding(agent,object),time) &
HoldsAt(InRoom(agent,room),time) ->
HoldsAt(InRoom(object,room),time).

; Gamma

HoldsAt(InRoom(Nathan,LivingRoom),0).
HoldsAt(InRoom(Book,LivingRoom),0).

; added:
!HoldsAt(Holding(Nathan,Book),0).
[agent,time] !HoldsAt(Holding(agent,agent),time).

completion Happens

range time 0 2
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter7/HotAirBalloon.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{MillerShanahan:1999,
;   author = "Rob Miller and Murray Shanahan",
;   year = "1999",
;   title = "The event calculus in classical logic---\uppercase{A}lternative axiomatisations",
;   journal = "Link{\"{o}}ping Electronic Articles in Computer and Information Science",
;   volume = "4",
;   number = "016",
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option encoding 3
option trajectory on

load foundations/Root.e
load foundations/EC.e

sort balloon
sort agent
sort height: integer

agent Nathan
balloon Balloon

fluent HeaterOn(balloon)
fluent Height(balloon,height)
noninertial Height

event TurnOnHeater(agent,balloon)
event TurnOffHeater(agent,balloon)

; Sigma

[agent,balloon,time]
Initiates(TurnOnHeater(agent,balloon),HeaterOn(balloon),time).

[agent,balloon,time]
Terminates(TurnOffHeater(agent,balloon),HeaterOn(balloon),time).

; Delta

Delta: Happens(TurnOnHeater(Nathan,Balloon),0).
Delta: Happens(TurnOffHeater(Nathan,Balloon),2).

; Psi

[balloon,height1,height2,time]
HoldsAt(Height(balloon,height1),time) &
HoldsAt(Height(balloon,height2),time) ->
height1=height2.

; Pi

[balloon,height1,height2,offset,time]
HoldsAt(Height(balloon,height1),time) &
height2 = (height1 + offset) ->
Trajectory(HeaterOn(balloon),time,Height(balloon,height2),offset).

[balloon,height1,height2,offset,time]
HoldsAt(Height(balloon,height1),time) &
height2 = (height1 - offset) ->
AntiTrajectory(HeaterOn(balloon),time,Height(balloon,height2),offset).

; Gamma

HoldsAt(Height(Balloon,0),0).

; added:
!HoldsAt(HeaterOn(Balloon),0).

completion Delta Happens

range time 0 3
range height 0 2
range offset 1 2

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter7/FallingObjectWithEvents.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object
sort agent
sort height: integer

agent Nathan
object Apple

fluent Falling(object)
fluent Height(object,height)

event Drop(agent,object)
event HitGround(object)

; Sigma

[agent,object,time]
Initiates(Drop(agent,object),Falling(object),time).

[agent,object,height,time]
Releases(Drop(agent,object),Height(object,height),time).

[object,time]
Terminates(HitGround(object),Falling(object),time).

[object,height,time]
HoldsAt(Height(object,height),time) ->
Initiates(HitGround(object),Height(object,height),time).

; Delta

Delta: [object,time]
HoldsAt(Falling(object),time) &
HoldsAt(Height(object,0),time) ->
Happens(HitGround(object),time).

Delta: Happens(Drop(Nathan,Apple),0).

; Psi

[object,height1,height2,time]
HoldsAt(Height(object,height1),time) &
HoldsAt(Height(object,height2),time) ->
height1=height2.

; Pi

[object,height1,height2,offset,time]
HoldsAt(Height(object,height1),time) &
height2 = (height1 - offset) ->
Trajectory(Falling(object),time,Height(object,height2),offset).

; Gamma

!HoldsAt(Falling(Apple),0).
HoldsAt(Height(Apple,3),0).

completion Delta Happens

range time 0 5
range height 0 3
range offset 1 3

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter7/FallingObjectWithAntiTrajectory.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option encoding 3
option trajectory on

load foundations/Root.e
load foundations/EC.e

sort object
sort agent
sort height: integer

agent Nathan
object Apple

fluent Falling(object)
fluent Height(object,height)
noninertial Height

event Drop(agent,object)
event HitGround(object)

; Sigma

[agent,object,time]
Initiates(Drop(agent,object),Falling(object),time).

[object,time]
Terminates(HitGround(object),Falling(object),time).

; Delta

Delta: [object,time]
HoldsAt(Falling(object),time) &
HoldsAt(Height(object,0),time) ->
Happens(HitGround(object),time).

Delta: Happens(Drop(Nathan,Apple),0).

; Psi

[object,height1,height2,time]
HoldsAt(Height(object,height1),time) &
HoldsAt(Height(object,height2),time) ->
height1=height2.

; Pi

[object,height1,height2,offset,time]
HoldsAt(Height(object,height1),time) &
height2 = (height1 - offset) ->
Trajectory(Falling(object),time,Height(object,height2),offset).

[object,height,offset,time]
HoldsAt(Height(object,height),time) ->
AntiTrajectory(Falling(object),time,Height(object,height),offset).

; Gamma

!HoldsAt(Falling(Apple),0).
HoldsAt(Height(Apple,3),0).

completion Delta Happens

range time 0 5
range height 0 3
range offset 1 3

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter3/Telephone2.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

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
HoldsAt(Idle(phone),time) ->
Initiates(PickUp(agent,phone),DialTone(phone),time).

[agent,phone,time]
HoldsAt(Idle(phone),time) ->
Terminates(PickUp(agent,phone),Idle(phone),time).

[agent,phone,time]
HoldsAt(DialTone(phone),time) ->
Initiates(SetDown(agent,phone),Idle(phone),time).

[agent,phone,time]
HoldsAt(DialTone(phone),time) ->
Terminates(SetDown(agent,phone),DialTone(phone),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
HoldsAt(Idle(phone2),time) ->
Initiates(Dial(agent,phone1,phone2),Ringing(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
HoldsAt(Idle(phone2),time) ->
Terminates(Dial(agent,phone1,phone2),DialTone(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
HoldsAt(Idle(phone2),time) ->
Terminates(Dial(agent,phone1,phone2),Idle(phone2),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
!HoldsAt(Idle(phone2),time) ->
Initiates(Dial(agent,phone1,phone2),BusySignal(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
!HoldsAt(Idle(phone2),time) ->
Terminates(Dial(agent,phone1,phone2),DialTone(phone1),time).

[agent,phone,time]
HoldsAt(BusySignal(phone),time) ->
Initiates(SetDown(agent,phone),Idle(phone),time).

[agent,phone,time]
HoldsAt(BusySignal(phone),time) ->
Terminates(SetDown(agent,phone),BusySignal(phone),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Idle(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Idle(phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Terminates(SetDown(agent,phone1),Ringing(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Initiates(PickUp(agent,phone2),Connected(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Terminates(PickUp(agent,phone2),Ringing(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Idle(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Disconnected(phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Terminates(SetDown(agent,phone1),Connected(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone2),Idle(phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone2),Disconnected(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Terminates(SetDown(agent,phone2),Connected(phone1,phone2),time).

[agent,phone,time]
HoldsAt(Disconnected(phone),time) ->
Initiates(SetDown(agent,phone),Idle(phone),time).

[agent,phone,time]
HoldsAt(Disconnected(phone),time) ->
Terminates(SetDown(agent,phone),Disconnected(phone),time).

; Delta

Happens(PickUp(Agent1,Phone1),0).
Happens(Dial(Agent1,Phone1,Phone2),1).
Happens(PickUp(Agent2,Phone2),2).

; Psi

[phone,time]
!HoldsAt(Ringing(phone,phone),time).

[phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) &
phone1!=phone2 ->
!HoldsAt(Ringing(phone2,phone1),time).

[phone,time]
!HoldsAt(Connected(phone,phone),time).

[phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) &
phone1!=phone2 ->
!HoldsAt(Connected(phone2,phone1),time).

mutex Idle, DialTone, BusySignal, Disconnected

[phone1,phone2,time]
HoldsAt(Idle(phone1),time) ->
!HoldsAt(Ringing(phone1,phone2),time) &
!HoldsAt(Connected(phone1,phone2),time).

; etc.

; Gamma

[phone] HoldsAt(Idle(phone),0).

completion Happens

range time 0 3
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter3/Telephone1.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

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
HoldsAt(Idle(phone),time) ->
Initiates(PickUp(agent,phone),DialTone(phone),time).

[agent,phone,time]
HoldsAt(Idle(phone),time) ->
Terminates(PickUp(agent,phone),Idle(phone),time).

[agent,phone,time]
HoldsAt(DialTone(phone),time) ->
Initiates(SetDown(agent,phone),Idle(phone),time).

[agent,phone,time]
HoldsAt(DialTone(phone),time) ->
Terminates(SetDown(agent,phone),DialTone(phone),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
HoldsAt(Idle(phone2),time) ->
Initiates(Dial(agent,phone1,phone2),Ringing(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
HoldsAt(Idle(phone2),time) ->
Terminates(Dial(agent,phone1,phone2),DialTone(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
HoldsAt(Idle(phone2),time) ->
Terminates(Dial(agent,phone1,phone2),Idle(phone2),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
!HoldsAt(Idle(phone2),time) ->
Initiates(Dial(agent,phone1,phone2),BusySignal(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
!HoldsAt(Idle(phone2),time) ->
Terminates(Dial(agent,phone1,phone2),DialTone(phone1),time).

[agent,phone,time]
HoldsAt(BusySignal(phone),time) ->
Initiates(SetDown(agent,phone),Idle(phone),time).

[agent,phone,time]
HoldsAt(BusySignal(phone),time) ->
Terminates(SetDown(agent,phone),BusySignal(phone),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Idle(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Idle(phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Terminates(SetDown(agent,phone1),Ringing(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Initiates(PickUp(agent,phone2),Connected(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Terminates(PickUp(agent,phone2),Ringing(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Idle(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Disconnected(phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Terminates(SetDown(agent,phone1),Connected(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone2),Idle(phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone2),Disconnected(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Terminates(SetDown(agent,phone2),Connected(phone1,phone2),time).

[agent,phone,time]
HoldsAt(Disconnected(phone),time) ->
Initiates(SetDown(agent,phone),Idle(phone),time).

[agent,phone,time]
HoldsAt(Disconnected(phone),time) ->
Terminates(SetDown(agent,phone),Disconnected(phone),time).

; Delta

Delta: Happens(PickUp(Agent1,Phone1),0).
Delta: Happens(Dial(Agent1,Phone1,Phone2),1).
Delta: Happens(PickUp(Agent2,Phone2),2).

; Gamma

[phone] HoldsAt(Idle(phone),0).
[phone] !HoldsAt(DialTone(phone),0).
[phone] !HoldsAt(BusySignal(phone),0).
[phone1,phone2] !HoldsAt(Ringing(phone1,phone2),0).
[phone1,phone2] !HoldsAt(Connected(phone1,phone2),0).
[phone] !HoldsAt(Disconnected(phone),0).

completion Delta Happens

range time 0 3
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter12/DefaultLocation.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object
sort agent: object
sort device: object
sort tv: device
sort room

agent Nathan
tv TV
room LivingRoom, Kitchen

event TurnOn(agent,device)
event Walk(agent,room,room)

fluent InRoom(object,room)
fluent On(device)
fluent PluggedIn(device)
fluent BrokenSwitch(device)

predicate Ab1(device,time)
predicate Ab2(room,time)

; Sigma

[agent,room1,room2,time]
Initiates(Walk(agent,room1,room2),InRoom(agent,room2),time).

[agent,room1,room2,time]
room1!=room2 ->
Terminates(Walk(agent,room1,room2),InRoom(agent,room1),time).

[agent,device,time]
!Ab1(device,time) ->
Initiates(TurnOn(agent,device),On(device),time).

; Delta

[agent,room1,room2,time]
Happens(Walk(agent,room1,room2),time) ->
room1!=room2 &
HoldsAt(InRoom(agent,room1),time).

[agent,device,time]
Happens(TurnOn(agent,device),time) ->
{room} HoldsAt(InRoom(agent,room),time) &
       HoldsAt(InRoom(device,room),time).

[event1,event2,time]
Happens(event1,time) &
Happens(event2,time) ->
event1=event2.

; Theta

Theta: [device,time] HoldsAt(BrokenSwitch(device),time) -> Ab1(device,time).
Theta: [device,time] !HoldsAt(PluggedIn(device),time) -> Ab1(device,time).

; Psi

[object,room1,room2,time]
HoldsAt(InRoom(object,room1),time) &
HoldsAt(InRoom(object,room2),time) ->
room1=room2.

; Gamma

[tv] !HoldsAt(On(tv),0).
[tv] !HoldsAt(BrokenSwitch(tv),0).
[tv] HoldsAt(PluggedIn(tv),0).

HoldsAt(InRoom(Nathan,Kitchen),0).

[time]
!Ab2(LivingRoom,time) ->
{tv} HoldsAt(InRoom(tv,LivingRoom),time).

; goal

{tv} Happens(TurnOn(Nathan,tv),1).

; for two TVs:
;[tv,time] !HoldsAt(InRoom(tv,Kitchen),time).
;[tv,time] {room} HoldsAt(InRoom(tv,room),time).

completion Theta Ab1
completion Theta Ab2

range time 0 2
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter12/Device.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent
sort device

agent Nathan
device Device1, AntiqueDevice1

predicate Ab1(device,time)

fluent On(device)
fluent PluggedIn(device)
fluent BrokenSwitch(device)

event TurnOn(agent,device)

; Sigma

[agent,device,time]
!Ab1(device,time) ->
Initiates(TurnOn(agent,device),On(device),time).

; Delta

Happens(TurnOn(Nathan,Device1),0).

; Theta

Theta: [device,time] HoldsAt(BrokenSwitch(device),time) -> Ab1(device,time).
Theta: [device,time] !HoldsAt(PluggedIn(device),time) -> Ab1(device,time).
Theta: [time] Ab1(AntiqueDevice1,time).

; Gamma

!HoldsAt(On(Device1),0).
!HoldsAt(BrokenSwitch(Device1),0).
HoldsAt(PluggedIn(Device1),0).

; added:
[time] !HoldsAt(On(AntiqueDevice1),time).
[time] HoldsAt(PluggedIn(AntiqueDevice1),time).

; entailed:
; HoldsAt(On(Device1),1).

completion Theta Ab1
completion Happens

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter12/ErraticDevice.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent
sort device

agent Nathan
device Device1

predicate Ab1(device,time)

fluent On(device)
fluent PluggedIn(device)
fluent BrokenSwitch(device)
fluent Erratic(device)

fluent DeterminingFluent(device)
noninertial DeterminingFluent

event TurnOn(agent,device)

; Sigma

[agent,device,time]
!Ab1(device,time) ->
Initiates(TurnOn(agent,device),On(device),time).

; Delta

Happens(TurnOn(Nathan,Device1),0).

; Theta


Theta: [device,time] HoldsAt(BrokenSwitch(device),time) -> Ab1(device,time).
Theta: [device,time]
HoldsAt(Erratic(device),time) & HoldsAt(DeterminingFluent(device),time) ->
Ab1(device,time).

Theta: [device,time] !HoldsAt(PluggedIn(device),time) -> Ab1(device,time).

; Gamma

!HoldsAt(On(Device1),0).
!HoldsAt(BrokenSwitch(Device1),0).
HoldsAt(Erratic(Device1),0).
HoldsAt(PluggedIn(Device1),0).

; added:
HoldsAt(DeterminingFluent(Device1),1).

completion Theta Ab1
completion Happens

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter12/DefaultEvent.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option modeldiff on

load foundations/Root.e
load foundations/EC.e

sort agent
sort clock

fluent Beeping(clock)
fluent AlarmTime(clock,time)
fluent AlarmOn(clock)

event SetAlarmTime(agent,clock,time)
event StartBeeping(clock)
event TurnOnAlarm(agent,clock)
event TurnOffAlarm(agent,clock)

predicate Ab1(clock,time)

agent Nathan
clock Clock

; Sigma

[agent,clock,time1,time2,time]
HoldsAt(AlarmTime(clock,time1),time) &
time1!=time2 ->
Initiates(SetAlarmTime(agent,clock,time2),AlarmTime(clock,time2),time).

[agent,clock,time1,time2,time]
HoldsAt(AlarmTime(clock,time1),time) &
time1!=time2 ->
Terminates(SetAlarmTime(agent,clock,time2),AlarmTime(clock,time1),time).

[agent,clock,time]
Initiates(TurnOnAlarm(agent,clock),AlarmOn(clock),time).

[agent,clock,time]
Terminates(TurnOffAlarm(agent,clock),AlarmOn(clock),time).

[clock,time]
Initiates(StartBeeping(clock),Beeping(clock),time).

[agent,clock,time]
Terminates(TurnOffAlarm(agent,clock),Beeping(clock),time).

; Delta

[clock,time]
HoldsAt(AlarmTime(clock,time),time) &
HoldsAt(AlarmOn(clock),time) &
!Ab1(clock,time) ->
Happens(StartBeeping(clock),time).

Happens(SetAlarmTime(Nathan,Clock,2),0).
Happens(TurnOnAlarm(Nathan,Clock),1).

; Psi

[clock,time1,time2,time]
HoldsAt(AlarmTime(clock,time1),time) &
HoldsAt(AlarmTime(clock,time2),time) ->
time1=time2.

; Gamma

!HoldsAt(AlarmOn(Clock),0).
!HoldsAt(Beeping(Clock),0).
HoldsAt(AlarmTime(Clock,3),0).

completion Happens
completion Theta Ab1

range time 0 3
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter12/MethodD.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; Method (D)
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object

object A,B

fluent P(object)
fluent Q(object)
fluent R(object)

predicate Ab1(object,time)
predicate Ab2(object,time)

[object,time]
HoldsAt(P(object),time) & !Ab1(object,time) ->
HoldsAt(Q(object),time).

[object,time]
HoldsAt(R(object),time) & !Ab2(object,time) ->
!HoldsAt(Q(object),time).

[object,time]
HoldsAt(R(object),time) -> HoldsAt(P(object),time).

HoldsAt(R(A),0).
HoldsAt(P(B),0).
!HoldsAt(R(B),0).

Theta: 
[object,time]
HoldsAt(R(object),time) -> Ab1(object,time).

range time 0 0
range offset 1 1

completion Theta Ab1
completion Theta Ab2

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter12/BrokenDevice.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent
sort device

agent Nathan
device Device1

predicate Ab1(device,time)

fluent On(device)
fluent PluggedIn(device)
fluent BrokenSwitch(device)

event TurnOn(agent,device)

; Sigma

[agent,device,time]
!Ab1(device,time) ->
Initiates(TurnOn(agent,device),On(device),time).

; Delta

Happens(TurnOn(Nathan,Device1),0).

; Theta

Theta: [device,time] HoldsAt(BrokenSwitch(device),time) -> Ab1(device,time).
Theta: [device,time] !HoldsAt(PluggedIn(device),time) -> Ab1(device,time).

; Gamma

!HoldsAt(On(Device1),0).
HoldsAt(BrokenSwitch(Device1),0).

; added:
HoldsAt(PluggedIn(Device1),0).

; entailed:
; !HoldsAt(On(Device1),1).

completion Theta Ab1
completion Happens

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter12/MethodB.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; Method (D)
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object

object A,B

fluent P(object)
fluent Q(object)
predicate Ab(object,time)

[object,time]
HoldsAt(P(object),time) & !Ab(object,time) ->
HoldsAt(Q(object),time).

HoldsAt(P(A),0).
HoldsAt(P(B),0).

Theta: Ab(A,0).

range time 0 0
range offset 1 1

completion Theta Ab

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter13/ModelFinding.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent

fluent Awake(agent)
event WakeUp(agent)

[agent,time] Initiates(WakeUp(agent),Awake(agent),time).
[agent,time] Happens(WakeUp(agent),time) -> !HoldsAt(Awake(agent),time).

agent James

range time 0 1
range offset 1 1



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter13/Postdiction.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent

fluent Awake(agent)
event WakeUp(agent)

[agent,time] Initiates(WakeUp(agent),Awake(agent),time).
[agent,time] Happens(WakeUp(agent),time) -> !HoldsAt(Awake(agent),time).

agent James
Delta: Happens(WakeUp(James),0).
HoldsAt(Awake(James),1).

completion Delta Happens

range time 0 1
range offset 1 1



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter13/Deduction2.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option timediff off

load foundations/Root.e
load foundations/EC.e

sort agent

fluent Awake(agent)
event WakeUp(agent)

[agent,time] Initiates(WakeUp(agent),Awake(agent),time).

agent James
!HoldsAt(Awake(James),0).
Delta: Happens(WakeUp(James),0).

completion Delta Happens

range time 0 1
range offset 1 1



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter13/Deduction1.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent

fluent Awake(agent)
event WakeUp(agent)

[agent,time] Initiates(WakeUp(agent),Awake(agent),time).

agent James
!HoldsAt(Awake(James),0).
Delta: Happens(WakeUp(James),0).

completion Delta Happens

range time 0 1
range offset 1 1



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter13/Abduction.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent

fluent Awake(agent)
event WakeUp(agent)

[agent,time] Initiates(WakeUp(agent),Awake(agent),time).

agent James
!HoldsAt(Awake(James),0).
HoldsAt(Awake(James),1).

range time 0 1
range offset 1 1



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter4/AlarmClock.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent
sort clock

fluent Beeping(clock)
fluent AlarmTime(clock,time)
fluent AlarmOn(clock)

event SetAlarmTime(agent,clock,time)
event StartBeeping(clock)
event TurnOnAlarm(agent,clock)
event TurnOffAlarm(agent,clock)

agent Nathan
clock Clock

; Sigma

[agent,clock,time1,time2,time]
HoldsAt(AlarmTime(clock,time1),time) &
time1!=time2 ->
Initiates(SetAlarmTime(agent,clock,time2),AlarmTime(clock,time2),time).

[agent,clock,time1,time2,time]
HoldsAt(AlarmTime(clock,time1),time) &
time1!=time2 ->
Terminates(SetAlarmTime(agent,clock,time2),AlarmTime(clock,time1),time).

[agent,clock,time]
Initiates(TurnOnAlarm(agent,clock),AlarmOn(clock),time).

[agent,clock,time]
Terminates(TurnOffAlarm(agent,clock),AlarmOn(clock),time).

[clock,time]
Initiates(StartBeeping(clock),Beeping(clock),time).

[agent,clock,time]
Terminates(TurnOffAlarm(agent,clock),Beeping(clock),time).

; Delta

[clock,time]
HoldsAt(AlarmTime(clock,time),time) &
HoldsAt(AlarmOn(clock),time) ->
Happens(StartBeeping(clock),time).

Happens(SetAlarmTime(Nathan,Clock,2),0).
Happens(TurnOnAlarm(Nathan,Clock),1).

; Psi

[clock,time1,time2,time]
HoldsAt(AlarmTime(clock,time1),time) &
HoldsAt(AlarmTime(clock,time2),time) ->
time1=time2.

; Gamma

!HoldsAt(AlarmOn(Clock),0).
!HoldsAt(Beeping(Clock),0).
HoldsAt(AlarmTime(Clock,3),0).

completion Happens

range time 0 3
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter4/BankAccountServiceFee.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option modeldiff on

load foundations/Root.e
load foundations/EC.e

sort account
sort value: integer

account Account1, Account2

predicate EndOfMonth(time)
function ServiceFee(account): value
function MinimumBalance(account): value

fluent ServiceFeeCharged(account)
fluent Balance(account,value)

event Transfer(account,account,value)
event MonthlyReset(account)
event ChargeServiceFee(account)

; Sigma

[account1,account2,value1,value2,value3,value4,time]
HoldsAt(Balance(account1,value1),time) &
HoldsAt(Balance(account2,value2),time) &
value3>0 &
value1>=value3 &
value4=(value2+value3) ->
Initiates(Transfer(account1,account2,value3),Balance(account2,value4),time).

[account1,account2,value1,value2,value3,time]
HoldsAt(Balance(account1,value1),time) &
HoldsAt(Balance(account2,value2),time) &
value3>0 &
value1>=value3 ->
Terminates(Transfer(account1,account2,value3),Balance(account2,value2),time).

[account1,account2,value1,value2,value3,value4,time]
HoldsAt(Balance(account1,value1),time) &
HoldsAt(Balance(account2,value2),time) &
value3>0 &
value1>=value3 &
value4=(value1-value3) ->
Initiates(Transfer(account1,account2,value3),Balance(account1,value4),time).

[account1,account2,value1,value2,value3,time]
HoldsAt(Balance(account1,value1),time) &
HoldsAt(Balance(account2,value2),time) &
value3>0 &
value1>=value3 ->
Terminates(Transfer(account1,account2,value3),Balance(account1,value1),time).

[account,time]
Initiates(ChargeServiceFee(account),ServiceFeeCharged(account),time).

[account,time]
Terminates(MonthlyReset(account),ServiceFeeCharged(account),time).

[account,value1,value2,time]
HoldsAt(Balance(account,value1),time) &
value2 = (value1-ServiceFee(account)) ->
Initiates(ChargeServiceFee(account),
          Balance(account,value2),
          time).

[account,value,time]
HoldsAt(Balance(account,value),time) ->
Terminates(ChargeServiceFee(account),Balance(account,value),time).

; Delta

[account,value,time]
HoldsAt(Balance(account,value),time) &
value<MinimumBalance(account) &
!HoldsAt(ServiceFeeCharged(account),time) ->
Happens(ChargeServiceFee(account),time).

[account,time]
EndOfMonth(time) ->
Happens(MonthlyReset(account),time).

Happens(Transfer(Account1,Account2,1),0).
Happens(Transfer(Account1,Account2,1),0).

; Psi

[account,value1,value2,time]
HoldsAt(Balance(account,value1),time) &
HoldsAt(Balance(account,value2),time) ->
value1=value2.

; Gamma

!HoldsAt(ServiceFeeCharged(Account1),0).
!HoldsAt(ServiceFeeCharged(Account2),0).
HoldsAt(Balance(Account1,3),0).
HoldsAt(Balance(Account2,1),0).
MinimumBalance(Account1)=3.
MinimumBalance(Account2)=1.
ServiceFee(Account1)=1.
ServiceFee(Account2)=1.
[time] !EndOfMonth(time).

completion Happens

range time 0 3
range value 1 3
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Exercises/Counter.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{DeneckerDupreBelleghem:1998,
;   author = "Marc Denecker and Daniele Theseider Dupr\'{e} and Kristof Van Belleghem",
;   year = "1998",
;   title = "An inductive definition approach to ramifications",
;   journal = "Link{\"{o}}ping Electronic Articles in Computer and Information Science",
;   volume = "3",
;   number = "007",
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort counter
counter Counter1

event FalseToTrue(counter)
event TrueToFalse(counter)

fluent Count(counter,integer)
fluent True(counter)
fluent InputLine(counter)
noninertial InputLine

Delta: [counter,time]
!HoldsAt(True(counter),time) &
HoldsAt(InputLine(counter),time) ->
Happens(FalseToTrue(counter),time).

Delta: [counter,time]
HoldsAt(True(counter),time) &
!HoldsAt(InputLine(counter),time) ->
Happens(TrueToFalse(counter),time).

[counter,time] Initiates(FalseToTrue(counter),True(counter),time).

[counter,time] Terminates(TrueToFalse(counter),True(counter),time).

[counter,integer1,integer2,time]
HoldsAt(Count(counter,integer1),time) &
(integer2 = (integer1 + 1)) ->
Initiates(FalseToTrue(counter),Count(counter,integer2),time).

[counter,integer,time]
HoldsAt(Count(counter,integer),time) ->
Terminates(FalseToTrue(counter),Count(counter,integer),time).

[counter,integer1,integer2,time]
HoldsAt(Count(counter,integer1),time) &
HoldsAt(Count(counter,integer2),time) ->
integer1 = integer2.

!HoldsAt(True(Counter1),0).
!HoldsAt(InputLine(Counter1),0).
HoldsAt(InputLine(Counter1),1).
HoldsAt(InputLine(Counter1),2).
HoldsAt(InputLine(Counter1),3).
!HoldsAt(InputLine(Counter1),4).
!HoldsAt(InputLine(Counter1),5).
!HoldsAt(InputLine(Counter1),6).
HoldsAt(InputLine(Counter1),7).
HoldsAt(InputLine(Counter1),8).
HoldsAt(InputLine(Counter1),9).

HoldsAt(Count(Counter1,0),0).

completion Happens

range integer 0 6
range time 0 10
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Exercises/TeacherTells.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option modeldiff on

load foundations/Root.e
load foundations/EC.e

sort agent
sort room
sort fact

agent Teacher, Student
room Kitchen, Classroom
fact Fact1, Fact2

fluent InRoom(agent,room)
fluent ListeningTo(agent,agent)
fluent Know(agent,fact)

event Tell(agent,agent,fact)

; Sigma

[agent1,agent2,fact,time]
({room} HoldsAt(InRoom(agent1,room),time) &
        HoldsAt(InRoom(agent2,room),time)) &
HoldsAt(ListeningTo(agent2,agent1),time) ->
Initiates(Tell(agent1,agent2,fact),Know(agent2,fact),time).

; Delta

Happens(Tell(Teacher,Student,Fact1),0).

; Psi

[agent,room1,room2,time]
HoldsAt(InRoom(agent,room1),time) &
HoldsAt(InRoom(agent,room2),time) ->
room1 = room2.

; Gamma

[agent,fact] !HoldsAt(Know(agent,fact),0).
[agent1,agent2] HoldsAt(ListeningTo(agent1,agent2),0).
[agent] HoldsAt(InRoom(agent,Classroom),0).

completion Happens

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Exercises/MixingPaints.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort palette
sort color

palette Palette1
color Red, Yellow, Blue, Green

event PlaceOnPalette(palette,color)
fluent OnPalette(palette,color)

[palette,color,time]
!Happens(PlaceOnPalette(palette,Yellow),time) |
!Happens(PlaceOnPalette(palette,Blue),time) ->
Initiates(PlaceOnPalette(palette,color),OnPalette(palette,color),time).

[palette,color1,color2,time]
Happens(PlaceOnPalette(palette,Yellow),time) &
color1 = Blue &
color2 = Green ->
Initiates(PlaceOnPalette(palette,color1),OnPalette(palette,color2),time).

[palette,color1,color2,time]
!(Happens(PlaceOnPalette(palette,Yellow),time) &
  Happens(PlaceOnPalette(palette,Blue),time)) &
HoldsAt(OnPalette(palette,color1),time) &
color1 != color2 ->
Terminates(PlaceOnPalette(palette,color2),OnPalette(palette,color1),time).

[palette,color1,color2,time]
Happens(PlaceOnPalette(palette,Yellow),time) &
HoldsAt(OnPalette(palette,color2),time) &
color1 = Blue &
color2 != Green ->
Terminates(PlaceOnPalette(palette,color1),OnPalette(palette,color2),time).

; state constraint

[palette,color1,color2,time]
HoldsAt(OnPalette(palette,color1),time) &
HoldsAt(OnPalette(palette,color2),time) ->
color1 = color2.

; (1) place green over red
HoldsAt(OnPalette(Palette1,Red),0).
Delta: Happens(PlaceOnPalette(Palette1,Green),0).

; (2) place yellow+blue over green
Delta: Happens(PlaceOnPalette(Palette1,Yellow),1).
Delta: Happens(PlaceOnPalette(Palette1,Blue),1).

; (3) place yellow
Delta: Happens(PlaceOnPalette(Palette1,Yellow),2).

; (4) place blue
Delta: Happens(PlaceOnPalette(Palette1,Blue),3).

; (5) place green
Delta: Happens(PlaceOnPalette(Palette1,Yellow),4).
Delta: Happens(PlaceOnPalette(Palette1,Blue),4).

completion Delta Happens

range time 0 5
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Exercises/SnoozeAlarm.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; Example: Alarm Clock with snooze alarm added
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent
sort clock

fluent Beeping(clock)
fluent AlarmTime(clock,time)
fluent AlarmOn(clock)

event SetAlarmTime(agent,clock,time)
event StartBeeping(clock)
event TurnOnAlarm(agent,clock)
event TurnOffAlarm(agent,clock)

event PressSnooze(agent,clock)

agent Nathan
clock Clock

; Sigma

[agent,clock,time1,time2,time]
HoldsAt(AlarmTime(clock,time1),time) &
time1!=time2 ->
Initiates(SetAlarmTime(agent,clock,time2),AlarmTime(clock,time2),time).

[agent,clock,time1,time2,time]
HoldsAt(AlarmTime(clock,time1),time) &
time1!=time2 ->
Terminates(SetAlarmTime(agent,clock,time2),AlarmTime(clock,time1),time).

[agent,clock,time]
Initiates(TurnOnAlarm(agent,clock),AlarmOn(clock),time).

[agent,clock,time]
Terminates(TurnOffAlarm(agent,clock),AlarmOn(clock),time).

[clock,time]
Initiates(StartBeeping(clock),Beeping(clock),time).

[agent,clock,time]
Terminates(TurnOffAlarm(agent,clock),Beeping(clock),time).

; added axioms:

[agent,clock,time2,time]
HoldsAt(Beeping(clock),time) &
time2 = time+9 ->
Initiates(PressSnooze(agent,clock),AlarmTime(clock,time2),time).

[agent,clock,time1,time2,time]
HoldsAt(Beeping(clock),time) &
HoldsAt(AlarmTime(clock,time1),time) &
time2 = time+9 &
time1 != time2 ->
Terminates(PressSnooze(agent,clock),AlarmTime(clock,time1),time).

[agent,clock,time]
Terminates(PressSnooze(agent,clock),Beeping(clock),time).

; Delta

[clock,time]
HoldsAt(AlarmTime(clock,time),time) &
HoldsAt(AlarmOn(clock),time) ->
Happens(StartBeeping(clock),time).

Happens(SetAlarmTime(Nathan,Clock,2),0).
Happens(TurnOnAlarm(Nathan,Clock),1).
Happens(PressSnooze(Nathan,Clock),4).

; Psi

[clock,time1,time2,time]
HoldsAt(AlarmTime(clock,time1),time) &
HoldsAt(AlarmTime(clock,time2),time) ->
time1=time2.

; Gamma

!HoldsAt(AlarmOn(Clock),0).
!HoldsAt(Beeping(Clock),0).
HoldsAt(AlarmTime(Clock,3),0).

completion Happens

range time 0 15
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Exercises/TelephoneBugs.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; Example: Telephone
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

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
HoldsAt(Idle(phone),time) ->
Initiates(PickUp(agent,phone),DialTone(phone),time).

[agent,phone,time]
HoldsAt(Idle(phone),time) ->
Terminates(PickUp(agent,phone),Idle(phone),time).

[agent,phone,time]
HoldsAt(DialTone(phone),time) ->
Initiates(SetDown(agent,phone),Idle(phone),time).

[agent,phone,time]
HoldsAt(DialTone(phone),time) ->
Terminates(SetDown(agent,phone),DialTone(phone),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
HoldsAt(Idle(phone2),time) ->
Initiates(Dial(agent,phone1,phone2),Ringing(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
HoldsAt(Idle(phone2),time) ->
Terminates(Dial(agent,phone1,phone2),DialTone(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
HoldsAt(Idle(phone2),time) ->
Terminates(Dial(agent,phone1,phone2),Idle(phone2),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
!HoldsAt(Idle(phone2),time) ->
Initiates(Dial(agent,phone1,phone2),BusySignal(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
!HoldsAt(Idle(phone2),time) ->
Terminates(Dial(agent,phone1,phone2),DialTone(phone1),time).

[agent,phone,time]
HoldsAt(BusySignal(phone),time) ->
Initiates(SetDown(agent,phone),Idle(phone),time).

[agent,phone,time]
HoldsAt(BusySignal(phone),time) ->
Terminates(SetDown(agent,phone),BusySignal(phone),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Idle(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Idle(phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Terminates(SetDown(agent,phone1),Ringing(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Initiates(PickUp(agent,phone2),Connected(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Terminates(PickUp(agent,phone2),Ringing(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Idle(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Disconnected(phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Terminates(SetDown(agent,phone1),Connected(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone2),Idle(phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone2),Disconnected(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Terminates(SetDown(agent,phone2),Connected(phone1,phone2),time).

[agent,phone,time]
HoldsAt(Disconnected(phone),time) ->
Initiates(SetDown(agent,phone),Idle(phone),time).

[agent,phone,time]
HoldsAt(Disconnected(phone),time) ->
Terminates(SetDown(agent,phone),Disconnected(phone),time).

; Delta

; (1) Two agents dial each other simultaneously without first
; picking up phone.
Happens(Dial(Agent1,Phone1,Phone2),0).
Happens(Dial(Agent2,Phone2,Phone1),0).

; (2) Two agents dial each other simultaneously.
Happens(PickUp(Agent1,Phone1),1).
Happens(PickUp(Agent2,Phone2),1).
Happens(Dial(Agent1,Phone1,Phone2),2).
Happens(Dial(Agent2,Phone2,Phone1),2).
Happens(SetDown(Agent1,Phone1),3).
Happens(SetDown(Agent2,Phone2),3).

; (3) One agent dials another agent just as the other
; agent picks up the phone.
Happens(PickUp(Agent1,Phone1),4).
Happens(Dial(Agent1,Phone1,Phone2),5).
Happens(PickUp(Agent2,Phone2),5).

; Psi

[phone,time]
!HoldsAt(Ringing(phone,phone),time).

[phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) &
phone1!=phone2 ->
!HoldsAt(Ringing(phone2,phone1),time).

[phone,time]
!HoldsAt(Connected(phone,phone),time).

[phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) &
phone1!=phone2 ->
!HoldsAt(Connected(phone2,phone1),time).

mutex Idle, DialTone, BusySignal, Disconnected

[phone1,phone2,time]
HoldsAt(Idle(phone1),time) ->
!HoldsAt(Ringing(phone1,phone2),time) &
!HoldsAt(Connected(phone1,phone2),time).

; contradicts (3) above:
;[phone1,phone2,time]
;HoldsAt(DialTone(phone2),time) ->
;!HoldsAt(Ringing(phone1,phone2),time) &
;!HoldsAt(Connected(phone1,phone2),time).

; etc.

; Gamma

[phone] HoldsAt(Idle(phone),0).

completion Happens

range time 0 6
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter11/HungryCat.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @inproceedings{WinikoffEtAl:2002,
;   author = "Michael Winikoff and Lin Padgham and James Harland and John Thangarajah",
;   year = "2002",
;   title = "Declarative \& procedural goals in intelligent agent systems",
;   editor = "Dieter Fensel and Fausto Giunchiglia and Deborah McGuinness and Mary-Anne Williams",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{E}ighth \uppercase{I}nternational \uppercase{C}onference on \uppercase{P}rinciples of \uppercase{K}nowledge \uppercase{R}epresentation and \uppercase{R}easoning",
;   pages = "470--481",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann",
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object
sort agent: object
sort food: object
sort surface
sort plan

reified sort belief

agent Cat
surface Floor, Chair, Shelf, Table
food Food1, Food2
plan P1, P1a, P1b, P2, P2a

predicate SelectedPlan(agent,belief,plan,time)
predicate SoundPlan(agent,belief,plan,time)

fluent On(object,surface)
fluent Goal(agent,belief)
fluent CanJump(surface,surface)
fluent Plan(agent,belief,plan)
fluent Satiated(agent)
fluent Believe(agent,belief)

event AddPlan(agent,belief,plan)
event DropPlan(agent,belief,plan)
event Jump(agent,surface,surface)
event Move(surface,surface,surface)
event Eat(agent,food)
event Wait(agent)

belief BSatiated(agent)
belief BCanJump(surface,surface)
belief BOn(object,surface)

; Sigma

; A5
[agent,belief,plan,time]
Initiates(AddPlan(agent,belief,plan),Plan(agent,belief,plan),time).

; A6
[agent,belief,plan,time]
Terminates(DropPlan(agent,belief,plan),Plan(agent,belief,plan),time).

[agent,surface1,surface2,time]
HoldsAt(On(agent,surface1),time) &
HoldsAt(CanJump(surface1,surface2),time) ->
Initiates(Jump(agent,surface1,surface2),On(agent,surface2),time).

[agent,surface1,surface2,time]
HoldsAt(On(agent,surface1),time) &
HoldsAt(CanJump(surface1,surface2),time) ->
Terminates(Jump(agent,surface1,surface2),On(agent,surface1),time).

[surface1,surface2,surface3,time]
Initiates(Move(surface1,surface2,surface3),CanJump(surface1,surface3),time).

[surface1,surface2,surface3,time]
Terminates(Move(surface1,surface2,surface3),CanJump(surface1,surface2),time).

[agent,food,surface,time]
HoldsAt(On(agent,surface),time) &
HoldsAt(On(food,surface),time) ->
Initiates(Eat(agent,food),Satiated(agent),time).

[agent,food,surface,time]
HoldsAt(On(agent,surface),time) &
HoldsAt(On(food,surface),time) ->
Terminates(Eat(agent,food),On(food,surface),time).

[agent,surface1,surface2,belief,time]
HoldsAt(Believe(agent,BOn(agent,surface1)),time) &
HoldsAt(Believe(agent,BCanJump(surface1,surface2)),time) &
(belief = BOn(agent,surface2)) ->
Initiates(Jump(agent,surface1,surface2),
          Believe(agent,belief),
          time).

[agent,surface1,surface2,belief,time]
HoldsAt(Believe(agent,BOn(agent,surface1)),time) &
HoldsAt(Believe(agent,BCanJump(surface1,surface2)),time) &
(belief = BOn(agent,surface1)) ->
Terminates(Jump(agent,surface1,surface2),
           Believe(agent,belief),
           time).

[agent,surface1,surface2,surface3,belief,time]
(belief = BCanJump(surface1,surface3)) ->
Initiates(Move(surface1,surface2,surface3),
          Believe(agent,belief),
          time).

[agent,surface1,surface2,surface3,belief,time]
(belief = BCanJump(surface1,surface2)) ->
Terminates(Move(surface1,surface2,surface3),
           Believe(agent,belief),
           time).

[agent,food,surface,belief,time]
HoldsAt(Believe(agent,BOn(agent,surface)),time) &
HoldsAt(Believe(agent,BOn(food,surface)),time) &
(belief = BSatiated(agent)) ->
Initiates(Eat(agent,food),Believe(agent,belief),time).

[agent,food,surface,belief,time]
HoldsAt(Believe(agent,BOn(agent,surface)),time) &
HoldsAt(Believe(agent,BOn(food,surface)),time) &
(belief = BOn(food,surface)) ->
Terminates(Eat(agent,food),Believe(agent,belief),time).

; Delta

; A7
[agent,belief,plan,time]
HoldsAt(Goal(agent,belief),time) &
!HoldsAt(Believe(agent,belief),time) &
SelectedPlan(agent,belief,plan,time) &
(!{plan1} HoldsAt(Plan(agent,belief,plan1),time)) ->
Happens(AddPlan(agent,belief,plan),time).

; A8
[agent,belief,time]
HoldsAt(Plan(agent,belief,P1),time) &
!HoldsAt(Believe(agent,belief),time) &
SoundPlan(agent,belief,P1,time) ->
Happens(Jump(Cat,Floor,Chair),time).

[agent,belief,time]
HoldsAt(Plan(agent,belief,P1a),time) &
!HoldsAt(Believe(agent,belief),time) &
SoundPlan(agent,belief,P1a,time) ->
Happens(Wait(Cat),time).

[agent,belief,time]
HoldsAt(Plan(agent,belief,P2),time) &
!HoldsAt(Believe(agent,belief),time) &
SoundPlan(agent,belief,P2,time) ->
Happens(Jump(Cat,Chair,Shelf),time).

; A9
[agent,belief,plan,time]
HoldsAt(Plan(agent,belief,plan),time) ->
Happens(DropPlan(agent,belief,plan),time).

; A10
[agent,belief,time]
HoldsAt(Plan(agent,belief,P1),time) &
!HoldsAt(Believe(agent,belief),time) &
SoundPlan(agent,belief,P1,time) ->
Happens(AddPlan(agent,belief,P1a),time).

[agent,belief,time]
HoldsAt(Plan(agent,belief,P1a),time) &
!HoldsAt(Believe(agent,belief),time) &
SoundPlan(agent,belief,P1a,time) ->
Happens(AddPlan(agent,belief,P1b),time).

[agent,belief,time]
HoldsAt(Plan(agent,belief,P2),time) &
!HoldsAt(Believe(agent,belief),time) &
SoundPlan(agent,belief,P2,time) ->
Happens(AddPlan(agent,belief,P2a),time).

; reactive behavior
[agent,food,surface,time]
!HoldsAt(Satiated(agent),time) &
HoldsAt(On(agent,surface),time) &
HoldsAt(On(food,surface),time) ->
Happens(Eat(agent,food),time).

; narrative

Happens(Move(Chair,Table,Shelf),2).

; SelectedPlan - plan library

;[agent,belief,plan,time]
;SelectedPlan(agent,belief,plan,time) <->
;(agent=Cat & belief=BSatiated(Cat) & plan=P1 & time=0) |
;(agent=Cat & belief=BSatiated(Cat) & plan=P2 & time=4).

[agent,belief,plan,time]
SelectedPlan(agent,belief,plan,time) <->
({surface1,surface2,surface3,food}
 HoldsAt(Believe(agent,BOn(agent,surface1)),time) &
 HoldsAt(Believe(agent,BCanJump(surface1,surface2)),time) &
 HoldsAt(Believe(agent,BCanJump(surface2,surface3)),time) &
 HoldsAt(Believe(agent,BOn(food,surface3)),time) &
 belief=BSatiated(agent) &
 plan=P1 &
 time=0) |
({surface1,surface2,surface3,food}
 HoldsAt(Believe(agent,BOn(agent,surface1)),time) &
 HoldsAt(Believe(agent,BCanJump(surface1,surface2)),time) &
 HoldsAt(Believe(agent,BCanJump(surface2,surface3)),time) &
 HoldsAt(Believe(agent,BOn(food,surface3)),time) &
 belief=BSatiated(agent) &
 plan=P2 &
 time=4).


; SoundPlan

[agent,belief,plan,time]
SoundPlan(agent,belief,plan,time) <->
(plan=P1 ->
 HoldsAt(Believe(agent,BCanJump(Floor,Chair)),time) &
 HoldsAt(Believe(agent,BCanJump(Chair,Table)),time)) &
((plan=P1a | plan=P1b) ->
  HoldsAt(Believe(agent,BCanJump(Chair,Table)),time)).

; Gamma

[agent,belief]
HoldsAt(Goal(agent,belief),0) <->
(agent=Cat & belief=BSatiated(Cat)).

[agent,belief,plan] !HoldsAt(Plan(agent,belief,plan),0).

[object,surface] HoldsAt(On(object,surface),0) <->
(object=Cat & surface=Floor) |
(object=Food1 & surface=Table) |
(object=Food2 & surface=Shelf).

[surface1,surface2] HoldsAt(CanJump(surface1,surface2),0) <->
(surface1=Floor & surface2=Chair) |
(surface1=Chair & surface2=Table) |
(surface1=Shelf & surface2=Table).

[agent,object,surface]
HoldsAt(Believe(agent,BOn(object,surface)),0) <->
(agent=Cat & object=Cat & surface=Floor) |
(agent=Cat & object=Food1 & surface=Table).

[agent,surface1,surface2]
HoldsAt(Believe(agent,BCanJump(surface1,surface2)),0) <->
(agent=Cat & surface1=Floor & surface2=Chair) |
(agent=Cat & surface1=Chair & surface2=Table) |
(agent=Cat & surface1=Shelf & surface2=Table).

!HoldsAt(Believe(Cat,BSatiated(Cat)),0).

; ADDED:
!HoldsAt(Satiated(Cat),0).

completion Happens

range time 0 7
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter11/Lottery.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{OrtonyCloreCollins:1988,
;   author = "Andrew Ortony and Gerald L. Clore and Allan M. Collins",
;   year = "1988",
;   title = "The Cognitive Structure of Emotions",
;   address = "Cambridge",
;   publisher = "Cambridge University Press",
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option modeldiff on

load foundations/Root.e
load foundations/EC.e

sort agent
sort aboutevent
sort desirability: integer

agent Kate, Lisa
aboutevent WinLotteryKate, WinLotteryLisa

fluent Joy(agent,aboutevent)
fluent Desirability(agent,agent,aboutevent,desirability)
fluent Believe(agent,aboutevent)
fluent Like(agent,agent)
fluent HappyFor(agent,agent,aboutevent)

event WinLottery(agent)
event AddJoy(agent,aboutevent)
event AddHappyFor(agent,agent,aboutevent)

; Sigma

[agent,aboutevent,time]
Initiates(AddJoy(agent,aboutevent),Joy(agent,aboutevent),time).

[agent1,agent2,aboutevent,time]
Initiates(AddHappyFor(agent1,agent2,aboutevent),
          HappyFor(agent1,agent2,aboutevent),
          time).

[agent1,agent2,aboutevent,time]
(agent1=Kate & aboutevent=WinLotteryKate) |
(agent1=Lisa & aboutevent=WinLotteryLisa) ->
Initiates(WinLottery(agent1),Believe(agent2,aboutevent),time).

; Delta

[agent,aboutevent,desirability,time]
!HoldsAt(Joy(agent,aboutevent),time) &
HoldsAt(Desirability(agent,agent,aboutevent,desirability),time) &
desirability=1 &
HoldsAt(Believe(agent,aboutevent),time) ->
Happens(AddJoy(agent,aboutevent),time).

[agent1,agent2,aboutevent,desirability1,desirability2,time]
!HoldsAt(HappyFor(agent1,agent2,aboutevent),time) &
HoldsAt(Desirability(agent1,agent2,aboutevent,desirability1),time) &
desirability1=1 &
HoldsAt(Desirability(agent1,agent1,aboutevent,desirability2),time) &
desirability2=1 &
HoldsAt(Like(agent1,agent2),time) &
HoldsAt(Believe(agent1,aboutevent),time) &
agent1 != agent2 ->
Happens(AddHappyFor(agent1,agent2,aboutevent),time).

Happens(WinLottery(Kate),0).

; Psi

[agent1,agent2,aboutevent,desirability1,desirability2,time]
HoldsAt(Desirability(agent1,agent2,aboutevent,desirability1),time) &
HoldsAt(Desirability(agent1,agent2,aboutevent,desirability2),time) ->
desirability1 = desirability2.

; Gamma

[agent,aboutevent] !HoldsAt(Joy(agent,aboutevent),0).
[agent1,agent2,aboutevent] !HoldsAt(HappyFor(agent1,agent2,aboutevent),0).
[aboutevent] !HoldsAt(Believe(Kate,aboutevent),0).
[aboutevent] !HoldsAt(Believe(Lisa,aboutevent),0).
[agent1,agent2,time] HoldsAt(Like(agent1,agent2),time).

[time] HoldsAt(Desirability(Lisa,Kate,WinLotteryKate,1),time).
[time] HoldsAt(Desirability(Kate,Kate,WinLotteryKate,1),time).
[time] HoldsAt(Desirability(Lisa,Lisa,WinLotteryKate,1),time).
[time] HoldsAt(Desirability(Kate,Kate,WinLotteryLisa,0),time).
[time] HoldsAt(Desirability(Kate,Lisa,WinLotteryLisa,0),time).
[time] HoldsAt(Desirability(Kate,Kate,WinLotteryLisa,0),time).
[time] HoldsAt(Desirability(Kate,Lisa,WinLotteryKate,0),time).
[time] HoldsAt(Desirability(Lisa,Lisa,WinLotteryLisa,0),time).
[time] HoldsAt(Desirability(Lisa,Kate,WinLotteryLisa,1),time).

completion Happens

range time 0 3
range desirability -1 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Manual/Example1a.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; deduction

option timediff off

load foundations/Root.e
load foundations/EC.e

sort agent

fluent Awake(agent)
event WakeUp(agent)

[agent,time] Initiates(WakeUp(agent),Awake(agent),time).

agent James
!HoldsAt(Awake(James),0).
Delta: Happens(WakeUp(James),0).

completion Delta Happens

range time 0 1
range offset 1 1



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Manual/Example1.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; deduction

load foundations/Root.e
load foundations/EC.e

sort agent

fluent Awake(agent)
event WakeUp(agent)

[agent,time] Initiates(WakeUp(agent),Awake(agent),time).

agent James
!HoldsAt(Awake(James),0).
Delta: Happens(WakeUp(James),0).

completion Delta Happens

range time 0 1
range offset 1 1



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Manual/Example4.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
load foundations/Root.e
load foundations/EC.e

sort agent

fluent Awake(agent)
event WakeUp(agent)

[agent,time] Initiates(WakeUp(agent),Awake(agent),time).
[agent,time] Happens(WakeUp(agent),time) -> !HoldsAt(Awake(agent),time).

agent James, Jessie
!HoldsAt(Awake(James),0).
!HoldsAt(Awake(Jessie),0).
HoldsAt(Awake(James),1).

range time 0 1
range offset 1 1



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Manual/Example3.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
load foundations/Root.e
load foundations/EC.e

sort agent

fluent Awake(agent)
event WakeUp(agent)

[agent,time] Initiates(WakeUp(agent),Awake(agent),time).

agent James, Jessie
!HoldsAt(Awake(James),0).
HoldsAt(Awake(James),1).

range time 0 1
range offset 1 1



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Manual/Example2.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
load foundations/Root.e
load foundations/EC.e

sort agent

fluent Awake(agent)
event WakeUp(agent)

[agent,time] Initiates(WakeUp(agent),Awake(agent),time).

agent James
!HoldsAt(Awake(James),0).
HoldsAt(Awake(James),1).

range time 0 1
range offset 1 1



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2004b/RunningAndDriving2.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @inproceedings{Mueller:2004b,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
;   editor = "Valerie Barr and Zdravko Markov",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
;   pages = "147--152",
;   address = "Menlo Park, CA",
;   publisher = "AAAI Press",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent

fluent Tired(agent)

event Move(agent)
event Run(agent)
event Drive(agent)

[agent,time]
Happens(Move(agent),time) ->
Happens(Run(agent),time) | Happens(Drive(agent),time).

xor Run, Drive

[agent,time] Initiates(Run(agent),Tired(agent),time).

agent James

!HoldsAt(Tired(James),0).
Happens(Move(James),0).

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2004b/OffOn.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @inproceedings{Mueller:2004b,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
;   editor = "Valerie Barr and Zdravko Markov",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
;   pages = "147--152",
;   address = "Menlo Park, CA",
;   publisher = "AAAI Press",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent
sort switch

fluent On(switch)
fluent Off(switch)
event TurnOn(agent,switch)
event TurnOff(agent,switch)

noninertial Off

[switch,time] HoldsAt(Off(switch),time) <-> !HoldsAt(On(switch),time).

[agent,switch,time] Initiates(TurnOn(agent,switch),On(switch),time).
[agent,switch,time] Terminates(TurnOff(agent,switch),On(switch),time).

agent James
switch Switch1

!HoldsAt(On(Switch1),0).
Happens(TurnOn(James,Switch1),0).

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2004b/TV2.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @inproceedings{Mueller:2004b,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
;   editor = "Valerie Barr and Zdravko Markov",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
;   pages = "147--152",
;   address = "Menlo Park, CA",
;   publisher = "AAAI Press",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent
sort switch
sort tv

function TVOf(switch): tv
fluent SwitchOn(switch)
fluent TVOn(tv)
fluent PluggedIn(tv)
event TurnOn(agent,switch)
event TurnOff(agent,switch)

[agent,switch,time] Initiates(TurnOn(agent,switch),SwitchOn(switch),time).

[agent,switch,tv,time]
TVOf(switch)=tv & HoldsAt(PluggedIn(tv),time) ->
Initiates(TurnOn(agent,switch),TVOn(tv),time).

agent James
switch Switch1
tv TV1

TVOf(Switch1)=TV1.
!HoldsAt(PluggedIn(TV1),0).
!HoldsAt(SwitchOn(Switch1),0).
!HoldsAt(TVOn(TV1),0).
Happens(TurnOn(James,Switch1),0).

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2004b/Approve.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; example of concurrent events with cumulative or canceling effects
;
; @inproceedings{Mueller:2004b,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
;   editor = "Valerie Barr and Zdravko Markov",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
;   pages = "147--152",
;   address = "Menlo Park, CA",
;   publisher = "AAAI Press",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent

event ApproveOf(agent,agent)
event DisapproveOf(agent,agent)
fluent Happy(agent)
fluent Confused(agent)

[agent1,agent2,time]
!Happens(DisapproveOf(agent1,agent2),time) ->
Initiates(ApproveOf(agent1,agent2),Happy(agent2),time).

[agent1,agent2,time]
!Happens(ApproveOf(agent1,agent2),time) ->
Terminates(DisapproveOf(agent1,agent2),Happy(agent2),time).

[agent1,agent2,time]
Happens(DisapproveOf(agent1,agent2),time) ->
Initiates(ApproveOf(agent1,agent2),Confused(agent2),time).

agent James, Peter

[agent] !HoldsAt(Happy(agent),0) & !HoldsAt(Confused(agent),0).

Happens(ApproveOf(Peter,James),0).
Happens(DisapproveOf(Peter,James),0).

completion Happens

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2004b/Leaf.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @inproceedings{Mueller:2004b,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
;   editor = "Valerie Barr and Zdravko Markov",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
;   pages = "147--152",
;   address = "Menlo Park, CA",
;   publisher = "AAAI Press",
; }
;

option trajectory on

load foundations/Root.e
load foundations/EC.e

sort object
sort height: integer

fluent Height(object,height)
fluent Falling(object)
event StartFalling(object)
event HitsGround(object)

[object,height1,height2,time]
HoldsAt(Height(object,height1),time) &
HoldsAt(Height(object,height2),time) ->
height1=height2.

[object,time]
Initiates(StartFalling(object),Falling(object),time).

[object,height,time]
Releases(StartFalling(object),Height(object,height),time).

[object,height1,height2,offset,time]
HoldsAt(Height(object,height1),time) &
height2=height1-offset ->
Trajectory(Falling(object),time,Height(object,height2),offset).

[object,time]
HoldsAt(Falling(object),time) &
HoldsAt(Height(object,0),time) ->
Happens(HitsGround(object),time).

;[object,height1,height2,time]
;HoldsAt(Height(object,height1),time) &
;height1 != height2 ->
;Terminates(HitsGround(object),Height(object,height2),time).

[object,height,time]
HoldsAt(Height(object,height),time) ->
Initiates(HitsGround(object),Height(object,height),time).

[object,time]
Terminates(HitsGround(object),Falling(object),time).

object Leaf

!HoldsAt(Falling(Leaf),0).
HoldsAt(Height(Leaf,4),0).
Happens(StartFalling(Leaf),2).

completion Happens

range time 0 7
range offset 1 4
range height 0 4

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2004b/RunningAndDriving1.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @inproceedings{Mueller:2004b,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
;   editor = "Valerie Barr and Zdravko Markov",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
;   pages = "147--152",
;   address = "Menlo Park, CA",
;   publisher = "AAAI Press",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent

fluent Tired(agent)

event Move(agent)
event Run(agent)
event Drive(agent)

[agent,time]
Happens(Move(agent),time) ->
Happens(Run(agent),time) | Happens(Drive(agent),time).

xor Run, Drive

[agent,time] Initiates(Run(agent),Tired(agent),time).

agent James

!HoldsAt(Tired(James),0).
Happens(Move(James),0).
HoldsAt(Tired(James),1).

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2004b/TV1.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @inproceedings{Mueller:2004b,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
;   editor = "Valerie Barr and Zdravko Markov",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
;   pages = "147--152",
;   address = "Menlo Park, CA",
;   publisher = "AAAI Press",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent
sort switch
sort tv

function TVOf(switch): tv
fluent SwitchOn(switch)
fluent TVOn(tv)
fluent PluggedIn(tv)
event TurnOn(agent,switch)
event TurnOff(agent,switch)

[agent,switch,time] Initiates(TurnOn(agent,switch),SwitchOn(switch),time).

[agent,switch,tv,time]
TVOf(switch)=tv & HoldsAt(PluggedIn(tv),time) ->
Initiates(TurnOn(agent,switch),TVOn(tv),time).

agent James
switch Switch1
tv TV1

TVOf(Switch1)=TV1.
HoldsAt(PluggedIn(TV1),0).
!HoldsAt(SwitchOn(Switch1),0).
!HoldsAt(TVOn(TV1),0).
Happens(TurnOn(James,Switch1),0).

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2004b/RouletteWheel.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @inproceedings{Mueller:2004b,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
;   editor = "Valerie Barr and Zdravko Markov",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
;   pages = "147--152",
;   address = "Menlo Park, CA",
;   publisher = "AAAI Press",
; }
;

load foundations/Root.e
load foundations/EC.e

sort wheel
sort value: integer

fluent WheelValueDeterminingFluent(wheel,value)
fluent WheelValue(wheel,value)
noninertial WheelValueDeterminingFluent
event Spin(wheel)

[wheel,value1,value2,time]
HoldsAt(WheelValue(wheel,value1),time) &
HoldsAt(WheelValue(wheel,value2),time) ->
value1=value2.

[wheel,value1,value2,time]
HoldsAt(WheelValueDeterminingFluent(wheel,value1),time) &
HoldsAt(WheelValueDeterminingFluent(wheel,value2),time) ->
value1=value2.

[wheel,value,time]
HoldsAt(WheelValueDeterminingFluent(wheel,value),time) ->
Initiates(Spin(wheel),WheelValue(wheel,value),time).

[wheel,value1,value2,time]
HoldsAt(WheelValue(wheel,value1),time) &
HoldsAt(WheelValueDeterminingFluent(wheel,value2),time) &
value1!=value2 ->
Terminates(Spin(wheel),WheelValue(wheel,value1),time).

[wheel,time]
{value} HoldsAt(WheelValueDeterminingFluent(wheel,value),time).

wheel Wheel

HoldsAt(WheelValue(Wheel,7),0).
Happens(Spin(Wheel),0).
HoldsAt(WheelValueDeterminingFluent(Wheel,7),1).

completion Happens

range value 7 10
range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2004b/PickUp.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @inproceedings{Mueller:2004b,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
;   editor = "Valerie Barr and Zdravko Markov",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
;   pages = "147--152",
;   address = "Menlo Park, CA",
;   publisher = "AAAI Press",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object
sort agent: object
sort physobj: object
sort location

fluent At(object,location)
fluent Holding(agent,physobj)
event PickUp(agent,physobj)
event SetDown(agent,physobj)
event Move(agent,location,location)

; state constraints

[agent,location,physobj,time]
HoldsAt(At(agent,location),time) &
HoldsAt(Holding(agent,physobj),time) ->
HoldsAt(At(physobj,location),time).

[object,location1,location2,time]
HoldsAt(At(object,location1),time) &
HoldsAt(At(object,location2),time) ->
location1=location2.

; effect axioms

[agent,location1,location2,time]
Initiates(Move(agent,location1,location2),At(agent,location2),time).

[agent,location1,location2,time]
Terminates(Move(agent,location1,location2),At(agent,location1),time).

[agent,physobj,time]
Initiates(PickUp(agent,physobj),Holding(agent,physobj),time).

[agent,physobj,time]
Terminates(SetDown(agent,physobj),Holding(agent,physobj),time).

; preconditions

[agent,location1,location2,time]
Happens(Move(agent,location1,location2),time) ->
HoldsAt(At(agent,location1),time).

[agent,physobj,time]
Happens(PickUp(agent,physobj),time) ->
{location} HoldsAt(At(agent,location),time) &
           HoldsAt(At(physobj,location),time).

; releases

[agent,physobj,location,time]
Releases(PickUp(agent,physobj),At(physobj,location),time).

[agent,physobj,location,time]
HoldsAt(At(agent,location),time) ->
Initiates(SetDown(agent,physobj),At(physobj,location),time).

;[agent,physobj,location1,location2,time]
;HoldsAt(At(agent,location1),time) &
;location1 != location2 ->
;Terminates(SetDown(agent,physobj),At(physobj,location2),time).

agent James
physobj Coin
location L1, L2, L3, L4

!HoldsAt(Holding(James,Coin),0).
HoldsAt(At(Coin,L4),0).
HoldsAt(At(James,L1),0).
Happens(Move(James,L1,L2),0).
Happens(Move(James,L2,L3),1).
Happens(Move(James,L3,L4),2).
Happens(PickUp(James,Coin),3).
Happens(Move(James,L4,L3),4).
Happens(Move(James,L3,L2),5).
Happens(SetDown(James,Coin),6).
Happens(Move(James,L2,L3),7).
Happens(Move(James,L3,L4),8).

completion Happens

range time 0 9
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/FrankEtAl2003/Story1.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{FrankEtAl:2003,
;   author = "Stefan L. Frank and Mathieu Koppen and Leo G. M. Noordman and Wietske Vonk",
;   year = "2003",
;   title = "Modeling knowledge-based inferences in story comprehension",
;   journal = "Cognitive Science",
;   volume = "27",
;   pages = "875--910",
; }
;

option modeldiff on

load foundations/Root.e
load foundations/EC.e

sort agent

load examples/FrankEtAl2003/FrankEtAl.e

agent Bob, Jilly

!HoldsAt(Raining(),0).
!HoldsAt(SunShining(),0).

(HoldsAt(PlaySoccer(Bob),1) & HoldsAt(PlaySoccer(Jilly),1)) |
(HoldsAt(PlayHideAndSeek(Bob),1) & HoldsAt(PlayHideAndSeek(Jilly),1)) |
(HoldsAt(PlayComputerGame(Bob),1) & HoldsAt(PlayComputerGame(Jilly),1)).

HoldsAt(Win(Bob),1) | HoldsAt(Win(Jilly),1).

range time 0 1
range offset 0 0

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/FrankEtAl2003/FrankEtAl.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{FrankEtAl:2003,
;   author = "Stefan L. Frank and Mathieu Koppen and Leo G. M. Noordman and Wietske Vonk",
;   year = "2003",
;   title = "Modeling knowledge-based inferences in story comprehension",
;   journal = "Cognitive Science",
;   volume = "27",
;   pages = "875--910",
; }
;

fluent SunShining()
fluent Raining()
fluent Outside(agent)
fluent PlaySoccer(agent)
fluent PlayHideAndSeek(agent)
fluent PlayComputerGame(agent)
fluent PlayWithDog(agent)
fluent Win(agent)

noninertial Outside, PlaySoccer, PlayHideAndSeek, PlayComputerGame
noninertial PlayWithDog, Win

xor PlaySoccer, PlayHideAndSeek, PlayComputerGame, PlayWithDog

[agent,time]
HoldsAt(PlaySoccer(agent),time) ->
HoldsAt(Outside(agent),time).

[agent,time]
HoldsAt(PlaySoccer(agent),time) ->
({agent1} agent1!=agent & HoldsAt(PlaySoccer(agent1),time)).

[agent,time]
HoldsAt(PlayHideAndSeek(agent),time) ->
({agent1} agent1!=agent & HoldsAt(PlayHideAndSeek(agent1),time)).

[agent,time]
HoldsAt(PlayComputerGame(agent),time) ->
!HoldsAt(Outside(agent),time).

[agent,time]
HoldsAt(Win(agent),time) ->
(HoldsAt(PlaySoccer(agent),time) |
 HoldsAt(PlayHideAndSeek(agent),time) |
 (HoldsAt(PlayComputerGame(agent),time) &
  ({agent1} agent1!=agent & HoldsAt(PlayComputerGame(agent1),time)))).

[agent,time]
HoldsAt(PlaySoccer(agent),time) &
HoldsAt(Win(agent),time) ->
!HoldsAt(PlaySoccer(agent),time+1).

[agent,time]
HoldsAt(PlayHideAndSeek(agent),time) &
HoldsAt(Win(agent),time) ->
!HoldsAt(PlayHideAndSeek(agent),time+1).

[agent,time]
HoldsAt(PlayComputerGame(agent),time) &
HoldsAt(Win(agent),time) ->
!HoldsAt(PlayComputerGame(agent),time+1).

[agent,time]
HoldsAt(Win(agent),time) ->
HoldsAt(PlaySoccer(agent),time-1) |
HoldsAt(PlayHideAndSeek(agent),time-1) |
HoldsAt(PlayComputerGame(agent),time-1).

[agent,time]
HoldsAt(PlaySoccer(agent),time) ->
!HoldsAt(Raining(),time).

[agent,time]
HoldsAt(Win(agent),time) ->
!({agent1} agent1!=agent & HoldsAt(Win(agent1),time)).

[agent1,agent2,time]
HoldsAt(PlayHideAndSeek(agent1),time) &
HoldsAt(PlayHideAndSeek(agent2),time) ->
((HoldsAt(Outside(agent1),time) & HoldsAt(Outside(agent2),time)) |
 (!HoldsAt(Outside(agent1),time) & !HoldsAt(Outside(agent2),time))).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/GiunchigliaEtAl2004/MonkeyPrediction.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Giunchiglia:2004,
;   author = "Enrico Giunchiglia and Joohyung Lee and Vladimir Lifschitz and Norman C. McCain and Hudson Turner",
;   year = "2004",
;   title = "Nonmonotonic causal theories",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "49--104",
; }
;

; deduction

load foundations/Root.e
load foundations/EC.e
load examples/GiunchigliaEtAl2004/MonkeyBananas.e

HoldsAt(At(Monkey,L1),0).
HoldsAt(At(Bananas,L2),0).
HoldsAt(At(Box,L3),0).
Happens(Walk(L3),0).
Happens(PushBox(L2),1).

completion Happens

range time 0 2
range offset 0 0

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/GiunchigliaEtAl2004/MonkeyPlanning.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Giunchiglia:2004,
;   author = "Enrico Giunchiglia and Joohyung Lee and Vladimir Lifschitz and Norman C. McCain and Hudson Turner",
;   year = "2004",
;   title = "Nonmonotonic causal theories",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "49--104",
; }
;

; planning

load foundations/Root.e
load foundations/EC.e
load examples/GiunchigliaEtAl2004/MonkeyBananas.e

HoldsAt(At(Monkey,L1),0).
HoldsAt(At(Bananas,L2),0).
HoldsAt(At(Box,L3),0).
HoldsAt(HasBananas(),4).

; PLAN Happens(Walk(L3),0).
; PLAN Happens(PushBox(L2),1).
; PLAN Happens(ClimbOn(),2).
; PLAN Happens(GraspBananas(),3).

; one event at a time
[event1,event2,time] Happens(event1,time) & Happens(event2,time) ->
event1=event2.

range time 0 4
range offset 0 0

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/GiunchigliaEtAl2004/MonkeyPostdiction.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Giunchiglia:2004,
;   author = "Enrico Giunchiglia and Joohyung Lee and Vladimir Lifschitz and Norman C. McCain and Hudson Turner",
;   year = "2004",
;   title = "Nonmonotonic causal theories",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "49--104",
; }
;

; postdiction

load foundations/Root.e
load foundations/EC.e
load examples/GiunchigliaEtAl2004/MonkeyBananas.e

HoldsAt(At(Monkey,L1),0).
HoldsAt(At(Bananas,L2),0).
Happens(Walk(L3),0).
Happens(PushBox(L2),1).

completion Happens

range time 0 2
range offset 0 0

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/GiunchigliaEtAl2004/MonkeyBananas.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Giunchiglia:2004,
;   author = "Enrico Giunchiglia and Joohyung Lee and Vladimir Lifschitz and Norman C. McCain and Hudson Turner",
;   year = "2004",
;   title = "Nonmonotonic causal theories",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "49--104",
; }
;

sort object
sort location

object Monkey, Bananas, Box
location L1, L2, L3

fluent At(object,location)
fluent OnBox()
fluent HasBananas()

event Walk(location)
event PushBox(location)
event ClimbOn()
event ClimbOff()
event GraspBananas()

[object,location1,location2,time]
HoldsAt(At(object,location1),time) &
HoldsAt(At(object,location2),time) ->
location1=location2.

[object,location,time]
object=Monkey ->
Initiates(Walk(location),At(object,location),time).

[object,location1,location2,time]
object=Monkey &
HoldsAt(At(object,location1),time) ->
Terminates(Walk(location2),At(object,location1),time).

[location,time]
Happens(Walk(location),time) ->
!HoldsAt(At(Monkey,location),time) &
!HoldsAt(OnBox(),time).

[location,time]
HoldsAt(HasBananas(),time) &
HoldsAt(At(Monkey,location),time) ->
HoldsAt(At(Bananas,location),time).

[object,location,time]
object=Box | object=Monkey ->
Initiates(PushBox(location),At(object,location),time).

[object,location1,location2,time]
(object=Box | object=Monkey) &
HoldsAt(At(object,location1),time) ->
Terminates(PushBox(location2),At(object,location1),time).

[location,time]
Happens(PushBox(location),time) ->
({location1}
  HoldsAt(At(Box,location1),time) &
  HoldsAt(At(Monkey,location1),time)) &
!HoldsAt(At(Monkey,location),time) &
!HoldsAt(OnBox(),time).

[time] Initiates(ClimbOn(),OnBox(),time).

[time]
Happens(ClimbOn(),time) ->
!HoldsAt(OnBox(),time).

[time] Terminates(ClimbOff(),OnBox(),time).

[time]
Happens(ClimbOff(),time) ->
HoldsAt(OnBox(),time).

[time] Initiates(GraspBananas(),HasBananas(),time).

[object,location,time]
object=Bananas ->
Releases(GraspBananas(),At(object,location),time).

[time]
Happens(GraspBananas(),time) ->
({location1}
  HoldsAt(At(Bananas,location1),time) &
  HoldsAt(At(Monkey,location1),time)) &
HoldsAt(OnBox(),time).

[time]
HoldsAt(OnBox(),time) ->
{location1} HoldsAt(At(Box,location1),time) &
            HoldsAt(At(Monkey,location1),time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Antoniou1997/Student.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; conflicting defaults: method (D)
; \fullciteA[p. 157]{Antoniou:1997}
;
; @book{Antoniou:1997,
;   author = "Grigoris Antoniou",
;   year = "1997",
;   title = "Nonmonotonic Reasoning",
;   address = "Cambridge, MA",
;   publisher = "MIT Press",
; }
;

load foundations/Root.e
load foundations/EC.e

sort x

predicate Adult(x)
predicate Student(x)
predicate Employed(x)
predicate Ab1(x)
predicate Ab2(x)

x Mary

Student(Mary).

[x] Adult(x) & !Ab1(x) -> Employed(x).
[x] Student(x) & !Ab2(x) -> !Employed(x).
[x] Student(x) -> Adult(x).
Theta: [x] Student(x) -> Ab1(x).

range time 0 0
range offset 1 1

completion Theta Ab1
completion Theta Ab2

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Antoniou1997/Dropout.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; dealing with conflicting defaults by adding conditions
; to one of the conflicting rules
; \fullciteA[p. 56]{Antoniou:1997}
;
; @book{Antoniou:1997,
;   author = "Grigoris Antoniou",
;   year = "1997",
;   title = "Nonmonotonic Reasoning",
;   address = "Cambridge, MA",
;   publisher = "MIT Press",
; }
;

load foundations/Root.e
load foundations/EC.e

sort x

predicate Dropout(x)
predicate Adult(x)
predicate Employed(x)
predicate Ab1(x)
predicate Ab2(x)

x Bill

Dropout(Bill).

[x] Dropout(x) & !Ab1(x) -> Adult(x).
[x] Adult(x) & !Dropout(x) & !Ab2(x) -> Employed(x).

range time 0 0
range offset 1 1

completion Theta Ab1
completion Theta Ab2

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Shanahan1999/Happy.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @incollection{Shanahan:1999,
;   author = "Shanahan, Murray",
;   year = "1999",
;   title = "The Event Calculus explained",
;   editor = "Michael J. Wooldridge and Manuela M. Veloso",
;   booktitle = "Artificial Intelligence Today: Recent Trends and Developments",
;   series = "Lecture Notes in Computer Science",
;   volume = "1600",
;   pages = "409--430",
;   address = "Berlin",
;   publisher = "Springer",
; }
;
; deduction
;
; modifications from Shanahan's formulation:
; InitiallyN -> !HoldsAt
; InitiallyP -> HoldsAt
; timestamps
;

load foundations/Root.e
load foundations/EC.e

sort person
event Feed(person)
event Clothe(person)
fluent Happy(person)
fluent Hungry(person)
fluent Cold(person)
noninertial Happy

[person,time]
HoldsAt(Happy(person),time) <->
!HoldsAt(Hungry(person),time) &
!HoldsAt(Cold(person),time).

[person,time]
Terminates(Feed(person),Hungry(person),time).

[person,time]
Terminates(Clothe(person),Cold(person),time).

person Fred

HoldsAt(Hungry(Fred),0).
!HoldsAt(Cold(Fred),0).
Happens(Feed(Fred),1).

completion Happens

range time 0 2
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Shanahan1999/ThielscherCircuit.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Thielscher:1997,
;   author = "Michael Thielscher",
;   year = "1997",
;   title = "Ramification and causality",
;   journal = "Artificial Intelligence",
;   volume = "89",
;   pages = "317--364",
; }
;
; @incollection{Shanahan:1999,
;   author = "Shanahan, Murray",
;   year = "1999",
;   title = "The Event Calculus explained",
;   editor = "Michael J. Wooldridge and Manuela M. Veloso",
;   booktitle = "Artificial Intelligence Today: Recent Trends and Developments",
;   series = "Lecture Notes in Computer Science",
;   volume = "1600",
;   pages = "409--430",
;   address = "Berlin",
;   publisher = "Springer",
; }
;
; deduction
;
; modifications from Shanahan's formulation:
; timestamps
;

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
Stopped(Light(),time) &
Initiated(Switch1(),time) &
Initiated(Switch2(),time) ->
Happens(LightOn(),time).

[time]
Started(Switch2(),time) &
Initiated(Relay(),time) ->
Happens(Open2(),time).

[time]
Stopped(Relay(),time) &
Initiated(Switch1(),time) &
Initiated(Switch3(),time) ->
Happens(CloseRelay(),time).

[time] Initiates(LightOn(),Light(),time).

[time] Terminates(Open2(),Switch2(),time).

[time] Initiates(CloseRelay(),Relay(),time).

[time] Initiates(Close1(),Switch1(),time).

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



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Shanahan1999/CoinToss.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Kartha:1994,
;   author = "G. Neelakantan Kartha",
;   year = "1994",
;   title = "Two counterexamples related to \uppercase{B}aker's approach to the frame problem",
;   journal = "Artificial Intelligence",
;   volume = "69",
;   number = "1--2",
;   pages = "379--391",
; }
;
; @incollection{Shanahan:1999,
;   author = "Shanahan, Murray",
;   year = "1999",
;   title = "The Event Calculus explained",
;   editor = "Michael J. Wooldridge and Manuela M. Veloso",
;   booktitle = "Artificial Intelligence Today: Recent Trends and Developments",
;   series = "Lecture Notes in Computer Science",
;   volume = "1600",
;   pages = "409--430",
;   address = "Berlin",
;   publisher = "Springer",
; }
;
; model finding
;
; modifications from Shanahan's formulation:
; InitiallyP -> HoldsAt
; pruning of models irrelevant to example
; timestamps
;

load foundations/Root.e
load foundations/EC.e

event Toss()
fluent ItsHeads()
fluent Heads()
noninertial ItsHeads

[time] HoldsAt(ItsHeads(),time) -> Initiates(Toss(),Heads(),time).
[time] !HoldsAt(ItsHeads(),time) -> Terminates(Toss(),Heads(),time).

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



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Shanahan1999/ChessBoard.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; due to Raymond Reiter
;
; @inproceedings{KarthaLifschitz:1994,
;   author = "G. Neelakantan Kartha and Vladimir Lifschitz",
;   year = "1994",
;   title = "Actions with indirect effects (preliminary report)",
;   editor = "Jon Doyle and Erik Sandewall and Pietro Torasso",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{F}ourth \uppercase{I}nternational \uppercase{C}onference on \uppercase{P}rinciples of \uppercase{K}nowledge \uppercase{R}epresentation and \uppercase{R}easoning",
;   pages = "341--350",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann",
; }
;
; @incollection{Shanahan:1999,
;   author = "Shanahan, Murray",
;   year = "1999",
;   title = "The Event Calculus explained",
;   editor = "Michael J. Wooldridge and Manuela M. Veloso",
;   booktitle = "Artificial Intelligence Today: Recent Trends and Developments",
;   series = "Lecture Notes in Computer Science",
;   volume = "1600",
;   pages = "409--430",
;   address = "Berlin",
;   publisher = "Springer",
; }
;
; model finding
;
; modifications from Shanahan's formulation:
; InitiallyN -> !HoldsAt
; pruning of models irrelevant to example
; timestamps
;

load foundations/Root.e
load foundations/EC.e

event Throw()
fluent ItsBlack()
fluent ItsWhite()
fluent OnBlack()
fluent OnWhite()
noninertial ItsBlack, ItsWhite

[time]
HoldsAt(ItsWhite(),time) ->
Initiates(Throw(),OnWhite(),time).

[time]
HoldsAt(ItsBlack(),time) ->
Initiates(Throw(),OnBlack(),time).

[time] HoldsAt(ItsWhite(),time) | HoldsAt(ItsBlack(),time).

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



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Shanahan1999/RussianTurkey.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Sandewall:1994,
;   author = "Sandewall, Erik",
;   year = "1994",
;   title = "Features and Fluents: The Representation of Knowledge about Dynamical Systems",
;   volume = "I",
;   address = "Oxford",
;   publisher = "Oxford University Press",
; }
;
; @incollection{Shanahan:1999,
;   author = "Shanahan, Murray",
;   year = "1999",
;   title = "The Event Calculus explained",
;   editor = "Michael J. Wooldridge and Manuela M. Veloso",
;   booktitle = "Artificial Intelligence Today: Recent Trends and Developments",
;   series = "Lecture Notes in Computer Science",
;   volume = "1600",
;   pages = "409--430",
;   address = "Berlin",
;   publisher = "Springer",
; }
;
; model finding
;
; modifications from Shanahan's formulation:
; InitiallyP -> HoldsAt
; added [time] Terminates(Shoot(),Loaded(),time).
; added !HoldsAt(Loaded(),0) to prune models
; timestamps
;

load foundations/Root.e
load foundations/EC.e

event Load()
event Shoot()
event Spin()
fluent Loaded()
fluent Alive()

[time] Initiates(Load(),Loaded(),time).
[time] HoldsAt(Loaded(),time) -> Terminates(Shoot(),Alive(),time).
[time] Releases(Spin(),Loaded(),time).
[time] Terminates(Shoot(),Loaded(),time).

HoldsAt(Alive(),0).
!HoldsAt(Loaded(),0).
Happens(Load(),1).
Happens(Spin(),2).
Happens(Shoot(),3).

completion Happens

range time 0 4
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/AkmanEtAl2004/ZooTest4.2.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Akman:2004,
;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
;   year = "2004",
;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "105--140",
; }
;

option encoding 3

load foundations/Root.e
load foundations/EC.e
load examples/AkmanEtAl2004/ZooWorld.e

human Homer
elephant Jumbo

Species(Homer)=HumanSpecies.
Adult(Homer).
Species(Jumbo)=ElephantSpecies.
Adult(Jumbo).

!HoldsAt(Opened(GateAO),0).
{position} HoldsAt(Pos(Homer,position),0) & Outside=Loc(position).
{position} HoldsAt(Pos(Jumbo,position),0) & CageA=Loc(position).

{position} HoldsAt(Pos(Homer,position),5) & CageA=Loc(position).
{position} HoldsAt(Pos(Jumbo,position),5) & Outside=Loc(position).

[animal,time] !HoldsAt(Mounted(Homer,animal),time).

[human] HoldsAt(PosDeterminingFluent(human,1),5).
[event,animal] !HoldsAt(DoneBy(event,animal),5).

;HoldsAt(Pos(Homer,7),0).
;HoldsAt(Pos(Jumbo,4),0).
;Happens(Move(Jumbo,3),0).
;Happens(Open(Homer,GateAO),0).
;Happens(Move(Homer,4),1).
;Happens(Move(Jumbo,1),1).
;Happens(Move(Jumbo,3),2).
;Happens(Mount(Homer,Jumbo),2).
;Happens(Move(Jumbo,4),3).
;!Happens(Move(Homer,2),3).
;Happens(Move(Jumbo,7),4).
;!Happens(Mount(Homer,Jumbo),3).
;!Happens(Mount(Homer,Jumbo),4).
;[position] !Happens(Move(Homer,position),4).

range time 0 5
range position 1 8
range offset 0 0

completion Happens

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/AkmanEtAl2004/ZooTest5.1.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Akman:2004,
;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
;   year = "2004",
;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "105--140",
; }
;

option encoding 3

load foundations/Root.e
load foundations/EC.e
load examples/AkmanEtAl2004/ZooWorld.e

human Homer
elephant Jumbo
horse Silver

Species(Homer)=HumanSpecies.
Adult(Homer).
Species(Jumbo)=ElephantSpecies.
Adult(Jumbo).
Species(Silver)=HorseSpecies.
Adult(Silver).

{position}
!HoldsAt(Pos(Homer,position),0) &
HoldsAt(Pos(Jumbo,position),0) &
HoldsAt(Pos(Homer,position),1) &
!HoldsAt(Pos(Jumbo,position),1).
HoldsAt(Mounted(Homer,Silver),0).

option manualrelease on
[human, animal] !ReleasedAt(Mounted(human, animal),0).
[gate] !ReleasedAt(Opened(gate),0).
[position] ReleasedAt(Pos(Homer,position),0).
[position] !ReleasedAt(Pos(Jumbo,position),0).
[position] !ReleasedAt(Pos(Silver,position),0).

[human] HoldsAt(PosDeterminingFluent(human,1),1).
[event,animal] !HoldsAt(DoneBy(event,animal),1).

;HoldsAt(Opened(GateAO),0).
;HoldsAt(Pos(Homer,3),0).
;HoldsAt(Pos(Jumbo,2),0).
;HoldsAt(Pos(Silver,3),0).
;Happens(Move(Jumbo,4),0).
;Happens(ThrowOff(Silver,Homer),0).
;HoldsAt(PosDeterminingFluent(Homer,2),0).

range time 0 1
range position 1 8
range offset 0 0

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/AkmanEtAl2004/ZooTest3.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Akman:2004,
;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
;   year = "2004",
;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "105--140",
; }
;

option encoding 3

load foundations/Root.e
load foundations/EC.e
load examples/AkmanEtAl2004/ZooWorld.e

human Homer
dog Snoopy

Species(Homer)=HumanSpecies.
Adult(Homer).
Species(Snoopy)=DogSpecies.
Adult(Snoopy).

!HoldsAt(Opened(GateAO),0).
{position} HoldsAt(Pos(Homer,position),0) & Outside=Loc(position).
{position} HoldsAt(Pos(Snoopy,position),0) & CageA=Loc(position).

{position} HoldsAt(Pos(Homer,position),2) & CageA=Loc(position).
{position} HoldsAt(Pos(Snoopy,position),2) & Outside=Loc(position).

[human] HoldsAt(PosDeterminingFluent(human,1),2).
[event,animal] !HoldsAt(DoneBy(event,animal),2).

range time 0 2
range position 1 8
range offset 0 0

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/AkmanEtAl2004/ZooWorld.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Akman:2004,
;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
;   year = "2004",
;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "105--140",
; }
;

sort position: integer
sort location
sort cage: location
sort gate
sort animal
sort elephant: animal
sort horse: animal
sort dog: animal
sort human: animal
sort species

function Loc(position): location
function Side1(gate): position
function Side2(gate): position
function Species(animal): species

predicate Accessible(position,position,time)
predicate Adult(animal)
predicate Large(animal)
predicate LargeSpecies(species)
predicate Neighbor(position,position)
predicate Sides(position,position,gate)

event Close(human,gate)
event GetOff(human,animal)
event Mount(human,animal)
event Move(animal,position)
event Open(human,gate)
event ThrowOff(animal,human)

fluent AbnormalEncroachment(human)
noninertial AbnormalEncroachment
fluent DoneBy(event,animal)
noninertial DoneBy
fluent Mounted(human,animal)
fluent MountFails(human)
noninertial MountFails
fluent Moves(animal)
noninertial Moves
fluent Opened(gate)
fluent Pos(animal,position)
fluent PosDeterminingFluent(human,position)
noninertial PosDeterminingFluent
fluent ThrowOffFails(animal,human)
noninertial ThrowOffFails

species HumanSpecies, ElephantSpecies, HorseSpecies, DogSpecies
location Outside

LargeSpecies(HumanSpecies).
LargeSpecies(ElephantSpecies).
LargeSpecies(HorseSpecies).
!LargeSpecies(DogSpecies).

[event,animal,time]
HoldsAt(DoneBy(event,animal),time) <->
(Happens(event,time) &
 (({gate} event=Close(animal,gate)) |
  ({animal1} event=GetOff(animal,animal1))|
  ({animal1} event=Mount(animal,animal1))|
  ({position} event=Move(animal,position))|
  ({gate} event=Open(animal,gate)) |
  ({human1} event=ThrowOff(animal,human1)))).

[event1,event2,animal,time]
HoldsAt(DoneBy(event1,animal),time) &
HoldsAt(DoneBy(event2,animal),time) ->
event1=event2.

[animal] Large(animal) <-> (Adult(animal) & LargeSpecies(Species(animal))).

[position] {position1} position1!=position & Neighbor(position,position1).

[position] !Neighbor(position,position).

[position1,position2]
Neighbor(position1,position2) ->
Neighbor(position2,position1).

[cage] cage!=Outside.

[position1,position2,gate]
Sides(position1,position2,gate) <->
((Side1(gate)=position1 &
  Side2(gate)=position2) |
 (Side2(gate)=position1 &
  Side1(gate)=position2)).

[gate] Loc(Side1(gate))!=Loc(Side2(gate)).

[position1,position2,gate1,gate2]
Sides(position1,position2,gate1) &
Sides(position1,position2,gate2) ->
gate1=gate2.

[position1,position2,gate]
Sides(position1,position2,gate) ->
Neighbor(position1,position2).

[position1,position2]
Loc(position1) != Loc(position2) &
Neighbor(position1,position2) ->
{gate} Sides(position1,position2,gate).

[animal,position1,position2,time]
HoldsAt(Pos(animal,position1),time) &
HoldsAt(Pos(animal,position2),time) ->
position1=position2.

[animal,time]
{position} HoldsAt(Pos(animal,position),time).

[animal1,animal2,position,time]
(animal1!=animal2 &
 Large(animal1) &
 Large(animal2) &
 HoldsAt(Pos(animal1,position),time) &
 HoldsAt(Pos(animal2,position),time)) ->
(({human} human=animal1 & HoldsAt(Mounted(human,animal2),time)) |
 ({human} human=animal2 & HoldsAt(Mounted(human,animal1),time))).

[human,position1,position2,time]
HoldsAt(PosDeterminingFluent(human,position1),time) &
HoldsAt(PosDeterminingFluent(human,position2),time) ->
position1=position2.

[animal,position,time]
Initiates(Move(animal,position),Pos(animal,position),time).

[animal,position1,position2,time]
HoldsAt(Pos(animal,position1),time) ->
Terminates(Move(animal,position2),Pos(animal,position1),time).

[animal,position,time]
Happens(Move(animal,position),time) ->
!HoldsAt(Pos(animal,position),time).

[human,position,time]
Happens(Move(human,position),time) ->
!{animal} HoldsAt(Mounted(human,animal),time).

[human,gate,time]
Initiates(Open(human,gate),Opened(gate),time).

[human,gate,time]
Happens(Open(human,gate),time) ->
!HoldsAt(Opened(gate),time) &
(!{animal} HoldsAt(Mounted(human,animal),time)) &
({position}
 (Side1(gate)=position | Side2(gate)=position) &
 HoldsAt(Pos(human,position),time)).

[human,gate,time]
Terminates(Close(human,gate),Opened(gate),time).

[human,gate,time]
Happens(Close(human,gate),time) ->
HoldsAt(Opened(gate),time) &
(!{animal} HoldsAt(Mounted(human,animal),time)) &
{position}
(Side1(gate)=position | Side2(gate)=position) &
HoldsAt(Pos(human,position),time).

[human,animal,position,time]
HoldsAt(Mounted(human,animal),time) &
HoldsAt(Pos(animal,position),time) ->
HoldsAt(Pos(human,position),time).

[animal,time]
HoldsAt(Moves(animal),time) <->
({position}
 HoldsAt(Pos(animal,position),time) &
 !HoldsAt(Pos(animal,position),time+1)).

[human,time]
HoldsAt(MountFails(human),time) <->
({animal}
  Happens(Mount(human,animal),time) &
  HoldsAt(Moves(animal),time)).

[human,animal,position,time]
!HoldsAt(Moves(animal),time) ->
Releases(Mount(human,animal),Pos(human,position),time).

[human,animal,time]
!HoldsAt(Moves(animal),time) ->
Initiates(Mount(human,animal),Mounted(human,animal),time).

[human,animal,position,time]
HoldsAt(Pos(animal,position),time) &
HoldsAt(Moves(animal),time) ->
Initiates(Mount(human,animal),Pos(human,position),time).

[human,animal,position,time]
HoldsAt(Pos(human,position),time) &
HoldsAt(Moves(animal),time) ->
Terminates(Mount(human,animal),Pos(human,position),time).

[human,animal,time]
Happens(Mount(human,animal),time) ->
Large(animal).

[human,animal,time]
HoldsAt(Mounted(human,animal),time) ->
Large(animal).

[human1,human2,time]
Happens(Mount(human1,human2),time) ->
!Large(human1).

[human1,human2,time]
HoldsAt(Mounted(human1,human2),time) ->
!Large(human1).

[human,animal,time]
Happens(Mount(human,animal),time) ->
!{human1} human1!=human & HoldsAt(Mounted(human1,animal),time).

[human1,human2,animal,time]
HoldsAt(Mounted(human1,animal),time) &
HoldsAt(Mounted(human2,animal),time) ->
human1=human2.

[human,animal,time]
Happens(Mount(human,animal),time) ->
!{human1} human1!=human & HoldsAt(Mounted(human1,human),time).

[human1,human2,time]
Happens(Mount(human1,human2),time) ->
{animal} HoldsAt(Mounted(human2,animal),time).

[human1,human2,time]
HoldsAt(Mounted(human1,human2),time) ->
!{animal} HoldsAt(Mounted(human2,animal),time).

[human,animal,time]
Happens(Mount(human,animal),time) ->
!{animal1} HoldsAt(Mounted(human,animal1),time).

[human,animal,time]
!HoldsAt(Moves(animal),time) ->
Terminates(GetOff(human,animal),Mounted(human,animal),time).

[human,animal,position,time]
!HoldsAt(Moves(animal),time) &
HoldsAt(PosDeterminingFluent(human,position),time) ->
Initiates(GetOff(human,animal),Pos(human,position),time).

[human,animal,position,time]
!HoldsAt(Moves(animal),time) &
HoldsAt(Pos(human,position),time) ->
Terminates(GetOff(human,animal),Pos(human,position),time).

[human,animal,position1,position2,time]
!HoldsAt(Moves(animal),time) &
HoldsAt(Pos(human,position1),time) &
position1!=position2 ->
Terminates(GetOff(human,animal),Pos(human,position2),time).

[human,animal,time]
Happens(GetOff(human,animal),time) ->
HoldsAt(Mounted(human,animal),time).

[animal1,human,time]
HoldsAt(ThrowOffFails(animal1,human),time) <->
({position,animal2}
 animal2!=human &
 HoldsAt(PosDeterminingFluent(human,position),time) &
 Large(animal2) &
 HoldsAt(Pos(animal2,position),time+1)).

[animal,human,position,time]
HoldsAt(PosDeterminingFluent(human,position),time) &
!HoldsAt(ThrowOffFails(animal,human),time) ->
Initiates(ThrowOff(animal,human),Pos(human,position),time).

[animal,human,position,time]
HoldsAt(Pos(human,position),time) &
!HoldsAt(ThrowOffFails(animal,human),time) ->
Terminates(ThrowOff(animal,human),Pos(human,position),time).

[animal,human,position1,position2,time]
!HoldsAt(ThrowOffFails(animal,human),time) &
HoldsAt(Pos(human,position1),time) &
!HoldsAt(PosDeterminingFluent(human,position2),time) &
position1!=position2 ->
Terminates(ThrowOff(animal,human),Pos(human,position2),time).

[human,time]
(!{animal} Happens(ThrowOff(animal,human),time) |
           Happens(GetOff(human,animal),time)) ->
HoldsAt(PosDeterminingFluent(human,1),time).

[human,position,animal1,animal2,time]
HoldsAt(PosDeterminingFluent(human,position),time) &
HoldsAt(ThrowOffFails(animal1,human),time) &
HoldsAt(Pos(animal2,position),time) ->
Initiates(ThrowOff(animal1,human),Mounted(human,animal2),time).

[human,animal,time]
!HoldsAt(ThrowOffFails(animal,human),time) ->
Terminates(ThrowOff(animal,human),Mounted(human,animal),time).

[animal,human,time]
Happens(ThrowOff(animal,human),time) ->
HoldsAt(Mounted(human,animal),time).

[animal,human,time]
Happens(ThrowOff(animal,human),time) ->
!Happens(GetOff(human,animal),time).

[animal,human,time]
Happens(GetOff(human,animal),time) ->
!Happens(ThrowOff(animal,human),time).

[position1,position2,time]
Accessible(position1,position2,time) <->
(Neighbor(position1,position2) &
 !{gate} Sides(position1,position2,gate) &
         !HoldsAt(Opened(gate),time)).

[animal,position1,position2,time]
(position1!=position2 &
 HoldsAt(Pos(animal,position1),time) &
 HoldsAt(Pos(animal,position2),time+1)) ->
Accessible(position1,position2,time).

[human,time]
HoldsAt(AbnormalEncroachment(human),time) <->
(HoldsAt(MountFails(human),time) |
 ({position,animal1,animal2}
   HoldsAt(PosDeterminingFluent(human,position),time) &
   !HoldsAt(ThrowOffFails(animal2,human),time) &
   Happens(ThrowOff(animal2,human),time) &
   animal1!=human &
   Large(animal1) &
   HoldsAt(Pos(animal1,position),time) &
   !HoldsAt(Pos(animal1,position),time+1))).

[animal1,animal2,position,time]
HoldsAt(Pos(animal1,position),time) &
!HoldsAt(Pos(animal1,position),time+1) &
!HoldsAt(Pos(animal2,position),time) &
HoldsAt(Pos(animal2,position),time+1) ->
(!Large(animal1) |
 !Large(animal2) |
 ({human} human=animal2 & HoldsAt(AbnormalEncroachment(human),time))).

[animal1,animal2,position1,position2,time]
animal1!=animal2 &
Large(animal1) & Large(animal2) &
HoldsAt(Pos(animal1,position1),time) &
HoldsAt(Pos(animal1,position2),time+1) &
HoldsAt(Pos(animal2,position1),time) &
HoldsAt(Pos(animal2,position2),time+1) ->
!{gate} Sides(position1,position2,gate).

[animal1,animal2,position1,position2,time]
animal1!=animal2 &
Large(animal1) & Large(animal2) &
HoldsAt(Pos(animal1,position1),time) &
HoldsAt(Pos(animal1,position2),time+1) &
HoldsAt(Pos(animal2,position2),time) &
HoldsAt(Pos(animal2,position1),time+1) ->
!{gate} Sides(position1,position2,gate).

[gate,position1,position2,time]
HoldsAt(Opened(gate),time) &
!HoldsAt(Opened(gate),time+1) &
Sides(position1,position2,gate) ->
!{animal}
HoldsAt(Pos(animal,position1),time) &
HoldsAt(Pos(animal,position2),time+1).

gate GateAO
cage CageA

Loc(1)=CageA.
Loc(2)=CageA.
Loc(3)=CageA.
Loc(4)=CageA.
Loc(5)=Outside.
Loc(6)=Outside.
Loc(7)=Outside.
Loc(8)=Outside.

[position1,position2]
Neighbor(position1,position2) <->
((position1=1 & position2=2) |
 (position1=1 & position2=3) |
 (position1=1 & position2=4) |
 (position1=2 & position2=3) |
 (position1=2 & position2=4) |
 (position1=3 & position2=4) |
 (position1=5 & position2=6) |
 (position1=5 & position2=7) |
 (position1=5 & position2=8) |
 (position1=6 & position2=7) |
 (position1=6 & position2=8) |
 (position1=7 & position2=8) |
 (position2=1 & position1=2) |
 (position2=1 & position1=3) |
 (position2=1 & position1=4) |
 (position2=2 & position1=3) |
 (position2=2 & position1=4) |
 (position2=3 & position1=4) |
 (position2=5 & position1=6) |
 (position2=5 & position1=7) |
 (position2=5 & position1=8) |
 (position2=6 & position1=7) |
 (position2=6 & position1=8) |
 (position2=7 & position1=8) |
 (position1=4 & position2=7) |
 (position2=4 & position1=7)).

Side1(GateAO)=4.
Side2(GateAO)=7.

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/AkmanEtAl2004/ZooTest4.1.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Akman:2004,
;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
;   year = "2004",
;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "105--140",
; }
;

option encoding 3

load foundations/Root.e
load foundations/EC.e
load examples/AkmanEtAl2004/ZooWorld.e

human Homer
elephant Jumbo

Species(Homer)=HumanSpecies.
Adult(Homer).
Species(Jumbo)=ElephantSpecies.
Adult(Jumbo).

!HoldsAt(Opened(GateAO),0).
{position} HoldsAt(Pos(Homer,position),0) & Outside=Loc(position).
{position} HoldsAt(Pos(Jumbo,position),0) & CageA=Loc(position).

{position} HoldsAt(Pos(Homer,position),4) & CageA=Loc(position).
{position} HoldsAt(Pos(Jumbo,position),4) & Outside=Loc(position).

[human] HoldsAt(PosDeterminingFluent(human,1),4).
[event,animal] !HoldsAt(DoneBy(event,animal),4).

; ccalc.2.0b.8.3 single model
;HoldsAt(Pos(Homer,7),0).
;HoldsAt(Pos(Jumbo,2),0).
;Happens(Move(Jumbo,4),0).
;Happens(Open(Homer,GateAO),0).
;Happens(Mount(Homer,Jumbo),1).
;Happens(ThrowOff(Jumbo,Homer),2).
;HoldsAt(PosDeterminingFluent(Homer,1),2).
;Happens(Move(Jumbo,7),3).
;Happens(Mount(Homer,Jumbo),3).

range time 0 4
range position 1 8
range offset 0 0

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/AkmanEtAl2004/ZooTest2.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Akman:2004,
;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
;   year = "2004",
;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "105--140",
; }
;

option encoding 3

load foundations/Root.e
load foundations/EC.e
load examples/AkmanEtAl2004/ZooWorld.e

human Homer

Species(Homer)=HumanSpecies.
Adult(Homer).

!HoldsAt(Opened(GateAO),0).
{position} HoldsAt(Pos(Homer,position),0) & Outside=Loc(position).
{position} HoldsAt(Pos(Homer,position),2) & CageA=Loc(position).

[human] HoldsAt(PosDeterminingFluent(human,1),2).
[event,animal] !HoldsAt(DoneBy(event,animal),2).

range time 0 2
range position 1 8
range offset 0 0

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/AkmanEtAl2004/ZooTest6.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Akman:2004,
;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
;   year = "2004",
;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "105--140",
; }
;

option encoding 3

load foundations/Root.e
load foundations/EC.e
load examples/AkmanEtAl2004/ZooWorld.e

human Homer
elephant Jumbo

Species(Homer)=HumanSpecies.
Adult(Homer).
Species(Jumbo)=ElephantSpecies.
Adult(Jumbo).

HoldsAt(Mounted(Homer,Jumbo),0).
HoldsAt(Pos(Jumbo,1),0).
Happens(ThrowOff(Jumbo,Homer),0).

option manualrelease on
[human, animal] !ReleasedAt(Mounted(human, animal),0).
[gate] !ReleasedAt(Opened(gate),0).
[position] ReleasedAt(Pos(Homer,position),0).
[position] !ReleasedAt(Pos(Jumbo,position),0).

[human] HoldsAt(PosDeterminingFluent(human,1),1).
[event,animal] !HoldsAt(DoneBy(event,animal),1).

range time 0 1
range position 1 8
range offset 0 0

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/AkmanEtAl2004/ZooTest1.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors: 
; IBM - Initial implementation
;
; @article{Akman:2004,
;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
;   year = "2004",
;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "105--140",
; }
;

option encoding 3

load foundations/Root.e
load foundations/EC.e
load examples/AkmanEtAl2004/ZooWorld.e

human Homer
elephant Jumbo

Species(Homer)=HumanSpecies.
Adult(Homer).
Species(Jumbo)=ElephantSpecies.
Adult(Jumbo).

!HoldsAt(Opened(GateAO),0).
HoldsAt(Pos(Homer,6),0).
[time] HoldsAt(Pos(Jumbo,1),time).

; goal
HoldsAt(Mounted(Homer,Jumbo),4).

;ABDUCE
;Happens(Move(Homer,7),0).
;Happens(Open(Homer,GateAO),1).
;Happens(Move(Homer,4),2).
;Happens(Mount(Homer,Jumbo),3).

[human] HoldsAt(PosDeterminingFluent(human,1),4).
[event,animal] !HoldsAt(DoneBy(event,animal),4).

range time 0 4
range position 1 8
range offset 0 0

option timediff off
option modeldiff on

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/AkmanEtAl2004/ZooTest5.2.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Akman:2004,
;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
;   year = "2004",
;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "105--140",
; }
;

option encoding 3

load foundations/Root.e
load foundations/EC.e
load examples/AkmanEtAl2004/ZooWorld.e

human Homer
elephant Jumbo
horse Silver

Species(Homer)=HumanSpecies.
Adult(Homer).
Species(Jumbo)=ElephantSpecies.
Adult(Jumbo).
Species(Silver)=HorseSpecies.
Adult(Silver).

{position}
!HoldsAt(Pos(Homer,position),0) &
HoldsAt(Pos(Jumbo,position),0) &
HoldsAt(Pos(Homer,position),1) &
!HoldsAt(Pos(Jumbo,position),1).
[animal,time] !Happens(ThrowOff(animal,Homer),time).

[human] HoldsAt(PosDeterminingFluent(human,1),1).
[event,animal] !HoldsAt(DoneBy(event,animal),1).

;HoldsAt(Opened(GateAO),0).
;HoldsAt(Pos(Homer,3),0).
;HoldsAt(Pos(Jumbo,2),0).
;HoldsAt(Pos(Silver,7),0).
;Happens(Move(Jumbo,4),0).
;Happens(Move(Silver,8),0).
;Happens(Mount(Homer,Jumbo),0).

range time 0 1
range position 1 8
range offset 0 0

; End of file.
