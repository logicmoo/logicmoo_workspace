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
