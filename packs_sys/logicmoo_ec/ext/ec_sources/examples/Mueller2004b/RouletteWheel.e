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
