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
