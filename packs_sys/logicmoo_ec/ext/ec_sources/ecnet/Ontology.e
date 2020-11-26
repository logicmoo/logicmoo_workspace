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
; integer
;

sort diameter: integer

; object

sort object

sort agent: object

sort physobj: object
sort bed: physobj
sort snowflake: physobj
sort sky: physobj

sort stuff: physobj

sort surface: physobj
sort ground: surface

sort snow: stuff
sort ball

sort food: physobj
sort fruit: food
sort orange: fruit
sort salad: food

sort clothing: physobj
sort scarf: clothing
sort hat: clothing

sort vegetablematter: physobj
sort coal: vegetablematter

sort bodypart: physobj
sort hand: bodypart

sort papertowels: physobj
sort device: physobj
sort electronicdevice: device
sort lamp: electronicdevice

sort cat: physobj
sort horse: physobj

sort weapon: physobj
sort gun: weapon
sort bomb: weapon
sort bullet: weapon

; location

sort location
sort room: location, outside: location

; portal

sort portal
sort door: portal, staircase: portal
sort street: portal
sort track: portal

sort building

sort fire: object
sort smoke: physobj

sort furniture: physobj
sort chair: furniture
sort table: furniture

sort bill: physobj
sort ticket: physobj
sort envelope: physobj

sort text: physobj
sort book: text
sort letter: text
sort menu: text

sort paper: physobj

sort content
sort script

sort container: physobj
sort cigarette: physobj
sort ashtray: physobj
sort umbrella: physobj

sort pen: physobj

; End of file.
