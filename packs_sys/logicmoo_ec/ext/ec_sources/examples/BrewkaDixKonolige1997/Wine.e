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
