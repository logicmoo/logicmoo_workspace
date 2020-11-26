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
