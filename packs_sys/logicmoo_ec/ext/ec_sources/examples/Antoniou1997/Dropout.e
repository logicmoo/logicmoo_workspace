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
