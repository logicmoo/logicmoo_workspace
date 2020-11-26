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
