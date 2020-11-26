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
; GSpace: grid space
;
; @book{Mueller:1998,
;   author = "Erik T. Mueller",
;   year = "1998",
;   title = "Natural Language Processing with \uppercase{T}hought\uppercase{T}reasure",
;   address = "New York",
;   publisher = "Signiform",
; }
;

sort coord: integer
sort grid

; object is at (coord1, coord2) in grid.
fluent GridAt(grid,object,coord,coord)

; agent walks from (coord1, coord2)
; to (coord3, coord4) in grid.
event GridWalk(grid,agent,coord,coord,coord,coord)

; A state constraint says that for a given grid an
; object is at one cell in that grid at a time:
[grid,object,coord1,coord2,coord3,coord4,time]
HoldsAt(GridAt(grid,object,coord1,coord2),time) &
HoldsAt(GridAt(grid,object,coord3,coord4),time) ->
coord1=coord3 & coord2=coord4.

; An effect axiom states that
; if an agent walks from one cell in a grid to another cell,
; the agent will be at second cell:
[grid,agent,coord1,coord2,coord3,coord4,time]
Initiates(GridWalk(grid,agent,coord1,coord2,coord3,coord4),
          GridAt(grid,agent,coord3,coord4),
          time).

; An effect axiom states that
; if an agent walks from one cell in a grid to another cell,
; the agent will no longer be at the first cell:
[grid,agent,coord1,coord2,coord3,coord4,time]
Terminates(GridWalk(grid,agent,coord1,coord2,coord3,coord4),
           GridAt(grid,agent,coord1,coord2),
           time).

; A precondition axiom states that for an agent to walk
; from one cell in a grid to another cell, the agent
; must be at the first cell, the second cell must not
; be occupied, and the first cell must be adjacent to
; the second cell:
[grid,agent,coord1,coord2,coord3,coord4,time]
Happens(GridWalk(grid,agent,coord1,coord2,coord3,coord4),time) ->
HoldsAt(GridAt(grid,agent,coord1,coord2),time) &
(!{object} HoldsAt(GridAt(grid,object,coord3,coord4),time)) &
(coord1=coord3 |
 coord1=coord3+1 |
 coord1=coord3-1) &
(coord2=coord4 |
 coord2=coord4+1 |
 coord2=coord4-1).

; End of file.
