
; location1 is adjacent to location2.
predicate Adjacent(location,location)

[location1,location2]
Adjacent(location1,location2) <-> 
 (location1=L1 & location2=L2) |
 (location1=L2 & location2=L1).

