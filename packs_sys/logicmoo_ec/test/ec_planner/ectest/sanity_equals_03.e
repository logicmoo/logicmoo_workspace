
predicate Neighbor(position,position)

[position1,position2]
Neighbor(position1,position2) <-
((position1=1 & position2=2) |
 (position1=1 & position2=3) |
 (position1=1 & position2=4) |
 (position1=2 & position2=3) |
 (position1=2 & position2=4) |
 (position1=3 & position2=4) |
 (position1=5 & position2=6) |
 (position1=5 & position2=7) |
 (position1=5 & position2=8) |
 (position1=6 & position2=7) |
 (position1=6 & position2=8) |
 (position1=7 & position2=8) |
 (position2=1 & position1=2) |
 (position2=1 & position1=3) |
 (position2=1 & position1=4) |
 (position2=2 & position1=3) |
 (position2=2 & position1=4) |
 (position2=3 & position1=4) |
 (position2=5 & position1=6) |
 (position2=5 & position1=7) |
 (position2=5 & position1=8) |
 (position2=6 & position1=7) |
 (position2=6 & position1=8) |
 (position2=7 & position1=8) |
 (position1=4 & position2=7) |
 (position2=4 & position1=7)).



; Prolog code starts with ;:-

;:- include(sanity_equals_01_extra).


