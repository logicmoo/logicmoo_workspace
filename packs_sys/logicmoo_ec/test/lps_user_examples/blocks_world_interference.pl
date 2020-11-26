:- expects_dialect(lps).

maxTime(10).
fluents location(_,_).

actions move(_,_).

initially location(b, floor), location(c, b), location(a, floor).

observe move(c,a) from 2 to 3.

if true
then make_tower([b,a,floor]) from T1 to T2.

clear(Block) at T if Block \= floor,
    not location(_,Block) at T.

clear(floor) at _.

make_tower([Block,floor]) from T1 to T2 if
    make_on(Block,floor) from T1 to T2.

make_tower([Block,Place|Places]) from T1 to T3 if
    Place \= floor,
    make_tower([Place|Places]) from T1 to T2,
    make_on(Block,Place) from T2 to T3.

make_on(Block,Place) from T1 to T4 if
    not location(Block,Place) at T1,
    make_clear(Place) from T1 to T2,
    make_clear(Block) from T2 to T3,
    move(Block,Place) from T3 to T4.

make_on(Block,Place) from T to T if
    location(Block,Place) at T.

make_clear(Place) from T to T if
    clear(Place) at T.

make_clear(Block) from T1 to T2 if
    location(Block1,Block) at T1,
    make_on(Block1,floor) from T1 to T2.

move(Block,Place)  initiates location(Block,Place).
move(Block,_)  terminates location(Block,Place).