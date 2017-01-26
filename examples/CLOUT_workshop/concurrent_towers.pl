maxTime(5).
fluents location(_,_).

actions move(_,_).

% works very nicely with this initial state which makes the two goals parallel and unrelated
% initially location(b,a),location(c,b),location(a,floor),
% location(d,e),location(e,f),location(f,floor).

initially location(f,floor), location(b,f),location(e,b),
location(a,floor), location(d,a),location(c,d).

if true
then make_tower([a,b,c,floor]) from _T1 to _T2.

if true
then make_tower([f,e,d,floor]) from _T1 to _T2.

% if true
% then make_tower([floor]) from _T1 to _T2.

clear(Block) at T if Block \= floor,
    not location(_,Block) at T.
/*
clear(Block) at T if
    not location(_,Block) at T.
*/
clear(floor) at _.

make_tower([Block,Place|Places]) from T1 to T3 if
    make_tower([Place|Places]) from T1 to T2,
    make_on(Block,Place) from T2 to T3.

make_tower([Block,floor]) from T1 to T2 if
    make_on(Block,floor) from T1 to T2.

%
make_tower([floor]) from _T1 to _T2 if true.
%

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

/** <examples>
?- go(Timeline).
*/
