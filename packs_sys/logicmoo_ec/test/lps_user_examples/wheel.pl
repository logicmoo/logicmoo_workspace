:- expects_dialect(lps).

maxTime(30).

fluents wheel/1.
actions turn.

initially wheel(vertical).
observe turn from 1 to 2.

if turn to T then turn from T.

/* I don't know why this doesn't work:
 * 
turn updates vertical to horizontal in wheel(vertical).
turn updates horizontal to vertical in wheel(horizontal).
*/

turn updates OldOrientation to NewOrientation in wheel(OldOrientation) if
opposite(OldOrientation, NewOrientation).

opposite(vertical, horizontal).
opposite(horizontal, vertical).




d(wheel(Orientation), [type:line, from: [X1,Y1], to: [X2, Y2], strokeColor: blue]) :-
  	(Orientation = vertical, X1 = 10, Y1 = 10, X2 = 10, Y2 = 30;
    Orientation = horizontal, X1 = 0, Y1 = 20, X2 = 20, Y2 = 20).
  
 d(timeless, [type: circle,  center:[10,20], radius:10, strokeColor:blue]).
 
 