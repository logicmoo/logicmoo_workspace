:- dynamic(current_state/1).
:- assert(current_state([])).

wumpus(3,1).
pit(1,3).
pit(2,5).
pit(3,3).
gold(4,4).

	


perform(turn, []) :-
	write('turn'), nl,
	current_state([at(X,Y),facing(D)]),
	retract(current_state([at(X,Y),facing(D)])),
	( D < 4 -> D1 is D+1 ; D1 is 1 ),
	assert(current_state([at(X,Y),facing(D1)])).

perform(enter, [Breeze,Stench,Glitter]) :-
	write('enter'), nl,
	current_state(Z),
	retract(current_state(Z)),
	assert(current_state([at(1,1),facing(1)])),
	( gold(1,1) -> Glitter = true ; Glitter = false ),
	( (wumpus(1,2) ; wumpus(2,1)) -> Stench = true ;
	    Stench = false ),
	( (pit(2,1) ; pit(1,2)) -> Breeze = true ;
	    Breeze = false ).

perform(exit, []) :-
	write('exit'), nl,
	current_state([at(X,Y),facing(D)]),
	retract(current_state([at(X,Y),facing(D)])),
	assert(current_state([])).

perform(grab, []) :-
	write('grab'), nl.

perform(shoot, [Scream]) :-
	write('shoot'), nl,
	current_state([at(X,Y),facing(D)]),
	wumpus(WX, WY),
	( in_direction(X, Y, D, WX, WY), Scream = true ; Scream = false ).

perform(go, [Breeze,Stench,Glitter]) :-
	write('go'), nl,
	current_state([at(X,Y),facing(D)]),
	retract(current_state([at(X,Y),facing(D)])),
	( D = 1 -> X1 is X, Y1 is Y+1 ;
	  D = 3 -> X1 is X, Y1 is Y-1 ;
	  D = 2 -> X1 is X+1, Y1 is Y ;
	  D = 4 -> X1 is X-1, Y1 is Y ),
	assert(current_state([at(X1,Y1),facing(D)])),
	( gold(X1,Y1) -> Glitter = true ; Glitter = false ),
	X_east is X1+1, X_west is X1-1, Y_north is Y1+1, Y_south is Y1-1,
	( (wumpus(X_east,Y1) ; wumpus(X1,Y_north) ;
           wumpus(X_west,Y1) ; wumpus(X1,Y_south)) -> Stench = true ;
	    Stench = false ),
	( (pit(X_east,Y1) ; pit(X1,Y_north) ;
           pit(X_west,Y1) ; pit(X1,Y_south)) -> Breeze = true ;
	    Breeze = false ).

