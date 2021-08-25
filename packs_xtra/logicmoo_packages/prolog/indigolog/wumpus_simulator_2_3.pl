/*
or_holds([],W,[_|Z]) <=> or_holds(W,Z).
elementary_action(enter).
elementary_action(exit).
elementary_action(turn).
elementary_action(grab).
elementary_action(shoot).
elementary_action(go).
compound_action(do(_,_))
compound_action(go_to(_,_)).
*/

% :- use_module( library(random)).

:- dynamic current_state/1.

init_simulator :- init_scenario,
                  retractall(current_state(_)), assertz(current_state([])).

:- dynamic wumpus/2,pit/2,gold/2.

init_scenario :-

   retractall(wumpus(_,_)), retractall(pit(_,_)), retractall(gold(_,_)),

   xdim(XD), ydim(YD),

   random(0,4294967296,N1), random(0,4294967296,N2), XW is N1 mod XD + 1, YW is N2 mod YD + 1,
   ( XW=1, YW=1 -> true ; assertz(wumpus(XW,YW)), write(wumpus(XW,YW)) ),

   random(0,4294967296,N3), random(0,4294967296,N4), XG is N3 mod XD + 1, YG is N4 mod YD + 1,
   assertz(gold(XG,YG)), write(gold(XG,YG)),

   no_of_random_pits(P), create_pits(P).

create_pits(0) :- !.
create_pits(M) :-
   xdim(XD), ydim(YD),
   random(0,4294967296,N1), random(0,4294967296,N2), XP is N1 mod XD + 1, YP is N2 mod YD + 1,
   ( XP+YP < 4 -> create_pits(M) ; assertz(pit(XP,YP)), write(pit(XP,YP)) ),
   M1 is M-1, create_pits(M1).


perform(turn, []) :-
        write('turn'), nl,
        current_state([at(X,Y),facing(D)]),
        retract(current_state([at(X,Y),facing(D)])),
        ( D < 4 -> D1 is D+1 ; D1 is 1 ),
        assertz(current_state([at(X,Y),facing(D1)])).

perform(enter, [Breeze,Stench,Glitter]) :-
        write('enter'), nl,
        current_state(Z),
        retract(current_state(Z)),
        assertz(current_state([at(1,1),facing(1)])),
        ( gold(1,1) -> Glitter = true ; Glitter = false ),
        ( (wumpus(1,2) ; wumpus(2,1)) -> Stench = true ;
            Stench = false ),
        ( (pit(2,1) ; pit(1,2)) -> Breeze = true ;
            Breeze = false ).

perform(exit, []) :-
        write('exit'), nl,
        current_state([at(X,Y),facing(D)]),
        retract(current_state([at(X,Y),facing(D)])),
        assertz(current_state([])).

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
        assertz(current_state([at(X1,Y1),facing(D)])),
        ( gold(X1,Y1) -> Glitter = true ; Glitter = false ),
        X_east is X1+1, X_west is X1-1, Y_north is Y1+1, Y_south is Y1-1,
        ( (wumpus(X_east,Y1) ; wumpus(X1,Y_north) ;
           wumpus(X_west,Y1) ; wumpus(X1,Y_south)) -> Stench = true ;
            Stench = false ),
        ( (pit(X_east,Y1) ; pit(X1,Y_north) ;
           pit(X_west,Y1) ; pit(X1,Y_south)) -> Breeze = true ;
            Breeze = false ).
