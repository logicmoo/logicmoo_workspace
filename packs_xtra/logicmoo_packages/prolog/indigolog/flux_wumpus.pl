
:- dynamic gridsize/1.
gridsize(8).

elementary_action(enter).
elementary_action(exit).
elementary_action(turn).
elementary_action(grab).
elementary_action(shoot).
elementary_action(go).
compound_action(go_to(_,_)).

 :-dynamic(state_update/4).

 state_update(Z1,enter,Z2,[B,S,G]) :-
   update(Z1,[at(1,1),facing(2)],[],Z2),
   breeze_perception(1,1,B,Z2),
   stench_perception(1,1,S,Z2),
   glitter_perception(1,1,G,Z2).

 state_update(Z1,exit,Z2,[]) :-
   holds(facing(D),Z1),
   update(Z1,[],[at(1,1),facing(D)],Z2).

   /*
 state_update(Z1,turn,Z2,[]) :-
   holds(facing(D),Z1),
   (D#<4 #/\ D1#=D+1) #\/ (D#=4 #/\ D1#=1),
   update(Z1,[facing(D1)],[facing(D)],Z2).
*/

state_update(A57, turn, A59, []) :-
        holds(facing(A69), A57),
        /*fd_arith : */ fd_gec_ent(0, -1, A69, 3, 0, A85),
        /*fd_arith : */ fd_eq([-1, -1 * A69, 1 * B06], A94),
        /*fd_arith : */ fd_eq([-2, 1 * A85, 1 * A94], B15),
        '#='(A69, 4, B34),
        '#='(B06, 1, B41),
        /*fd_arith : */ fd_eq([-2, 1 * B34, 1 * B41], B50),
        /*fd_arith : */ fd_gec(B15, 1, B50, -1, 0),
        update(A57, [facing(B06)], [facing(A69)], A59).


 state_update(Z1,grab,Z2,[]) :-
   holds(at(X,Y),Z1),
   update(Z1,[has(gold)],[gold(X,Y)],Z2).

 state_update(Z1,shoot,Z2,[S]) :-
   ( S=true, update(Z1,[dead],[has(arrow)],Z2)
     ; S=false, update(Z1,[],[has(arrow)],Z2) ).

 state_update(Z1,go,Z2,[B,S,G]) :-
   holds(at(X,Y),Z1), holds(facing(D),Z1),
   adjacent(X,Y,D,X1,Y1),
   update(Z1,[at(X1,Y1)],[at(X,Y)],Z2),
   breeze_perception(X1,Y1,B,Z2),
   stench_perception(X1,Y1,S,Z2),
   glitter_perception(X1,Y1,G,Z2).

 state_update(Z1,go_to(X1,Y1),Z2,[]) :-
   holds(at(X,Y),Z1),
   update(Z1,[at(X1,Y1)],[at(X,Y)],Z2).

    :-dynamic(stench_perception/4).

 /*
    stench_perception(X,Y,Percept,Z) :-
      XE#=X+1, XW#=X-1, YN#\=Y+1, YS#=Y-1,
      ( Percept=false, not_holds(wumpus(XE,Y),Z),
                       not_holds(wumpus(XW,Y),Z),
                       not_holds(wumpus(X,YN),Z),
                       not_holds(wumpus(X,YS),Z) ;
        Percept=true,
          or([wumpus(XE,Y),wumpus(X,YN),
              wumpus(XW,Y),wumpus(X,YS)],Z) ).
*/
    stench_perception(X,Y,Percept,Z) :-
      '#='(XE,X+1),'#='(XW,X-1),'#='(YN,Y+1),'#='(YS,Y-1), 
      ( Percept=false, not_holds(wumpus(XE,Y),Z),
                       not_holds(wumpus(XW,Y),Z),
                       not_holds(wumpus(X,YN),Z),
                       not_holds(wumpus(X,YS),Z) ;
        Percept=true,
          or([wumpus(XE,Y),wumpus(X,YN),
              wumpus(XW,Y),wumpus(X,YS)],Z) ).


 breeze_perception(X,Y,Percept,Z) :-
   %% XE#=X+1, XW#=X-1, YN#=Y+1, YS#=Y-1,
   '#='(XE,X+1),'#='(XW,X-1),'#='(YN,Y+1),'#='(YS,Y-1), 
   ( Percept=false, not_holds(pit(XE,Y),Z),
                    not_holds(pit(XW,Y),Z),
                    not_holds(pit(X,YN),Z),
                    not_holds(pit(X,YS),Z) ;
     Percept=true,
       or([pit(XE,Y),pit(X,YN),
           pit(XW,Y),pit(X,YS)],Z) ).

 glitter_perception(X,Y,Percept,Z) :-
   Percept=false, not_holds(gold(X,Y),Z) ;
   Percept=true,  holds(gold(X,Y),Z).


 :-dynamic(adjacent/5).

 /*
 adjacent(X,Y,D,X1,Y1) :-
   gridsize(N),
   [X,Y,X1,Y1]::1..N, D::1..4,
       (D#=1) #/\ (X1#=X)   #/\ (Y1#=Y+1) % north
   #\/ (D#=3) #/\ (X1#=X)   #/\ (Y1#=Y-1) % south
   #\/ (D#=2) #/\ (X1#=X+1) #/\ (Y1#=Y)   % east
   #\/ (D#=4) #/\ (X1#=X-1) #/\ (Y1#=Y).  % west
 */

adjacent(A57, A58, A59, A60, A61) :-
        gridsize(A66),
    '::'([A57, A58, A60, A61], '..'(1 , A66)),
    fd_domain : fd_dom_simple(A59, dom(['..'(1,4)], 4)),
        #=(A59, 1, B07),
        #=(A60, A57, B14),
        /*fd_arith : */ fd_eq([-2, 1 * B07, 1 * B14], B23),
        /*fd_arith : */ fd_eq([-1, -1 * A58, 1 * A61], B44),
        /*fd_arith : */ fd_eq([-2, 1 * B23, 1 * B44], B65),
        #=(A59, 3, B84),
        #=(A60, A57, B91),
        /*fd_arith : */ fd_eq([-2, 1 * B84, 1 * B91], C00),
        /*fd_arith : */ fd_eq([1, -1 * A58, 1 * A61], C21),
        /*fd_arith : */ fd_eq([-2, 1 * C00, 1 * C21], C42),
        /*fd_arith : */ fd_gec_ent(B65, 1, C42, -1, 0, C67),
        #=(A59, 2, C74),
        /*fd_arith : */ fd_eq([-1, -1 * A57, 1 * A60], C83),
        /*fd_arith : */ fd_eq([-2, 1 * C74, 1 * C83], D04),
        #=(A61, A58, D23),
        /*fd_arith : */ fd_eq([-2, 1 * D04, 1 * D23], D32),
        /*fd_arith : */ fd_gec_ent(C67, 1, D32, -1, 0, D57),
        #=(A59, 4, D64),
        /*fd_arith : */ fd_eq([1, -1 * A57, 1 * A60], D73),
        /*fd_arith : */ fd_eq([-2, 1 * D64, 1 * D73], D94),
        #=(A61, A58, E13),
        /*fd_arith : */ fd_eq([-2, 1 * D94, 1 * E13], E22),
        /*fd_arith : */ fd_gec(D57, 1, E22, -1, 0).


init(Z0) :- Z0 = [has(arrow),wumpus(WX,WY)|Z],
	    gridsize(N),
            '::'([WX,WY] , ['..'(1,N)]),
            not_holds(wumpus(1,1),Z0),
            not_holds_all(wumpus(_,_),Z),
            not_holds(dead,Z),
            not_holds(pit(1,1),Z),
            N2 is N+1,
            not_holds_all(pit(_,0),Z), %boundary
            not_holds_all(pit(_,N2),Z),
            not_holds_all(pit(0,_),Z),
            not_holds_all(pit(N2,_),Z),
            not_holds_all(at(_,_),Z),
            not_holds_all(facing(_),Z),
            duplicate_free(Z0).


 main_wumpus :- init(Z0), execute(enter,Z0,Z1),
         Cpts=[1,1,[1,2]], Vis=[[1,1]], Btr=[], 
         main_loop(Cpts,Vis,Btr,Z1).


 main_loop([X,Y,Choices|Cpts],Vis,Btr,Z) :-
   Choices=[Dir|Dirs] ->
     (explore(X,Y,Dir,Vis,Z,Z1) ->
        knows_val([X1,Y1],at(X1,Y1),Z1),
        hunt_wumpus(X1,Y1,Z1,Z2),
        (knows(gold(X1,Y1),Z2) ->
           execute(grab,Z2,Z3), go_home(Z3)
         ; Cpts1=[X1,Y1,[1,2,3,4],X,Y,Dirs|Cpts],
           Vis1=[[X1,Y1]|Vis], Btr1=[X,Y|Btr],
           main_loop(Cpts1,Vis1,Btr1,Z2) )
      ; main_loop([X,Y,Dirs|Cpts],Vis,Btr,Z) )
   ; backtrack(Cpts,Vis,Btr,Z).

 explore(X,Y,D,V,Z1,Z2) :-
   adjacent(X,Y,D,X1,Y1), \+ member([X1,Y1],V),
   knows_not(pit(X1,Y1),Z1),
   (knows_not(wumpus(X1,Y1),Z1);knows(dead,Z1)),
   turn_to(D,Z1,Z), execute(go,Z,Z2).

 backtrack(_,_,[],Z) :- execute(exit,Z,_).
 backtrack(Cpts,Vis,[X,Y|Btr],Z) :- 
   go_back(X,Y,Z,Z1), main_loop(Cpts,Vis,Btr,Z1).

 go_back(X,Y,Z1,Z2) :-
   holds(at(X1,Y1),Z1), adjacent(X1,Y1,D,X,Y),
   turn_to(D,Z1,Z), execute(go,Z,Z2).
 turn_to(D,Z1,Z2) :-
   knows(facing(D),Z1) -> Z2=Z1
   ; execute(turn,Z1,Z), turn_to(D,Z,Z2).

 hunt_wumpus(X,Y,Z1,Z2) :-
   \+ knows(dead,Z1),
   knows_val([WX,WY],wumpus(WX,WY),Z1),
   in_direction(X,Y,D,WX,WY)
   -> turn_to(D,Z1,Z), execute(shoot,Z,Z2)
    ; Z2=Z1.

:-dynamic(in_direction/5).
/*
 in_direction(X,Y,D,X1,Y1) :-
   gridsize(N),
   '::'([X,Y,X1,Y1], '..'(1,N)), '::'(D,'..'(1,4)),
       ( D #= 1 ) #/\ (X1#=X) #/\ (Y1#>Y)  % north
   #\/ (D#=3) #/\ (X1#=X) #/\ (Y1#<Y)  % south
   #\/ (D#=2) #/\ (X1#>X) #/\ (Y1#=Y)  % east
   #\/ (D#=4) #/\ (X1#<X) #/\ (Y1#=Y). % west
*/
in_direction(VAR157, VAR158, VAR159, VAR160, VAR161) :-
        gridsize(VAR166),
        '::'([VAR157, VAR158, VAR160, VAR161], '..'(1 , VAR166)),
        fd_domain : fd_dom_simple(VAR159, dom(['..'(1,4)], 4)),
        #=(VAR159, 1, VAR207),
        #=(VAR160, VAR157, VAR214),
        /*fd_arith : */ fd_eq([-2, 1 * VAR207, 1 * VAR214], VAR223),
        /*fd_arith : */ fd_gec_ent(VAR161, -1, VAR158, -1, 0, VAR248),
        /*fd_arith : */ fd_eq([-2, 1 * VAR223, 1 * VAR248], VAR257),
        #=(VAR159, 3, VAR276),
        #=(VAR160, VAR157, VAR283),
        /*fd_arith : */ fd_eq([-2, 1 * VAR276, 1 * VAR283], VAR292),
        /*fd_arith : */ fd_gec_ent(VAR158, -1, VAR161, -1, 0, VAR317),
        /*fd_arith : */ fd_eq([-2, 1 * VAR292, 1 * VAR317], VAR326),
        /*fd_arith : */ fd_gec_ent(VAR257, 1, VAR326, -1, 0, VAR351),
        #=(VAR159, 2, VAR358),
        /*fd_arith : */ fd_gec_ent(VAR160, -1, VAR157, -1, 0, VAR371),
        /*fd_arith : */ fd_eq([-2, 1 * VAR358, 1 * VAR371], VAR380),
        #=(VAR161, VAR158, VAR399),
        /*fd_arith : */ fd_eq([-2, 1 * VAR380, 1 * VAR399], VAR408),
        /*fd_arith : */ fd_gec_ent(VAR351, 1, VAR408, -1, 0, VAR433),
        #=(VAR159, 4, VAR440),
        /*fd_arith : */ fd_gec_ent(VAR157, -1, VAR160, -1, 0, VAR453),
        /*fd_arith : */ fd_eq([-2, 1 * VAR440, 1 * VAR453], VAR462),
        #=(VAR161, VAR158, VAR481),
        /*fd_arith : */ fd_eq([-2, 1 * VAR462, 1 * VAR481], VAR490),
        /*fd_arith : */ fd_gec(VAR433, 1, VAR490, -1, 0).


 go_home(Z) :-
   plan(find_path([]),Plan,Z),
   distances(L), retract(distances(L)), assert(distances([])),
   execute(Plan,Z,Z1), execute(exit,Z1,_).

:-dynamic(plan_proc/2).

/*
plan_proc(find_path(_160), _158) :-
        _158 = (?(home)#[?(poss_go(_174, _175, _160, _177)), go_to(_174, _175), find_path(_177)]).
*/

 plan_proc(find_path(Vis),P) :-
   P = '#'(?(home) ,
        [?(poss_go(X,Y,Vis,Vis1)), go_to(X,Y),
	 find_path(Vis1)]).

 home(S,Z) :- knows(at(1,1),S,Z).

:- dynamic(distances/1).
:- assert(distances([])).

poss_go(X1,Y1,Vis,Vis1,S,Z) :-
   knows_val([X,Y],at(X,Y),S,Z),
   ( D=1 ; D=2 ; D=3 ; D=4 ),
   adjacent(X,Y,D,X1,Y1),
   \+ member([X1,Y1],Vis),
   knows_not(pit(X1,Y1),Z),
   ( \+ knows(dead,Z)->knows_not(wumpus(X1,Y1),Z)
     ; true ),
   Vis1=[[X,Y]|Vis],
   distances(L), length(Vis1,N), \+ ( member([X1,Y1,M],L), M=<N ),
   retract(distances(L)), assert(distances([[X1,Y1,N]|L])).

plan_cost(find_path(_), Plan, Cost) :-
   length(Plan, Cost).

 execute_compound_action(go_to(X,Y),Z1,Z2) :-
   holds(at(X1,Y1),Z1), adjacent(X1,Y1,D,X,Y),
   turn_to(D,Z1,Z), execute(go,Z,Z2).

