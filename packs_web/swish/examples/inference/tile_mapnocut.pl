/* Tile Map Generation
Version without cut
 */

/** <examples>

?- Height=10,Width=10,mc_mh_sample_arg(map(Height,Width,M),constraints(Height,Width),1,1,M,[[Map] - _]).
?- Height=10,Width=10,mc_rejection_sample_arg(map(Height,Width,M),constraints(Height,Width),1,M,[[Map] - _]).
?- Height=10,Width=10,mc_sample_arg_first(map(Height,Width,M),1,M,[ Map- _ ]).

*/

:- use_module(library(mcintyre)).
:- if(current_predicate(use_rendering/1)).
:- use_rendering(tiles).
:- endif.



:-mc.

:-begin_lpad.

map(H,W,M):-
  tiles(Tiles),
  length(Rows,H),
  M=..[map,Tiles|Rows],
  foldl(select(H,W),Rows,1,_).

select(H,W,Row,N0,N):-
  length(RowL,W),
  N is N0+1,
  Row=..[row|RowL],
  foldl(pick_row(H,W,N0),RowL,1,_).

pick_row(H,W,N,T,M0,M):-
  M is M0+1,
  pick_tile(N,M0,H,W,T).

% constraints on map generation
% the center tile is water
pick_tile(HC,WC,H,W,water):-
  HC is H//2,
  WC is W//2.

% on the other places tiles are chosen at random with this distribution
pick_tile(Y,_,H,_,T):discrete(T,[grass:0.5,water:0.2,tree:0.1,rock:0.1]):-
  Y =\= H//2.

pick_tile(Y,X,H,W,T):discrete(T,[grass:0.5,water:0.2,tree:0.1,rock:0.1]):-
  Y is H//2,
  X =\= W//2.

% constraints after map generation (soft constraints)
% tiles adjacent to water are more probably water
% Markov logic constraint:
% -ln 0.1 forall Y,X,Y1,X1 such that adjacent(Y,X,Y1,X1,H,W):
% pick_tile(Y,X,H,W,water)=>pick_tile(Y1,X1,H,W,water)

constraint_water(Y,X,_Y1,_X1,H,W):-
  pick_tile(Y,X,H,W,T),
  T \= water.

constraint_water(_Y,_X,Y1,X1,H,W):-
  pick_tile(Y1,X1,H,W,water).

constraint_water(Y,X,Y1,X1,H,W):0.1:-
  pick_tile(Y,X,H,W,water),
  \+ pick_tile(Y1,X1,H,W,water).

constraints(H,W):-
  HC is H//2,
  WC is W//2,
  H1 is HC,
  H2 is HC,
  W1 is WC,
  W2 is WC,
  findall((Y,X,Y1,X1),(
    between(H1,H2,Y),between(W1,W2,X),adjacent(Y,X,Y1,X1,H,W)),L),
  maplist(call_const(H,W),L).

call_const(H,W,(Y,X,Y1,X1)):-
  constraint_water(Y,X,Y1,X1,H,W).

:-end_lpad.

adjacent(Y,X,Y1,X1,H,W):-
  Y >= 1, Y =< H,
  X >= 1, X =< W,
  side(Y,X,Y1,X1,H,W).

adjacent(Y,X,Y1,X1,H,W):-
  Y >= 1, Y =< H,
  X >= 1, X =< W,
  corner(Y,X,Y1,X1,H,W).

side(Y,X,Y1,X1,H,W):-
  left(Y,X,Y1,X1,H,W).

side(Y,X,Y1,X1,H,W):-
  right(Y,X,Y1,X1,H,W).

side(Y,X,Y1,X1,H,W):-
  above(Y,X,Y1,X1,H,W).

side(Y,X,Y1,X1,H,W):-
  below(Y,X,Y1,X1,H,W).

% upper left corner
corner(Y,X,Y1,X1,H,W):-
  left(Y1,X,Y1,X1,H,W),
  above(Y,X1,Y1,X1,H,W).

% upper right corner
corner(Y,X,Y1,X1,H,W):-
  right(Y1,X,Y1,X1,H,W),
  above(Y,X1,Y1,X1,H,W).

% lower left corner
corner(Y,X,Y1,X1,H,W):-
  left(Y1,X,Y1,X1,H,W),
  below(Y,X1,Y1,X1,H,W).

% lower right corner
corner(Y,X,Y1,X1,H,W):-
  right(Y1,X,Y1,X1,H,W),
  below(Y,X1,Y1,X1,H,W).

left(Y,X,Y,X1,_,_):-
  X > 1,
  X1 is X-1.

right(Y,X,Y,X1,_,W):-
  X < W,
  X1 is X+1.

above(Y,X,Y1,X,_,_):-
  Y > 1,
  Y1 is Y-1.

below(Y,X,Y1,X,H,_):-
  Y < H,
  Y1 is Y+1.

tile_names(T):-
  findall(Tile,tile(Tile,_URL),T).

tiles(T):-
  findall(tile(Tile,URL),tile(Tile,URL),T).

% available tiles
% format: tile(name,URL)
% tiles from https://github.com/silveira/openpixels
tile(grass,'/icons/tiles/grass.png').
tile(water,'/icons/tiles/water.png').
tile(rock,'/icons/tiles/rock.png').
tile(tree,'/icons/tiles/tree.png').
