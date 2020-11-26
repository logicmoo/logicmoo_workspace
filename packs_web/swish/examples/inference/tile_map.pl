/* Tile Map Generation */

/** <examples>

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
  WC is W//2,!.

% in the central area water is more probable (there tend to be a lake in the
% center)
pick_tile(Y,X,H,W,T):discrete(T,[grass:0.05,water:0.9,tree:0.025,rock:0.025]):-
  central_area(Y,X,H,W),!.

% on the other places tiles are chosen at random with this distribution
pick_tile(_,_,_,_,T):discrete(T,[grass:0.5,water:0.3,tree:0.1,rock:0.1]).


:-end_lpad.

central_area(Y,X,H,W):-
  HC is H//2,
  WC is W//2,
  adjacent(HC,WC,Y1,X1,H,W),
  adjacent(Y1,X1,Y,X,H,W).

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
tile(grass,'http://cplint.lamping.unife.it/icons/tiles/grass.png').
tile(water,'http://cplint.lamping.unife.it//icons/tiles/water.png').
tile(rock,'http://cplint.lamping.unife.it//icons/tiles/rock.png').
tile(tree,'http://cplint.lamping.unife.it//icons/tiles/tree.png').
