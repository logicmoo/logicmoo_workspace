%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%% boards.pl

%================================================================================
% Starting Game
%================================================================================
% Initialize the board (create all the empty squares),
% set the initial stage to be assign, which will terminate
% if there is no assignment stage for the game. 
% The game starts with PLAYER in control, and MOVE_COUNT = 0.
% The initial setup is determined, possibly using a random
% setup as returned from the hook:  random_assignments(Assignments).
% (See parse.pl)
 

% ----------
% START_GAME
% ----------
% Passes a dummy variable to start_game/1, as for now the current
% game is passed through the asserted procedures:
%  player_current_game/1
%  opponent_current_game/1
% 
% This is usually achieved earlier by calling the predicate:
%    file_make_test_game/1
% with a file in which a game is stored.  
% A cleaner verion would pass the game as a variable too, and the dummy
% variable reminds us of this!

start_game :- start_game(_Game).

start_game(Game) :-
	initialize_state_properties,
	create_board_and_setup(Game).


default_init_move_count(0).
default_init_stage(assign).
default_init_control(player).

%initialize_state_properties :- 
%	put_stage(assign),
%	put_control(player),
%	set_initial_move_count.

initialize_state_properties :- 
	default_init_stage(Stage),
	default_init_control(Player),
	default_init_move_count(Count), 
	initialize_state_properties(Stage,Player,Count).

initialize_state_properties(Stage,Player,Count) :- 
	put_stage(Stage),
	put_control(Player),
	add(move_count(Count)).


create_board_and_setup(Game) :-
  	initialize_board(Game),
	create_initial_setup(Game).


create_initial_setup(Game) :-
	game_has_assignments(Game,Ass),
	do_assignments(Ass).


%================================================================================
% Initial Generation of Board
%================================================================================
% A board is a set of squares, initially empty, and a connectivity
% relation between them.

initialize_board(_Game) :-
	make_empty_board.

make_empty_board :-
	current_board(B),
	board_size(B,X,Y),
	array_squares(X,Y,Squares),
	make_empty(Squares).

make_empty([]).
make_empty([Sq|Rest]) :-
	set_empty(Sq),
	make_empty(Rest).

%================================================================================
% Determining initial setup:  
%================================================================================
% a. arbitrary, put the pieces on the squares.
% b. (player) assigns:  Put the pieces in player's hands,
%    and assert that player can assign to the specified squares.
% (c. random: by this point, the initial assignment is already determined by
%    an external source.  It will thus look like an arbitrary assignment,
%    so we need no special case.)
do_assignments(As) :-
	arbitrary_assignment(As), !,
	do_arbitrary_assignment(As).
do_assignments(As) :-
	assignment_decision(As,Assigner,PieceNames,Squares), 
	player(Assigner), !,
	invert(Assigner=Squares,opponent,OppAssigner=OppSquares),
	do_assignments_for_player(player,Assigner,PieceNames,Squares),
	do_assignments_for_player(opponent,OppAssigner,PieceNames,OppSquares).

assignment_decision(As,Assigner,PieceNames,Squares) :- 
	decision(As,Assigner,PieceNames,Squares).


do_assignments_for_player(Owner,Placer,PieceNames,Squares) :-
	make_assignable_squares(Placer,Squares),
	place_pieces_in_hand(PieceNames,Owner,Placer).

make_assignable_squares(Player,Squares) :- 
	( retract(assignable_squares(Player,_)) -> true ; true),
	assert(assignable_squares(Player,Squares)).

	

% Arbitary can be:
% 1.  a list of assignments of pieces to squares.
% This is the general case for symmetric opening games.
% 2.  a structure assign(A1,A2), where A1 is an assignment
% for player, and A2 for opponent.  
% This is only used for interface setup, at the moment.
% (Actually, we don't use this at all yet).  
arbitrary_assignment([_|_]).
%arbitrary_assignment(assign(A1,A2)).

do_arbitrary_assignment(As) :- 
	place_pieces_on_squares(As).

place_pieces_on_squares(Assign) :- 
	uncollect(Assign,As), % from generator/gen.pl
	place_pieces_on_squares(As,player), 
	invert(As,opponent,IAs),
	place_pieces_on_squares(IAs,opponent).

place_pieces_on_squares([],_).
place_pieces_on_squares([A|As],Player) :- 
	assign_piece_to_square(A,Player), 
	place_pieces_on_squares(As,Player).

assign_piece_to_square(P=Sq,Player) :-
	piece_struct(Piece,P,Player),
	place_piece(Piece,Sq).


% place_pieces_in_hand(P,Player,Hand)
% Player owns the pieces.  
% Hand will assign them. 

place_pieces_in_hand([],_Player,_Hand).
place_pieces_in_hand([P|Ps],Player,Hand) :-
	  place_piece_in_hand(P,Player,Hand),
	  place_pieces_in_hand(Ps,Player,Hand).

place_piece_in_hand(P,Player,Hand) :-
	piece_struct_name(Piece,P),
	owns(Piece,Player),
	put_in_hand(Piece,Hand).

%================================================================================
% Board Connectivity
%================================================================================
% Convert squares and directions as generated to simple ordered pairs.
% Really need Game as an arg here, to decide vertical and board size.
connected(S1,S2,Dir) :-
	square(S1,Xf,Yf),
	square(S2,Xt,Yt),
	direction(Dir,DX,DY),
	current_board_type(T),
	conn_for_type(T,(Xf,Yf),(Xt,Yt),(DX,DY)).

conn_for_type(planar,From,To,Dir) :- conn(From,To,Dir).
conn_for_type(vertical_cylinder,From,To,Dir) :- conn_cyl(From,To,Dir).


%The structure of the board is represented by the connected relation that
%defines how each location (Xf,Yf) on the board is connected to each 
%other location (Xt,Yt) via a vector (Dx,Dy) defined as:
%Xt = Xf + Dx,
%Yt = Yf + Dy
%Note: this is intended to be an ``operational'' predicate:
%This predicate is used for all generated games.  
% (Thanks to Nick Flann for this predicate).
conn((Xf,Yf),(Xt,Yt),(Dx,Dy)):-
   (var(Xf),var(Yf) ->
    Xf is Xt - Dx,
    Yf is Yt - Dy,
    legal_location((Xf,Yf))
   |var(Xt),var(Yt) ->
    Xt is Xf + Dx,
    Yt is Yf + Dy,
    legal_location((Xt,Yt))
   |var(Dx),var(Dy) ->
    Dx is Xt - Xf,
    Dy is Yt - Yf
   |otherwise ->
    Xf is Xt - Dx,
    Yf is Yt - Dy
   ).


% This pred. is instantiated for the board dimensions chosen
% in the game.  
% Make max and min specific constants. 
legal_location(Sq):- on_board((Sq)).

on_board((X,Y)):-
   current_board_size(XMax,YMax),
   X >= 1,
   X =< XMax,
   Y >= 1,
   Y =< YMax.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Cylindrical Boards
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


conn_cyl((Xf,Yf),(Xt,Yt),(Dx,Dy)):-
   (var(Xf),var(Yf) ->
    Xf1 is Xt - Dx,
    Yf1 is Yt - Dy,
    legal_location_cyl((Xf1,Yf1),(Xf,Yf))
   |var(Xt),var(Yt) ->
    Xt1 is Xf + Dx,
    Yt1 is Yf + Dy,
    legal_location_cyl((Xt1,Yt1),(Xt,Yt))
   |var(Dx),var(Dy) ->
    Dx is Xt - Xf,
    Dy is Yt - Yf
   |otherwise ->
    Xf1 is Xt - Dx,
    Yf1 is Yt - Dy,
    legal_location_cyl((Xf1,Yf1),(Xf,Yf))
   ).



% This pred. is instantiated for the board dimensions chosen
% in the game.  
% This won't work backwards to generate a from square.
% Add an XN to cover 1 negative wrap, as mod undef for X<0.
legal_location_cyl((X1,Y1),(X,Y)):-
   current_board_size(XN,_YN),
   X is ( (X1 + XN - 1) mod XN ) + 1,
   Y = Y1,
   on_board((X,Y)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Scaling # leaps to until it is off the board or wraps 
% back to original square (for vertical cylinder boards).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

valid_min(N,N) :- number(N), !.
valid_min(_,1).


% VALID_MAX(Constraint,Dir,Max)
% If Constraint already a number, return it.
% Otherwise, find the constraint based on the
% direction and the board. 
valid_max(N,_,N) :- number(N), !.
valid_max(_,Dir,Max) :-
	direction(Dir,Dx,Dy),
	XMag is abs(Dx),
	YMag is abs(Dy),
	valid_max_dir(XMag,YMag,Max).

% VALID_MAX_DIR(+XMag,+YMag,-Max)
% Given a magnitude for X and Y offsets
% of each leap, MAX is the number of
% leaps which need be considered on the board.
%
% If both are 0, only 1 leap is necessary, max!
% If X is 0, it's just the number of leaps to 
%     cross the board to last rank.
% If Y is 0, use the max leaps to cross the
%    X axis, which varies on the type of board.
% If neither 0 but on a cylinder board, the Y axis
%    is the constraint.
% Else, the max is the min of the maxes on both axes.
valid_max_dir(0,0,1) :- !.
valid_max_dir(0,Dy,Max) :- !,
	current_board_size(_XMax,YMax),
	Max is YMax // Dy.
valid_max_dir(Dx,0,Max) :- !,
	current_board_type(Type),
	current_board_size(XMax,_YMax),
	max_leaps(Type,XMax,Dx,Max).
valid_max_dir(_Dx,Dy,Max) :- 
	current_board_type(vertical_cylinder), !,
	current_board_size(_XMax,YMax),
	Max is YMax // Dy.
valid_max_dir(Dx,Dy,Max) :- 
	current_board_size(Bx,By),
	XMax is Bx // Dx,
	YMax is By // Dy,
	min(XMax,YMax,Max).


max_leaps(planar,BMax,Delta,Max) :- !,
	Max is BMax // Delta.
max_leaps(vertical_cylinder,BMax,Delta,Max) :-
	wrap_leaps(BMax,Delta,Max).
	


wl(A,B,C) :- wrap_leaps(A,B,C).

wrap_leaps(Board,D,Max) :-
	gcf(Board,D,F),
	max(Board,D,M),
	Max is M // F.

% gcf(HighestCurrentFactor,CurrentRemainder,GCF).
% This is the Euclidan algorithm.
gcf(A,0,A) :- !.
gcf(A,B,F) :-
	M is A mod B,
	gcf(B,M,F).

