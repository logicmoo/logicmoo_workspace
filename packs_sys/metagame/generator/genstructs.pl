%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%% genstructs.pl
%%% Data Structures used in generating and manipulating game structures. 

:- my_ensure_loaded(library(invert)).

%===========================================================================
%			   Data Structures
%===========================================================================

%===========================================================================
% GAME Data Structure
%===========================================================================
game(X) :- functor(X,game,5).

game(game(N,B,P,G,C),N,B,P,G,C).

game_name(G,X) :- arg(1,G,X).
game_board(G,X) :- arg(2,G,X).
game_pieces(G,X) :- arg(3,G,X).
game_goal(G,X) :- arg(4,G,X).
game_constraints(G,X) :- arg(5,G,X). 

%===========================================================================
% BOARD Data Structure
%===========================================================================

board(B,Size,Type,Inv,R,Arr,K,Types,Set,Ass) :- 
	board_size(B,Size), 
	board_type(B,Type), 
	board_inversion(B,Inv), 
	board_promote_rows(B,R),
	board_array_rows(B,Arr),
	board_killed(B,K),
	board_piece_types(B,Types),
	board_placed_pieces(B,Set),
	board_assignments(B,Ass).
board(X) :- functor(X,board,9).

board_size(B,X) :- arg(1,B,X).
board_type(B,X) :- arg(2,B,X).
board_inversion(B,X) :- arg(3,B,X).
board_promote_rows(B,X) :- arg(4,B,X).
board_killed(B,X) :- arg(5,B,X).
board_piece_types(B,X) :- arg(6,B,X). 
board_placed_pieces(B,X) :- arg(7,B,X). 
board_assignments(B,X) :- arg(8,B,X).
board_array_rows(B,X) :- arg(9,B,X).

board_unplaced_pieces(Board,Set) :- 
	board_placed_pieces(Board,Placed),
	board_piece_types(Board,Types),
	ord_subtract(Types,Placed,Set).


board_size(B,X,Y) :-
	board_size(B,Size),
	size(Size,X,Y).

board_max_size(B,M) :-
	board_size(B,X,Y),
	max(X,Y,M).


board_assigned_squares(Board,Squares) :-
	board_assignments(Board,As),
	assignments_use_squares(As,Squares).

board_player_assigned_squares(Board,Player,Squares) :-
	board_assigned_squares(Board,Squares1),
	invert_board_squares_player(Player,Board,Squares1,Squares).

assignments_use_squares(As,Squares) :- 
	decision(As,_Player,_PieceSet,Squares), !.
assignments_use_squares(Assigns,Squares) :- 
	assigned_squares(Assigns,Squares).

assigned_squares(Assigns,Squares) :- 
	uncollect(Assigns,Pairs),
	unpair(Pairs,Squares).

invert_board_squares_player(player,_Board,Squares,Squares).
invert_board_squares_player(opponent,Board,Squares1,Squares) :- 
	invert_board_squares(Board,Squares1,Squares).

invert_board_squares(Board,Squares1,Squares) :- 
	board_size(Board,XN,YN),
	board_inversion(Board,Inv),
	invert_squares_dim(Squares1,Inv,XN,YN,Squares).

invert_squares_dim([],_Inv,_XN,_YN,[]).
invert_squares_dim([Sq|Sqs],Inv,XN,YN,[ISq|ISqs]) :- 
	invert_square_dim(Inv,XN,YN,Sq,ISq),
	invert_squares_dim(Sqs,Inv,XN,YN,ISqs).

invert_square_on_board(Board,Sq1,Sq2) :-
	board_size(Board,XN,YN),
	board_inversion(Inv),
	invert_square_dim(Inv,XN,YN,Sq1,Sq2).

%===========================================================================
% SIZE Data Structure
%===========================================================================

size(size(X,Y),X,Y).

%===========================================================================
% PIECE Data Structure
%===========================================================================

piece(Def) :- piece_definition(Def).

piece_definition(Def) :- functor(Def,piece,5).

piece_definition(Def,Name,Movement,Capture,Promote,Con) :-
	piece_definition(Def),
	piece_name(Def,Name),
	piece_movement(Def,Movement),
	piece_capture(Def,Capture),
	piece_promote(Def,Promote),
	piece_constraints(Def,Con).

piece_name(Piece,X) :- arg(1,Piece,X).
piece_movement(Piece,X)  :-   arg(2,Piece,X).
piece_capture(Piece,X)  :-   arg(3,Piece,X).
piece_promote(Piece,X)  :-   arg(4,Piece,X).
piece_constraints(Piece,X)  :-   arg(5,Piece,X).

%===========================================================================
% DIRECTION Data Structure
%===========================================================================

direction(dir(X,Y),X,Y).

%===========================================================================
% SYMMETRY Data Structure
%===========================================================================

symmetry(X) :- functor(X,symmetry,3).

symmetry(symmetry(F,S,R),F,S,R).

sym_forward(Sym,F) :- arg(1,Sym,F).
sym_side(Sym,S) :- arg(2,Sym,S).
sym_rotation(Sym,R) :- arg(3,Sym,R).

forward(Sym) :- sym_forward(Sym,yes).
side(Sym) :- sym_side(Sym,yes).
rotation(Sym) :- sym_rotation(Sym,yes).


has_symmetry(Sym,forward) :- forward(Sym).
has_symmetry(Sym,side) :- side(Sym).
has_symmetry(Sym,rotation) :- rotation(Sym).
	               

%===========================================================================
% LEAPER Data Structure
%===========================================================================

leaper(leaper).

%===========================================================================
% RIDER Data Structure
%===========================================================================

rider(rider(Must,Min,Max),Must,Min,Max).
rider(R) :- functor(R,rider,3).

rider_must(R,Must) :- arg(1,R,Must).
rider_min(R,Min) :- arg(2,R,Min).
rider_max(R,Max) :- arg(3,R,Max).

rider_must(Rider) :- rider_must(Rider,yes).

%===========================================================================
% HOPPER Data Structure
%===========================================================================

hopper(hopper(Restr,B,O,A),Restr,B,O,A).
hopper(H) :- functor(H,hopper,4).

hopper_type(H,X) :- hopper(H), arg(1,H,X).
hopper_before(H,X) :- hopper(H), arg(2,H,X).
hopper_over(H,X) :- hopper(H), arg(3,H,X).
hopper_after(H,X) :- hopper(H), arg(4,H,X).

%===========================================================================
% MOVEMENT Data Structure
%===========================================================================

movement(M) :- functor(M,movement,3).

movement_type(M,X) :- arg(1,M,X).
movement_dir(M,X) :- arg(2,M,X).
movement_sym(M,X) :- arg(3,M,X).

%===========================================================================
% COMPLEX_MOVEMENT Data Structure
%===========================================================================
complex_movement(or(M1,M2),M1,M2).

%===========================================================================
% COMPLEX_CAPTURE Data Structure
%===========================================================================
complex_capture(or(C1,C2),C1,C2).

%===========================================================================
% CAPTURE Data Structure
%===========================================================================

capture(capture(Move,Methods,Restr,Effect),Move,Methods,Restr,Effect).

capture(X) :- functor(X,capture,4).

%capture_dir(H,X) :- capture(H), arg(1,H,X).
capture_movement(H,X) :- capture(H), arg(1,H,X).
capture_methods(H,X) :- capture(H), arg(2,H,X).
capture_type(H,X) :- capture(H), arg(3,H,X).
capture_effect(H,X) :- capture(H), arg(4,H,X).



%===========================================================================
% COMPARISON Data Structure
%===========================================================================

comparison(X) :- functor(X,comparison,2).
comparison(comparison(Comp,Num),Comp,Num).

comparison_comp(M,X) :- comparison(M),arg(1,M,X).
comparison_num(M,X) :- comparison(M),arg(2,M,X).
%comparison_sym(M,X) :- comparison(M),arg(3,M,X).

%===========================================================================
% SQUARE Data Structure
%===========================================================================

square(square(X,Y),X,Y).


%===========================================================================
% METHOD Data Structure
%===========================================================================

method(X) :- functor(X,method,3).
method(method(C,R,H),C,R,H).

method_clobber(M,X) :- method(M),arg(1,M,X).
method_retrieve(M,X) :- method(M),arg(2,M,X).
method_hop(M,X) :- method(M),arg(3,M,X).

%===========================================================================
% PIECE_DESCRIPTION Data Structure
%===========================================================================

piece_description(piece_desc(Player,Piece),Player,Piece).
piece_description(X) :- functor(X,piece_desc,2).

piece_description_player(M,X) :- piece_description(M),arg(1,M,X).
piece_description_piece(M,X) :- piece_description(M),arg(2,M,X).
%piece_description_sym(M,X) :- piece_description(M),arg(3,M,X).

%===========================================================================
% CONSTRAINT Data Structure
%===========================================================================

constraint(constraint(Must,Cont),Must,Cont).
constraint(X) :- functor(X,constraint,2).

constraint_must_capture(M,X) :- arg(1,M,X).
constraint_continue_captures(M,X) :- arg(2,M,X).

constraint_continue_captures(M) :- 
	constraint(M), constraint_continue_captures(M,yes).
constraint_must_capture(M) :- 
	constraint(M), constraint_must_capture(M,yes).

%===========================================================================
% COMPLEX_GOAL Data Structure
%===========================================================================
complex_goal(or(M1,M2),M1,M2).

%===========================================================================
% Simple GOAL Data Structures
%===========================================================================

% ARRIVE
arrive_goal(arrive(Desc,Sq),Desc,Sq).

arrive_goal(arrive(Desc,Sq),Player,Type,Sq) :- 
	piece_description(Desc,Player,Type).
	
% ERADICATE
eradicate_goal(eradicate(Desc),Desc).

eradicate_goal(eradicate(Desc),Player,Type) :- 
	piece_description(Desc,Player,Type).

% STALEMATE
stalemate_goal(stalemate(Player),Player).


%===========================================================================
% Decision data structures
%===========================================================================

%decision(Decision,Chooser,Options,Constraints).

% For promote decisions:
% Chooser is just the player who makes this decision.
% [Player_Gen,Piece_Gen] is a description of the options.
% No constraints.

% For assignement decisions:
% Chooser is just the player who makes this decision.
% [Piece1,...], a set of pieces, is the Options.
% Constraints is a set of squares on which the pieces can be placed.

decision(X) :- functor(X,decision,3).
decision(decision(C,O,Con),C,O,Con).

decision_chooser(D,C) :- decision(D,C,_,_).
decision_options(D,O) :-  decision(D,_,O,_).
decision_constraints(D,Con) :- decision(D,_,_,Con).
