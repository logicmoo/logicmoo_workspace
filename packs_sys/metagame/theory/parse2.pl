%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%% parse2.pl
%%% Taking some preds out of parse, as not state-dependent.


current_game_for_player(player,Game) :- 
	player_current_game(Game).
current_game_for_player(opponent,Game) :- 
	opponent_current_game(Game).


current_game_name(GameName) :-
	player_current_game(G),
	game_name(G,GameName). 


current_game_piece_name(Name) :- 
	player_current_game(Game),
	game_piece_def(Game,Name,_). 


game_piece_name(Game,Name) :- 
	game_piece_def(Game,Name,_). 

% Finds def. corresponding to the name of the piece in piecestruct.
game_piece_struct_def(Game,PieceStruct,Def) :-
	piece_struct_name(PieceStruct,Name),
	game_piece_def(Game,Name,Def).

% Finds the definition corresponding to a particular NAME of piece.
game_piece_def(Game,Name,Def) :-
	game_piece_defs(Game,Defs),
	member(Def,Defs),
	piece_name(Def,Name).

game_piece_defs(G,P) :- game_pieces(G,P).


%% Goals
game_has_goal(Game,Goal) :-
	game_goal(Game,CompGoal),
	goal_component(CompGoal,Goal).

goal_component(CompGoal,Goal) :-
	member(Goal,CompGoal).

% Constraints

game_must_capture(Game) :-
	constraint_must_capture(Con),
	game_constraints(Game,Con).
	
game_continues(Game) :- 
	constraint_continue_captures(Con),
	game_constraints(Game,Con).

current_game_must_capture :-
	player_current_game(Game),
	game_must_capture(Game).





% Constraints on pieces

game_piece_has_constraint(Piece,Constraint,Game) :-
	game_piece_struct_def(Game,Piece,Def),
	piece_constraints(Def,Constraint).


% Movement

game_piece_has_movement(Piece,Movement,Game) :-
	game_piece_struct_def(Game,Piece,Def),
	piece_movement(Def,CompMovement),
	movement_component(CompMovement,Movement).

movement_component(CompMovement,Movement) :-
	member(Movement,CompMovement).

% Backtracking gives all the dirs that the piece
% can leap to, based on its symmetries.
% (not so happy having this in parse file!).

leap(M,Dir) :-	
	movement_type(M,L),
	leaper(L),
	movement_sym_dir(M,Dir).
	
% Valid_min and valid_max are defined in boards.pl
% Valid_max computes the greatest number of leaps
% which need to be considered before the piece would
% be off the board or wrapped back to the original square.
ride(M,Dir,Min,Max,Longest) :-
	movement_type(M,R),
	rider(R,Longest,Min1,Max1),
	movement_sym_dir(M,Dir),
	valid_min(Min1,Min),
	valid_max(Max1,Dir,Max).

% When unified with the longest component, succeeds if it should.
longest(yes).


hop(M,Dir,Before,Over,After,Description) :-
	movement_type(M,H),
	hopper(H,Description,Before,Over,After),
	movement_sym_dir(M,Dir).

movement_sym_dir(M,Dir) :-
	movement_dir(M,D),
	movement_syms(M,Syms),
	sym_dir(D,Syms,Dir).

% Uses sym_set from grammar.pl.
% This is absurdly inefficient!

movement_syms(M,Syms) :-
	movement_sym(M,S),
	sym_set(S,Syms,[]).
	

% Capturing
game_piece_has_capture(Piece,Capture,Game) :-
	game_piece_struct_def(Game,Piece,Def),
	piece_capture(Def,CompCapture),
	capture_component(CompCapture,Capture).

capture_has_movement(Capture,M) :-
	capture_movement(Capture,Ms),
	member(M,Ms).

capture_component(CompCapture,Capture) :-
	member(Capture,CompCapture).


capture_has_method(Capture,Method) :-
	capture_methods(Capture,Methods),
	component_of_method(Method,Methods).

component_of_method(clobber,M) :- method_clobber(M,yes).
component_of_method(retrieve,M) :- method_retrieve(M,yes).
component_of_method(hop,M) :- method_hop(M,yes).


capture_methods_list(Capturing,Meths) :- 
	bagof(Meth,capture_has_method(Capturing,Meth),Meths).


% Promoting
% Could simplify by using same structure produced by generator,
% instead of translating.
% Must handle simple case also (promote to fish).

game_piece_has_promoting(Piece,Promoting,Game) :-
	game_piece_struct_def(Game,Piece,Def),
	piece_promote(Def,Promoting).

game_piece_promoting(Piece,Promoting,Game) :-
	game_piece_has_promoting(Piece,Promote,Game),
	promoting_info(Promote,Promoting).

% Either a decision, or a simple type.
promoting_info(promote(Promote),promote(Promote)).
promoting_info(Promote,Promoting) :-
	decision_chooser(Promote,Player),
        decision_options(Promote,Descr),
	Promoting = promote(Player,Descr).


game_promote_rank(Game,Rank) :- 
	game_board(Game,Board),
	board_promote_rows(Board,Rank).


