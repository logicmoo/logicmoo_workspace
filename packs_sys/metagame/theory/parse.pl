%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

% parse.pl



% For now, avoid explicit inversion in the legal rules by producing an
% entire inverted game from each player's perspective, used when
% they are in control.  
% Then interpreter can always assume PLAYER is to move! 
%
% Changed to eliminate choice points.
%current_game(Game) :-
%	control(opponent), !,
%	opponent_current_game(Game).
%current_game(Game) :-
%	player_current_game(Game).

current_game(Game) :-
	control(Player) 
	 -> current_game_for_player(Player,Game)
         ;  player_current_game(Game).

%% Goals
		
% GAME_PLAYER_HAS_GOAL(?Game,+Player,-Goal) :-
% If Player is in control, then already using a properly inverted game
% so don't need to invert it to check his goals.
% Otherwise, we're not using it from his perspective, so invert it.

game_player_has_goal(Game,Player,Goal) :-
	current_game_for_player(Player,Game),
	game_has_goal(Game,Goal).


%% Initial Setup

current_board(B) :- current_game(G), game_board(G,B).

current_board_size(X,Y) :- player_current_game(G), game_board(G,B), board_size(B,X,Y).

current_board_type(Type) :- player_current_game(G), game_board(G,B), board_type(B,Type).

current_board_inversion(Type) :- player_current_game(G), game_board(G,B), board_inversion(B,Type).



% GAME_ASSIGNMENTS(+Game,-Assign)
% The game has the initial piece assignments.
game_assignments(Game,Assign) :-
	player_current_game(Game),
	game_board(Game,Board),
	board_assignments(Board,Assign).


% GAME_HAS_ASSIGNMENTS(+Game,-Assign)
% The game has the initial piece assignments.
% If the game is defined to have a random setup,
% the setup used for this contest must already have been determined,
% and stored as:
%             current_random_assignments(Assignments).
% 
% Otherwise, just use the fully-specified assignments as generated.

game_has_assignments(Game,Assign) :-
	game_assignments(Game,Assign1),
	full_assignment_if_random(Assign1,Assign).

full_assignment_if_random(As,Assignments) :-
	random_assignment_decision(As), !,
	random_assignment(Assignments).
full_assignment_if_random(As,As).



% CURRENT_RANDOM_SETUP_GAME(+Game)
% True if current game starts with a random setup.
current_random_setup_game :- 
	player_current_game(Game),
	random_setup_game(Game).


% RANDOM_SETUP_GAME(+Game)
% True if game starts with a random setup.
random_setup_game(Game) :- 
	game_assignments(Game,Assign),
	random_assignment_decision(Assign).

random_assignment_decision(AssignmentDef) :-
	assignment_decision(AssignmentDef,Assigner,_PieceNames,_Squares), 
	Assigner = random.
	


%%% random_assignment(Assignments)
%%% If this is a random game, the assignments for this contest
%%% must already have been determined externally, 
%%% such that this call succeeds.
%%% The result must be an assignment list,
%%% the result of parsing the arbitrary_assignment part of the
%%% game definition in grammar.pl, of the following form:
%%%   Assignments =  [piece1=[square1,..],...].
%%% For example:  
%%% [piece1=[square(2,1)],piece2=[square(1,2),square(3,1)]]



%% Pieces

% Moved to parse2.pl, use player_current_game instead!
%current_game_piece_name(Name) :- 
%	current_game(Game),
%	game_piece_def(Game,Name,_). 

current_game_piece_struct_def(PieceStruct,Def) :-
	piece_struct_name(PieceStruct,Name),
	current_game_piece_def(Game,Name,Def).

current_game_piece_def(Name,Def) :-
	current_game(Game),	
	game_piece_def(Game,Name,Def).

game__piece_has_movement(Piece,Movement) :-
	current_game(Game),
	game_piece_has_movement(Piece,Movement,Game).

game__piece_has_capture(Piece,Capture) :-
	current_game(Game),	
        game_piece_has_capture(Piece,Capture,Game).

game__piece_has_constraint(Piece,Constraint) :-
	player_current_game(Game),	
	game_piece_has_constraint(Piece,Constraint,Game).

% Whether a piece continues capturing.
game__piece_continues(Piece) :- 
	constraint_continue_captures(Con),
	game__piece_has_constraint(Piece,Con).

game__piece_must_capture(Piece) :- 
	constraint_must_capture(Con),
	game__piece_has_constraint(Piece,Con).

% Whether a piece must capture.
% It must capture either if the game has this constraint,
% or the piece itself does.
% Note this assumes Piece is bound, won't nec. generate them.
game__piece_must(Piece) :-
	( current_game_must_capture
	-> true
	; game__piece_must_capture(Piece)
	).

%---------------------------------------------

% Promoting
% Could simplify by using same structure produced by generator,
% instead of translating.
% Must handle simple case also (promote to fish).

game__piece_has_promoting(Piece,Promoting) :-
	current_game(Game),	
	game_piece_has_promoting(Piece,Promoting,Game).

game__piece_promoting(Piece,Promoting) :-
	current_game(Game),	
	game_piece_promoting(Piece,Promoting,Game).

game__promote_rank(Rank) :- 
	current_game(Game),
	game_promote_rank(Game,Rank).


