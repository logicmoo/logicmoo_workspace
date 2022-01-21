%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

% gen.pl
% Generates new symmetric chess-like games.

:- my_ensure_loaded(library(piece_names)).
:- my_ensure_loaded(library(genstructs)).

%================================================================================
% GENERATE GAME
%================================================================================

generate_game(game(Name,Board,PieceDefs,Goal,Constraints)) :- 
	record_seed,
	new_game_name(Name),
	generate_board(Board),
	write('Generated Board'), nl,
	generate_pieces(Board,PieceDefs),
	write('Generated Pieces'), nl,
	generate_global_constraints(Constraints), 
	write('Generated Global Constraints'), nl,
	generate_goal(Board,Goal),
	write('Generated Goal ... And Game.'), nl.

new_game_name(Name) :-
	gensym(game,Name).

reset_game_name :- reset_gensym(game).



%================================================================================
% GENERATE BOARD
%================================================================================

% Parameters used:
%     board_type
%     board_size
%     board_crowding
%     row_crowding
%     placement_method
%     promotion_fraction
%     promote_only
%     
% board_size:  Avg. size of square board. Chess has 8 squares.
% board_crowding: Fraction of board to use for placing initial arrays. 
%    Chess uses 1/2 the board
% row_crowding: Fraction of each row in init array to fill.  Chess fills
%    the rows entirely. 
% piece_variety: Of possible pieces (# used array locations), fraction which
%    will be unique piece types.  
% promote_only: A number of pieces-types which are generated, but not placed 
%    on the initial board, and thus can only appear by promotion.
%    While piece_variety is a fraction constrained by the board size,
%    this is unconstrained, and is thus a range parameter.

% Thus, the number of unique piece-types used in a game will be:
% placed_pieces:  (board_size^2/2)*board_crowding*row_crowding*piece_variety
% promote_only:   [Lower,Upper]
% total = placed_pieces + promote_only
%
% Note that the total number of piece types is a major source of complexity
% in these games.  It does not generally influence the branching factor
% (as new pieces only appear when old ones go away),
% but it increases exponentially the number of possible positions which can 
% occur. 



%board(Size,Type,Inversion,MaxRow,PromoteRow,_KilledSquares,
%	       UniquePieces,UniquePlacedPieces,Assignments)) :-

generate_board(B) :-
	board(B),
	board_size(B,Size), 
	board_type(B,Type), 
	board_inversion(B,Inversion), 
	board_promote_rows(B,PromoteRow),
	board_array_rows(B,MaxRow),
	board_piece_types(B,UniquePieces),
	board_placed_pieces(B,UniquePlacedPieces),
	board_assignments(B,Assignments),
	choose_board_size(Size),
	size(Size,XMax,YMax),
	choose_board_type(Type),
	choose_board_inversion(Inversion),
	choose_board_crowding(Crowd),
	choose_initial_array_rows(YMax,Crowd,MaxRow),
	choose_promotion_regions(MaxRow,MinRow),
	PromoteRow is YMax - MinRow, 
	array_squares(XMax,MaxRow,ArSquares),
	placeable_pieces(ArSquares,UniquePlacedPieces,PlacedPieceSet),
	place_pieces_if_arbitrary(ArSquares,PlacedPieceSet,Assignments),
	promote_only_pieces(UniquePlacedPieces,PromotePieces),
	append(UniquePlacedPieces,PromotePieces,UniquePieces).



% array_squares(+XMax,+YMax,-Squares)
array_squares(MaxCol,MaxRow,Squares) :-
	squares_in_rows(1,MaxRow,MaxCol,Squares1),
	reverse(Squares1,Squares).

% squares_in_rows(MinRow,MaxRow,MaxCol,Squares)
% Generates MaxCol squares in each of rows [MinRow .. MaxRow].

squares_in_rows(Min,Max,Size,Squares) :-
	Min1 is Min - 1,
	squares_in_rows(Max,Min1,Max,Size,Squares).

squares_in_rows(Min,Min,_Max,_Size,[]) :- !.
squares_in_rows(_,_,_,0,[]) :- !.
squares_in_rows(Row,Min,Max,Size,Squares) :- 
	squares_in_row(Row,Size,RowSquares),
	Row1 is Row - 1,
	squares_in_rows(Row1,Min,Max,Size,RestRows),
	append(RowSquares,RestRows,Squares).


squares_in_row(_Row,0,[]) :- !.
squares_in_row(Row,Size,[Square|Squares]) :-
	Size1 is Size - 1,
	square(Square,Size,Row),  % Column first, then row.
	squares_in_row(Row,Size1,Squares).


% PLACE_PIECES_IF_ARBITRARY(+ArSquares,+PieceSet,-Assignments)
% PieceSet is the set of pieces which will be assigned to 
% squares in ArSquares (the initial array squares).
% This is either done arbitrarily, in which case the pairings
% are made now, or by some method of decision, in which 
% case the piece set and assignable squares will be given with
% the corresponding method, the assignment itself to be done at 
% game time. 
%
% Parameter: placement_method
%
place_pieces_if_arbitrary(ArSquares,PieceSet,Assignments) :-
	choose_placement_method(Method),
	placement_for_method(Method,PieceSet,ArSquares,Assignments).

placement_for_method(arbitrary,PieceSet,ArSquares,Assignments) :- !,
	assign_pieces_to_squares(PieceSet,ArSquares,Assignments).
placement_for_method(Method,PieceSet,ArSquares,Assignments) :- 
	decision(Assignments,Method,PieceSet,ArSquares).

assign_pieces_to_squares(Pieces,Squares,Assignments) :-
	randomly_pair(Pieces,Squares,Assignments1),
	collect_placements(Assignments1,Assignments).

collect_placements(In,Out) :-
	collect_placements(In,[],Out).

collect_placements([],X,X).
collect_placements([A=Elt|Rest],Current,New) :-
	collapse(A=Elt,Current,New1),
	collect_placements(Rest,New1,New).

collapse(A=Elt,[],[A=[Elt]]) :- !.
collapse(A=Elt,[A=Elts|Rest],[A=[Elt|Elts]|Rest]) :- !.
collapse(A=Elt,[H|Rest],[H|Out]) :- collapse(A=Elt,Rest,Out).
%collapse(X,Y,[X|Y]).

uncollect([],[]).
uncollect([A=Elts|Rest],Out) :-
	uncollapse(A,Elts,As),
	uncollect(Rest,R),
	append(As,R,Out).

uncollapse(_,[],[]).
uncollapse(A,[H|T],[A=H|Ts]) :- uncollapse(A,T,Ts).

unpair([],[]).
unpair([_Piece=Sq|Rest],[Sq|Squares]) :- 
	unpair(Rest,Squares).


% PLACEABLE_PIECES(+ArSquares,-UniquePieces,-PieceSet)
% -------------
% ARSQUARES: a set of squares to which pieces might be 
% initially assigned.  
% UNIQUEPIECES:  a set of unique piece names (containing at least 1 piece)
% PIECESET: duplicates these unique pieces to achieve a set of the right size.
% 
% Parameters used here:
% row_crowding: what fraction of squares to place pieces on in initial array.
% piece_variety: of a possible number of pieces, what fraction should be unique.
%   (Checkers has low variety, chess has high, shogi even higher).
% row_crowding * possible_set_size --> number of squares to be assigned pieces
% piece_variety * assigned_squares --> number of unique pieces to be generated.
%
placeable_pieces(ArSquares,UniquePieces,PieceSet) :-
	length(ArSquares,Possible),
	unique_piece_size(Possible,UniqueNum,SetSize),
	tracing_gen_format(pieces,"Out of <~p> max possible types, chose <~p>, <~p> unique~n",
	       [Possible,SetSize,UniqueNum]),
	n_piece_names(UniqueNum,UniquePieces),
	duplicate_pieces(UniquePieces,SetSize,PieceSet),
	tracing_gen_format(pieces,"Unique: ~p~n Resulting set: ~p~n",[UniquePieces,PieceSet]).

unique_piece_size(Possible,UniqueNum,SetSize) :- 
	piece_set_size(Possible,SetSize),
	choose_parameter(piece_variety,Variety_Factor),
	UniqueNum1 is integer(SetSize * Variety_Factor),
	max(UniqueNum1,1,UniqueNum).

% PIECE_SET_SIZE(+Possible,-SetSize)
% Out of some possible maximum size, chooses some subset of 
% squares to assign pieces to initially, based on the parameter
% ROW_CROWDING, the average fraction of possible squares to use. 
piece_set_size(Possible,SetSize) :-
	choose_parameter(row_crowding,Crowding),
	Size is integer(Crowding * Possible),
	max(Size,1,SetSize).

% PROMOTE_ONLY_PIECES(-Pieces)
% Generates a number of unique piece types, which will
% only be seen via promotion (i.e. they are not placed on 
% the initial board).
% We ensure that no more than 26 pieces will be placed in total,
% else we can no longer distinguish them using letters!
% Parameter:  promote_only
% Note this parameter is independent of board size,
% and is specified as a range.
%
promote_only_pieces(PlacedPieces,Pieces) :-
	choose_parameter(promote_only_pieces,N),
	length(PlacedPieces,Used),
	Next is Used + 1,
	Last1 is Next + N,
	min(Last1,26,Last),
	n_piece_names(Next,Last,Pieces).



% DUPLICATE_PIECES
% continue duplicating random elements until achieve
% a set of the desired size.  Resulting list is sorted.
duplicate_pieces(UniquePieces,SetSize,PieceSet) :-
	duplicate_elements(UniquePieces,SetSize,PieceSet1),
	stable_sort(PieceSet1,PieceSet).

duplicate_elements(UniqueElements,SetSize,Elementset) :-
	length(UniqueElements,L),
	duplicate_elements(UniqueElements,L,SetSize,DuplicateSet),
	append(UniqueElements,DuplicateSet,Elementset).

duplicate_elements(_Types,Size,Size,[]) :- !.
duplicate_elements(Types,Size,Target,[E|Elements]) :-
	random_element(Types,E),
	Size1 is Size + 1,
	duplicate_elements(Types,Size1,Target,Elements).


%================================================================================
% Generating unique piece names
%================================================================================


n_piece_names(N,Pieces) :-
	n_piece_names(1,N,Pieces).


n_piece_names(N,Max,[]) :- N > Max, !.
n_piece_names(N,Max,[P|Pieces]) :-
	new_piece_name(N,P),
	N1 is N + 1,
	n_piece_names(N1,Max,Pieces).



new_piece_name(Index,Name) :-
	random_success(possible_index_piece(Index,Name)).


possible_index_piece(I,Name) :- indexed_names(I,Names), member(Name,Names).


%================================================================================

choose_board_size(Size) :-
	choose_parameter(board_size,XMax),
	choose_parameter(board_size,YMax),
	size(Size,XMax,YMax).

choose_board_type(Type) :-
	choose_parameter(board_type,Type).

choose_board_inversion(Type) :-
	choose_parameter(board_inversion,Type).


choose_board_crowding(Crowd) :-
	choose_parameter(board_crowding,Crowd).

choose_initial_array_rows(Size,Crowding,MaxRow) :-
	MaxRow1 is integer(Size * Crowding / 2),
	max(1,MaxRow1,MaxRow).

choose_promotion_regions(MaxRow,MinRow) :-
	choose_promotion_fraction(D),
	MinRow is integer(MaxRow * D).

choose_promotion_fraction(D) :-
	choose_parameter(promotion_fraction,D).


choose_placement_method(M) :-
	choose_parameter(placement_method,M).


%================================================================================
% GENERATE PIECES
%================================================================================

generate_pieces(Board,PieceDefs) :-
	board_piece_types(Board,Types),
	generate_pieces(Types,Board,PieceDefs1),
	sort(PieceDefs1,PieceDefs).


% generate_pieces(+PieceTypes,+Board,-PieceDefs)
generate_pieces([],_,[]).
generate_pieces([P|Ps],Board,[Def|Defs]) :-
	generate_piece(P,Board,Def),
	generate_pieces(Ps,Board,Defs).


generate_piece(Name,Board,Piece) :-
	piece(Piece),
	piece_name(Piece,Name),
	assign_movement_power(Piece,Board),
	assign_capture_power(Piece,Board),
	assign_promotion_power(Piece,Board),
	assign_piece_constraints(Piece,Board).


%============================================================
% Constructing movements
%============================================================


assign_movement_power(Piece,Board) :-
	create_complex_movement(Board,Movement),
	piece_movement(Piece,Movement).
	
create_complex_movement(Board,Movement) :-
	create_movement(Board,M1),
	complexify_movement(Board,M1,Movement).

complexify_movement(Board,MIn,[MIn|MOut]) :-
	choose_parameter(movement_complexity), !,
	create_complex_movement(Board,MOut).
complexify_movement(_,X,[X]).           



create_movement(Board,Movement) :-
	movement(Movement),
	choose_direction(Board,Dir),
	movement_dir(Movement,Dir),
	choose_symmetries(Syms),
	movement_sym(Movement,Syms),
	choose_movement_type(Type),
	constrain_movement(Type,Dir,Board,Movement).


%========================================
% Directions of movement
%========================================

% Scales in both directions, s.t. a wide board
% is likely to have pieces which move farther in 
% Y direction than in X.  
% Subtracts 1 from X & Y, since piece must be on
% a square initially, so 1 less to move total.
choose_direction(Board,Dir) :-
	choose_locality(L),
	board_size(Board,X,Y),
%	XMax1 is (X-1)*L // 1,
%	YMax1 is (Y-1)*L // 1,
	XMax1 is integer((X-1)*L),
	YMax1 is integer((Y-1)*L),
	max(1,XMax1,XMax),
	max(1,YMax1,YMax),
	choose_dir(XMax,YMax,Dir).

choose_dir(XMax,YMax,Dir) :-
	choose_delta(XMax,Dx),
	choose_delta(YMax,Dy),
	legal_dir(XMax,YMax,Dx,Dy,Dir).

% If (0,0), choose both again.
% Rules out ZERO leaper (Dickens).
legal_dir(XMax,YMax,0,0,Dir) :- !,
	choose_dir(XMax,YMax,Dir).
legal_dir(_,_,Dx,Dy,Dir) :-
	direction(Dir,Dx,Dy).
	
% A delta is any random integer from [0,Max].
choose_delta(Max,Delta) :-
	M is Max+1,
	random(0,M,Delta).

max_delta(Dir,D) :- 
	direction(Dir,X,Y),
	max(X,Y,D).

%========================================
% Symmetries
%========================================
% Chooses the three symmetries with their respective (and independent)
% probabilities (which are parameters). 
%
choose_symmetries(Sym) :-
	symmetry(Sym),
	choose_parameter(symmetry(rotation),R),
	choose_parameter(symmetry(forward),F),
	choose_parameter(symmetry(side),S),
	sym_forward(Sym,F),
	sym_side(Sym,S),
	sym_rotation(Sym,R).

%========================================
% Constraints on movements
%========================================
% Movement types are chosen with different exclusive probabilities,
% using the parameter:  movement_type.
% Leapers are unconstrained.
% Riders can have a min, max, and longest restrictions (must ride).
% Hoppers have number before, over, after, and hopped-over piece_type restrictions.

choose_locality(L) :- choose_parameter(locality,L).

choose_movement_type(T) :- choose_parameter(movement_type,T).

constrain_movement(leaper,_,_Board,Movement) :- !,
	leaper(L),
	movement_type(Movement,L).
constrain_movement(rider,Dir,Board,Movement) :- !,
	rider(Rider),
	choose_must_ride(Rider),
	choose_min_ride(Board,Dir,Rider),
	choose_max_ride(Board,Dir,Rider),
	movement_type(Movement,Rider).
constrain_movement(hopper,Dir,Board,Movement) :- !,
	hopper(Hopper),
	choose_hopper_type(Board,R),
	hopper_type(Hopper,R),
	choose_before(Hopper,Board),
	choose_over(Hopper,Board),
	choose_after(Hopper,Board),
	valid_hopper(Hopper,Dir,Board,Movement).


%========================================
% Riders
%========================================

choose_must_ride(Rider) :-
	choose_parameter(must_ride,Must),
	rider_must(Rider,Must).

choose_min_ride(_Board,_Dir,Rider) :-
	rider_min(Rider,1).

% CHOOSE_MAX_RIDE(+Board,+Dir,+Rider)
% Parameters:
% locality: the fraction of a board which should be traversible
%           by an average piece movement.
% The max ride is then some number of rides s.t. a piece won't
% go beyond this locality by riding the max distance. 
choose_max_ride(Board,Dir,Rider) :-
	choose_locality(L),
	board_max_size(Board,Size),
	max_delta(Dir,D),
%	Dist is L*Size // 1,
%	Rides is Dist // D,
	Rides is integer(L * Size / D),
	set_max_rides(Rider,Rides).


set_max_rides(Rider,Rides) :- 
	Rides > 1, !,
	rider_max(Rider,Rides).
set_max_rides(Rider,_) :-  rider_max(Rider,any).


%========================================
% Hoppers
%========================================

choose_before(Hopper,Board) :-
	constrain_hopper(before,Board,Hopper).

choose_over(Hopper,Board) :-
	constrain_hopper(over,Board,Hopper).

choose_after(Hopper,Board) :-
	constrain_hopper(after,Board,Hopper).


% CONSTRAIN_HOPPER(+Type,+Board,+Hopper)
% Different Hop fields (before, over, after)
% are constrained or not with their own probabililities,
% defined by parameters.
% If they aren't to be constrained, an unconstraining value
% is placed in that slot (by hopper_any/2).
%
constrain_hopper(Type,Board,Hopper) :-
	(choose_parameter(constrain(hopper(Type,_)))
          -> hopper_equation(Type,Board,Hopper)
	  ;  hopper_any(Type,Hopper)).

% HOPPER_EQUATION(Type,Board,Hopper)
% If a certain field (like BEFORE) is to be constrained,
% 
hopper_equation(Type,Board,Hopper) :-
	board_max_size(Board,Dist),
	hopper_component(Type,Hopper,Comp),
	choose_parameter(hopper(Type,Dist),P),
	choose_equation(P,Comp).

% Must always hop over at least one piece.
hopper_any(over,Hopper) :- !,
	comparison(C,geq,1),
	hopper_component(over,Hopper,C).
hopper_any(Type,Hopper) :-
	comparison(C,geq,0),
	hopper_component(Type,Hopper,C).



%========================================
% Hopper Type
%========================================
% Given a hopper might hop over some pieces, 
% the legality of the hop may be 
% restricted to only some kinds of pieces, based on who
% owns them, and what type they are.
% Doesn't distinguish between order or positions of hopped
% pieces (as in hop over 2 empty, then 1player,1opponent,1queen),
% though this would be interesting extension.

choose_hopper_type(Board,C) :- 
	choose_piece_description(Board,C).

hopper_component(before,Hopper,X) :-
	hopper_before(Hopper,X).
hopper_component(over,Hopper,X) :-
	hopper_over(Hopper,X).
hopper_component(after,Hopper,X) :-
	hopper_after(Hopper,X).



% Check constraints possible on board, else get new one.
valid_hopper(Hopper,Dir,Board,Movement) :-
	hopper_can_move(Hopper,Dir,Board), !,
	movement_type(Movement,Hopper).
valid_hopper(_Hopper,Dir,Board,Movement) :-
	constrain_movement(hopper,Dir,Board,Movement).


% HOPPER_CAN_MOVE(?Hopper,+Dir,+Board)
% A hopper can move if it could be on square (1,1),
% and make the minimum number of rides along
% its principal direction <Dx,Dy>, and still be on the board.
% 
% Note it is possible a hopper might be able to hop even if 
% this is false, because of symmetries, but here we ensure
% it can without using symmetries.
hopper_can_move(Hopper,Dir,Board) :-
	hopper_min_rides(Hopper,Rides), % Must be at least 1.
	board_size(Board,BX,BY),
	direction(Dir,DX,DY),
	Rides * DX < BX,
	Rides * DY < BY.


valid_hopper_max_dir(Dx,Dy,Max) :- 
	current_board_size(Bx,By),
	XMax is Bx // Dx,
	YMax is By // Dy,
	min(XMax,YMax,Max).


% hopper_min_rides(+Hopper,-Min)
% The Hopper requires at least MIN rides to make
% a valid move:
%   Before_min + Over_min + After_min
%   +1 (as hop finishes with a leap)
hopper_min_rides(H,Min) :-
	component_min(H,before,Min1),
	component_min(H,over,Min2),
	component_min(H,after,Min3),
	Min is Min1 + Min2 + Min3 + 1.

component_min(H,T,Min) :-
	hopper_component(T,H,Eq),
	min_rides(Eq,Min).
	
min_rides(Eq,Min) :-
	comparison(Eq,geq,Min), !.
min_rides(Eq,Min) :-
	comparison(Eq,eq,Min), !.
min_rides(_Eq,0).



choose_equation(Num,Eq) :-
	choose_parameter(comparative,C),
	comparison(Eq,C,Num).


%===========================================================================
% Generalized Piece Descriptions
%===========================================================================
% Piece Description has two components:
%    Player
%    Name
% Generalizing player, we can get:  Player, Any_Player
% Generalizing Name, we can get:  Piece_list, Any_Piece
%
%| ?- board_piece_types(B,[piece1,piece2]),choose_piece_description(B,Desc).
%
%B = board(_233,_234,_235,_236,_237,[piece1,piece2],_239,_240),
%Desc = piece_desc(opponent,piece2) ? 

choose_piece_description(Board,Desc) :-
	player_generalization(Board,Player),
	piece_generalization(Board,Pieces),
	piece_description(Desc,Player,Pieces).

piece_generalization(Board,Gen) :-
	choose_piece_generalization_level(L),
	generalize_piece(L,Board,Gen).

generalize_piece(specific,Board,Pieces) :-
	choose_general_piece_set(Board,Pieces).
generalize_piece(any,_Board,any_piece).


%----------------------------------------------------------------
% Choosing subsets of defined pieces
%----------------------------------------------------------------

% CHOOSE_GENERAL_PIECE_SET(Board,Pieces)
% Chooses a set of any defined pieces.
choose_general_piece_set(Board,Pieces) :- 
	board_piece_types(Board,General),
	choose_set_by_param(General,more_general_pieces,Pieces).

% CHOOSE_UNPLACED_PIECE_SET(Board,Pieces)
% Chooses a set of unplaced (promote_only) pieces.
choose_unplaced_piece_set(Board,Pieces) :- 
	board_unplaced_pieces(Board,Unplaced),
	choose_set_by_param(Unplaced,more_arrival_pieces,Pieces).

% =======================================
% CHOOSE_SET_BY_PARAM(+Set,+Param,-Items)
% =======================================
% Choose conditionally subject to the boolean parameter
% Param.  
% 
choose_set_by_param(Set,Param,Items) :- 
	Goal = choose_parameter(Param),
	choose_conditionally_from_set(Set,Goal,Items).


% ================================================
% CHOOSE_CONDITIONALLY_FROM_SET(+Set,+Goal,-Items)
% ================================================
% Choose a random Item from Set, and if Goal is 
% satisfied, continue choosing more items.
% Ends when no more items to choose, or Goal fails.
% Goal should be random from this to be useful. 
% Resulting list is sorted. 
%
choose_conditionally_from_set(Set,Goal,Items) :- 
	random_permute(Set,Set1),
	choose_conditionally_from_set1(Set1,Goal,Items1),
	sort(Items1,Items).

choose_conditionally_from_set1([],_,[]).
choose_conditionally_from_set1([Item|Rest],Goal,[Item|Items]) :- 
	( call(Goal) ->
	  choose_conditionally_from_set(Rest,Goal,Items)
	; Items = []
	).

player_generalization(Board,Gen) :-
	choose_player_generalization_level(L), 
	generalize_player(L,Board,Gen).

% Simplified:  just either choose specific or any.  
generalize_player(specific,Board,Player) :- !,
	random_player(Board,Player).
generalize_player(any,_Board,any_player).

random_player(_Board,Player) :-
	players(Players),
	random_element(Players,Player).

players([player,opponent]).


random_piece(Board,Piece) :-
	board_piece_types(Board,Types),
	random_element(Types,Piece).

random_square(Board,Sq) :-
	board_size(Board,XMax,YMax),
	random(XMax,X), 
	random(YMax,Y), 
	square(Sq,X,Y).

choose_player_generalization_level(L) :-
	choose_parameter(player_generalization_level,L).

choose_piece_generalization_level(L) :-
	choose_parameter(piece_generalization_level,L).

%============================================================
% Constructing capturing powers
%============================================================

assign_capture_power(Piece,Board) :-
	create_complex_capture(Board,Capture),
	piece_capture(Piece,Capture).

create_complex_capture(Board,Capture) :-
	create_capture(Board,C1),
	complexify_capture(Board,C1,Capture).

complexify_capture(Board,CIn,[CIn|C1]) :-
	choose_parameter(capture_complexity), !,
	create_complex_capture(Board,C1).
complexify_capture(_,X,[X]).           



create_capture(Board,Capture) :-
	choose_capture_movements(Board,Movement),
        capture_movement(Capture,Movement),
	choose_capture_methods(Method),
	capture_methods(Capture,Method),
	choose_capture_type(Board,Restrict),
	capture_type(Capture,Restrict),
	choose_capture_effect(Effect),
	capture_effect(Capture,Effect).


%========================================
% Capture Movements
%========================================
% (Given a piece which moves in some ways)
% Find how a piece can move for purposes of capturing.
%
% An interesting additional structure parameter could make
% it prefer to just use same movement as capture.

choose_capture_movements(Board,Movement) :-
	create_complex_movement(Board,Movement).



%========================================
% Capture Methods
%========================================
% Given a movement, how does piece capture another piece when
% making that movement?
% 
% Kill HOP if piece has no hopping movements when capturing?  
	
choose_capture_methods(Methods) :-
	choose_parameter(capture_method(retrieve),R),
	choose_parameter(capture_method(clobber),C), 
	choose_parameter(capture_method(hop),H),
	method_retrieve(Methods,R),
	method_clobber(Methods,C),
	method_hop(Methods,H).



%========================================
% Capture Effect
%========================================
% Given a movement that finds a captured piece,
% how is the board changed (what happens to both
% pieces?).
	
choose_capture_effect(E) :- 
	choose_parameter(capture_effect,E).


%========================================
% Capture Type
%========================================
% Given a movement that finds a potentially 
% captured piece, the legality of the capture may be 
% restricted to only some pieces, based on who
% owns them, and what type they are.
	
choose_capture_type(Board,C) :- 
	choose_piece_description(Board,C).

%============================================================
% Constructing promotion powers
%============================================================

assign_promotion_power(Piece,Board) :-
	choose_promotion(Board,Prom),
	piece_promote(Piece,Prom).

choose_promotion(Board,Prom) :-
	choose_parameter(specific_promotion)
	  -> specific_promotion(Board,Prom)
	  ;  promotion_decision(Board,Prom). 

specific_promotion(Board,promote(Piece)) :-
	random_piece(Board,Piece).

promotion_decision(Board,D) :-
	random_player(Player),
	choose_piece_description(Board,Desc),
	decision(D), 
	decision_chooser(D,Player),
        decision_options(D,Desc).

random_player(P) :- board_players(_,Players), random_element(Players,P).

board_players(B,[player,opponent]) :- board(B).


%============================================================
% Constructing Piece Move Constraints
%============================================================
% Given a piece can execute different powers,
% there are constraints on whether one type of 
% power must take priority, and whether a 
% power can be repeated.  If so there's a choice
% whether it MUST be repeated.  

assign_piece_constraints(Piece,Board) :-
	choose_piece_constraints(Board,Con),
	piece_constraints(Piece,Con).
	
choose_piece_constraints(_Board,Con) :- 
	constraint(Con),
	choose_parameter(must_capture,Must),
	constraint_must_capture(Con,Must),
	choose_parameter(continue_captures,Cont),
	constraint_continue_captures(Con,Cont).


piece_must_capture(Piece,Must) :-
	piece_constraints(Piece,Con),
	constraint_must_capture(Con,Must).

piece_continue_captures(Piece,Continue) :-
	piece_constraints(Piece,Con),
	constraint_continue_captures(Con,Continue).



%============================================================
% Constructing Global Capture Constraints
%============================================================
% Given a piece can execute different powers,
% there are constraints on whether one type of 
% power must take priority, and whether a 
% power can be repeated.  If so there's a choice
% whether it MUST be repeated.  

generate_global_constraints(Con) :-
	choose_game_constraints(Con).
	
% Games don't continue capturing, only pieces do. 
% 
choose_game_constraints(Con) :- 
	constraint(Con),
	choose_parameter(must_capture,Must),
	constraint_must_capture(Con,Must),
%	choose_parameter(continue_captures,Cont),
	constraint_continue_captures(Con,no).


game_must_capture(Game,Must) :-
	game_constraints(Game,Con),
	constraint_must_capture(Con,Must).

game_continue_captures(Game,Continue) :-
	game_constraints(Game,Con),
	constraint_continue_captures(Con,Continue).

%================================================================================
% GENERATE GOAL
%================================================================================
% G = [stalemate(opponent),(eradicate(piece_desc(player,t)),
%                            arrive(piece_desc(opponent,t),[square(2,2)]))] ?  
% 
% First geneates a stalemate goal, to stalemate either (not both) of the players.
% Then, based on GOAL COMPLEXITY parameters, continues to add in 
% arrival and eradicate goals.  The more complex the goal, the easier it is to 
% achieve, as a player wins if ANY of his goals are achieved (but not those of his opponent). 
% 
% Each additional goal to be added is ensured to add a new winning condition,
% in the sense that it is not subsumed by any of the existing goals.
%
% After the goal conditions are made as complex as desired, 
% the goals are SIMPLIFIED.  This proceeds as follows:
% 1. Remove FULLY DUPLICATE goals
% 2. Remove SUBSUMED goals.
%    This is necessary for goals added which subsume earlier ones (ie were not
%    subsumed themselves).  Thus the earlier ones must be removed. 
%
% Any other simplifications require more serious theorem proving, 
% which is beyond the scope of the generator. 
% 
% Subsumed goals:
% ----------------
% When the generator produces many goals, it is very likely that several of them
% will be somehow redundant.  The generator deals with the following types of 
% redundancy as follows:
%  
% A goal G1 for SUBSUMES another G2 when G2 being true in a position implies G1 will
% also (or already) be true.  
% a. Both goals are for PLAYER (like eradicate player's [a,b] and player's [a]).
%    In this case the subsumption is straightforward, based on the sets of pieces
%    mentioned in the two goals. 
% b. One goal is for PLAYER, the other for OPPONENT.  This is more tricky,
%    and (in the case of arrival goals) involves inverting the squares mentioned in
%    the opponent's goal.
%
% One effect of this checking is that we will not generate pairs of goals which REQUIRE
% to be achieved simultaneously (like a goal to eradicate my king and your king). 
% However, it still allows situations in which two goals HAPPEN TO BE are achieved 
% together, and this outcome is declared to be a draw.
%
% For example, consider the goals:  
%     stalemate(opponent)
%     eradicate([player,any_piece]).
%
% Here, it is possible to capture your last piece, thus achieving both goals
% (as it is your move but your stalemated, so you achieve the first, while I achieve
% the second as you have no more pieces).  
% However, it may still be possible to stalemate you w/o eradicating all your pieces,
% so I could still win alone.  Thus neither goal subsumes the other. 
%
generate_goal(Board,Goals) :-
	choose_stalemate_goal(Board,Goal1),
	complexify_goal(Board,[],ComplexGoal),
	   tracing_gen_format(goals,"~n** Simplifying goals ...~n",[]),
	   tracing_gen(simplify,
	       ( format("Before: ~n",[]),
		 ppl([Goal1|ComplexGoal])
	       )),
	simplify_goals([Goal1|ComplexGoal],Board,Goals),
	   tracing_gen_format(goals,"~n** Done Simplifying.~n",[]),
	   tracing_gen(simplify,
	       ( format("~nAfter: ~n",[]),
		 ppl(Goals)
	       )).

complexify_goal(Board,GIn,G) :-
	choose_parameter(goal_complexity), !,
	unsubsumed_goal(GIn,Board,GNew),
	complexify_goal(Board,[GNew|GIn],G).
complexify_goal(_,X,X).           


% Keep creating goals until we make one which is not
% subsumed by the existing ones.  
unsubsumed_goal(Goals,Board,NewGoal) :- 
	create_goal(Board,Goal),
	ensure_new_goal(Goals,Board,Goal,NewGoal).

ensure_new_goal(Goals,Board,Goal,NewGoal) :-
	member(Goal,Goals), !,
	unsubsumed_goal(Goals,Board,NewGoal).
ensure_new_goal(Goals,Board,Goal,NewGoal) :-
	subsumed_goal(Goal,Goals,Board), !,
	unsubsumed_goal(Goals,Board,NewGoal).
ensure_new_goal(_Goals,_Board,Goal,Goal).
	
% -------------------------
% CREATE_GOAL(+Board,-Goal)
% -------------------------
% Determines the type of goal based on param:  GOAL_TYPE.
create_goal(Board,Goal) :- 
	choose_parameter(goal_type,Type),
	choose_goal_of_type(Type,Board,Goal).

choose_goal_of_type(arrive,B,Goal) :-
	choose_arrive_goal(B,Goal).
choose_goal_of_type(eradicate,B,Goal) :-
	choose_eradicate_goal(B,Goal).

% ==================================
% CHOOSE_STALEMATE_GOAL(+Board,Goal)
% ==================================
% Just pick a random player to be stalemated.
% Could make this depend on a parameter.
choose_stalemate_goal(Board,Stale) :-
	random_player(Board,Player),
	stalemate_goal(Stale,Player).

% ================================
% CHOOSE_ARRIVE_GOAL(+Board,-Goal)
% ================================
% Arrival goals have list of squares in the grammar.
% Here we generate a list with only 1 square, since the same effect
% can be achieved by multiple arrival goals.

choose_arrive_goal(Board,Goal) :-
	choose_parameter(arrive_goal_player,Player),
	choose_arrive_goal_for_player(Player,Type,Squares,Board),
	piece_description(Desc,Player,Type),
	arrive_goal(Goal,Desc,Squares).
	
choose_arrive_goal_for_player(player,Type,Squares,Board) :- 
	player_arrive_goal(Type,Squares,Board).
choose_arrive_goal_for_player(any_player,Type,Squares,Board) :- 
	player_arrive_goal(Type,Squares,Board).
choose_arrive_goal_for_player(opponent,Type,Squares,Board) :- 
	opponent_arrive_goal(Type,Squares,Board).

% Player and any_player arrival goals restricted to promote_only pieces.
player_arrive_goal(Type,Squares,Board) :- 
	choose_unplaced_piece_set(Board,Type),
	random_square(Board,Sq),
	Squares = [Sq].

% Opponent arrival goals constrained not to be square
% to which opponent's pieces might be assigned.
%
opponent_arrive_goal(Type,Squares,Board) :- 
	piece_generalization(Board,Type),
	opponent_random_free_square(Board,Sq),
	Squares = [Sq].


opponent_random_free_square(Board,Sq) :- 
	board_size(Board,XMax,YMax),
	array_squares(XMax,YMax,BSquares),
	random_permute(BSquares,RSquares),
	board_player_assigned_squares(Board,opponent,Assigned),
	free_different_member(RSquares,Assigned,Sq).

free_different_member([],_,_) :- 
	format("Error: no free squares for opponent arrival goal!~n",[]),
	!, fail.
free_different_member([S|Sqs],Assigned,Sq) :- 
	( member(S,Assigned) ->
	  free_different_member(Sqs,Assigned,Sq)
	; Sq=S
	).
	  

% ===================================
% CHOOSE_ERADICATE_GOAL(+Board,-Goal)
% ===================================
% Checks that eradicate goal contains at least 1 piece which will
% actually be on  the board at the start of the game (after
% placements). Otherwise the game would always be a draw.

choose_eradicate_goal(Board,Goal) :-
	choose_parameter(eradicate_goal_player,Player),
	choose_eradicate_goal_for_player(Player,Type,Board),
	piece_description(Desc,Player,Type),
	eradicate_goal(Goal,Desc).
	
choose_eradicate_goal_for_player(_Player,PieceGen,Board) :- 
	eradicate_piece_generalization(Board,PieceGen).
	
% -----------------------------------------
% ERADICATE_PIECE_GENERALIZATION(Board,Gen)
% -----------------------------------------
% Either choose any_piece, or some specific set of pieces.
% Depends on params:
%   eradicate_generalization_level (specific or any)
%   more_eradicate_pieces (boolean)
%
eradicate_piece_generalization(Board,Gen) :-
	choose_eradicate_generalization_level(L),
	generalize_eradicate_piece(L,Board,Gen).

% =================================
% ERADICATE_PIECE_SET(Pieces,Board)
% =================================
% Choose a random piece which we are sure will be on the 
% board.  Then possibly choose any other pieces.
%
eradicate_piece_set(Board,SortedPieces) :- 
	random_placed_piece(Board,Placed),
	board_piece_types(Board,Types),
	select(Placed,Types,Rest),
	choose_set_by_param(Rest,more_eradicate_pieces,Pieces),
	sort([Placed|Pieces],SortedPieces).

random_placed_piece(Board,Piece) :- 
	board_placed_pieces(Board,Pieces),
	random_element(Pieces,Piece).
	
choose_eradicate_generalization_level(L) :- 
	choose_parameter(eradicate_generalization_level,L).

generalize_eradicate_piece(specific,Board,Pieces) :-
	eradicate_piece_set(Board,Pieces).
generalize_eradicate_piece(any,_Board,any_piece).


%--------------------------------------------------------------------------------
%			   Simplifying Goals
%--------------------------------------------------------------------------------


% ===========================================
% SIMPLIFY_GOALS(+Complex,+Board,-Simplified)
% ===========================================

simplify_goals(Complex,Board,Simplified) :-
	remove_duplicates(Complex,Simp1),
	remove_subsumed(Simp1,Board,Simplified).

remove_subsumed(Goals,Board,Simplified) :- 
	remove_subsumed(Goals,Goals,Board,Simplified).

% Removes all the goals in arg1 which are subsumed by 
% a goal in arg2, given current board Board, resulting in arg4.
remove_subsumed([],_,_,[]).
remove_subsumed([Goal|Goals],AllGoals,Board,Gs) :- 
	subsumed_goal(Goal,AllGoals,Board), !,
	remove_subsumed(Goals,AllGoals,Board,Gs).
remove_subsumed([Goal|Goals],AllGoals,Board,[Goal|Gs]) :- 
	remove_subsumed(Goals,AllGoals,Board,Gs).


% ==================================
% SUBSUMED_GOAL(+Goal,+Goals,+Board)
% ==================================
% A goal is subsumed by a set of goals if there is some *other*
% goal Sub in Goals which subsumes it.
subsumed_goal(Goal,Goals,Board) :- 
	member(Sub,Goals),
	\+ Sub = Goal,
	subsumes(Sub,Goal,Board),
	tracing_gen_format(subsume,"~nSubsumed Goal: ~p~nSubsumed by: ~p~n",[Goal,Sub]).

% -----------------------------
% SUBSUMES(Subsumer,Goal,Board)
% -----------------------------
subsumes(Sub,Goal,Board) :- goal_implies(Goal,Sub,Board).

goal_implies(Erad1,Erad2,_Board) :- 
	eradicate_goal(Erad1,Player1,Type1),
	eradicate_goal(Erad2,Player2,Type2),
	erad_implies(Player1,Type1,Player2,Type2).
goal_implies(Arr1,Arr2,Board) :- 
	arrive_goal(Arr1,Player1,Type1,Sq1),
	arrive_goal(Arr2,Player2,Type2,Sq2),
	arrive_implies(Player1,Type1,Sq1,Player2,Type2,Sq2,Board).

% ------------
% erad_implies
% ------------
% a. true(erad(player,any_piece)) => true(erad(player,[piece1,piece2])).
%    true(erad(player,[piece1,piece2,piece3)) => true(erad(player,[piece1,piece2])).
%    true(erad(player,[piece1,piece2,piece3])) 
%    => (for opponent) true(erad(opponent,[piece1,piece2])).
%
erad_implies(_Player1,Type1,_Player2,Type2) :- 
	type_contains(Type1,Type2).

% --------------
% arrive_implies
% --------------
% a. true(arrive(player,[piece1],square1)) => true(arrive(player,[piece1,piece2],square1)).
%    true(arrive(player,[piece1],square1)) => 
%    true(arrive(player,[piece1,piece2],[square1,square2)).
% b. true(arrive(player,[piece1],square1)) => 
%    true(arrive(opponent,[piece1,piece2],square2))  {when sq2 is inverted sq1}
arrive_implies(Player1,Type1,Sq1,Player1,Type2,Sq2,_Board) :- !,
	arrive_implies(Type1,Sq1,Type2,Sq2).
arrive_implies(_Player1,Type1,Sq1,_Player2,Type2,Sq2,Board) :-
	invert_board_squares(Board,Sq1,Sq1Inv),
	arrive_implies(Type1,Sq1Inv,Type2,Sq2).
	
arrive_implies(Type1,Sq1,Type2,Sq2) :- 
	type_contains(Type2,Type1),
	squares_contains(Sq2,Sq1).

squares_contains(Sq1,Sq2) :- 
	ord_subset(Sq2,Sq1).

type_contains(any_piece,_).
type_contains(Type1,Type2) :- 
	ord_subset(Type2,Type1).

% --------------------------------------------------------------------
% These are not used in the generator, but may be useful elsewhere.	
% --------------------------------------------------------------------

% True if Desc *cannot* match any PLACED pieces.
unplaced_piece_description(Board,Desc) :-
	\+ placed_piece_description(Board,Desc).

% True if Desc *can* match some PLACED pieces.
placed_piece_description(Board,Desc) :-
	piece_description_piece(Desc,Piece),
	contains_placed_piece(Piece,Board).

contains_placed_piece(any_piece,_) :- !.
contains_placed_piece(Pieces,Board) :- 
	board_placed_pieces(Board,Placed),
	member(P,Pieces),
	member(P,Placed), !.

% --------------------------------------------------------------------

%================================================================================
% TRACING execution of game generation routines
%================================================================================

% The following tracing modules are used in this file:
%	goals:  info  goal generation
%	simplify:  info on goal simplification
%       subsume:  info on goal redundancy checking and elimination
%	pieces: info on piece generation.
% 
% Each module can be set on/off, using set_gen_verbosity (see below), or 
% using trace_gen_<module>. 
%
% All can be turned off with silent_gen.

:- my_ensure_loaded(library(tracing)).

tracing_gen(Type,Call) :- 
	( tracing(gen(Type)) -> call(Call) ; true ).

% Might cause trouble later when want to use streams also.
tracing_gen_format(Type,String,Args) :- 
	( tracing(gen(Type))
	-> format(String,Args)
	; true 
	).

tracing_gen_timing(Type,Call) :- 
	trace_timing(gen(Type),Call).

set_gen_verbosity(Level,Status) :- set_tracing(gen(Level),Status).

silent_gen :- all_gen(off).
loud_gen :- all_gen(on).

all_gen(Status) :- 
	set_gen_verbosity(goals,Status), 
	set_gen_verbosity(simplify,Status), 
	set_gen_verbosity(subsume,Status), 
	set_gen_verbosity(pieces,Status). 

trace_gen_subsume :- set_gen_verbosity(subsume,on). 
trace_gen_simplify :- set_gen_verbosity(simplify,on). 
trace_gen_goals :- set_gen_verbosity(goals,on). 
trace_gen_pieces :- set_gen_verbosity(pieces,on). 

%:- trace_gen_simplify.
%:- silent_gen.


