%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%% prom.pl

% PROMOTION Strategy
% A portion of a piece's value on a square is its possibility
% to move and promote into something else.  
% The value comes from the value derived by getting that promoted piece
% on the promotion square, and then deriving whatever intrinsic
% value such a piece would have there.
% However, we don't actually get the intrinsic value immediately.
% 1. Clear the promotion square (and a path to it, perhaps)
% 2. Move the piece to the promotion square.
% 3. Execute a number of moves in promotion region to create the
%    desired promoted piece (assuming we could do each in just 
%    1 move for now).  
% 
% Note that as promoting is usually optional, we should discard those
% possibilities which would give the promoting player  negative utility.  
%
% The value of a piece to be promoted is the max value of anything we might 
% promote it into.  This avoids the situation where we don't promote a piece 
% because we prefer the option of all possible promotions to any one, but then
% we get a chess pawn sitting on the 7th rank enjoying its options!  
%
% For the moment, we may ignore the possibility to promote into the opponent's
% on a square which would win us the game, and instead consider only those
% promotions which keep the piece in our control.  
% ==> No, now we even consider promoting to opponent's piece if that gives us
% better value.  
%
% Also, we  choose the most accessible square, but independent of choice of
% promotion piece.  Thus, it may not be the best square for some piece which
% would  be our best choice piece on some other square. 
%

prom_value(Piece,Sq,SqT,Value,Pos,Tables) :- 
	owns(Piece,Player),
	useful_promotes(Piece,Player,Tables), 
	focussed_promsq_cost(Piece,Player,Sq,SqT,PathCost,Pos,Tables),
	best_promotion_choice_value(Piece,Player,SqT,PathCost,_PieceT,Value,Pos,Tables).



best_promotion_choice_value(Piece,Player,SqT,PathCost,
	                    BestPiece,BestValue,Pos,Tables) :- 
	findall( Value-PieceT,
	  promotion_choice_value(Piece,Player,SqT,PathCost,PieceT,Value,Pos,Tables),
	  Pairs),
	best_player_choice(Player,Pairs,BestPiece,BestValue).	   

promotion_choice_value(Piece,Player,SqT,PathCost,PieceT,Value,Pos,Tables) :- 
	piece_player_prom_distance(Piece,Player,PieceT,PromDist,Tables),
	PieceT \== Piece,  % Ie PromDist > 0
	intrinsic_value(PieceT,SqT,IVal,Pos,Tables),
	prom_likelihood(PromDist,PathCost,Prob),
	reasonable_likelihood(Prob),
	expected_value(Prob,IVal,Value),
	favorable_to_owner(Player,Value).



useful_promotes(Piece,Player,Tables) :- 
	player_safe_new_prom(Piece,Player,_PieceT,_SqT,Tables) -> true. 


% Player can promote Piece to PieceT on SqT.
% Backtracks over all SqT. 
player_safe_new_prom(Piece,Player,PieceT,SqT,Tables) :- 
	piece_player_promotion(Piece,Player,PieceT,Tables),
	Piece \== PieceT,
	opposite_role(Player,Opp),
	player_promotion_square(Player,SqT),
	\+ goal_square(PieceT,SqT,Opp,_).


	
% Of the nearest prom squares, choose the one most accessible
% in the current position.
% This is the one with the lowest cost to clear it.
% 
% [ Technically we shouldn't choose among only the nearest,
% as one may be farther but empty, and this would be preferable.
% However, we don't want to search through all of promotion region
% and do this comparison if we can avoid it.  ]

focussed_promsq_cost(Piece,Player,Sq,SqT,Cost,Pos,Tables) :- 
	square_index(Sq,SqI),
	piece_index(Piece,PieceI),
	square_piece_psqs(SqI,PieceI,SqTIs,Dist,Tables),
	cheapest_destination(Piece,Player,Sq,SqTIs,Dist,SqT,Cost,Pos,Tables).

% More efficient if we store the target squares, instead of indices,
% in promsq matrix. 
cheapest_destination(Piece,Player,Sq,SqTIs,Dist,SqT,Cost,Pos,Tables) :- 
	findall(Cost1-SqT1,
	  ( member(SqTI1,SqTIs),
	    square_index(SqT1,SqTI1),
	    cost_to_promsq(Piece,Player,Sq,SqT1,Dist,Cost1,Pos,Tables)
	  ),
	  Dests),
	  sort(Dests,[Cost-SqT|_]).


cost_to_promsq(Piece,Player,Sq,SqT,SqDist,PathCost,Pos,_Tables) :- 
%	distance_to_promsq(Piece,Sq,SqT,SqDist,Tables),
	clear_path_cost(Piece,Player,Sq,SqT,SqDist,PathCost,Pos).


distance_to_promsq(Piece,Sq,SqT,SqDist,Tables) :- 
	square_piece_promsq(Sq,Piece,SqT,SqDist,Tables).



% The likelihood of achieving the promotion decreases
% exponentially with the number of moves required to 
% achieve it.  
% We subtract 1 from the total as if a piece promotes 
% immediately into another, the cost is just the path 
% distance.  
prom_likelihood(PromDist,PathCost,Prob) :- 
	MinMoves is PromDist + PathCost - 1,
	distance_value(MinMoves,Prob).



% This must measure the value independent of promotions
% this piece could make, as these will already be taken
% into account. 
% Thus we shut down both the threat analysis and the 
% other promotions.  
% Also shutdown dynamic-mob, to save time (if have more time, keep it).
intrinsic_value(Piece,Sq,Val,Pos,Tables) :- 
	shutdown_advisor(threat,Tables),
	shutdown_advisor(prom,Tables),
	shutdown_advisor(dynamic_mobility,Tables),
	local_evaluation(Piece,Sq,Val,Pos,Tables).


testp :- 
	checkpoint(init,S),
	get_advices(A,S),
	ppl(A).





%============================================================================
% Tables for Promotion Goals
%============================================================================

% promotes_into(OrigPiece,PromPiece,Distance)
% OrigPiece, if it could use its promotion power each turn,
% could promote into PromPiece after Distance turns.
%
% We first make a graph, where each node is a piece(Player,Type)
% for each type of piece in the game def.  We add an edge from P1 to P2
% if P1 could promote directly into P2.  
% From this we can then answer several of these types of questions by 
% standard graph algorithms. 

promotes_into(Piece,PieceI,Prom,PromI,Player,Chooser) :- 
	promotes_into(Piece,Prom,Player,Chooser),
	piece_index(Piece,PieceI),
	piece_index(Prom,PromI).
	

opponent_promotes(Piece) :- 
	promotes_into(Piece,_,Player,Chooser), 
	opposite_role(Player,Chooser).  
	

promotes_into(OrigPiece,PromPiece,Player,Chooser) :- 
	owns(OrigPiece,Player),
	current_game_for_player(Player,Game),
	game_piece_promoting(OrigPiece,Promoting,Game),
	promote_choice(Promoting,Player,Chooser,PromPiece).

promote_choice(Promoting,Player,Player,PromPiece) :- 
	simple_promote(Promoting,Player,PromPiece).
promote_choice(Promoting,_Player,Chooser,PromPiece) :- 
	Promoting = promote(Chooser,Descr),
	matches(Descr,PromPiece).

	
forced_promote(Player,PlI,Piece,PieceI,Prom,PromI) :- 
	player_index(Player,PlI),
	forced_promote(Player,Piece,PieceI,Prom,PromI).

forced_promote(Player,Piece,PieceI,Prom,PromI) :- 
%	player_role(Player), 
	promotes_into(Piece,PieceI,Prom,PromI,Player,Player).


%==============================================================================
% Piece-Piece (Immediate Player_Promotion) Matrix
%==============================================================================

build_promotion_matrix(Matrix) :- 
	map_player_table(promotion_matrix,[],Matrix).

print_promotion_matrix :- 
	print_promotion_matrix(_Piece,_Player,_PieceT).

print_promotion_matrix(Piece,Player,PieceT) :- 
	format("Player Piece Promotion Matrix:~n",[]),
	( piece_player_promotion(Piece,Player,PieceT),
	  format("~p: ~p -> ~p~n",[Player,Piece,PieceT]),
%	  format("matrix[~p][~p] = ~p~n",[Piece,Player,PieceT]),
	  fail
        ; true
	).

ppp(Player,Piece,PieceT) :- piece_player_promotion(Piece,Player,PieceT).

% PIECE_PLAYER_PROMOTION(?Piece,?Player,?PieceT)
% Very nice table indicating when a player could
% move directly from one piece to another on an empty board.
piece_player_promotion(Piece,Player,PieceT) :- 
	advice_tables(Tables),
	piece_player_promotion(Piece,Player,PieceT,Tables).

% PIECE_PLAYER_PROMOTION(?Piece,?Player,?PieceT,+Tables)
% Very nice table indicating when a player could
% move directly from one piece to another on an empty board.
piece_player_promotion(Piece,Player,PieceT,Tables) :- 
	piece_player_promotion(Piece,_PieceI,Player,_PlayerI,PieceT,_PieceTI,Tables).

piece_player_promotion(Piece,PieceI,Player,PlayerI,PieceT,PieceTI,Tables) :- 
	  player_index(Player,PlayerI),
	  piece_index(Piece,PieceI),
	  piece_player_piece(PieceI,PlayerI,PieceTI,Tables),
	  piece_index(PieceT,PieceTI).

piece_player_piece(PieceI,PlayerI,PieceTI,Tables) :- 
	promotion_matrix(Tables,M),
	piece_player_piece1(PieceI,PlayerI,PieceTI,M).

piece_player_piece1(PieceI,PlayerI,PieceTI,M) :- 
	player_table_entry(_,PlayerI,M,Entry),
	member1_pair(PieceI-Ts,Entry),
	member(PieceTI,Ts).

piece_player_pieces(PieceI,PlayerI,TIs,Tables) :- 
	promotion_matrix(Tables,M),
	piece_player_pieces(PieceI,PlayerI,TIs,M).

piece_player_pieces1(PieceI,PlayerI,TIs,M) :- 
	player_table_entry(_,PlayerI,M,Entry),
	member1_pair(PieceI-TIs,Entry).


% Reversed order of args to use map table. 
%promotion_matrix(Player,PlIndex,Matrix) :- 
promotion_matrix(Player,PlIndex,Matrix) :- 
	player_index(Player,PlIndex),
	setof(PieceIndex-PieceTIndices, Piece^
	     player_pieces(Player,PlIndex,Piece,PieceIndex,PieceTIndices),
	     Matrix
	     ),
	tracing_anal_format(tables,"Built promotion matrix for <~p>~n",[Player]).


player_pieces(Player,PlIndex,Piece,PieceIndex,PieceTIndices) :- 
	piece_index(Piece,PieceIndex),
	( setof(PieceTIndex, PieceT^
	       player_piece_piece(Player,PlIndex,Piece,PieceIndex,PieceT,PieceTIndex),
	       PieceTIndices
	       )
	-> true
	; PieceTIndices=[]
	).

player_piece_piece(Player,Piece,PieceT) :- 
	player_piece_piece(Player,_PlIndex,Piece,_PieceIndex,PieceT,_PieceTIndex).
	  
player_piece_piece(Player,PlI,Piece,PieceI,PieceT,PieceTI) :- 
	piece_index(Piece,PieceI),
	player_index(Player,PlI),
	forced_promote(Player,PlI,Piece,PieceI,PieceT,PieceTI),
	piece_index(PieceT,PieceTI),
	tracing_anal_format(detailed,"~p: ~p -> ~p~n",[Player,Piece,PieceT]).



%==============================================================================
% Promotion Distance Matrix
%==============================================================================

% Requires transition-matrix (at the moment).
build_prom_distance_matrix(Trans,Matrix) :- 
	map_player_table(prom_distance_matrix,[Trans],Matrix).



prom_distance_matrix(Player,PlayerIndex,Matrix) :- 
	promotion_matrix(Trans),
	prom_distance_matrix(Player,PlayerIndex,Trans,Matrix).

prom_distance_matrix(Player,PlayerIndex,Trans,Matrix) :- 
	player_table_entry(Player,PlayerIndex,Trans,Matrix1),
	s_floyd(Matrix1,Matrix),
	tracing_anal_format(tables,"Built prom_distance matrix for <~p>~n",[Player]).


piece_player_list_dist(PieceI,PlayerI,PieceTI,Dist,Tables) :- 
	prom_distance_matrix(Tables,Matrix),
	piece_player_list_dist1(PieceI,PlayerI,PieceTI,Matrix,Dist).
	
piece_player_list_dist1(PieceI,PlayerI,PieceTI,Table,Dist) :- 
	player_table_entry(_,PlayerI,Table,Entry),
	piece_matrix_distance(PieceI,PieceTI,Entry,Dist). 

piece_player_list_prom_distance(PieceI,PlayerI,PieceTI,Dist,Tables) :- 
	prom_distance_matrix(Tables,Matrix),
	piece_player_list_prom_distance1(PieceI,PlayerI,PieceTI,Matrix,Dist).
	
piece_player_list_prom_distance1(PieceI,PlayerI,PieceTI,Table,Dist) :- 
	player_table_entry(_,PlayerI,Table,Entry),
	piece_matrix_distance(PieceI,PieceTI,Entry,Dist).	



print_prom_distance_matrix :- 
	print_prom_distance_matrix(_,_,_).

print_prom_distance_matrix(Piece,Player,PieceT) :- 
	format("Piece Player Prom_distance Matrix:~n",[]),
	( piece_player_prom_distance(Piece,Player,PieceT,Dist),
	  format("~p: ~p -> ~p <~p>~n",[Player,Piece,PieceT,Dist]),
%	  format("matrix[~p][~p] = ~p~n",[Piece,Player,PieceT]),
	  fail
        ; true
	).


ppd(P,Pl,PieceT,V) :- piece_player_prom_distance(P,Pl,PieceT,V).

% PIECE_PLAYER_PROM_DISTANCE(?Piece,?Player,?PieceT,?Dist)
% Very nice table indicating the minimum number of promotions
% (prom_distance) a player would
% take to move from one piece to another on an empty board.
piece_player_prom_distance(Piece,Player,PieceT,Dist) :- 
	advice_tables(Tables),
	piece_player_prom_distance(Piece,Player,PieceT,Dist,Tables).

% PIECE_PLAYER_PROM_DISTANCE(?Piece,?Player,?PieceT,?Dist,+Tables)
% Very nice table indicating the minimum number of promotions
% (prom_distance) a player would
% take to move from one piece to another on an empty board.
piece_player_prom_distance(Piece,Player,PieceT,Dist,Tables) :- 
	  player_index(Player,PlayerI),
	  piece_index(Piece,PieceI),
	  piece_index(PieceT,PieceTI),
	  piece_player_list_prom_distance(PieceI,PlayerI,PieceTI,Dist,Tables).


	
%==============================================================================
% Piece Promotion Square Distance (Promsq) Table
%==============================================================================
% Requires distance table now.  

build_promsq_matrix(Matrix) :- 
	distance_table(DTable),
	build_promsq_matrix(DTable,Matrix).

% Uses saved distance-table.
build_promsq_matrix(DTable,Matrix) :- 
	map_piece_table(promsq_matrix,[DTable],Matrix).

promsq_matrix(Piece,PieceIndex,Matrix) :- 
	distance_table(DTable),
	promsq_matrix(Piece,PieceIndex,DTable,Matrix).
	
promsq_matrix(Piece,PieceIndex,DTable,Matrix) :- 
	piece_index(Piece,PieceIndex),
	map_square_table(square_promsq,[Piece,PieceIndex,DTable],Matrix),
	tracing_anal_format(tables,"Built promsq table for <~p>~n",[Piece]).



% If nothing found, puts a variable in that entry. 
square_promsq(Sq,SqI,Piece,PieceI,DTable,SqTIs/Dist) :- 
	nearest_promotion_squares(Piece,PieceI,_Player,Sq,SqI,_SqTs,SqTIs,DTable,Dist), !.
square_promsq(_Sq,_SqI,_Piece,_PieceI,_DTable,_Ignore).




% Returns the list of SqTIs (all with the minimum Dist). 
square_piece_psqs(SqI,PieceI,SqTIs,Dist,Tables) :- 
	promsq_matrix(Tables,M),
	pindex_table_entry(PieceI,M,Entry),
	slindex_table_entries(SqI,Entry,SqTIs,Dist).


% Backtracks over each SqTI (all with the minimum Dist). 
square_piece_psq(SqI,PieceI,SqTI,Dist,Tables) :- 
	promsq_matrix(Tables,M),
	pindex_table_entry(PieceI,M,Entry),
	slindex_table_entry(SqI,Entry,SqTI,Dist).



spq(S,P,SqT,D) :- square_piece_promsq(S,P,SqT,D).

% SQUARE_PIECE_PROMSQ(?Sq,?Piece,?SqT,?Dist)
% Very nice table indicating for a piece on a square,
% one of the nearest promotion squares SqT is Dist squares away. 
% Backtracks over all nearest squares. 
square_piece_promsq(Sq,Piece,SqT,Dist) :- 
	advice_tables(Tables),
	square_piece_promsq(Sq,Piece,SqT,Dist,Tables).

% SQUARE_PIECE_PROMSQ(?Sq,?Piece,?SqT,?Dist,Tables)
% Very nice table indicating for a piece on a square,
% one of the nearest promotion squares SqT is Dist squares away. 
square_piece_promsq(Sq,Piece,SqT,Dist,Tables) :- 
	  piece_index(Piece,PieceI),
	  square_index(Sq,SqI),
	  square_piece_psq(SqI,PieceI,SqTI,Dist,Tables),
	  square_index(SqT,SqTI).




% Printing promsq matrix
%
print_promsq_matrix :- print_promsq_matrix(_,_).

print_promsq_matrix(Sq,Piece) :- 
	format("Square Piece Promsq Matrix:~n",[]),
	( square_piece_promsq(Sq,Piece,SqT,Dist),
	  format("matrix[~p][~p] = ~p (~p)~n",[Sq,Piece,SqT,Dist]),
	  fail
        ; true
	).



%=============================================================================
/*
Nearest promotion square/s: The closest square a piece could move to and
promote (on an empty board).  We could either use just the nearest
one, or if there are ties the nearest maximizing some value, etc.
*/

% NEAREST_PROMOTION_SQUARES(?Piece,?Player,?Sq,-SqTs,-Dist)
% SqTs are the nearest promotion square from Sq for Player's Piece,
% and getting to any of them would require Dist moves on an empty board. 

nearest_promotion_squares(Piece,Player,Sq,SqTs,Dist) :- 
	nearest_promotion_squares(Piece,_PieceI,Player,Sq,_SqI,SqTs,_SqTIs,Dist).

nearest_promotion_squares(Piece,PieceI,Player,Sq,SqI,SqTs,SqTIs,Dist) :- 
	distance_table(DTable),
	nearest_promotion_squares(Piece,PieceI,Player,Sq,SqI,SqTs,SqTIs,DTable,Dist).

nearest_promotion_squares(Piece,PieceI,Player,Sq,SqI,SqTs,SqTIs,DTable,Dist) :- 
	piece_index(Piece,PieceI),
	square_index(Sq,SqI),
	owns(Piece,Player),
	prom_square_indices(Player,PromSquares),
	nearest_squares(PromSquares,PieceI,SqI,SqTs,SqTIs,DTable,Dist).


% NEAREST_SQUARES(Squares,PieceI,SqI,SqTs,SqTIs,DTable,Dist) :- 
% SqTIs are the indices of the nearest members SqTs of Squares (list of
% indices) for piece index PieceI from SqI, and Dist is the
% min distance.
% If no reachable square was found, this fails. 
% DTable is the distance table. 

nearest_squares(Squares,PieceI,SqI,SqTs,SqTIs,DTable,Dist) :- 
	square_distances(Squares,PieceI,SqI,DTable,AllDists),
	closest_dists(AllDists,Dist,SqTIs),
	maplist(square_index,SqTs,SqTIs).


square_distances(Squares,PieceI,SqI,DTable,AllDists) :- 
	findall(Dist-SqTI,
	( member(SqTI,Squares),
	  SqTI \== SqI,
	  square_piece_sq_dist1(SqI,PieceI,SqTI,DTable,Dist)
	),
	AllDists).  
	   
closest_dists(AllDists,V,Es) :- 
	p_to_s_graph(AllDists,Graph),
	first_connected(Graph,V-Es).

first_connected([H|Rest],First) :-
	first1(H,Rest,First).

first1(V-Es,Rest,First) :- 
	( Es = [] -> first_connected(Rest,First)
	; First = V-Es
	).
 
