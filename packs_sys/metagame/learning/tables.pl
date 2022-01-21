%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================



%=============================================================================
% Converting square_tables to weighted digraph matrices.
%=============================================================================
% At top level, certainly good to use a fixed structure,
% as always have entries for those squares.
% At second level, don't always have entries. 
% Could actually decide on best representation given 
% number of args to be mapped, where if small use list
% and if large use array.
% Accessing routines would have to check which is used,
% but that's not hard.   

matrix_to_square_table([],A) :- !, new_square_table(A).
matrix_to_square_table([V-E|D],A) :-
	matrix_to_square_table(D,A),
	pairs_to_square_table(E,A2),
	arg(V,A,A2).

pairs_to_square_table([],A) :- !, new_square_table(A).
pairs_to_square_table([V-W|D],A) :-
	pairs_to_square_table(D,A),
	arg(V,A,W).

square_table_to_matrix(A,D) :- 
	square_table_to_list(A,List),
	square_tables_to_matrix(List,D).

square_tables_to_matrix([],[]).
square_tables_to_matrix([I-A|ARest],[I-D|DRest]) :- 
	square_table_to_list(A,D),
	square_tables_to_matrix(ARest,DRest).

%=============================================================================
% Converting weighted digraphs to logarithmic arrays (not used now)
%=============================================================================

d_to_array([],A) :- !, new_array(A).
d_to_array([V-E|D],A) :-
	d_to_array(D,A1),
	pairs_to_array(E,A2),
	aset(V,A1,A2,A).

pairs_to_array([],A) :- !, new_array(A).
pairs_to_array([V-W|D],A) :-
	pairs_to_array(D,A1),
	aset(V,A1,W,A).

array_to_d(A,D) :- 
	alist(A,List),
	arrays_to_d(List,D).

arrays_to_d([],[]).
arrays_to_d([I-A|ARest],[I-D|DRest]) :- 
	alist(A,D),
	arrays_to_d(ARest,DRest).


%================================================================================
% make_basic_tables
%================================================================================

% Do 'trace savetables' to keep these tables around,
% otherwise they get cleaned up. 
% We use the temp file here to avoid several processes
% writing to the same one at once. 
compile_basic_tables :-
	with_temp_file(basic,File,
	  (compile_basic_tables_to_file(File),
	   compile(File))).


compile_basic_tables_to_file(File) :-
	format("~nCompiling Basic Tables to file: ~w~n",[File]),
	make_basic_tables,
	with_output_file(File,write,
	    list_basic_tables).

list_basic_tables :- 
	whenever(
	  basic_table_pred(Pred),
	  listing(Pred)).


make_basic_tables :- 
	assert_square_indices,
	assert_piece_indices,
	assert_prom_square_indices.

basic_table_pred(opponent_prom_sq/1).
basic_table_pred(player_prom_sq/1).
basic_table_pred(prom_square_indices/2).

basic_table_pred(piece_type_index/2).
basic_table_pred(piece_type_count/1).
basic_table_pred(index_to_piece/2).
basic_table_pred(total_piece_count/1).

basic_table_pred(index_to_square/2).
basic_table_pred(total_square_count/1).
basic_table_pred(board_dim/1).




%================================================================================
% SQUARE_TABLE data structure
%================================================================================

new_square_table(A) :- square_table(A).

square_table_distance(Item1,Item2,Table,Distance) :- 
	arg(Item1,Table,Sub),
	arg(Item2,Sub,Distance1),
	interpret_distance(Distance1,Distance).

square_matrix_distance(Item1,Item2,Table,Distance) :- 
	member1_pair(Item1-Sub,Table),
	member1_pair(Item2-Distance1,Sub),
	interpret_distance(Distance1,Distance).



% Could make clause for default distance when non-reachable.
interpret_distance(Distance,Distance) :- nonvar(Distance).


square_table_to_list(A,List) :- 
	functor(A,square_table,N),
	sqtl(1,N,A,List).

sqtl(N1,N,_A,[]) :- N1 > N, !.
sqtl(N,Max,A,[N-I|In]) :- 
	arg(N,A,I),
	N1 is N+1,
	sqtl(N1,Max,A,In).
	

square_table(Table) :- 
	total_square_count(Count),
	functor(Table,square_table,Count).

sindex_table_entry(Index,Table,Entry) :- 
	arg(Index,Table,Entry).

% An sl_table is like a square table where each
% element is a structure:  List/Data, instead of a normal value.
slindex_table_entries(Index,Table,List,Data) :- 
	arg(Index,Table,Val),
	nonvar(Val),
	Val = List/Data.
	
% Like the above, but here backtrack over elements of that list. 
slindex_table_entry(Index,Table,Entry,Data) :- 
	slindex_table_entries(Index,Table,List,Data),
	member(Entry,List).
	




square_table_entry(Square,Index,Table,Entry) :- 
	square_index(Square,Index),
	sindex_table_entry(Index,Table,Entry).
	
square_indices(Is) :- setof(I,P^square_index(P,I),Is).


	

%================================================================================
% Mapping square tables
%================================================================================

% Example:
% map_square_table(transition_table,Square,SIndex,[Matrix],Table)
% Will call the predicate: 
% transition_table(Square,SIndex,Matrix,Entry)
% With each Entry being the corresponding slot of the Square'th 
% entry in the final Table. 
% Here we're saying args must be insensitive to side-effects.
% That is, we are using the same copy of the args each time.

map_square_table(Pred,Table) :- 
	map_square_table(Pred,[],Table).

map_square_table(Pred,Args,Table) :- 
	square_table(Table),
	square_indices(Indices),
	map_for_squares(Indices,Pred,Args,Table).

map_for_squares([],_,_,_).
map_for_squares([S|Ss],Pred,Args,Table) :- 
	square_table_entry(Square,S,Table,Entry),
	append([Pred,Square,S|Args],[Entry],GoalList),
	Goal =.. GoalList,
	call(Goal),
	map_for_squares(Ss,Pred,Args,Table).
	



% Maps a goal across to corresponding square tables.
% maps_square_table(transition_table,Square,PIndex,[Matrix],Entry)
% transition_table(Square,PIndex,Matrix,Entry)

maps_square_table(Pred,Table1,Table2) :- 
	maps_square_table(Pred,[],Table1,Table2).

maps_square_table(Pred,Args,Table1,Table2) :- 
	square_table(Table1),
	square_table(Table2),
	square_indices(Indices),
	maps_for_squares(Indices,Pred,Args,Table1,Table2).

maps_for_squares([],_,_,_,_).
maps_for_squares([S|Ss],Pred,Args,Table1,Table2) :- 
	square_table_entry(Square,S,Table1,Entry1),
	square_table_entry(Square,S,Table2,Entry2),
	append([Pred,Square,S|Args],[Entry1,Entry2],GoalList),
	Goal =.. GoalList,
	call(Goal),
	maps_for_squares(Ss,Pred,Args,Table1,Table2).
	

% Counts the number of square indices for
% which Goal is true.  
% This is often an easy way to count some function 
% across the whole board.
count_bagof_squares(Sq,Goal,Squares) :- 
	count_bagof(Sq, (X,Y)^
	( square_to_index(X,Y,Sq),
	  Goal
	),
	Squares).


add_portray_square_table :- 
	new_square_table(T),
	assert((portray(T) :- portray_square_table(T))).

portray_square_table(_T) :- format("<Square Table>",[]).



%================================================================================
% PIECE_TABLE data structure
%================================================================================

new_piece_table(A) :- piece_table(A).


piece_table(Table) :- 
	total_piece_count(Count),
	functor(Table,piece_table,Count).

pindex_table_entry(Index,Table,Entry) :- 
	arg(Index,Table,Entry).

piece_table_entry(Piece,Index,Table,Entry) :- 
	piece_index(Piece,Index),
	pindex_table_entry(Index,Table,Entry).
	
piece_matrix_distance(Item1,Item2,Table,Distance) :- 
	member1_pair(Item1-Sub,Table),
	member1_pair(Item2-Distance1,Sub),
	interpret_distance(Distance1,Distance).




piece_indices(Is) :- setof(I,P^piece_index(P,I),Is).

add_portray_piece_table :- 
	new_piece_table(T),
	assert((portray(T) :- portray_piece_table(T))).

portray_piece_table(_T) :- format("<Piece Table>",[]).

portray_tables :- 
	add_portray_piece_table,
        add_portray_square_table.



%================================================================================
% Mapping piece tables
%================================================================================

% Example:
% map_piece_table(transition_table,Piece,PIndex,[Matrix],Table)
% Will call the predicate: 
% transition_table(Piece,PIndex,Matrix,Entry)
% With each Entry being the corresponding slot of the Piece'th 
% entry in the final Table. 
% Here we're saying args must be insensitive to side-effects.
% That is, we are using the same copy of the args each time.

map_piece_table(Pred,Table) :- 
	map_piece_table(Pred,[],Table).

map_piece_table(Pred,Args,Table) :- 
	piece_table(Table),
	piece_indices(Indices),
	map_for_pieces(Indices,Pred,Args,Table).

map_for_pieces([],_,_,_).
map_for_pieces([P|Ps],Pred,Args,Table) :- 
	piece_table_entry(Piece,P,Table,Entry),
	append([Pred,Piece,P|Args],[Entry],GoalList),
	Goal =.. GoalList,
	call(Goal),
	map_for_pieces(Ps,Pred,Args,Table).
	



% Maps a goal across to corresponding piece tables.
% maps_piece_table(transition_table,Piece,PIndex,[Matrix],Entry)
% transition_table(Piece,PIndex,Matrix,Entry)

maps_piece_table(Pred,Table1,Table2) :- 
	maps_piece_table(Pred,[],Table1,Table2).

maps_piece_table(Pred,Args,Table1,Table2) :- 
	piece_table(Table1),
	piece_table(Table2),
	piece_indices(Indices),
	maps_for_pieces(Indices,Pred,Args,Table1,Table2).

maps_for_pieces([],_,_,_,_).
maps_for_pieces([P|Ps],Pred,Args,Table1,Table2) :- 
	piece_table_entry(Piece,P,Table1,Entry1),
	piece_table_entry(Piece,P,Table2,Entry2),
	append([Pred,Piece,P|Args],[Entry1,Entry2],GoalList),
	Goal =.. GoalList,
	call(Goal),
	maps_for_pieces(Ps,Pred,Args,Table1,Table2).

%==============================================================================
% Making Square Index Table
%==============================================================================

% SQUARE_INDEX(?Square,?Index)
% True when square-struct SQUARE has the Index in the table. 
% This is Bidirectional, and gives indexing for both arguments. 
% If both args are unbound, will generate all squares and their indices
% for the current game. 
% Square should be a square_struct: square(type,player).
% This should only be used after the table has been created with
% ASSERT_SQUARE_INDICES.
%
square_index(Square,Index) :- 
	( var(Index) ->
	  square_to_index(Square,Index)
	; index_to_square(Index,Square)
	).


current_board_dim(Dim) :-
%	current_board_size(_X,Dim).
	current_board_size(Dim,_Y).

% Makes available the pred: BOARD_DIM/1.
set_board_dim :-
	abolish(board_dim/1),
	current_board_dim(Dim),
	assert((board_dim(Dim))).
	
% Makes available the pred: TOTAL_SQUARE_COUNT/1.
set_square_count :- 
	set_board_dim,
	abolish(total_square_count/1),
	current_board_size(X,Y),
	Total is X*Y,
	assert((total_square_count(Total))).


% Makes a table mapping each index into a different square struct.
assert_square_indices :-
	abolish(index_to_square/2),
	set_square_count,
	whenever(square_to_index(Square,Key),
	   assert_square_index(Square,Key)),
	tracing_anal(index,print_square_indices).

assert_square_index(Square,Index) :- 
	assert(index_to_square(Index,Square)).

print_square_indices :- 
	format("Square Index Table:~n",[]),
	( index_to_square(I,P),
	  format("~p --> ~p~n",[I,P]),
	  fail
        ; true
	).

square_to_index(square(X,Y),Index) :-
	board_square(X,Y),
	board_dim(Dim),
	Index is Dim*(Y-1)+X.

square_to_index(X,Y,Index) :-
	board_square(X,Y),
	board_dim(Dim),
	Index is Dim*(Y-1)+X.

board_square(square(X,Y)) :- 
	board_square(X,Y).

% Need board_square predicate.
board_square(X,Y) :- 
	current_board_size(XMax,YMax),
	between(1,YMax,Y),
	between(1,XMax,X).


/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Piece-Type Index table looks like this:
1 --> firefly
2 --> slug
3 --> termite
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Piece Index table looks like this:
1 --> white firefly
2 --> white slug
3 --> white termite
4 --> black firefly
5 --> black slug
6 --> black termite
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

% PIECE_INDEX(?Piece,?Index)
% True when piece-struct PIECE has the Index in the table. 
% This is Bidirectional, and gives indexing for both arguments. 
% If both args are unbound, will generate all pieces and their indices
% for the current game. 
% Piece should be a piece_struct: piece(type,player).
% This should only be used after the table has been created with
% ASSERT_PIECE_INDICES.
%
piece_index(Piece,Index) :- 
	( var(Index) ->
	  piece_to_index(Piece,Index)
	; index_to_piece(Index,Piece)
	).

current_game_piece_count(Count) :- 
	player_current_game(Game),
	game_piece_count(Count,Game).
	
game_piece_count(Count,Game) :- 
	game_piece_names(Game,Names),
	length(Names,Count).

nth_piece_name(N,P) :- 
	player_current_game(G),
	game_piece_names(G,Names),
	nth(N,Names,P).

% Makes a table mapping each index into a different piece struct.
assert_piece_indices :-
	assert_piece_type_indices,
	abolish(index_to_piece/2),
	whenever(piece_to_index(Piece,Key),
	   assert_piece_index(Piece,Key)),
	tracing_anal(index,print_piece_indices).

assert_piece_type_indices :-
	abolish(piece_type_index/2),
	set_piece_type_count, 
	whenever(nth_piece_name(N,P),
	   assert_piece_type_index(P,N)).
		    
% Makes available the pred: piece_type_count/1.
% Also pred: total_piece_count/1 (just double the above).
set_piece_type_count :- 
	abolish(piece_type_count/1),
	abolish(total_piece_count/1),
	current_game_piece_count(Count),
	assert((piece_type_count(Count))),
	Total is Count*2,
	assert((total_piece_count(Total))).

	
assert_piece_type_index(Piece,Index) :- 
%	dir_key(Dir,Key),
	assert(piece_type_index(Piece,Index)).

assert_piece_index(Piece,Index) :- 
%	piece_key(Piece,Index),
	assert(index_to_piece(Index,Piece)).

print_piece_type_indices :- 
	( piece_type_index(P,I),
	  format("~p --> ~p~n",[I,P]),
	  fail
	; true
	).

print_piece_indices :- 
	format("Piece Index Table:~n",[]),
	( index_to_piece(I,P),
	  format("~p --> ~p~n",[I,P]),
	  fail
        ; true
	).


piece_to_index(piece(Name,Player),Key) :-
	piece_player_mult(Player,Mult),
	piece_type_index(Name,Y),
	piece_type_count(Count),
	Key is Count*Mult + Y.
	
piece_player_mult(player,0).
piece_player_mult(opponent,1).

	

%================================================================================
% PLAYER_TABLE data structure
%================================================================================

total_player_count(2).

player_index(player,1).
player_index(opponent,2).

player_table(Table) :- 
	total_player_count(Count),
	functor(Table,player_table,Count).

player_table_entry(Player,Index,Table,Entry) :- 
	player_index(Player,Index),
	pindex_table_entry(Index,Table,Entry).
	
player_indices(Is) :- setof(I,P^player_index(P,I),Is).


%================================================================================
% Mapping player tables
%================================================================================

% Example:
% map_player_table(transition_table,Player,PIndex,[Matrix],Table)
% Will call the predicate: 
% transition_table(Player,PIndex,Matrix,Entry)
% With each Entry being the corresponding slot of the Player'th 
% entry in the final Table. 
% Here we're saying args must be insensitive to side-effects.
% That is, we are using the same copy of the args each time.

map_player_table(Pred,Table) :- 
	map_player_table(Pred,[],Table).

map_player_table(Pred,Args,Table) :- 
	player_table(Table),
	player_indices(Indices),
	map_for_players(Indices,Pred,Args,Table).

map_for_players([],_,_,_).
map_for_players([P|Ps],Pred,Args,Table) :- 
	player_table_entry(Player,P,Table,Entry),
	append([Pred,Player,P|Args],[Entry],GoalList),
	Goal =.. GoalList,
	call(Goal),
	map_for_players(Ps,Pred,Args,Table).
	



% Maps a goal across to corresponding player tables.
% maps_player_table(transition_table,Player,PIndex,[Matrix],Entry)
% transition_table(Player,PIndex,Matrix,Entry)

maps_player_table(Pred,Table1,Table2) :- 
	maps_player_table(Pred,[],Table1,Table2).

maps_player_table(Pred,Args,Table1,Table2) :- 
	player_table(Table1),
	player_table(Table2),
	player_indices(Indices),
	maps_for_players(Indices,Pred,Args,Table1,Table2).

maps_for_players([],_,_,_,_).
maps_for_players([P|Ps],Pred,Args,Table1,Table2) :- 
	player_table_entry(Player,P,Table1,Entry1),
	player_table_entry(Player,P,Table2,Entry2),
	append([Pred,Player,P|Args],[Entry1,Entry2],GoalList),
	Goal =.. GoalList,
	call(Goal),
	maps_for_players(Ps,Pred,Args,Table1,Table2).


%==============================================================================
% Making Promotion Square Index Table
%==============================================================================

assert_prom_square_indices :- 
	abolish(prom_square_indices/2),
	assert_prom_square_indices(player),
	assert_prom_square_indices(opponent).
	
assert_prom_square_indices(Player) :- 
	find_prom_square_indices(Player,SqIs),
	assert(prom_square_indices(Player,SqIs)),
	assert_player_prom_sqs(Player,SqIs).

assert_player_prom_sqs(player,SqIs) :- 
	abolish(player_prom_sq/1),
	whenever(member(Sq,SqIs),
	   assert(player_prom_sq(Sq))).
assert_player_prom_sqs(opponent,SqIs) :- 
	abolish(opponent_prom_sq/1),
	whenever(member(Sq,SqIs),
	   assert(opponent_prom_sq(Sq))).

player_prom_pred(player,Sq,player_prom_sq(Sq)).
player_prom_pred(opponent,Sq,opponent_prom_sq(Sq)).
	

find_prom_square_indices(Player,SqIs) :- 
	prom_sqs(Player,_,SqIs).

% PLAYER_PROMOTION_SQUARE(?Player,?PlI,?Sq,?SqI)
% True if Sq is a square in promotion region for Player,
% both with their respective indices.  
% This uses the constructed table and is thus very efficient.
player_promotion_square(Player,PlI,Sq,SqI) :- 
	player_index(Player,PlI),
	square_index(Sq,SqI),
	prom_square_for_player(Player,SqI).

player_promotion_square(Player,Sq) :- 
	player_promotion_square(Player,_PlI,Sq,_SqI).

prom_square_for_player(player,Sq) :- player_prom_sq(Sq).
prom_square_for_player(opponent,Sq) :- opponent_prom_sq(Sq).
	
in_promote_region(Sq,Player) :- 
	player_promotion_square(Player,Sq).


% Could make faster by tabulating. 
prom_sqs(Player,Sqs,SqIs) :-  
	setof(Sq-SqI,
	  prom_sq(Player,Sq,SqI),
	  Pairs),
	pair_list(Sqs,SqIs,Pairs).
	 

prom_sq(Player,Sq,SqI) :- 
	square_index(Sq,SqI),
	prom_sq(Player,Sq).

prom_sq(Player,Sq) :- 
	current_game_for_player(Player,Game),
	game_promote_rank(Game,Rank),
	invert(Sq,Player,Sq1),
	square(Sq1,_X,Y), 
	Y >= Rank.
	



/*
% Random: Time is 0.367 sec.
in_promote_region(Sq,Player,_) :- 
	in_promote_region(Sq,Player).
*/
