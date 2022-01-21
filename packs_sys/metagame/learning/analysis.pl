%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%% analysis.pl
%%% Analysing game definitions to extract useful info.
/*
I really want to make some reachability tables:
1. piece-square:  for each square, the set of squares a piece token
can eventually reach starting from that square.
2. piece-piece: for each piece (and square?) the set of other piece
types which could be promoted into, given we start with a piece on
that square. 
3. piece-notpiece: for each piece, init square, and target square, the
set of piece types which could eventually be captured (or just removed?)
from that target square, given a piece type on the init square.
*/

/*
:- module(analysis, [
	build_distance_table/0,
	build_transition_table/0,
	build_mobility_table/0,
	print_distance_table/0,
	print_transition_table/0,
	print_mobility_table/0,
	square_piece_mobility/3,
	square_piece_transition/3,
	square_piece_distance/4,
	square_index/2,
	piece_index/2,
	board_square/2
		    ]).
	
*/



%==============================================================================
% Square-Piece-Square (Immediate Transition) Matrix
%==============================================================================
% This matrix is the only one which refers to the piece movements, 
% board state, etc.  The others are based only on this.
% Thus it should be performed before the others. 
%

build_transition_matrix :- 
	new_empty_state(State),
	build_transition_matrix(_Matrix,State).


build_transition_matrix(Matrix,State) :- 
	map_piece_table(transition_matrix,[State],Matrix).

print_transition_matrix :- 
	print_transition_matrix(_Sq,_Piece,_SqT).

print_transition_matrix(Sq,Piece,SqT) :- 
	format("Piece Square Transition Matrix:~n",[]),
	( square_piece_transition(Sq,Piece,SqT),
	  format("~p: ~p -> ~p~n",[Piece,Sq,SqT]),
%	  format("matrix[~p][~p] = ~p~n",[Sq,Piece,SqT]),
	  fail
        ; true
	).


spt(S,P,V) :- square_piece_transition(S,P,V).

% SQUARE_PIECE_TRANSITION(?Sq,?Piece,?SqT)
% Very nice table indicating when a piece could
% move directly from one square to another on an empty board.
square_piece_transition(Sq,Piece,SqT) :- 
	advice_tables(Tables),
	square_piece_transition(Sq,Piece,SqT,Tables).

% SQUARE_PIECE_TRANSITION(?Sq,?Piece,?SqT,+Tables)
% Very nice table indicating when a piece could
% move directly from one square to another on an empty board.
square_piece_transition(Sq,Piece,SqT,Tables) :- 
	square_piece_transition(Sq,_SqI,Piece,_PieceI,SqT,_SqTI,Tables).

square_piece_transition(Sq,SqI,Piece,PieceI,SqT,SqTI,Tables) :- 
	  piece_index(Piece,PieceI),
	  square_index(Sq,SqI),
	  square_piece_sq(SqI,PieceI,SqTI,Tables),
	  square_index(SqT,SqTI).

square_piece_sq(SqI,PieceI,SqTI,Tables) :- 
	transition_matrix(Tables,M),
	square_piece_sq1(SqI,PieceI,SqTI,M).

square_piece_sq1(SqI,PieceI,SqTI,M) :- 
	piece_table_entry(_,PieceI,M,Entry),
	member1_pair(SqI-Ts,Entry),
	member(SqTI,Ts).

square_piece_sqs(SqI,PieceI,TIs) :- 
	transition_matrix(M),
	square_piece_sqs(SqI,PieceI,TIs,M).

square_piece_sqs(SqI,PieceI,TIs,M) :- 
	piece_table_entry(_,PieceI,M,Entry),
	member1_pair(SqI-TIs,Entry).


% Reversed order of args to use map table. 
%transition_matrix(Piece,PieceIndex,Matrix,State) :- 
transition_matrix(Piece,PieceIndex,State,Matrix) :- 
%	new_empty_state(State), % for testing
	piece_index(Piece,PieceIndex),
	setof(SqIndex-SqTIndices, Sq^
	     safe_transitions_type(Piece,PieceIndex,Sq,SqIndex,moving,SqTIndices,State),
	     Matrix
	     ),
	tracing_anal_format(tables,"Built transition matrix for <~p>~n",[Piece]).

% The safe transition code is in exclude.pl

%==============================================================================
% Piece Square Mobility Table
%==============================================================================
% Requires transition table now.  
% Could probably put here a line to build it, but maybe wasteful!

build_mobility_matrix(Matrix) :- 
	transition_matrix(Trans),
	build_mobility_matrix(Trans,Matrix).

% Uses saved transition-matrix.
build_mobility_matrix(Trans,Matrix) :- 
	map_piece_table(mobility_matrix,[Trans],Matrix).

mobility_matrix(Piece,PieceIndex,Matrix) :- 
	transition_matrix(Trans),
	mobility_matrix(Piece,PieceIndex,Trans,Matrix).
	
mobility_matrix(Piece,PieceIndex,Trans,Matrix) :- 
	piece_index(Piece,PieceIndex),
	map_square_table(square_p_mob,[Piece,PieceIndex,Trans],Matrix),
	tracing_anal_format(tables,"Built mobility table for <~p>~n",[Piece]).

square_p_mob(Sq,SqIndex,Piece,PieceIndex,Trans,Value) :- 
	piece_square_mob(Piece,PieceIndex,Sq,SqIndex,Trans,Value).

square_piece_mob(SqI,PieceI,Val) :- 
	advice_tables(Tables),
	square_piece_mob(SqI,PieceI,Val,Tables).

square_piece_mob(SqI,PieceI,Val,Tables) :- 
	mobility_matrix(Tables,M),
	pindex_table_entry(PieceI,M,Entry),
	sindex_table_entry(SqI,Entry,Val).

spm(S,P,V) :- square_piece_mobility(S,P,V).

% SQUARE_PIECE_MOBILITY(?Sq,?Piece,?Value)
% Very nice table indicating the mobility a piece would
% have from a square on an empty board.
square_piece_mobility(Sq,Piece,Value) :- 
	advice_tables(Tables),
	square_piece_mobility(Sq,Piece,Value,Tables).

% SQUARE_PIECE_MOBILITY(?Sq,?Piece,?Value,+Tables)
% Very nice table indicating the mobility a piece would
% have from a square on an empty board.
square_piece_mobility(Sq,Piece,Value,Tables) :- 
	  piece_index(Piece,PieceI),
	  square_index(Sq,SqI),
	  square_piece_mob(SqI,PieceI,Value,Tables).


piece_square_mob(Piece,Sq,Value) :- 
	transition_matrix(TransMatrix),
	piece_square_mob(Piece,Sq,TransMatrix,Value).

piece_square_mob(Piece,Sq,TransMatrix,Value) :- 
	piece_square_mob(Piece,_PieceIndex,Sq,_SquareIndex,TransMatrix,Value).
	  
piece_square_mob(Piece,PieceIndex,Sq,SquareIndex,TransMatrix,Value) :- 
%	new_empty_state(State), % for testing
	piece_index(Piece,PieceIndex),
%	square_index(Sq,SquareIndex),
	trace_timing(anal(mob_count),
	   mob_count(PieceIndex,SquareIndex,TransMatrix,Value)),
	tracing_anal_format(detailed,"~p: ~p -> ~p~n",[Piece,Sq,Value]).

mob_count(Piece,Sq,Trans,Value) :- 
	square_piece_sqs(Sq,Piece,Moves,Trans),
	length(Moves,Value).

% Printing mobility matrix
%
print_mobility_matrix :- print_mobility_matrix(_,_).

print_mobility_matrix(Sq,Piece) :- 
	format("Square Piece Mobility Matrix:~n",[]),
	( square_piece_mobility(Sq,Piece,Value),
	  format("matrix[~p][~p] = ~p~n",[Sq,Piece,Value]),
	  fail
        ; true
	).


%==============================================================================
% Eventual Mobility Table
%==============================================================================
% Requires transition table now.  
% Could probably put here a line to build it, but maybe wasteful!

build_eventual_matrix(Matrix) :- 
	distance_matrix(Dist),
	build_eventual_matrix(Dist,Matrix).

% Uses saved transition-matrix.
build_eventual_matrix(Dist,Matrix) :- 
	map_piece_table(eventual_matrix,[Dist],Matrix).

eventual_matrix(Piece,PieceIndex,Matrix) :- 
	distance_matrix(Dist),
	eventual_matrix(Piece,PieceIndex,Dist,Matrix).
	
% Uses a fixed max DISTANCE. 
eventual_matrix(Piece,PieceIndex,DistMatrix,Matrix) :- 
	Distance = 4,
	piece_indist_matrix(Piece,PieceIndex,Distance,DistMatrix,Matrix),
	tracing_anal_format(tables,"Built eventual table for <~p>~n",[Piece]).


% SQUARE_PIECE_REACHABILITY(?Sq,?Piece,?Value)
% Very nice table indicating the reachability a piece would
% have from a square on an empty board.
square_piece_reachability(Sq,Piece,Value) :- 
	advice_tables(Tables),
	square_piece_reachability(Sq,Piece,Value,Tables).

% SQUARE_PIECE_REACHABILITY(?Sq,?Piece,?Value,+Tables)
% Very nice table indicating the reachability a piece would
% have from a square on an empty board.
square_piece_reachability(Sq,Piece,Value,Tables) :- 
	  piece_index(Piece,PieceI),
	  square_index(Sq,SqI),
	  square_piece_reach(SqI,PieceI,Value,Tables).


square_piece_reach(SqI,PieceI,Val,Tables) :- 
	eventual_matrix(Tables,M),
	pindex_table_entry(PieceI,M,Entry),
	sindex_table_entry(SqI,Entry,Val).


% Printing eventual matrix
%
print_eventual_matrix :- print_eventual_matrix(_,_).

print_eventual_matrix(Sq,Piece) :- 
	format("Square Piece Eventual Matrix:~n",[]),
	( square_piece_reachability(Sq,Piece,Value),
	  format("matrix[~p][~p] = ~p~n",[Sq,Piece,Value]),
	  fail
        ; true
	).

%==============================================================================
% Square-Piece-Square Distance Table
%==============================================================================

% Requires transition-matrix (at the moment).
build_distance_matrix(Trans,Matrix) :- 
	map_piece_table(distance_matrix,[Trans],Matrix).


build_distance_table :- 
	distance_matrix(M),
	build_distance_table(M,_).


% Requires distance-matrix 
build_distance_table(Trans,Matrix) :- 
	map_piece_table(distance_table,[Trans],Matrix).


distance_matrix(Piece,PieceIndex,Matrix) :- 
	transition_matrix(Trans),
	distance_matrix(Piece,PieceIndex,Trans,Matrix).

distance_matrix(Piece,PieceIndex,Trans,Matrix) :- 
	piece_table_entry(Piece,PieceIndex,Trans,Matrix1),
	s_floyd(Matrix1,Matrix),
	tracing_anal_format(tables,"Built distance matrix for <~p>~n",[Piece]).


distance_table(Piece,PieceIndex,Table) :- 
	distance_matrix(Matrix),
	distance_table(Piece,PieceIndex,Matrix,Table).

distance_table(Piece,PieceIndex,Dist,Table) :- 
	piece_table_entry(Piece,PieceIndex,Dist,Matrix),
	matrix_to_square_table(Matrix,Table),
	tracing_anal_format(tables,"Built distance table for <~p>~n",[Piece]).



square_piece_sq_dist(SqI,PieceI,SqTI,Dist,Tables) :- 
	distance_table(Tables,Table),
	square_piece_sq_dist1(SqI,PieceI,SqTI,Table,Dist).
	
% Cut seems to help speed here, don't know  why.
square_piece_sq_dist1(SqI,PieceI,SqTI,Table,Dist) :- 
	piece_table_entry(_,PieceI,Table,Entry),
	square_table_distance(SqI,SqTI,Entry,Dist), !.	

% Returns 10000 if no such dist is found. (used in arrive.pl)
square_piece_sq_dist_max(SqI,PieceI,SqTI,DTable,Dist) :- 
	( square_piece_sq_dist1(SqI,PieceI,SqTI,DTable,Dist)
	-> true
	; Dist = 10000
	).

square_piece_list_dist(SqI,PieceI,SqTI,Dist) :- 
	distance_matrix(Matrix),
	square_piece_list_dist(SqI,PieceI,SqTI,Matrix,Dist).
	
square_piece_list_dist(SqI,PieceI,SqTI,Table,Dist) :- 
	piece_table_entry(_,PieceI,Table,Entry),
	square_matrix_distance(SqI,SqTI,Entry,Dist).	

square_piece_list_distance(SqI,PieceI,SqTI,Dist) :- 
	distance_matrix(Matrix),
	square_piece_list_distance(SqI,PieceI,SqTI,Matrix,Dist).
	
square_piece_list_distance(SqI,PieceI,SqTI,Table,Dist) :- 
	piece_table_entry(_,PieceI,Table,Entry),
	square_matrix_distance(SqI,SqTI,Entry,Dist).	



print_distance_matrix :- 
	print_distance_matrix(_,_,_).


print_distance_matrix(Sq,Piece,SqT) :- 
	format("Square Piece Distance Matrix:~n",[]),
	( square_piece_list_distance(Sq,Piece,SqT,Dist),
	  format("~p: ~p -> ~p <~p>~n",[Piece,Sq,SqT,Dist]),
%	  format("matrix[~p][~p] = ~p~n",[Sq,Piece,SqT]),
	  fail
        ; true
	).


print_distance_table :- 
	print_distance_table(_,_,_).


print_distance_table(Sq,Piece,SqT) :- 
	format("Square Piece Distance Table:~n",[]),
	( square_piece_distance(Sq,Piece,SqT,Dist),
	  format("~p: ~p -> ~p <~p>~n",[Piece,Sq,SqT,Dist]),
%	  format("table[~p][~p] = ~p~n",[Sq,Piece,SqT]),
	  fail
        ; true
	).


piece_distance_table(Piece,Table) :- 
	distance_table(D),
	piece_table_entry(Piece,_,D,Table).


spd(S,P,SqT,V) :- square_piece_distance(S,P,SqT,V).

% SQUARE_PIECE_DISTANCE(?Sq,?Piece,?SqT,?Dist)
% Very nice table indicating the minimum distance a piece would
% take to move from one square to another on an empty board.
% The extra arg version returns indices also. 
square_piece_distance(Sq,Piece,SqT,Dist) :- 
	advice_tables(Tables),
	square_piece_distance(Sq,Piece,SqT,Dist,Tables).

% SQUARE_PIECE_DISTANCE(?Sq,?Piece,?SqT,?Dist,+Tables)
% Very nice table indicating the minimum distance a piece would
% take to move from one square to another on an empty board.
% The extra arg version returns indices also. 
square_piece_distance(Sq,Piece,SqT,Dist,Tables) :- 
	square_piece_distance(Sq,_SqI,Piece,_PieceI,SqT,_SqTI,Dist,Tables).

square_piece_distance(Sq,SqI,Piece,PieceI,SqT,SqTI,Dist,Tables) :- 
	  piece_index(Piece,PieceI),
	  square_index(Sq,SqI),
	  square_index(SqT,SqTI),
	  square_piece_sq_dist(SqI,PieceI,SqTI,Dist,Tables).



%==============================================================================
% Reachability using Transition Matrix
%==============================================================================

square_piece_reaches(Sq,Piece,Squares) :- 
	transition_matrix(Trans),
	piece_table_entry(Piece,_,Trans,PTrans),
	square_piece_reaches(Sq,_SqIndex,PTrans,Squares).

square_piece_reaches(Sq,SqIndex,Trans,Squares) :- 
	square_index(Sq,SqIndex),
	sq_piece_reaches(SqIndex,Trans,Squares).

sq_piece_reaches(Sq,Trans,Squares) :- 
	reachable(Sq,Trans,Squares).


indist_set(Piece,Sq,SqT,Dist,Set) :- 
%	piece_index(Piece,PI),
%	square_index(Sq,SqI),
%	square_index(SqT,SqTI),
	setof(SqT, 
	  indist(Piece,Sq,SqT,Dist),
	  Set).

indist_set2(Piece,Sq,SqT,Dist,Set) :- 
	setof(SqT, 
	  indist(Piece,Sq,SqT,Dist),
	  Set).


indist(Piece,Sq,SqT,Dist) :- 
	distance_table(Table),
	piece_square_within_distance(Piece,_,Sq,_SqI,SqT,_SqTI,Dist,Table).
	

piece_square_at_distance(Piece,PieceI,Sq,SqI,SqT,SqTI,Dist,Table) :- 
	piece_table_entry(Piece,PieceI,Table,T),
	square_index(Sq,SqI),
	square_index(SqT,SqTI),
	square_table_distance(SqI,SqTI,T,D),
	D =< Dist.
	
% Returns a list of the squarse
piece_dist_squares_matrix(P,PI,Sq,SqI,Squares) :- 
	distance_matrix(D),
	piece_dist_squares_matrix(P,PI,Sq,SqI,D,Squares).


piece_dist_squares_matrix(P,PI,_Sq,SqI,DistMatrix,Squares) :- 
	piece_table_entry(P,PI,DistMatrix,Matrix),
	member1_pair(SqI-Dist,Matrix),
	p_transpose(Dist,New),
	p_to_s_graph(New,Squares).

piece_dist_squares(P,PI,Sq,SqI,Distance,Squares) :- 
	distance_matrix(DistMatrix),
	piece_dist_squares(P,PI,Sq,SqI,Distance,DistMatrix,Squares).

piece_dist_squares(P,PI,Sq,SqI,Distance,DistMatrix,Squares) :- 
	piece_dist_squares_matrix(P,PI,Sq,SqI,DistMatrix,Matrix),
	member1_pair(Distance-Squares,Matrix).

piece_dist_count(P,Sq,Distance,Count) :- 
	piece_index(P,PI),
	square_index(Sq,SqI),
	piece_dist_count(P,PI,Sq,SqI,Distance,Count).

piece_dist_count(P,PI,Sq,SqI,Distance,Count) :- 
	distance_matrix(DistMatrix),
	piece_dist_count(P,PI,Sq,SqI,Distance,DistMatrix,Count).

piece_dist_count(P,PI,Sq,SqI,Distance,DistMatrix,Count) :- 
	piece_dist_squares(P,PI,Sq,SqI,Distance,DistMatrix,Squares),
	length(Squares,Count).


piece_indist_sum(Piece,PieceI,Sq,SqI,Distance,Count) :- 
	distance_matrix(DistMatrix),
	piece_indist_sum(Piece,PieceI,Sq,SqI,Distance,DistMatrix,Count).

piece_indist_sum(Piece,PieceI,Sq,SqI,Distance,DistMatrix,Count) :- 
%	piece_index(Piece,PI),
%	square_index(Sq,SqI),
	bagof(Count1, 
	  piece_indist_count(Piece,PieceI,Sq,SqI,Distance,DistMatrix,Count1),
	  Counts),
	square_index(Sq,SqI),
	sumlist(Counts,Count).
	    
piece_indist_count(Piece,PieceI,Sq,SqI,Distance,Count) :- 
	distance_matrix(DistMatrix),
	piece_indist_count(Piece,PieceI,Sq,SqI,Distance,DistMatrix,Count).

piece_indist_count(Piece,PieceI,Sq,SqI,Distance,DistMatrix,Count) :- 
	piece_dist_count(Piece,PieceI,Sq,SqI,Dist1,DistMatrix,Count),
	Dist1 =< Distance.


piece_indist_crunchsum(P,PI,Sq,SqI,Distance,Count) :- 
	distance_matrix(DistMatrix),
	piece_indist_crunchsum(P,PI,Sq,SqI,Distance,DistMatrix,Count).

piece_indist_crunchsum(P,PI,Sq,SqI,Distance,DistMatrix,Count) :- 
	piece_dist_squares_matrix(P,PI,Sq,SqI,DistMatrix,Squares),
	crunch_ds(Squares,S),
	count_less(S,Distance,Count).



piece_discounted_sum(P,PI,Sq,SqI,Distance,Count) :- 
	distance_matrix(DistMatrix),
	piece_discounted_sum(P,PI,Sq,SqI,Distance,DistMatrix,Count).

piece_discounted_sum(P,PI,Sq,SqI,Distance,DistMatrix,Count) :- 
	piece_dist_squares_matrix(P,PI,Sq,SqI,DistMatrix,Squares),
	crunch_ds(Squares,Series),
	discounted_sum(Series,Distance,Count).



piece_indist_matrix(P,PI,Distance,Matrix) :- 
	distance_matrix(DistMatrix),
	piece_indist_matrix(P,PI,Distance,DistMatrix,Matrix).

piece_indist_matrix(P,PI,Distance,DistMatrix,Matrix) :- 
	piece_index(P,PI),
	map_square_table(sq_piece_indist_crunchsum,
	                 [P,PI,Distance,DistMatrix],Matrix),
        true.			  
			  

% Discounted Sum: The value of a square decays exponentially with
% distance.
% Crunchsum:  We add all the squares within a given distance, 
% weighed equally independent of their distance.
% These do not give the same ordering:  A square with a better crunchsum 
% may have many moves far away.  The discounted sum will prefer closer
% moves, and put pieces on squares with (foremost) the highest immediate
% mobility.  
sq_piece_indist_crunchsum(_Sq,SqI,P,PI,Distance,DistMatrix,Count) :- 
%	  piece_indist_crunchsum(P,PI,_,SqI,Distance,DistMatrix,Count).
	  piece_discounted_sum(P,PI,_,SqI,Distance,DistMatrix,Count).




crunch_invert(SqDs,DSqs) :-
	p_transpose(SqDs,New),
	p_to_s_graph(New,DSqs).


crunchtop([],[]).
crunchtop([Sq-Sqs|G],[Sq-Ds|GRest]) :- 
	crunch_invert(Sqs,DSqs),
	crunch_ds(DSqs,Ds),
	crunchtop(G,GRest).

% crunch_ds([1-[a,b,c],2-[d,e,f],4-[g]],C).
% C = [1-3,2-3,4-1] 
%
crunch_ds([],[]).
crunch_ds([D-Sqs|G],[D-Count|Rest]) :- 
	length(Sqs,Count),
	crunch_ds(G,Rest).

count_less([],_,0).
count_less([Dist-_|_],Max,0) :- 
	Max < Dist, !.
count_less([_Dist-C1|As],Max,Count) :- 
%	Max >= Dist, !,
	count_less(As,Max,CRest),
	Count is C1+CRest.

% discounted_sum(Series,Discount,Sum).
discounted_sum(Series,Discount,Sum) :- 
	discounted_sum(Series,Discount,0,Sum).

discounted_sum([],_,Count,Count).
discounted_sum([Dist-C1|Rest],Discount,Sum1,Sum) :- 
	discount_value(Dist,C1,Discount,Val),
	Sum2 is Sum1 + Val,
	discounted_sum(Rest,Discount,Sum2,Sum).

discount_value(Distance,Count,_Discount,Val) :- 
	distance_value(Distance,V),
	Val is Count*V.


%==============================================================================
% Reverse matrices 
%==============================================================================

% Reverse matrices:  Tell for each square, how many moves
% away the other squares are from moving to it.   
% Interesting fact:  If the piece movements this is based upon
% are symmetric, this is the same as the forward matrix!
rev_transition_matrix(Piece,PieceIndex,Matrix,State) :- 
	transition_matrix(Piece,PieceIndex,Matrix1,State),
	s_transpose(Matrix1,Matrix).

rev_distance_matrix(Piece,PieceIndex,Matrix,State) :- 
	rev_transition_matrix(Piece,PieceIndex,Matrix1,State),
	s_floyd(Matrix1,Matrix).

rev_distance_table(Piece,PieceIndex,Table,State) :- 
	rev_distance_matrix(Piece,PieceIndex,Matrix,State),
	d_to_array(Matrix,Table).
	
	

%================================================================================
% TRACING execution of analysis routines
%================================================================================

% This main tracing module is called:  anal.
% The following tracing modules are used in this file:
%	index:  info on piece indexing
% 
% Each module can be set on/off, using set_anal_verbosity (see below), or 
% using trace_anal_<module>. 
%
% All can be turned off with silent_anal.

:- my_ensure_loaded(library(tracing)).

tracing_anal(Type,Call) :- 
	( tracing(anal(Type)) -> call(Call) ; true ).

% Might cause trouble later when want to use streams also.
tracing_anal_format(Type,String,Args) :- 
	( tracing(anal(Type))
	-> format(String,Args)
	; true 
	).

tracing_anal_timing(Type,Call) :- 
	trace_timing(anal(Type),Call).

set_anal_verbosity(Level,Status) :- set_tracing(anal(Level),Status).

silent_anal :- all_anal(off).
loud_anal :- all_anal(on).

all_anal(Status) :- 
	set_anal_verbosity(index,Status), 
	set_anal_verbosity(simplify,Status), 
	set_anal_verbosity(subsume,Status), 
	set_anal_verbosity(pieces,Status). 

trace_anal_tables :- set_anal_verbosity(tables,on). 
trace_anal_index :- set_anal_verbosity(index,on). 
trace_anal_subsume :- set_anal_verbosity(subsume,on). 
trace_anal_simplify :- set_anal_verbosity(simplify,on). 
trace_anal_pieces :- set_anal_verbosity(pieces,on). 

:- trace_anal_tables.
%:- silent_anal.




