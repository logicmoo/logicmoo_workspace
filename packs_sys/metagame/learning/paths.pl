%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%% paths.pl


path_dist(Piece,Player,SqF,SqT,0,0) :- !.
path_dist(Piece,Player,SqF,SqT,Max,N) :- 
	reaches(Piece,Player,SqF,SqT,In),
	
path_dist(_Piece,_Player,_SqF,_SqF,0,_S).
path_dist(Piece,Player,SqF,SqT,N,S) :- 
	reaches(Piece,Player,SqF,Sq1,S),
	path_dist(Piece,Player,Sq1,SqT,N1,S),
	N is N1+1.


%% PATH_DIST(Type,Player,SqF,SqT,Dist,Max,S0)
%% True if, in state <S0>, <Player>'s piece of <Type> could reach SqT from SqF on
%% current board in <Dist> moves, each of which uses either a moving or 
%% capturing power, where <Dist> <= <Max>.
%% <Type> either a piece_type or piece_struct [king or
%% piece(king,player)].

path_dist(Type,Player,SqF,SqT,Dist,Max,S0) :- 
	( piece_struct(Type,_T,Player) 
	-> Piece=Type
	;  piece_struct(Piece,Type,Player)
	),
	put_control(Player,S0,S),
	path_dist0(Piece,Player,SqF,SqT,Dist,Max,S),
	tracing_path_format(squares,
	    "<~p>: ~p -> ~p in ~p moves~n",[Piece,SqF,SqT,Dist]).
	
path_dist0(Piece,Player,SqF,SqT,N,Max,S) :- 
	path_dist1(Piece,Player,SqF,SqT,N,Max,S),
	N > 0.

path_dist1(_Piece,_Player,SqF,SqF,0,_Left,_S).
path_dist1(Piece,Player,SqF,SqT,N,Max,S) :- 
	Max > 0, 
	SqF \== SqT, 
	reaches(Piece,Player,SqF,Sq1,S),
	Max1 is Max-1,
	path_dist1(Piece,Player,Sq1,SqT,N1,Max1,S),
	N is N1+1.


new_empty_state(S) :- 
	new_state(S1),
	make_empty_board(S1,S).

new_state_of_type(empty,S) :- new_empty_state(S).
new_state_of_type(any,S) :- new_state(S).

piece_moves_empty(Piece,Sq,SqT) :- 
	new_empty_state(State),
	piece_moves(Piece,Sq,SqT,State).

piece_moves_any(Piece,Sq,SqT) :- 
	new_state(State),
	piece_moves(Piece,Sq,SqT,State).

piece_moves(Piece,Sq,SqT,State) :- 
     board_square(Sq),
     piece_index(Piece,_),
     moves(Piece,_Player,Sq,SqT,State),
     tracing_path_format(moves,"<~p>: ~p -> ~p~n",[Piece,Sq,SqT]).

piece_move(Piece,Sq,SqT,MoveType,StateType) :- 
	new_state_of_type(StateType,S),
	piece_move_for_type(MoveType,Piece,Sq,SqT,S).

% Hacked the put_control to get the right definitions.
% This should be fixed in legal.
piece_move_for_type(MoveType,Piece,Sq,SqT,S) :- 
     board_square(Sq),
     piece_index(Piece,_),
     owns(Piece,Player),
     put_control(Player,S,S1),
     move_for_type(MoveType,Piece,Sq,SqT,S1).
	
move_for_type(moving,Piece,Sq,SqT,State) :- 
     moves(Piece,_Player,Sq,SqT,State),
     tracing_path_format(moves,"<~p>: ~p -> ~p~n",[Piece,Sq,SqT]).
move_for_type(capturing,Piece,Sq,SqT,State) :- 
     captures(Piece,_Player,Sq,SqT,State),
     tracing_path_format(moves,"<~p>: ~p -> ~p~n",[Piece,Sq,SqT]).
move_for_type(capturing_specific,Piece,Sq,SqT,State) :- 
     captures(Piece,_Player,Sq,SqT,Effect,Captured,State),
     tracing_path_format(moves,
             "<~p>: ~p -> ~p x ~p (~p)~n",
	     [Piece,Sq,SqT,Captured,Effect]).
	


%================================================================================
% tracing execution of path-distance routines
%================================================================================

% The following tracing modules are used in this file:
%	squares:  info on squares evaluation
%	mobility:  info on piece mobility
% Each module can be set on/off, using set_path_verbosity (see below), or 
% using trace_path_<module>. 
%
% All can be turned off with silent_path.

:- my_ensure_loaded(library(tracing)).

tracing_path(Type,Call) :- 
	( tracing(path(Type)) -> call(Call) ; true ).

% Might cause trouble later when want to use streams also.
tracing_path_format(Type,String,Args) :- 
	( tracing(path(Type))
	-> format(String,Args)
	; true 
	).

tracing_path_timing(Type,Call) :- 
	trace_timing(path(Type),Call).

set_path_verbosity(Level,Status) :- set_tracing(path(Level),Status).

silent_path :- 
	set_path_verbosity(ordering,off), 
	set_path_verbosity(value,off), 
	set_path_verbosity(resources,off), 
	set_path_verbosity(timing,off), 
	set_path_verbosity(iteration,off).

trace_path_squares :- set_path_verbosity(squares,on). 
trace_path_ordering :- set_path_verbosity(ordering,on). 

:- silent_path.
%:- trace_path_squares.


/*=========================================================================

In^Out^(
                    on(King,square(5,1),In),
               moves(King,player,square(2,4),SqT,In),
     print(SqT),
     fail
     ).

In^Out^(
                    on(King,square(5,1),In),
               moves(King,player,square(2,8),SqT,In),
     print(SqT),
     fail
     ).

In^Out^(
                    on(King,square(5,1),In),
               captures(King,player,square(2,8),SqT,In),
     print(SqT),
     fail
     ).

In^Out^(
                    on(King,square(5,1),In),
               reaches(King,player,square(2,8),SqT,In),
     print(SqT),
     fail
     ).


In^Out^(
                    on(King,square(5,1),In),
               reaches(King,player,square(2,7),SqT,In),
     print(SqT),
     fail
     ).


In^Out^(
                    on(King,square(5,1),In),
               reaches(King,player,square(2,7),Sq1,In),
               reaches(King,player,Sq1,SqT,In),
     print(SqT),
     fail
     ).

In^Out^(
                    on(King,square(5,1),In),
               reaches(King,player,square(2,7),Sq1,In),
               reaches(King,player,Sq1,Sq2,In),
               reaches(King,player,Sq1,Sq3,In),
	       on(Piece,square(4,8),In),
	       on(Piece,Sq3,In),
     print(Sq3),
     fail
     ).


In^Out^(
                    on(Piece,player,Sq,In),
               reaches(Piece,player,Sq,Sq1,In),
               reaches(Piece,player,Sq1,Sq2,In),
               reaches(Piece,player,Sq2,Sq3,In),
	       on(PieceK,Sq3,In),
	       piece_struct_name(PieceK,king),
     print(Sq3), write(:), print(Piece), print(from), print(Sq), nl,
     fail
     ).

In^Out^(
                    on(Piece,player,Sq,In),
               reaches(Piece,player,Sq,SqT,In),
	format("<~p>: ~p -> ~p in ~p moves~n",[Piece,Sq,SqT,1]),
     fail
     ).

In^Out^(
                    on(Piece,player,Sq,In),
               moves(Piece,player,Sq,SqT,In),
	format("<~p>: ~p -> ~p in ~p moves~n",[Piece,Sq,SqT,1]),
     fail
     ).


In^Out^(
                    on(Piece,player,Sq,In),
               captures(Piece,player,Sq,SqT,In),
	format("<~p>: ~p -> ~p in ~p moves~n",[Piece,Sq,SqT,1]),
     fail
     ).




In^Out^(
	path_dist(queen,player,square(4,1),SqT,Dist,2,In),
	fail
       ).

In^Out^(
	on(piece(king,opponent),SqT,In),
	on(Piece,player,Square,In), 
	path_dist(Piece,player,Square,SqT,Dist,3,In)
       ).


In^Out^(
                    on(Piece,player,Sq,In),
               reaches(Piece,player,Sq,Sq1,In),
               reaches(Piece,player,Sq1,Sq2,In),
               reaches(Piece,player,Sq2,Sq3,In),
	       on(PieceK,Sq3,In),
	       piece_struct_name(PieceK,king),
     print(Sq3), write(:), print(Piece), print(from), print(Sq), nl,
     fail
     ).

checkpoint(e3,In),
        on(piece(king,opponent),SqT,In),
        on(Piece,player,Square,In),
        path_dist(Piece,player,Square,SqT,Dist,3,In).


==================================================*/
