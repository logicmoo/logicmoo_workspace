%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

% matches.pl

% MATCHES(?Descr,?Piece)
% A Piece matches a Descr if both the player and type in 
% the description are at least as general as those of the 
% piece.
%
% piece_struct(P,guppy,player),piece_description(T,any_player,[guppy,frog]),
%	 matches(T,P).
matches(Descr,Piece) :-
	piece_description(Descr,Player_Gen,Piece_Gen),
	piece_struct(Piece,Name,Player),
	matches_player(Player_Gen,Player),
	matches_name(Piece_Gen,Name).


matches_player(any_player,Player) :- player_role(Player).
matches_player(player,player).
matches_player(opponent,opponent).

% Note a type must be either any_piece or a LIST, not singletons.
% Really should peval to get all piece names, instead of ref. back
% to game each time.
% Note also second clause should really check piece is a current
% game piece (peval also won't work without this!).
matches_name(any_piece,Name) :- 
	current_game_piece_name(Name).
matches_name([H|T],Name) :- 
	member(Name,[H|T]).	

