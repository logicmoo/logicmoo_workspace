%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

% print_boards.pl


%================================================================================
% Printing Boards
%================================================================================

print_state :-
	print_board,
	move_count(Count),
	control(P),
	nl,
	write_pieces_in_hand(player),
	write_pieces_in_hand(opponent),
	format("~nMove Number: ~d~nControl: ~p~n",[Count,P]),
	print_stage,
	print_opponent_promotes,
%	print_captured,
	print_effect,
	print_movement.

print_stage :- 
	( stage(S) ->
	  verbosely_format("Stage: ~p~n",[S])
	; true
	).

% This message isn't correct, depends on which player to move.
% Can't tell just from control, as sometimes see this when still
% player's move, otherwise when opponents.
print_opponent_promotes :-
	opponent_promotes(OldPiece,Sq)
          -> ( control(O),
	       format("~p must promote: ~p on ~p~n",[O,OldPiece,Sq]) ) 
           ; true.

print_movement :-
	moved_onto(Piece,Square) 
          -> format("~p moved onto ~p~n",[Piece,Square]) 
           ; true.

/*
print_effect :-
	effect(E) -> format("Effect: ~p~n",[E]) ; true.
*/

print_effect :- 
	( effects(Effect,Captured) 
	-> real_capture([captured(Effect,Captured)],[],String,[]),
	   print_tokens(String)
	; true
	).
	  

print_captured :-
	setof(C@Sq,captured(C,Sq),Caps) 
           -> format("Captured: ~p~n",[Caps]) 
           ; true.


pieces_in_hand(Player,Pieces) :-
	bagof(Piece,in_hand(Piece,Player),Pieces).

write_pieces_in_hand(Player) :-
	pieces_in_hand(Player,Pieces) 
	-> format("Pieces in ~p's hand: ~p~n",[Player,Pieces])
        ; true.

print_board :- 
	current_board_size(X,Y),
	nl,
	print_squares_in_rows(1,Y,X).


print_hline(0) :- format("+~n",[]), !.
print_hline(N) :- format("+---",[]), N1 is N-1, print_hline(N1).

print_column_labels(Size) :- 
	print_column_labels(1,Size).

print_column_labels(X,Max) :- X > Max, !, nl.
print_column_labels(C,Max) :- 
	write('  '),
	print_column_label(C),
	write(' '),
	C1 is C+1,
	print_column_labels(C1,Max).

print_column_label(C) :-
	nth_letter(C,Letter),
	write(Letter).
	



% print_squares_in_rows(MinRow,MaxRow,Size)
% Prints Size squares in each of rows [MinRow .. MaxRow].

print_squares_in_rows(Min,Max,Size) :-
	Min1 is Min - 1,
	print_hline(Size),
	print_squares_in_rows(Max,Min1,Max,Size).

print_squares_in_rows(Min,Min,_Max,Size) :- !,
	print_column_labels(Size).
print_squares_in_rows(_,_,_,0) :- !.
print_squares_in_rows(Row,Min,Max,Size) :- 
	print_squares_in_row(Row,Size),
	nl,
	print_hline(Size),
	Row1 is Row - 1,
	print_squares_in_rows(Row1,Min,Max,Size).

print_squares_in_row(Row,Size) :-
	print_squares_in_row(Row,1,Size).



print_end_row(Row) :-
	format("| ~p",[Row]).

print_squares_in_row(Row,Current,Max) :- Current > Max, !,
	print_end_row(Row).
print_squares_in_row(Row,Cur,Max) :-
	Cur1 is Cur + 1,
	print_square(Cur,Row),  % Column first, then row.
	print_squares_in_row(Row,Cur1,Max).



print_square(X,Y) :-
	write('|'),
	square(Square,X,Y),  % Column first, then row.
	print_piece_on_square(Square).


%%% question_marker is used only for special printing of
%%% variablized boards.
print_piece_on_square(Square) :-
	on(Piece,Square), 
	ground(Piece), !,
	print_piece_or_empty(Piece,Square).
print_piece_on_square(Square) :-
	question_marker(Square,Mark),
	write(Mark).



print_piece_or_empty(Piece,Square) :-
	piece_struct_name(Piece,Name), !,
	piece_struct_owner(Piece,Player),
	print_player_piece(Player,Name).
print_piece_or_empty(_Piece,Square) :-
%	empty(Square), 
	print_empty_square(Square).

print_empty_square(Square) :- 
	square(Square,X,Y),
	parity(X,Y,Par),
	parity_marker(Par,Mark),
	write(Mark).


parity(X,Y,Total) :-
	Total is (X + Y) mod 2.

%parity_marker(0,'@').
%parity_marker(1,'-').

parity_marker(0,'...').
parity_marker(1,'   ').

question_marker(_,' ? ').


print_colored(player,Name) :-
	format(" ~p ",[Name]).
print_colored(opponent,Name) :-
	format("*~p*",[Name]).


print_player_piece(Player,Piece) :- 
	player_piece_print_name(Player,Piece,Name),
	print_colored(Player,Name).

player_piece_print_name(Player,P,Name) :- 
	piece_print_name(P,PName),
	name_for_player(Player,PName,Name).

% If pieces are named pieceN, returns the value N.
% Otherwise, name must start with an alpha characters,
% and this first character will become the print name.
% (Thus, no different pieces should have same first character).
% Ex. piece1 prints as: a
%     bishop prints as: b


piece_print_name(P,PieceNum) :-
	bi_concat(piece,PieceNum,P),
	number_chars(PieceNum,NumChars), !.
piece_print_name(P,PieceNum) :-
	name(P,[InitChar|_Rest]),
	name(Letter,[InitChar]),
	nth_letter(PieceNum,Letter).
	
% NewName is the Nth alpha char, where PieceNum is N, an integer. 
% Used to be: 
% Lower case for PLAYER, upper case for OPPONENT. 
% Ex:  piece(piece1,player) prints as 'a',
%      piece(piece1,opponent) prints as 'A'.
% Now prints both same, but prints player Piece as ' P ',
% opponent as '*P*'
name_for_player(Player,PieceNum,NewName) :-
	player_offset(Player,Off),
	OrdNum is Off + PieceNum -1,
	name(NewName,[OrdNum]).
	


player_offset(player,O) :-
	name('A',[O]).
player_offset(opponent,O) :-
	name('A',[O]).

%================================================================================



