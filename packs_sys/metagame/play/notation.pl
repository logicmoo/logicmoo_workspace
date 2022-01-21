%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

% Notation.pl
%
% This defines the MOVE GRAMMAR for symmetric 
% chess-like games.  
%
% Note that the notation for a move, like that for 
% a game, ends in a period.

:- my_ensure_loaded(library(tokenizer)).
:- my_ensure_loaded(library(grammar)).

% Ensures parsing mode if really parsing, as otherwise get strange bugs.
move_notation(ComplexMove,Notation) :-
	( var(ComplexMove) -> set_parsing_mode ; true ),
	move_notation(ComplexMove,Notation,[]).


% MOVE_NOTATION
move_notation(M) --> 
	prelims(M,Pre),
	main(Pre,[]),
	period.


% PRELIMS - covers ending assignments and init promotes.
% Can also be bypassed (thus nothing) if last move was a placement.
prelims([P|Out],Out) --> 
	init_promote(P).
prelims([end_assign|Rest],Rest) --> [].
prelims(Rest,Rest) --> [].


% INIT_PROMOTE
%init_promote(opponent_promote(square(5,1),piece(piece2,player),piece(piece3,player)),S,[]).
%   S = [promote,'(',5,',',1,')',white,piece3] ? 
init_promote(opponent_promote([],[],[]))  --> [].
init_promote(opponent_promote(Sq,_OldPiece,NewPiece))  --> 
	[promote], gsquare(Sq), piece(NewPiece), [';'], line.


% CONSIDER_PROMOTE
consider_promote([T|Sel],Rest) -->
	attempt_promote(T),
	select_promote(Sel,Rest).

select_promote([promote_select(Square,OldPiece,OldPiece)|Rest],Rest) --> [].
select_promote([promote_select(Square,_OldPiece,NewPiece)|Rest],Rest) --> 
	[';'], line, [promote], gsquare(Square), piece(NewPiece).
select_promote(Rest,Rest) --> [].



% ATTEMPT_PROMOTE
% attempt_promote(try_promote(square(5,1),piece(piece2,player),piece(piece3,player)))
%    -->  [promote,'(',5,',',1,')',white,piece3,';']
% If it couldn't promote at all, don't mention it.
% If it promotes to same piece, mention is optional. 
% Thus if promotion isn't mentioned but it should promote, assume it chooses
% same piece. 
attempt_promote(try_promote(_Square,_OldPiece,[])) --> [].
attempt_promote(try_promote(Square,OldPiece,OldPiece)) --> [].
%attempt_promote(try_promote(_Square,_OldPiece,NewPiece)) --> {var(NewPiece)}, [].
attempt_promote(try_promote(Square,_OldPiece,NewPiece)) --> 
	[';'], [promote], gsquare(Square), piece(NewPiece).


% MAIN
% Repeated Transfers, followed by possible player promotion.
main([P|Out],Out) -->
	 placing(P).
main(In,Out) -->
	 transfers(In,T),
	 consider_promote(T,Out).


% TRANSFERS
transfers(In,Out) --> transfer(In,T), continued_transfers(T,Out).

% CONTINUED_TRANSFERS
% Either no more transfers, or ';' and more transfers.
% Could tighten this rule: can't continue unless did a movement.
continued_transfers(In,In) --> [].
continued_transfers([end_continues|Rest],Rest) --> [].
continued_transfers(In,Out) --> [';'], line, transfers(In,Out).


% TRANSFER : [move(piece(piece6,player),player,square(5,1),square(4,1)),
%              remove(piece(piece6,player),square(4,1))]
% --> [white,piece6,'(',5,',',1,')',->,'(',4,',',1,')',x,'(',4,',',1,')']
%
transfer([Move|Capture],Rest) --> moving(Move), capture(Capture,Rest).


placing(M) --> {M=place(Piece,Player,Sq)},
	piece(Piece), paren_color_player(Player), ['->'], gsquare(Sq).
placing(M) --> {M=assign(Piece,Player,Sq)},
	piece(Piece), paren_color_player(Player), ['->'], gsquare(Sq).

% MOVING
% moving(move(piece(piece1,opponent),opponent,square(1,6),square(2,4)),S,[]).
%    S = [black,piece1,'(',1,',',6,')',->,'(',2,',',4,')']
moving(move(Piece,_Player,From,To)) --> 
	piece(Piece), gsquare(From), ['->'], gsquare(To).

capture(In,Out) --> null_capture(In,Out).
capture(In,Out) --> real_capture(In,Out).

null_capture(X,X) --> [].

real_capture([C|Cs],Rest) --> [x], simp_capture(C),  capture(Cs,Rest).

simp_capture(M) --> remove(M).
simp_capture(M) --> possess(M).

% REMOVE
remove(capture(remove,Caps)) --> capture_effects_list(Caps).

% POSSESS
possess(capture(possess(Player),Caps)) --> capture_effects_list(Caps),
	['/'], paren_color_player(Player).

capture_effects_list([]) --> [].
capture_effects_list([C|Caps]) --> capture_effect(C), capture_effects_list(Caps).

% Capture effects are of the form:  piece square, like: white king (d,2)
%capture_effect(C) --> {captured(C,_Piece,Square)}, gsquare(Square).
capture_effect(C) --> {captured(C,Piece,Square)}, piece(Piece), gsquare(Square).



% A PIECE has both an owner and a name, like [white,bishop].
% A PIECE_NAME has just the name (as defined in grammar.pl
%
% piece(piece(bishop,player)) --> [white,bishop]
% Interesting idea: Allow variables here, or omitted components,
% for pattern-matching move notation.
piece(P) --> {piece_struct_name(P,Name), piece_struct_owner(P,O)},
	color_player(O), piece_name(Name).


% PAREN_COLOR_PLAYER
% Maps a player into the corresponding color, wrapped in parens.
paren_color_player(P) --> ['('], color_player(P), [')'].

color_player(player) --> [white].
color_player(opponent) --> [black].

%color_player(player,white).
%color_player(opponent,black).

color_player(white,player).
color_player(black,opponent).

player_color(player,white).
player_color(opponent,black).


%================================================================================
% Reading moves from files and strings, printing back.
%================================================================================

%%% Read from pascal-like MOVE NOTATION, into list.
%%% Reading is CASE-INSENSITIVE:  all alpha characters
%%% are converted to lower case when read in.   
%%% Also ignores extra blanks, tabs, and linefeeds.
%%% Comments occur from some point in a line started by %,
%%% and will be ignored to end of line.
%%% Can read games without spaces between operators and atoms,
%%% so squares can be written (X,Y) instead of ( X , Y ).  
%%%
%Notated Move played: [white,piece6,(,5,,,1,),->,(,4,,,1,),x,(,4,,,1,),;,
%	white,piece6,(,4,,,1,),->,(,3,,,1,),x,(,3,,,1,),;,
%	white,piece6,(,3,,,1,),->,(,2,,,1,),x,(,2,,,1,),;,
%	white,piece6,(,2,,,1,),->,(,1,,,1,),x,(,1,,,1,),'.']
% Stored in /generator/test_read.move


% print_read_move(File) 
% Note print
print_read_move_from_file(File) :- 
	format("~nReading move from file~n",[]),
	read_move_from_file_to_list(File,Move),
	format("~nRead move from file~n",[]),
	set_parsing_mode,
	format("~nParsing move in parse mode~n",[]),
	move_notation(ComplexMove,Move,[]),
	set_printing_mode,
	format("~nParsing move in print mode~n",[]),
	move_notation(ComplexMove,Move1,[]),
	format("~nPrinting move~n",[]),
	print_tokens(Move1).

read_move_from_file_to_list(File,Move) :-
	read_tokens_from_file(File,Move).

	
% MOVE_NOTATION_STRING(?Move,?String) 
% Converts between an internal move notation and
% a string of (ascii) characters.
move_notation_string(Move,String) :-
	var(String), !,
	move_notation(Move,Tokens),
	print_tokens_to_string(Tokens,String).
move_notation_string(Move,String) :-
	var(Move), 
	read_tokens_from_string(String,Tokens),
	move_notation(Move,Tokens).

	
