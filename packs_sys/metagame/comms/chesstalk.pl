%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%% pattern.pl

:- ensure_loaded(library(pipes)).

create :-
	global(chessprog,Prog),
	create(Prog).


create(Player) :-
	player_command(Player,Command),
	talk_interface(Command).
	
player_command(gnu,Command) :-
	gnu_command(Command).
player_command(morph,Command) :-
	morph_command(Command).

gnu_command(Command) :- 
    command_from_args([nice,-10,gnuchess],Command).

morph_command(Command) :- 
%    command_from_args(['( cd ~/Morph/ ; nice -5 morph -wh -f morphstart )'],Command).
    morph_setup_file(File),
    command_from_args(['( cd ~/Morph/ ; nice -5 morph -q -wh -f',File,' )'],Command).

tellm(S) :- tell_chess(S).
seem :- see_chess.

tell_chess(Statement) :- 
    tell_outstream(Statement).

see_chess :-
	global(instream,I),
	set_input(I).

morph_setup_file(File) :- 
    global(handicap,File), !.
morph_setup_file(morphstart).


% If there is a winning move, assume the opponent will play it,
% as you can't legally  leave your king in check in real chess. 
chess_choose(_Player,Move,SIn,SOut) :- victor_move(Move,SIn,SOut), !,
	format("King is in check; Game over!~n",[]).
chess_choose(Player,Move,SIn,SOut) :- 
	chess_choose_real(Player,Move,SIn,SOut).

chess_choose_real(Player,Move,SIn,SOut) :-
	update_chess(Player,SIn),
	current_input(OldStream),
	global(instream,I),
	set_input(I),
	( chess_move(Player,_,MoveString)
          -> set_input(OldStream),
	     ( completed_move(MoveString,Move,SIn,SOut) 
	       -> print_choice(Move,SIn,SOut)
	       ;  format("Strange string: ~w~n",[MoveString]),
	          fail 
	     )
	  ;  set_input(OldStream),     
	     fail
	).


init_chess_if(Role,SIn) :- 
	should_init(Role,SIn), !,
	init_chess(Role,SIn).
init_chess_if(_Role,_SIn).


% If SETUP global was set to yes, then setup the
% pos, and reset it to no so we don't set it up each time! 
should_init(_Role,SIn) :- 
	move_count(N,SIn),
	N < 2.
should_init(_Role,_SIn) :- 
	global(setup,yes),
	setg(setup,no).


init_chess(Role,SIn) :- 
	global(chessprog,Prog),
	init_prog(Prog,Role,SIn).


update_chess(Role,SIn) :-
%	( N < 2 -> init_chess(Role,SIn) ; true),
	init_chess_if(Role,SIn),
	( last_move(Move) 
	-> meta_to_chess(Move,ChMove),
	   format("To Chess: I played ~w~n",[ChMove]),
	   ChMove=TellMove,
	   tell_chess([TellMove])
        ; true
	), tell_chess_move.

tell_chess_move :- true.

flush_chess :- 
	global(chessprog,Prog),
	flush_prog(Prog).

flush_prog(gnu) :- 
	tell_chess([bd]).
flush_prog(morph).


% This doesn't work, as gnuchess just plays if you tell it to,
% regardless of whos move it is in the position! 
% So Don't tell chess anything. 
chess_prompt(_Player) :- 
	flush_chess.
%	player_color(Player,Color),
%	tell_chess([Color]).

	
meta_to_chess(Move,ChMove) :- 
	chess_notation(Move,String),
	concat_list(String,ChMove).


chess_move(Player,Number,Move) :-
	chess_prompt(Player),
	chess_read(Number,FullMove), !, 
	format("~s",[FullMove]),
	player_indent(Player,Indent),
	append(Indent,Move0,FullMove),
	chess_to_meta(Move0,Move).

chess_header_string(Pattern) :- 
	append_list(["My move is: "],Pattern).


chess_read(_Number,Move) :-
	chess_header_string(String),
	read_until_string(String),
	read_template(Move).

read_template(Move) :- 
%	read_line(Move),
	global(chessprog,Prog),
	read_template(Prog,Move).

read_template(gnu,Move) :- read_four(Move).
read_template(morph,Move) :- read_five(Move).

read_four([A,B,C,D]) :- get0(A), get0(B), get0(C), get0(D).
read_five([A,B,C,D]) :- get0(A), get0(B), get0(_), get0(C), get0(D).

player_indent(_,"").

chess_to_meta(A,B) :- chess_to_meta(B,A,[]).

chess_to_meta([]) --> [].
chess_to_meta([XM,YM|Sqs]) --> [X],[Y],	
	{chess_conv_square(X,Y,XM,YM)},
	chess_rest(Sqs).

chess_rest([]) --> [].
chess_rest(Sqs) --> [],
	chess_to_meta(Sqs).


chess_conv_square(X,Y,XM,YM) :-
	name(XM,[X]),
	name(YM,[Y]).


conv_x(X,XM) :- 
	XM is "h"-X+"a".
conv_y(Y,YM) :- 
	YM is "8"-Y+"1".


%======================================================================

% Ensures parsing mode if really parsing, as otherwise get strange bugs.
chess_notation(ComplexMove,Notation) :-
	( var(ComplexMove) -> set_parsing_mode ; true ),
	chess_notation(ComplexMove,Notation,[]).


chess_notation(M) --> 
	prelims(M,Pre),
	chess_main(Pre,[]).


% CONSIDER_PROMOTE
chess_consider_promote([T|Sel],Rest) -->
	chess_attempt_promote(T),
	chess_select_promote(Sel,Rest).


chess_select_promote([promote_select(Square,OldPiece,OldPiece)|Rest],Rest) --> [].
chess_select_promote([promote_select(Square,OldPiece,NewPiece)|Rest],Rest) --> 
	{OldPiece \== NewPiece,
	 piece_struct_name(NewPiece,Name),
	 name(Name,[NewC|_]),
	 name(NewL,[NewC])},
	 [NewL].
chess_select_promote(Rest,Rest) --> [].



% ATTEMPT_PROMOTE
% attempt_promote(try_promote(square(5,1),piece(piece2,player),piece(piece3,player)))
%    -->  [promote,'(',5,',',1,')',white,piece3,';']
% Unless it promotes, it isn't mentioned in the notation.
chess_attempt_promote(try_promote(_Square,_OldPiece,[])) --> [].
chess_attempt_promote(try_promote(Square,OldPiece,OldPiece)) --> [].


% MAIN
% Repeated Transfers, followed by possible player promotion.
chess_main(In,Out) -->
	 chess_first_transfer(In,First),
	 chess_continued_transfers(First,T),
	 chess_consider_promote(T,Out).


% TRANSFERS
chess_transfers(In,Out) --> chess_transfer(In,T), chess_continued_transfers(T,Out).

% CONTINUED_TRANSFERS
% Either no more transfers, or ';' and more transfers.
% Could tighten this rule: can't continue unless did a movement.
chess_continued_transfers(In,In) --> [].
chess_continued_transfers([end_continues|Rest],Rest) --> [].
chess_continued_transfers(In,Out) --> chess_transfers(In,Out).


% TRANSFER : [move(piece(piece6,player),player,square(5,1),square(4,1)),
%              remove(piece(piece6,player),square(4,1))]
% --> [white,piece6,'(',5,',',1,')',->,'(',4,',',1,')',x,'(',4,',',1,')']
%
chess_transfer([Move|Capture],Rest) --> chess_moving(Move), chess_capture(Capture,Rest).

chess_first_transfer([Move|Capture],Rest) --> chess_first_moving(Move), chess_capture(Capture,Rest).


% MOVING
% moving(move(piece(piece1,opponent),opponent,square(1,6),square(2,4)),S,[]).
%    S = [black,piece1,'(',1,',',6,')',->,'(',2,',',4,')']
chess_first_moving(move(Piece,_Player,From,To)) --> 
	chess_square(From), chess_square(To).

chess_moving(move(Piece,_Player,From,To)) --> 
	chess_square(To).



chess_capture(In,Out) --> chess_null_capture(In,Out).
chess_capture(In,Out) --> chess_real_capture(In,Out).

chess_null_capture(X,X) --> [].

chess_real_capture([C|Cs],Rest) --> chess_simp_capture(C),  chess_capture(Cs,Rest).

chess_simp_capture(M) --> chess_remove(M).
chess_simp_capture(M) --> chess_possess(M).

% REMOVE
chess_remove(capture(remove,Caps)) --> [].

% POSSESS
chess_possess(capture(possess(Player),Caps)) --> [].


chess_square(Sq) --> {
	square(Sq,X,Y) 
		  }, 
	[Col], {nth_letter(X,Col)}, 
	 number(Y).

	
%================================================================================
% User Interface
%================================================================================


create_top :- create.



square_chess_name(Player,Sq,Name) :- 
	square_chess_name(Player,Sq,Name,_S).

square_chess_name(Player,Sq,Name,S) :- 
	player_role(Player),
	( var(S) -> checkpoint(init,S) ; true ),
	on(Piece,Sq,S), 
	piece_struct_name(Piece,PName), 
	piece_struct_owner(Piece,Player),
	player_piece_print_name(Player,PName,ChessName),
	chess_square(Sq,ChessSq,[]),
        concat_list([ChessName|ChessSq],Name).


setup_gnu(S) :- 
	tell_chess([edit]),
	tell_chess(['#']),
	whenever( square_chess_name(player,_,Name,S),
	          tell_chess([Name])
	      ),
	tell_chess([c]),      
	whenever( square_chess_name(opponent,_,Name,S),
	          tell_chess([Name])
	      ),
	tell_chess(['.']).

init_prog(morph,_Role,_SIn).

% Careful when letting gnu play white in typical startup chess
% positions, as he thinks kings can castle! 
init_prog(gnu,Role,S) :- 
	global(handicap,H), 
	H > 0, !,
	tell_chess(['1']),
	tell_chess([new]),
	setup_gnu(S),
        set_gnu_depth,
	init_gnu_role(Role).


set_gnu_depth :- 
	( global(depth,D) ->
	    set_gnu_depth(D)
	; true
        ).

set_gnu_depth(DNum) :- 
	number_chars(DNum,Chars),
	atom_chars(D,Chars),
	tell_chess([depth]),
	tell_chess([D]).




init_gnu_role(Role) :- 
	player_color(Role,Color),
	tell_chess([Color]). 
%init_gnu_role(player) :- tell_chess([switch]). 
%init_gnu_role(opponent).



/*
init_prog(gnu,Role) :- 
	tell_chess(['1']),
	tell_chess([new]),
	tell_chess([get]),
	tell_chess(['metastart.game']),
	tell_chess(['']),
	init_gnu_role(Role).

init_gnu_role(player) :- tell_chess([switch]). 
init_gnu_role(opponent).

*/

/*
init_prog(gnu,Role,_S) :- 
	tell_chess(['1']),
	tell_chess([new]),
	tell_chess([get]),
	tell_chess(['metastart.game']),
	tell_chess(['']),
	init_gnu_role(Role).
*/

%==========================================================================
tell_com(_,_,_,X) :- tell_chess(X).
tell_top(X) :- tell_chess(X).

depth_com(_,_,_,X) :- set_gnu_depth(X).
depth_top(X) :- set_gnu_depth(X).

