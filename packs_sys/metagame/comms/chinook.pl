%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

:- ensure_loaded(library(pipes)).

create_chinook :- 
    command_from_args([nice,-10,chinook],Command),
    interface_record_streams(Command,_InStream,_OutStream).

tell_chinook(Statement) :- 
    tell_outstream(Statement).

tellc(S) :- tell_chinook(S).

seem :- global(instream,I),
	set_input(I).



%% pattern.pl

chinook_quit :- tell_chinook([q]), interface_close_streams.

chinook_choose(Player,Move,SIn,SOut) :-
	update_chinook(Player,SIn),
	current_input(OldStream),
	global(instream,I),
	set_input(I),
	( chinook_move(Player,_,MoveString)
	-> set_input(OldStream),
	   completed_move(MoveString,Move,SIn,SOut),
	   print_choice(Move,SIn,SOut)
	;  set_input(OldStream),
	   tell_chinook(['O']),
	   chinook_choose(Player,Move,SIn,SOut)).

:- add_global(handicap,0).

setup_chinook :- 
    global(handicap,H),
    tell_chinook([i]), 
    config(H).


%handicap_position(N,Init,Pos,Command) :-
    

config(0). 
config(1) :- tell_chinook('rb6').
config(2) :- tell_chinook('rb6rd6').
config(3) :- tell_chinook('rb6rd6rf6').
config(4) :- tell_chinook('rb6rd6rf6rh6').
config(8) :- tell_chinook('rb6rd6rf6rh6'),
	     tell_chinook('ra7rc7re7rg7').



update_chinook(Role,SIn) :-
	move_count(N,SIn),
	( N < 2 ->  setup_chinook ; true),
	( last_move(Move) 
	-> meta_to_ch(Move,ChMove),
	   format("To Chinook: I played ~w~n",[ChMove]),
	   concat(m,ChMove,TellMove),
	   tell_chinook([TellMove])
        ; true
	), tell_chinook_move.

tell_chinook_move :- 
	tell_chinook([g]).

	
meta_to_ch(Move,ChMove) :- 
	ch_notation(Move,String),
	concat_list(String,ChMove).


chinook_move(Player,Number,Move) :-
	chinook_read(Number,FullMove), !, 
	format(FullMove,[]),
	player_indent(Player,Indent),
	append(Indent,Move0,FullMove),
	ch_to_meta(Move0,Move).
%	append(Move0,".",Move1),
%	tokenize_chars(Move1,Move).
	


chinook_read(Number,Move) :-
%	append_list(["I move  ",[Num],". "],Pattern),
	append_list(["I move"],Pattern),
	format("Looking for pattern: ~w~n",[Pattern]),
	found(Pattern),
	read_keyboard_tokens([Number,_]),
	read_line(Move).

player_indent(player," ").
player_indent(opponent,"  ... ").


%chmv([],[]) --> [].
%chmv([C|Cs],[M|Ms]) -->


ch_to_meta([],[]).
ch_to_meta([X,Y|Rest],[XM,YM|RestM]) :-
	conv_square(X,Y,XM,YM),
	ch_to_meta(Rest,RestM).

conv_square(X,Y,XM,YM) :-
	conv_x(X,XM1),
	conv_y(Y,YM1),
	name(XM,[XM1]),
	name(YM,[YM1]).


conv_x(X,XM) :- 
	XM is "h"-X+"a".
conv_y(Y,YM) :- 
	YM is "8"-Y+"1".




%======================================================================

% Ensures parsing mode if really parsing, as otherwise get strange bugs.
ch_notation(ComplexMove,Notation) :-
	( var(ComplexMove) -> set_parsing_mode ; true ),
	ch_notation(ComplexMove,Notation,[]).


ch_notation(M) --> 
	prelims(M,Pre),
	ch_main(Pre,[]).


% CONSIDER_PROMOTE
ch_consider_promote([T|Sel],Rest) -->
	ch_attempt_promote(T),
	ch_select_promote(Sel,Rest).

ch_select_promote([promote_select(_,_,_)|Rest],Rest) --> [].
ch_select_promote(Rest,Rest) --> [].



% ATTEMPT_PROMOTE
% attempt_promote(try_promote(square(5,1),piece(piece2,player),piece(piece3,player)))
%    -->  [promote,'(',5,',',1,')',white,piece3,';']
% Unless it promotes, it isn't mentioned in the notation.
ch_attempt_promote(try_promote(_Square,_OldPiece,[])) --> [].
ch_attempt_promote(try_promote(Square,_OldPiece,NewPiece)) --> [].


% MAIN
% Repeated Transfers, followed by possible player promotion.
ch_main(In,Out) -->
	 ch_first_transfer(In,First),
	 ch_continued_transfers(First,T),
	 ch_consider_promote(T,Out).


% TRANSFERS
ch_transfers(In,Out) --> ch_transfer(In,T), ch_continued_transfers(T,Out).

% CONTINUED_TRANSFERS
% Either no more transfers, or ';' and more transfers.
% Could tighten this rule: can't continue unless did a movement.
ch_continued_transfers(In,In) --> [].
ch_continued_transfers([end_continues|Rest],Rest) --> [].
ch_continued_transfers(In,Out) --> ch_transfers(In,Out).


% TRANSFER : [move(piece(piece6,player),player,square(5,1),square(4,1)),
%              remove(piece(piece6,player),square(4,1))]
% --> [white,piece6,'(',5,',',1,')',->,'(',4,',',1,')',x,'(',4,',',1,')']
%
ch_transfer([Move|Capture],Rest) --> ch_moving(Move), ch_capture(Capture,Rest).

ch_first_transfer([Move|Capture],Rest) --> ch_first_moving(Move), ch_capture(Capture,Rest).


% MOVING
% moving(move(piece(piece1,opponent),opponent,square(1,6),square(2,4)),S,[]).
%    S = [black,piece1,'(',1,',',6,')',->,'(',2,',',4,')']
ch_first_moving(move(Piece,_Player,From,To)) --> 
	ch_square(From), ch_square(To).

ch_moving(move(Piece,_Player,From,To)) --> 
	ch_square(To).



ch_capture(In,Out) --> ch_null_capture(In,Out).
ch_capture(In,Out) --> ch_real_capture(In,Out).

ch_null_capture(X,X) --> [].

ch_real_capture([C|Cs],Rest) --> ch_simp_capture(C),  ch_capture(Cs,Rest).

ch_simp_capture(M) --> ch_remove(M).
ch_simp_capture(M) --> ch_possess(M).

% REMOVE
ch_remove(capture(remove,Caps)) --> [].

% POSSESS
ch_possess(capture(possess(Player),Caps)) --> [].


ch_square(Sq) --> {
	invert(Sq,Sq1),
	square(Sq1,X,Y) 
		  }, 
	[Col], {nth_letter(X,Col)}, 
	 number(Y).

	
