%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

% setup.pl
% An interface for setting up the board as we choose.
% We could modify a game file, but this would not allow
% other than symmetric games.

setup_position(empty,Squares,SIn,SOut) :- !,
	make_empty(Squares,SIn,SOut).
setup_position(Player,Assignments,SIn,SOut) :- 
	place_pieces_on_squares(Assignments,Player,SIn,SOut).


setup_com(_,SIn,SOut) :- 
	setup(SIn,SOut).

%======================================================================
% Setting up a position
%======================================================================

setup(SIn,SOut) :- 
	menu_command("~nEnter <setup> command ('help.'  gives  more information)~n",
	setup,[Done,SIn,S1]), !,
	continue_setup(Done,SIn,S1,SOut).
setup(SIn,SOut) :- 
	format("~nI did not understand your command. Please try again!~n",[]),
	setup(SIn,SOut).

% CONTINUE_SETUP(?Move,+SIn,+S1,-SOut).
% Ensure that we are done setting up, else back to the menu.
% If Move and S1 are bound, we accept S1 as the result.
% If only S1 is bound, we accept this as a new current state,
% and use it to get the final move.
% If neither, then some other command didn't do any work, 
% and we start again from our original state to get the move.
%
continue_setup(Move,_SIn,S1,S1) :- nonvar(Move), Move=done, !.
continue_setup(Move,_SIn,_S1,_Nothing) :- nonvar(Move), Move=abort, !.
continue_setup(Move,_SIn,S1,S1) :- nonvar(Move), nonvar(S1), !.
continue_setup(_Move,_SIn,S1,SOut) :- nonvar(S1), !,
	setup(S1,SOut).	
continue_setup(_Move,SIn,_S1,SOut) :- setup(SIn,SOut).


%----------------------------------------
% SETUP menu commands
%----------------------------------------

% put e 2 black bishop
put_setup(_,SIn,SOut,Row,Col,Color,Type) :-
	piece(Piece,[Color,Type],[]),
	with_alpha_squares(
	  gsquare(Square,['(',Row,',',Col,')'],[])),
	place_piece(Piece,Square,SIn,SOut).


% initprom e 2 black bishop
initprom_setup(_,SIn,SOut,Row,Col,Color,Type) :-
	piece(Piece,[Color,Type],[]),
	with_alpha_squares(
	  gsquare(Square,['(',Row,',',Col,')'],[])),
	put_stage(init_promote,SIn,S1),
	add_in(opponent_promotes(Piece,Square),S1,SOut).


% hand white white queen
hand_setup(_,SIn,SOut,HandColor,Color,Type) :-
	piece(Piece,[Color,Type],[]),
	player_color(Player,HandColor),
	put_in_hand(Piece,Player,SIn,SOut).

% empty e 5
empty_setup(_,SIn,SOut,Row,Col) :-
	with_alpha_squares(
	  gsquare(Square,['(',Row,',',Col,')'],[])),
	set_empty(Square,SIn,SOut).
	  
% move e 2 e 4
move_setup(_,SIn,SOut,Row1,Col1,Row2,Col2) :- 
	with_alpha_squares(
	  ( gsquare(Square1,['(',Row1,',',Col1,')'],[]),
	    gsquare(Square2,['(',Row2,',',Col2,')'],[])  )),
	  move_piece(_,Square1,Square2,SIn,SOut).
	

% stage assign
stage_setup(_,SIn,SOut,Stage) :- 
	put_stage(Stage,SIn,SOut).

% control black 
control_setup(_,SIn,SOut,Color) :- 
	player_color(Player,Color),
	put_control(Player,SIn,SOut).

% clear
clear_setup(_,SIn,SOut) :- 
	make_empty_board(SIn,SOut).
	

% add
add_setup(_,SIn,SOut) :- 
	format("Enter a property to be added: ~n",[]),
	read(Prop),
	add_in(Prop,SIn,SOut).

% del
del_setup(_,SIn,SOut) :- 
	format("Enter a property to be deleted: ~n",[]),
	read(Prop),
	del_in(Prop,SIn,SOut).


% RESTORE <name>
% Set current state to be a previously named checkpoint state.
restore_setup(_,_SIn,SOut,Name) :-
	restore_state(Name,SOut).
	
% CHECKPOINT
checkpoint_setup(_,SIn,_) :- 
	checkpoint_state(SIn).

% CHECKPOINT <NAME>
checkpoint_setup(_,SIn,_,Name) :- 
	checkpoint_state(Name,SIn).




% done
done_setup(done,SIn,SIn).

% abort: abandon changes and back to move menu.
abort_setup(abort,_SIn,_).

% display 
display_setup(_Move,SIn,_) :- 
	format("~nCurrent State:  ~n",[]),
	print_state(SIn).


help_setup(_,_,_) :- 
	help_setup.

help_setup :- 
	format("
Modifying Board Setup
---------------------
move <x1> <y1> <x2> <y2>         => transfer piece to different square
put <x> <y> <color> <piece>      => put a piece on square (<x>,<y>)
initprom <x> <y> <color> <piece> => opponent must promote piece on square
empty <row> <col>		 => empty square (<row>,<col>)
hand <Hcolor> <color> <piece>    => put piece in <Hcolor>'s hand
clear				 => makes all the square empty
control <color>			 => puts player <color> in control
stage <stage>			 => sets current stage to <stage>
add/del				 => add or delete a property from state
display				 => prints the board
done				 => exits setup stage with current setup
abort				 => exits setup stage, abandon changes
checkpoint <n>.                  => record state under name <n> 
restore <n>.                     => set state to that checkpointed as <n> 

Examples:

move e 2 e 4.
put e 2 white king.
hand white black queen.
control white.
stage assign.
initprom e 2 black bishop. 
done.
",[]).


