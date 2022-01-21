%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%% compile_syms.pl
%% Compiling out symmetries and game definition to improve efficiency.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Symmetries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Computing closures of some set under a set of transformations.
%
% Agenda: List of items still to close.
% Table: Those items already closed.
% Begin with full agenda and empty table.
% Repeat:
%  Take first item from agenda.
%  Apply Transforms to first item on agenda.
%  For each result not already in the table:
%   Add to table.
%   Add to agenda.
% Until empty agenda.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Symmetries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
The key property of symmetric closure:
Init. current_dirs: [dir].
New_Current_dirs1 := Apply all syms to current_dirs.
If new_dirs = current_dirs then done.
Else loop.
Result -> new_dirs.
*/%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sym_dir(Dir,Syms,DirOut) :-
	in_symmetric_closure(Dir,Syms,DirOut).


% IN_SYMMETRIC_CLOSURE(+Dir,+Syms,-DirOut)
% DirOut is in the  symmetric closure of Dir under Syms.
in_symmetric_closure(Dir,Syms,DirOut) :-
	closure([Dir],Syms,Dirs),
	member(DirOut,Dirs).

closure(Set,Transforms,Closure) :-
	close1(Set,Transforms,Set,Closure).

close1([],_,T,T).
close1([A|As],Ts,Table,Close) :-
	close_item(Ts,A,As,Table,NewAs,NewTable),
	close1(NewAs,Ts,NewTable,Close).

close_item([],_,As,T,As,T).
close_item([T|Ts],A,As,TIn,NewAs,TOut) :-
	transform_item(A,T,ATrans),
	schedule_if(ATrans,As,TIn,NewAs1,T1),
	close_item(Ts,A,NewAs1,T1,NewAs,TOut).

schedule_if(Item,As,Table,As,Table) :-
	member(Item,Table), !.
schedule_if(Item,As,Table,[Item|As],[Item|Table]).
	
	
transform_item(Dir,Sym,NewDir) :-
	symmetry(Sym,Dir,NewDir).

symmetry(forward,dir(X,Y),dir(X,Y1)) :-
	negates(Y,Y1).
symmetry(side,dir(X,Y),dir(X1,Y)) :-
	negates(X,X1).
symmetry(rotation,dir(X,Y),dir(Y,X)).


negates(N,N1) :-
	var(N) -> 
         N is N1*(-1)
        |otherwise -> 
         N1 is N*(-1).


% Interesting idea:  Given move descriptions, locate symmetries,
% by seeing if legality is invariant under these transformations!

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Compiling symmetries for a particular game
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


game_movement(Game,Movement) :-
	game_piece_def(Game,_,Def),
	piece_defines_movement(Def,Movement).

piece_defines_movement(Def,Movement) :-
	piece_movement(Def,CompMovement),
	movement_component(CompMovement,Movement).
piece_defines_movement(Def,M) :-
	piece_capture(Def,CompCapture),
	capture_component(CompCapture,Capture),
	capture_has_movement(Capture,M).


game_syms_dir(Game,Syms,Dir) :-
	game_movement(Game,M),
	movement_dir(M,Dir),
	movement_syms(M,Syms).

unique_sym_dirs(SymDirs) :-
	setof(Dir-Syms-Sym,
	   Game^( some_player_game(Game),
	          game_sym_dir(Game,Dir,Syms,Sym) ),
	  SymDirs).

game_sym_dir(Game,Dir,Syms,Sym) :-
	game_syms_dir(Game,Syms,Dir),
	in_symmetric_closure(Dir,Syms,Sym).

some_player_game(Game) :-
	player_current_game(Game) ; opponent_current_game(Game).


index_sym_dirs :-
	with_temp_file(syms,File,
	  (index_sym_dirs_to_file(File),
	   compile(File))).

index_sym_dirs_to_file(File) :-
	format("~nIndexing Symmetries to file: ~w~n",[File]),
	assert_sym_indices,
	with_output_file(File,write,
	   ( print_sym_overwrite,
	     listing(sym_index),
	     overwrite_game )),
	abolish(sym_index/3).


assert_sym_indices :-
	abolish(sym_index/3),
	unique_sym_dirs(Dirs),
	whenever(member(Dir-Syms-Sym,Dirs),
	   assert_sym_index(Dir,Syms,Sym)).

assert_sym_index(Dir,Syms,Sym) :-
	dir_key(Dir,Key),
	assert(sym_index(Key,Syms,Sym)).


print_all_syms :- 
	unique_sym_dirs(L), 
	member(Dir-_-Sym,L), 
	format("~p --> ~p~n",[Dir,Sym]),
	fail.

indexed_sym_dir(Dir,Syms,Sym) :- 
	dir_key(Dir,Key),
	sym_index(Key,Syms,Sym).


print_sym_overwrite :-
	format("
% Compiled Symmetry File

:- abolish(sym_dir/3).

sym_dir(Dir,Syms,Sym) :- 
	indexed_sym_dir(Dir,Syms,Sym).

",[]).


overwrite_game :- 
	print_game_overwrite,
	listing(player_current_game),
	listing(opponent_current_game).


print_game_overwrite :-
	format("
% Compiled Game File

:- abolish(player_current_game/1).
:- abolish(opponent_current_game/1).

",[]).


dir_key(dir(X,Y),Key) :-
	Key is 1000*X + Y.

square_key(square(X,Y),Key) :-
	Key is 1000*X + Y.


%=============================================================================
% Testing


time1 :- timing(fastsyms(FastS)), nl, print(FastS), 
	timing(slowsyms(SlowS)), nl, print(SlowS).

time_sym(N) :- timing(dotimes(N,fastsyms(_))),
               timing(dotimes(N,slowsyms(_))).


fastsyms(SS) :-
	setof(S,indexed_sym_dir(dir(2,1),[forward,side,rotation],S),SS).

slowsyms(SS) :- 
	setof(S,in_symmetric_closure(dir(2,1),[forward,side,rotation],S),SS).



/*  Using compile_syms and index_syms, on indexed chinese-chess,
making 4 random moves.

Data = [(user:time_sym/1)-0,(user:slowsyms/1)-0,(user:time1/0)-0,(user:fastsyms/
1)-0,(user:indexed_sym_dir/3)-70,(user:dotimes/2)-0,(user:game_syms_dir/3)-0,(us
er:dir_key/2)-70,(user:unique_sym_dirs/1)-0,(user:print_all_syms/0)-0,(user:asse
rt_sym_index/3)-0,(user:print_sym_overwrite/0)-0,(user:some_player_game/1)-0,(us
er:piece_defines_movement/2)-0,(user:assert_sym_indices/0)-0,(user:index_sym_dir
s_to_file/1)-0,(user:game_sym_dir/4)-0,(user:game_movement/2)-0,(user:index_sym_
dirs/0)-0,(user:sym_index/3)-387,(user:sym_dir/3)-28],
Selec = execution_time ? ;


%%% Using compile_syms and index_syms, on indexed chinese-chess,
but re-written back to use boardsstat (old symmetry),
making 4 random moves.


Data = [(user:time_sym/1)-0,(user:slowsyms/1)-0,(user:time1/0)-0,(user:fastsyms/
1)-0,(user:indexed_sym_dir/3)-0,(user:dotimes/2)-0,(user:game_syms_dir/3)-0,(use
r:dir_key/2)-0,(user:unique_sym_dirs/1)-0,(user:print_all_syms/0)-0,(user:assert
_sym_index/3)-0,(user:print_sym_overwrite/0)-0,(user:some_player_game/1)-0,(user
:piece_defines_movement/2)-0,(user:assert_sym_indices/0)-0,(user:index_sym_dirs_
to_file/1)-0,(user:game_sym_dir/4)-0,(user:game_movement/2)-0,(user:index_sym_di
rs/0)-0,(user:sym_index/3)-0,(user:transform_item/3)-403,(user:close1/4)-398,(us
er:symmetry/3)-567,(user:place_pieces_on_squares/4)-5,(user:close_item/6)-1422,(
user:closure/3)-40,(user:gcf/3)-0,(user:wrap_leaps/3)-0,(user:on_board/1)-350,(u
ser:conn_cyl/3)-0,(user:conn_for_type/4)-106,(user:place_piece_in_hand/5)-0,(use
r:do_assignments_for_player/6)-0,(user:schedule_if/5)-1549,(user:valid_max/3)-87
,(user:wl/3)-0,(user:max_leaps/4)-12,(user:valid_max_dir/3)-66,(user:legal_locat
ion/1)-65,(user:conn/3)-766,(user:connected/3)-364,(user:uncollapse/3)-4,(user:a
ssign_piece_to_square/4)-5,(user:uncollect/2)-1,(user:place_pieces_in_hand/5)-0,
(user:place_pieces_on_squares/3)-0,(user:arbitrary_assignment/1)-0,(user:create_
initial_setup/3)-0,(user:legal_location_cyl/2)-0,(user:make_assignable_squares/2
)-0,(user:make_empty/3)-16,(user:make_empty_board/2)-0,(user:initialize_board/3)
-0,(user:valid_min/2)-29,(user:do_assignments/3)-0,(user:set_initial_move_count/
2)-0,(user:in_symmetric_closure/3)-87,(user:start_game/3)-0,(user:sym_dir/3)-28,
(user:negates/2)-1153,(user:start_game/2)-0,(user:assignment_decision/4)-0],
Selec = execution_time ?


*/

