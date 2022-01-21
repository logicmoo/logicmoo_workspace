%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

% gen_menu.pl

% This menu is accessible from the top-level menu.
gen_top :- generate.

%======================================================================
% Game Generation Menu
%======================================================================

generate :- 
	menu_command("~nEnter <generation> command ('help.'  gives  more information)~n",
	gen,[]), !,
	continue_generate.
generate :- 
	format("~nI did not understand your command. Please try again!~n",[]),
	generate.

% CONTINUE_GENERATE.
% Here we always continue from this menu.
% The only way to get out is to use the 'done' command,
% which just calls metagame again.

continue_generate :- generate.

%----------------------------------------
% GENERATE menu commands
%----------------------------------------

help_gen :- 
	help_gen1,
	help_gen2.

help_gen1 :- 
    format("
Generating a Game:
------------------------------
games_library.       => show games in library
generate <file>.     => generate (and load) random game, save as <file>.game
game <file>.         => loads <file>.game as the current game
compile {on/off}.    => set whether to compile symmetries when loading a game.
quick.               => short form to turn of compiling symmetries. 
done.                => return to top-level menu
",[]).

help_gen2 :- 
    format("
Examining Game and Changing System State
-----------------------------------------
pieces.              => show the names of the pieces in the current game
define <piece>.      => print the definition of <piece> in the current game
goals.               => print the goals of the current game
board.               => print board definition of the current game
rules.               => print the full rules of the current game
set <p> <v>.         => set generator parameter <p> to value <v>
set <p>.             => set generator parameter <p> ('help set.')
set.                 => show generator parameter settings
randomize <N>        => use random seed #<N> (N = 1..10)
(un)trace {options}  => trace some system behavior ('help trace.') 
cd <dir>.            => change current directory to <dir> ('help cd.')
pwd.                 => show current directory name
ls.                  => show contents of current directory
prolog. (abort)      => abort to prolog
quit.                => exit session (back to shell)
",[]).


done_gen :- metagame.

set_gen(P,V) :- set_gen_parameter(P,V).

set_gen(P) :- change_gen_param(P).

set_gen :- show_gen_parameters.

randomize_gen(N) :- randomize(N).

generate_gen(File) :- 
	generate_and_load(File).
	
generate_gen :- 
	generate_and_load(random).

game_gen(File) :- load_game(File).

games_library_gen :-
	games_library.

cd_gen(Dir) :-
	cd_print(Dir).

pwd_gen :- pwd_print.

ls_gen :- ls.

define_gen(PieceName) :-
	show_piece_definition(PieceName).

goals_gen :-
	show_game_goals.

rules_gen :- show_rules.

pieces_gen :- show_piece_names.

board_gen :- show_board.

quick_gen :- set_parameter(compile_symmetries,off).

compile_gen(OnOff) :- set_parameter(compile_symmetries,OnOff).


restart_gen :- 
	format("~nRestarting ...~n",[]),
	metagame.

quit_gen :- 
	print_quit.

prolog_gen :- print_abort.

abort_gen :- 
	print_abort.

verbose_gen :-
	set_verbose.

quiet_gen :-
	set_quiet.


%-----------------------------------------------------------------
% Tracing
%-----------------------------------------------------------------

trace_gen(Module) :-
	set_tracing(Module,on).
trace_gen(Module,Component) :-
	set_tracing(Module,Component,on).

untrace_gen(Module) :-
	set_tracing(Module,off).
untrace_gen(Module,Component) :-
	set_tracing(Module,Component,off).

list_tracing_gen :- 
	list_tracing.
