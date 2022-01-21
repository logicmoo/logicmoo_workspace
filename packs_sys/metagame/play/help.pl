%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%% help.pl
% Help menus

%----------------------------------------
% Help from top menu
%----------------------------------------


help_top :- 
	help_top_general,
	help_tables,
	help_system.


help_top(F) :- 
	help_top_entry(F,G),
	call(G).
help_top(help) :-
	help_top_topics(Topics),
	format("To select a topic, type 'help <Topic>.'~n",[]),
	format("Help in this menu is available on the following topics:~n",[]),
	pwl(Topics).
	
help_top_topics(Topics) :- 
	findall(Topic,help_top_entry(Topic,_),AllTopics),
	sort(AllTopics,Topics).


% Just add new entries here, and define their fns,
% to hook them into help menu at top level. 
help_top_entry(player,help_player).
help_top_entry(show,help_show).
help_top_entry(trace,help_trace).
help_top_entry(cd,help_cd).
help_top_entry(iterate,help_iterate).
help_top_entry(weights,help_weights).
help_top_entry(eval_fn,help_eval_fn).
help_top_entry(evalfile,help_evalfile).
help_top_entry(history,help_history). 
help_top_entry(clock,help_clock). 
help_top_entry(tables,help_tables).
help_top_entry(advisors,help_advisors).
help_top_entry(playernum,help_playernum).


% generate <file>.     => generate (and load) a random game and save as <file>.game

help_top_general :- 
    format("
Generating and Playing a Game:
------------------------------
gen.                 => enters menu for generating random games.
games_library.       => show games in library
game <file>.         => loads <file>.game as the current game
evalfile <file>.     => loads <file>.eval as eval tables for current game
player <c> <p>.      => set player <p> to play color <c> ('help player.')
players <w> <b>.     => set player <w> as white, <b> as black.
players.             => shows current players. 
start.               => start playing current game from initial position
start <posname>.     => start playing current game from saved position <posname>
	                ('help history')
clock.               => prints current game clock (help clock)
",[]).


help_tables :- 
    format("
Constructing and Using Analysis Tables
--------------------------------------
build.               => builds tables using current advisors. 
showstatic.          => shows the static table values for all pieces.
advice <c> <p>.      => shows static advice for piece <p> of color <c>.
alladvisors.         => shows current weights for all advisors.
active.              => shows weights for only non-zero weighted advisors
playernum <c> <n>.   => set numbered player <n> to play color <c> ('help playernum.')
playernums <w> <b>.  => set numbered player <w> as white, <b> as black.
help advisors.       => more info on the advisors and what they do. 
",[]).




help_system :- 
	help_game,
	help_state.

help_game :- 
	format("
Examining Game
--------------
pieces.              => show the names of the pieces in the current game
define <piece>.      => print the definition of <piece> in the current game
goals.               => print the goals of the current game
board.               => print board definition of the current game
rules.               => print the full rules of the current game
",[]).


help_state :-
	format("	
Changing System State
---------------------
set <p> <v>.         => set parameter <p> to value <v>  (set global default)
set <c> <p> <v>.     => set parameter <p> to value <v>, for player <c> only
unset <c>            => clears params specific to player <c> 
set.                 => show parameter settings
randomize <N>        => use random seed #<N> (N = 1..10)
show {options}       => show configuration data ('help show.') 
(un)trace {options}  => trace some system behavior ('help trace.') 
cd <dir>.            => change current directory to <dir> ('help cd.')
pwd.                 => show current directory name
ls.                  => show contents of current directory
prolog. (abort)      => abort to prolog
quit.                => exit session (back to shell)
help help.           => list of all additional help topics. 
",[]).



help_playernum :- format("See the file learning/tourney.pl for more info.~n",[]). 

help_player :- 
	help_player_color,
	help_player_options,
	help_player_example.
	
help_player_color :- 
    format("
player <color> <player>.
------------------------
Tells the system how the moves will be chosen for color <color>.

<color> must be one of the following:

white           => the first player.
black           => the second player.
",[]).

help_player_options :- 
	help_player_options_simple,
	help_player_options_search.

help_player_options_simple :- 
    format("
<player> must be one of the following:

Simple Players
--------------
human              => chosen by a human using this interface.
random             => play a random legal move.
instant            => play first legal move available (faster)
cautious           => plays first non-losing move available (faster)
random_cautious    => play random non-losing move 
random_aggressive  => wins if can, else plays cautiously
",[]).

help_player_options_search :- 
	format("
Searching Players
-----------------
alpha_beta         => does alpha_beta search down to a fixed depth 
                      (specified by <depth> parameter).  
                      uses principal continuation heuristic.
                      Bounded by same time limits and move ordering as
                      iterate player.
iterate            => iterative deepening alpha-beta search.
                      uses either fixed or random ordering based on <ordering> 
                      parameter.
                      ('help iterate' for more info on this)
iterate_random     => iterate player with random move ordering when all equal.
                      ('help iterate' for more info on this)
iterate_fixed      => iterate player with fixed ordering.
",[]).


help_player_example :- 
	format("
Example:

     player white human.
     player black random_aggressive.

Both players can be specified at once by the command:
    player <white_player> <black_player>.

",[]).


help_cd :- 
    format("
cd <dir>.          => change current directory to <dir> 
---------

<dir> must be an atom or between single quotes:
Example:

     cd mygames.
     cd '~~/metagame/games/'.
",[]).


%----------------------------------------
% Help from move menu
%----------------------------------------

help_com(_,_,_) :- help_commands.


help_com(_,_,_,F) :- 
	help_com_entry(F,G),
	call(G).
help_com(_,_,_,help) :-
	help_com_topics(Topics),
	format("To select a topic, type 'help <Topic>.'~n",[]),
	format("Help in this menu is available on the following topics:~n",[]),
	pwl(Topics).
	
help_com_topics(Topics) :- 
	findall(Topic,help_com_entry(Topic,_),AllTopics),
	sort(AllTopics,Topics).

help_com_entry(notation,help_move).
help_com_entry(move,help_move).
help_com_entry(clock,help_clock).
help_com_entry(query,help_query).
help_com_entry(show,help_show).
help_com_entry(trace,help_trace).
help_com_entry(cd,help_cd).
help_com_entry(iterate_random,help_iterate).
help_com_entry(iterate,help_iterate).
help_com_entry(advice,help_advice).
help_com_entry(advisors,help_advisors).
help_com_entry(weights,help_weights).
help_com_entry(eval_fn,help_eval_fn).
help_com_entry(evalfile,help_evalfile).
help_com_entry(history,help_history).



help_commands :- 
	help_com_entry,
	help_com_selecting,
%	help_com_eval,
	help_system,
	help_com_state,
	help_syntax.

help_com_selecting :- 
    format("
SPECIAL MOVE SELECTION METHODS:
-------------------------------
select.            => backtrack through available moves
random.            => play a random legal move
random_aggressive  => play a winning, or random non-losing move, in that order
victor.            => play a move which wins immediately
endgame.           => play a move which ends the game immediately
cautious.          => play a move which blocks opponent's victory, if threatened
mate.              => play a move which forces a win in 2-ply
threaten.          => play a move which threatens victory next move
instant.           => play the first move generated (doesn't ask)
alpha_beta {<n>}.  => does n-ply alpha-beta search (or DEPTH parameter if no n)
iterate.           => does iterative-deepening search until timeout. 
advice.            => shows comments used in eval fn on current position (help)
advice <x> <y>     => shows local advice for piece now at (x,y)
evaluate.          => evaluate current position using current parameters. 
",[]).


help_com_state :- 
    format("
EXAMINING AND MODIFYING STATE OF GAME
-------------------------------------
display.        => print current state
clock.          => prints current game clock (help clock)
query.          => computes some function on the current position (help query)
setup.          => enters menu for setting up board positions
pass.           => transfer control to the other player (ie null move)
access.         => access state from a command level
checkpoint <n>. => record current state under name <n> (for debugging)
restore <n>.    => sets current state to that checkpointed as name <n> 
restart.        => abandon current game, choose new game and players
evalfile <file>.=> loads <file>.eval as eval tables for current game
                   (help evalfile)
next (prev) <n> => goto next (or previous) <n>th position (help history)
",[]).


help_com_entry :- 
    format("
BASIC MOVE ENTRY: 
-----------------
<Move>          => plays move <Move> ('help notation' for more information)
",[]).

help_syntax :- 
    format("
Note that all keyboard input can contain newlines, and that a period
signals the end of the input/command.
",[]).



help_move :- 
	help_move_basic,
	help_move_completion.

help_move_basic :-
    format("
BASIC MOVE ENTRY: 
-----------------
This follows the grammatical notation for moves, 
illustrated as follows:

Basic Movements, eg:
     white king (5,1) -> (5,2). 

Basic Movements with a removal capture, eg:
     white king (5,1) -> (5,2) x black rook (4,1).

Possession captures indicate player who will possess:
     white king (5,1) -> (5,2) x black rook (4,1) (white).

Multiple Captures, eg: (if bug captures <-1,0> by {retrieve clobber})
     white bug (4,1) -> (3,1) x white fish (3,1) black bug (5,1).

Continued Captures, eg:
     white checker (3,3) -> (5,5) x black checker (4,4);
     white checker (5,5) -> (3,7) x black king (4,6).

Placing a piece from a player's hand, eg:
     white king (white) -> (5,1).

Promoting a piece which moved to or past the promote_rank.
  If player promotes, this happens at end of his turn:
     white pawn (2,7) -> (2,8); promote (2,8) white queen.
  If opponent promotes, this happens at start of his turn:
     promote (2,8) black queen; black queen (2,8) -> (2,5).

",[]).

help_move_completion :-
    format("
MOVE COMPLETION: (when COMPLETIONS parameter is ON)
----------------
Instead of the full grammatical move notation, you can enter
a sequence of words which occurs in the complete move notation.
This will match all moves containing those words in that order,
possibly with other words in-between.

Unlike the mode for entry of full grammatical move notations,
squares are here refered to in a more convenient form:
	(X,Y)
where X is the letter for that column, and Y is the number for the row.

Some examples of moves to be completed are:

pawn.            => completes to moves involving a pawn.
white (c,3) ->.  => completes to moves moving a white piece from (c,3).
x.               => completes to moves which capture something.
x rook.          => completes to moves which capture a rook.
promote.         => completes to moves which enter promotion zone.
.                => completes to any legal moves.
",[]).


help_advice :-
    format("
advice
------
Prints the advice regarding the current position, from all active
advisors.  This advice will be weighted according to the values of the
paramters to determine the overall evaluation of the position. 

advice <row> <col>
------------------
Gives only the local advice for the piece at <row> <col> in the current
position.  

Example:
	advice f 4. ==> Will give local advice for whatever piece is there.
        advice.     ==> Gives all advice, including global advice.  

help advisors    ==> Gives more info on what advisors do. 
",[]).


help_query :-
    format("
query <function> <arguments>.
------------------------
Calls a querying function on its arguments.

<function> must be one of the following:

mobility        => prints number of moves available for each player
material        => prints number of pieces on board for each player
goal            => prints whether a goal has been achieved 

These functions have an optional argument, a <player>:

player             => returns value for white only
opponent           => returns value for black only

Example:

     query mobility player.
     query material opponent.
     query goal.
 ~n",[]).



help_show :-
    format("
show <function> <arguments>.
----------------------------
Calls a showing function on its arguments.

<function> must be one of the following:

weights           => prints weight vectors for eval fns
weights <player>  => prints weight vector for eval fn for <player>
                     <player> is {player, opponent, or default}

Example:

     show weights.	
     show weights player.
     show weights default.
 ~n",[]).


help_weights :- 
    format("
Accessing Evaluation Function Weights
-------------------------------------
The weights used by a particular player can be changed by the
command:
	weight <P> MATERIAL MOBILITY.   (negative values must be quoted)
Example:
	weight player 5 1.
	weight opponent '-1' 0.
Would make player value pieces and moves positively, 
with pieces worth five times the value of each move,
and would make opponent prefer losing material, and not 
care about mobility.  

Note that if a weight is set to 0, no time is spent in counting
that feature in a position.

To view the weights: 
	show weights.            ==> shows weights or both players
	show weights <player>.   ==> shows weights for <player>
",[]).

help_eval_fn :- 
    format("
EVALUATION FUNCTION
--------------------
The default  evaluation function is based on
material difference and mobility difference between the two 
players, where PLAYER (white) prefers positive differences,
and OPPONENT (black) prefers negative differences.

The function is:  
	EVAL = EVAL(PLAYER) - EVAL(OPPONENT)
where
	EVAL(P) =   WEIGHT(P,MATERIAL)*MATERIAL(P)
                  - WEIGHT(P,MOBILITY)*MOBILITY(P)
and
	MATERIAL(P) = Number of pieces P has on board
	MOBILITY(P) = Number of legal moves P has on board

For info on viewing and modifying these weights, (help weights)
",[]).


help_evalfile :- 
    format("
EVALUATION FUNCTION TABLES
--------------------------
evalfile <file>.  => loads <file>.eval as eval tables for current game

Some example tables are found in:
     'Metagame/games/chess.eval'
     'Metagame/games/turncoat.eval'
     
If you create your own for a specific game, these can be loaded
in either before starting a game (top-level menu), or
when it is the human's turn to make a move (move command menu).

The searching players ('help player') make use of whichever table file
is currently loaded.  

The following parameters are useful with evaluation tables:
   <square>: relative weight to give piece-square tables 
   <material>: relative weight to give specific material tables
   <gen_material>: relative weight to give a player for having
                   each piece of his color on the board.
   <piece_attacks>:  (not implemented)
   <piece_mobility>: (not implemented)
",[]).


help_iterate :- 
    format("
The <iterate> player
---------------------------
This player performs an iterative deepening alpha-beta search.
It uses the principal continuation heuristic. 
Move ordering is determined by the paramer ORDERING:
	random:  choose a random move when all evaluated equal.
        fixed:   choose the first move found when all evaluated equal. 

The iterative searcher will end the search after it has run out
of time, based on the parameter: <move_time_limit>.
For example, the command:
	set move_time_limit 20000. 
will force it to stop its search after 20 seconds (20,000 msec).

Note that a player loses the game if it uses more than <game_time_limit>
msecs, so set this with the command:
	set game_time_limit 3600000.   (for 1 hour time-limit)

More info is available about the current eval fn (help eval_fn).

Some tracing information is available on this player, (help trace). 
 ~n",[]).


help_trace :-
	help_trace_general,
	help_trace_play,
	help_trace_ab,
	help_trace_gen.

help_trace_general :-
    format("
trace   <module> {<component>}.
untrace <module> {<component>}.
-------------------------------
Enables or disables tracing some component of a module.
If the module does not have any components, no component
is needed here.

Listing traced Modules
----------------------
To list the modules which are currently being traced, do:
	list_tracing.

Traceable Modules
-----------------
Currently, the following tracing modules might be useful:
 ~n",[]).

help_trace_play :- 
    format("
Verbosity when playing games (play) 
-----------------------------------
	state:  print state as moves are chosen by players.
        move:   print move as moves are chosen by players. 
        clock:  print the clock as moves are played. 

These are all set to ON by default whenever a 
human is playing.  

Example:  
	trace play state.      => turns this on
	untrace play clock.    => turns this off

",[]).
 


help_trace_ab :- 
    format("
Tracing Alphabeta (ab) Search
-----------------------------
	ordering:  info regarding move ordering heuristics
	value:  info regarding value of moves found during search
                also traces principal continuations 
	resources: info regarding resource consumption during search
        timing: info on timeout checks during search
	iteration: info on time taken by each iteration of the search
        eval: show evaluation of each node evaluated.
        expand: show each node as it is expanded. 
        advice: show influence on evals for each node evaluated.
        state:  print each state visited.  

Example:  
	trace ab ordering.  => turns this on
	untrace ab value.   => turns this off

",[]).


help_trace_gen :- 
    format("
Tracing Game Generation (gen) 
-----------------------------
	goals:  info  goal generation
	simplify:  info on goal simplification
        subsume:  info on goal redundancy checking and elimination
	pieces: info on piece generation.

Example:  
	trace gen pieces.      => turns this on
	untrace gen subsume.   => turns this off
",[]).
 

%----------------------------------------
% Help from accept_move menu
%----------------------------------------

help_accept :- 
    format("
You can accept this choice, backtrack, or abort:

yes.  (y)       => accept this choice
next. (n,no)    => reject this choice and consider next (if any)
abort.          => accept no choices, go back to menu
help.           => show this list
",[]).


