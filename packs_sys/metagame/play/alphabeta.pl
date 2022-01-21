%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%% alphabeta.pl

%This is the hottest version yet:
%1. alpha-beta
%2. counts nodes evaluated
%3. time cutoffs, chooses best move up to cutoff.
%4. iterative deepening
%5. principal continuation heuristic
%6. plays forced moves immediately
%7. either default or random candidate move-ordering
%8. stops when forced win for a player (should do draws also)
%   - outputs win in N-ply message!
%
%Thus, there are no more (well, few) magic parameters here, and program
%can now play lots of games very nicely!

%================================================================================
% IMPLEMENTED PLAYERS
%================================================================================
% The following players (x_choose) are selectable from the 
% interface as players in their own right (from the top-level menu).
% They can also be called as move selectors (x_com) to assist a human in making
% a move, from the move-menu.
% The core routines supporting these procedures are called (x_move).
% Full documentation is made with these core routines. 

%========================================
% ALPHA_BETA_CHOOSE(Player,Move,SIn,SOut)
%========================================
% Plays move with highest minimaxed
% value, using the current evaluation function.
% Searches to a fixed depth determined by the parameter: DEPTH.
% 
% Uses whatever move ordering parameter ORDERING is set to.

alpha_beta_choose(Player,Move,SIn,SOut) :-
	toggle_weights_choose(alpha_beta_move,Player,Move,SIn,SOut).

%========================================
% ITERATE_CHOOSE(Player,Move,SIn,SOut)
%========================================
% Plays move with highest iterated minimaxed
% value, using the current evaluation function.
% Uses whatever move ordering parameter is set to.

iterate_choose(Player,Move,SIn,SOut) :-
	toggle_weights_choose(iterate_move,Player,Move,SIn,SOut).

%========================================
% ITERATE_RANDOM_CHOOSE(Player,Move,SIn,SOut)
%========================================
% Plays move with highest iterated minimaxed
% value, using the current evaluation function.
% Uses random move ordering heuristic.

iterate_random_choose(Player,Move,SIn,SOut) :-
	toggle_weights_choose(iterate_random_move,Player,Move,SIn,SOut).

%========================================
% ITERATE_FIXED_CHOOSE(Player,Move,SIn,SOut)
%========================================
% Plays move with highest iterated minimaxed
% value, using the current evaluation function.
% Uses fixed move ordering heuristic (use order from generator).

iterate_fixed_choose(Player,Move,SIn,SOut) :-
	toggle_weights_choose(iterate_fixed_move,Player,Move,SIn,SOut).


%=================================================
% TOGGLE_WEIGHTS_CHOOSE(Type,Player,Move,SIn,SOut)
%=================================================
% Sets alpha-beta parameters for Player (if any), 
% then calls the Type of search method with these weights, 
% finally (whether successful or not) sets them back to their original values. 
%
% This is *not* a player in itself, just a support function for 
% players based on alpha-beta which use these weights. 
%
toggle_weights_choose(Type,Player,Move,SIn,SOut) :-
	Goal =.. [Type,Move,SIn,SOut],
	( toggle_alpha_beta_weights(Player,Old) ->
	  ( timing(Goal) ->
	    print_choice(Move,SIn,SOut),
	    set_parameter(weights,Old)
	  ; set_parameter(weights,Old),
	    fail
	  )
	; ( timing(Goal) ->
	    print_choice(Move,SIn,SOut)
	  )
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parameters
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ab_depth(D) :- parameter(depth,D).

% Not used. 
bound(B) :- parameter(bound,B).

move_time_limit(T) :- parameter(move_time_limit,T).

move_horizon(T) :- parameter(move_horizon,T).

% Within what ranges we want our values.
approx_window(A,B) :-
	value_of_outcome(player,B),
	value_of_outcome(opponent,A).

reset_alpha_params :- 
	set_parameter(depth,1),
	set_parameter(move_time_limit,10000),
	set_parameter(move_horizon,1),
	set_parameter(ordering,random).
%	initialize_weights.


%================================================================================
% Time Management
%================================================================================

% ==============================================
% SEARCH_TIMEOUT_TIME(+StartTime,State,-EndTime)
% StartTime is the time this search started.
% State is the current position (in which P is the player to move)
% EndTime is the time we will stop the current search.
% ==============================================
%
% GAME_TIME_LEFT: The total amount of time remaining to play all our moves.
% MOVE_TIME_LIMIT: The max amount of time we can spend on this move.
% Can be infinite if no move-time-limit, or can give a suggested limit,
% or an enforced limit if these are the rules of the tournament. 
% ESTIMATED_MOVES_REMAINING: An estimate of the number of moves we will
% still need to play this game.  This can be determined in several ways:
% 1. If we are near the N-move-rule for ending the game, we can use this
%    number.
% 2. We can assume there is always some K moves left to play (like 30 moves).
%    This leads to spending more time in the early moves and speeding up 
%    later. 
% 3. This can be learned from past games, possibly context-dependent.
%    a. Just observe total game lengths, use that number for all positions.
%    b. Learn to estimate based on features of a position. 
%
% The result of the whole function is as follows:
%   Allocates for this move the avg. amount of time needed
%   to survive the number of moves we will need to play this game.
%   If this time exceeds the move_time_limit, just uses that. 
%
search_timeout_time(StartTime,SIn,EndTime) :- 
	control(P,SIn),
	game_time_left(P,GameLeft),
	move_time_limit(MoveLimit),
	estimated_moves_remaining(MovesLeft,SIn),
	search_timeout_time(StartTime,GameLeft,MovesLeft,MoveLimit,EndTime).

search_timeout_time(StartTime,GameLeft,MovesLeft,MoveLimit,EndTime) :- 
	Avg is GameLeft / MovesLeft,
	TimeAvail is min(MoveLimit,Avg),
	EndTime is StartTime + TimeAvail.

% Could be a more complicated procedure.
% For now use parameter: move_horizon.
%
%estimated_moves_remaining(30,_).
estimated_moves_remaining(Horizon,_) :- 
	move_horizon(Horizon).


% Ensures End is a number.  If not, time will not be checked.
% This is a way to disable the time checking! 
timeout_for_node(Node,End) :- 
%	primary_choice_node(Node), 
	tracing_ab(timing,print_timeout_check(Node)),
	number(End),
	current_time(Time),
	Time >= End,
	Diff is Time - End, 
	print_timeout_message(Diff).



%timeout_for_node([],Node,_) :- 
%	tracing_ab_format("Called to check timeout, but no other nodes~n",[]).
timeout_for_node([Node|_Rest],_BestNode,End) :- 
	timeout_for_node(Node,End).


print_timeout_check(Node) :- 
	format("Testing for timeout~n",[]),
	node_move(Node,Move),
	print_move(Move).


print_timeout_message(Diff) :- 
	tracing_ab_format(timing,"Out of Time by <~p> msec~n",[Diff]). 


current_time(T0) :- 
	statistics(runtime,[T0|_]). 

% An old function which ignores the limit on total game time.
search_timeout_time(EndTime) :- 
	move_time_limit(Limit),
	current_time(T0),
	EndTime is T0 + Limit.

%================================================================================

% ==================================
% ITERATE_FIXED_MOVE(Move,SIn,SOut)
% ==================================
% Uses fixed move ordering as provided by move generator. 
iterate_fixed_move(Move,SIn,SOut) :- 
	change_parameter(ordering,Ord,fixed),
	iterate_move(Move,SIn,SOut),
	change_parameter(ordering,_,Ord).

% ==================================
% ITERATE_RANDOM_MOVE(Move,SIn,SOut)
% ==================================
% Uses the random_ordering heuristic with iterate_move.
%
iterate_random_move(Move,SIn,SOut) :- 
	change_parameter(ordering,Ord,random),
	iterate_move(Move,SIn,SOut),
	change_parameter(ordering,_,Ord).

% ===========================
% ITERATE_MOVE(Move,SIn,SOut)
% ===========================
% If there is only one legal move, plays it immediately.
% Else, does iterative deepening alpha-beta search until out of
% time. Then plays the move selected, or the first move available
% if the search had no time to find anything at all. 
%
% Uses whatever move ordering is currently set (parameter: ordering).
iterate_move(Move,SIn,SOut) :- 
	current_time(StartTime),
	search_timeout_time(StartTime,SIn,EndTime),
	initialized_start_node(SIn,NodeIn),
	instant_move(M,SIn,SFirst),  % in advisors.pl
	( forced_move(M,SIn) 
	-> accept_forced_move(M,SFirst,Move,SOut)
	;  ( timing(iterate(1,StartTime,EndTime,NodeIn,NodeOut)),
	     nonvar(NodeOut)
	   )
	-> initialized_choice_node(SOut,NodeOut,Move)
	;  accept_rushed_move(M,SFirst,Move,SOut)
	).

% Accepting a forced or rushed move means printing a notice to this effect,
% and equating the input and output moves and states.
accept_forced_move(M,S,M,S) :- 
	print_forced_notice.   % in advisors.pl

accept_rushed_move(M,S,M,S) :- 
	print_rushed_notice.   % in advisors.pl

% A move is forced if all legal moves are identical to it. 
forced_move(Move,SIn) :- 
	(\+ ( legal(Move2,SIn,_S2), \+ Move = Move2)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	      Iterative-Deepening Alpha-Beta algorithm.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%================================================
% ITERATE(Depth,StartTime,EndTime,NodeIn,NodeOut)
%================================================
% Calls alpha beta with the current max depth, then 
% considers whether to iterate deeper or return the 
% best move found so far.  

iterate(Depth,StartTime,EndTime,NodeIn,NodeOut) :-
	find_advice_tables(Tables), 
	iterate(Depth,StartTime,EndTime,NodeIn,NodeOut,Tables).


iterate(Depth,StartTime,EndTime,NodeIn,NodeOut,Tables) :-
	approx_window(Alpha,Beta),
%	timing(alphabeta(Depth,Counts,EndTime,NodeIn,Alpha,Beta,Node1,Val,_)),
	tracing_ab_timing(iteration,alphabeta(Depth,Counts,EndTime,NodeIn,Alpha,Beta,Node1,Val,_,Tables)),
	tracing_ab_format(value,"Searching at depth <~p>: ~n",[Depth]),
	tracing_ab_format(value,"The best move found has a value of: ~p~n",[Val]),
	tracing_ab(resources,print_resource_consumption(Counts)),
	tracing_ab(value,print_pc_info(NodeIn)),
	iterate_deeper(Depth,StartTime,EndTime,Val,NodeIn,Node1,NodeOut,Tables).


%=================================================================
% ITERATE_DEEPER(Depth,StartTime,EndTime,Val,NodeIn,Node1,NodeOut)
%=================================================================
% If one player has a forced win, don't look any further.
% If not enough time for another iteration, just use the 
% best move we've found on the prev. iteration. 
% If still enough time, use pc to start anouther depth.
% 
% Would be nice to have a clause for draws, but can't tell if a draw
% or even on an evaluation fn. 
iterate_deeper(Depth,_StartTime,_EndTime,Val,_NodeIn,Node1,Node1,_Tables) :- 
	win_detected(Val,Depth), !.
iterate_deeper(Depth,StartTime,EndTime,_,_NodeIn,Node1,Node1,_Tables) :- 
	not_enough_time(Depth,StartTime,EndTime), !.
iterate_deeper(Depth,StartTime,EndTime,_,NodeIn,_,NodeOut,Tables) :- 
	node_cont(NodeIn,PC),
	node_state(NodeIn,SIn),
	pc_moves(PC,Moves),
	initialized_start_node(SIn,Node1),
	node_pc(Node1,Moves),
	Depth1 is Depth + 1,
	iterate(Depth1,StartTime,EndTime,Node1,NodeOut,Tables).

win_detected(Val,_Depth) :-
	var(Val), !,
	tracing_ab_format(value,"No useful info this iteration.~n",[]). 
win_detected(Val,Depth) :-
	player_role(Player),
	value_of_outcome(Player,Val), !,
	tracing_ab_format(value,"A win is detected for <~p> in <~p> ply!~n",[Player,Depth]).


% Could use a more sophisticated version below, to avoid
% even starting when there's clearly not enough time to
% do a full ply.  But with the PC heuristic, why not just
% keep on searching to see if we might have been wrong? 
not_enough_time(_Depth,_StartTime,EndTime) :- 
	current_time(Now),
	Now >= EndTime.
%not_enough_time(_Depth,StartTime,EndTime) :- 
%	current_time(Now),
%	TimeUsed is Now - StartTime,
%	TimeLeft is EndTime - Now,
%	TimeUsed > TimeLeft / 2.



%======================================
% ALPHA_BETA_MOVE(Move,SIn,SOut)
%======================================
% Searches to a fixed depth, based on the parameter: AB_DEPTH.

alpha_beta_move(Move,SIn,SOut) :- 
	ab_depth(Depth),
	alpha_beta_move(Depth,Move,SIn,SOut).

%======================================
% ALPHA_BETA_MOVE(Depth,Move,SIn,SOut)
%======================================
%
% Plays move with highest alpha_beta minimaxed
% value, using the current evaluation function.
% Searches according to the following parameters:
%
% BOUND (approx_window):  least optimistic value s.t. we take any move
% which is better than this for the player to move.
% (This is currently not used as a parameter, instead the bound is
% just win and loss values). 
% 
% DEPTH:  depth of tree at which we evaluate statically.
%
% If there is only one legal move, plays it immediately.
% Else, does alpha-beta search until reaches DEPTH or out of  time. 
% Then plays the move selected, or the first move available
% if the search had no time to find anything at all. 
%
% Uses whatever move ordering is currently set (parameter: ordering).
%
alpha_beta_move(Depth,Move,SIn,SOut) :-
	current_time(StartTime),
	search_timeout_time(StartTime,SIn,EndTime),
	initialized_start_node(SIn,NodeIn),
	instant_move(M,SIn,SFirst),  % in advisors.pl
	( forced_move(M,SIn) 
	-> accept_forced_move(M,SFirst,Move,SOut)
	;  ( timing(alpha_beta_iterate(Depth,_StartTime,EndTime,NodeIn,NodeOut)),
	     nonvar(NodeOut)
	   )
	-> initialized_choice_node(SOut,NodeOut,Move)
	;  accept_rushed_move(M,SFirst,Move,SOut)
	).


alpha_beta_iterate(Depth,_StartTime,EndTime,NodeIn,NodeOut) :- 
	approx_window(Alpha,Beta),
	tracing_ab_timing(iteration,
	   alphabeta(Depth,Counts,EndTime,NodeIn,Alpha,Beta,NodeOut,Val,_)),
	tracing_ab_format(value,"Searching at depth <~p>: ~n",[Depth]),
	tracing_ab_format(value,"The best move found has a value of: ~p~n",[Val]),
	tracing_ab(resources,print_resource_consumption(Counts)),
	tracing_ab(value,print_pc_info(NodeIn)).

/*
alpha_beta_move(Depth,Move,SIn,SOut) :-
	current_time(StartTime),
	search_timeout_time(StartTime,SIn,EndTime),
	approx_window(Alpha,Beta),
	initialized_search_nodes(Move,SIn,NodeIn,SOut,NodeOut),
	tracing_ab_timing(iteration,
	   alphabeta(Depth,Counts,EndTime,NodeIn,Alpha,Beta,NodeOut,Val,_)),
	tracing_ab_format(value,"Searching at depth <~p>: ~n",[Depth]),
	tracing_ab_format(value,"The best move found has a value of: ~p~n",[Val]),
	tracing_ab(resources,print_resource_consumption(Counts)),
	tracing_ab(value,print_pc_info(NodeIn)).
*/


pc_moves([],[]) :- !.
pc_moves(Node,[Move|RestMoves]) :- 
	node_move(Node,Move),
	node_cont(Node,Rest),
	pc_moves(Rest,RestMoves).
	
% If want to print continuation INCLUDING this node, delete first line.
% PC is thus a sequence of positions which follow down to DEPTH,
% or just the current move with its value if this is a terminal position.

print_pc_info(Node) :-
	node_cont(Node,PC),
	pc_moves(PC,Moves),
	format("The principal continuation here is: ~n",[]),
	print_moves(Moves),
	nl.

print_moves([]).
print_moves([M|Moves]) :- 
	print_move(M),
	print_moves(Moves).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		      Core Alpha-Beta algorithm.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Based on algorithm in Bratko, p. 366.

% Start the search at depth 0, as the top position is the root.
%
% Uses the advice tables constructed already, as information for the evaluation
% function.  If players using different info are both using this shell, 
% their respective tables should be passed in to the procedure which uses
% tables explicitly. 
alphabeta(MaxDepth,Counts,EndTime,Node, Alpha, Beta, GoodNode, Val,Complete) :- 
	find_advice_tables(Tables),
	alphabeta(MaxDepth,Counts,EndTime,Node, Alpha, Beta, GoodNode, Val,Complete,Tables).

alphabeta(MaxDepth,Counts,EndTime,Node, Alpha, Beta, GoodNode, Val,Complete,Tables) :- 
   alphabeta(0,MaxDepth,Counts,EndTime,Node, Alpha, Beta, GoodNode, Val,Complete,Tables).


% alphabeta(Depth,MaxDepth,Counts,EndTime,Node, Alpha, Beta, GoodNode, Val,Complete,Tables)
% Depth is the current depth in the search (where the root position depth=0).
% MaxDepth is the maximum depth to search down to (where static eval positions).
% Node is a search tree node representing a position.
% Best move from Node leads to a node GoodNode, with minimax 
% value Val.
% Complete is yes if Node was complete, no otherwise.

% Search stops when one of the following is true:
% 1. The node is a terminal position.
% 2. We have gone down Depth ply, so the current depth counter is 0.
% 3. We are out of time.

% When we evaluate, we'll here do so always from perspective of 
% PLAYER.  Thus, player likes positions where this is maximized,
% opponent likes these minimized.
%
% Note that when we check min_to_move(Pos), this tells us
% that the MAX player is the parent, who is thus making the 
% choice.  So this could be rewritten:  choice_for_max(Pos).
%
% From now on, a POS is a NODE data structure, which contains
% a state and more info.  So all the procedures which here
% operator on POS's will now operate on these structures. 
%
% If run out of time before can find a value for this node,
% GoodNode=_Var, Val=_Var, Complete=no. 
% And node_complete(Node,Complete).  

alphabeta(_,_,Counts,End, Node,  _Alpha, _Beta, _GoodNode, _Val,no,_) :- 
	timeout_for_node(Node,End), !,   % ran out of time.
	node_complete(Node,no),
	zero_counts(Counts).

alphabeta(_,_,Counts,_End, Node, _Alpha, _Beta, _GoodNode, Val,yes,Tables) :- 
	terminal_node(Node,Val,Tables), !,
	node_complete(Node,yes),
	node_cont(Node,[]),
	terminal_counts(Counts).
	
alphabeta(Max,Max,Counts,_End, Node,  _Alpha, _Beta, _GoodNode, Val,yes,Tables) :- !,
	eval_node(Node,Val,Tables),     % At max depth, evaluate
	node_complete(Node,yes),
	static_counts(Counts).

% So here we are not at a terminal Node.
% And not out of time.
% Find the best of the available moves at the next depth.
alphabeta(Depth,MaxDepth,Counts,EndTime,Node, Alpha, Beta, GoodNode, Val,Complete,Tables) :- 
  Depth < MaxDepth,
  Depth1 is Depth + 1,
  expand_node(Node,NodeList,Tables),  % Use heuristics to find ordered moves
  node_cont(Node,GoodNode),
%  format("Calling bbest from ab~n",[]),
  boundedbest(Depth1,MaxDepth,Counts1,EndTime,NodeList, Alpha, Beta, GoodNode, Val,Complete,Tables),
  close_node(Node,GoodNode,Val,Complete),
  node_complete(Node,Complete),
  add_expansion_counts(Counts1,Counts).



%============
% BOUNDEDBEST
%============
boundedbest(Depth,Max,Counts,EndTime,[Node|NodeList],Alpha,Beta,GoodNode,GoodVal,Complete,Tables) :-
%  format("Callng ab  from bbest~n",[]),
  alphabeta(Depth,Max,Counts1,EndTime,Node,Alpha,Beta,_,Val,Comp1,Tables),
%  format("About to call goodenough from bbest~n",[]),
  goodenough(Depth,Max,Counts2,EndTime,NodeList,Alpha,Beta,Node,Val,
             GoodNode,GoodVal,Comp1,Complete,Tables),
  sum_resource_counts(Counts1,Counts2,Counts).


%===========
% GOODENOUGH
%===========
% Finds a good enough position out of Poslist, in order
% to approximate the value of the parent.
% If the best value we've seen is already outside the alpha-beta
% window, we don't need consider any of these moves further,
% as they certainly will not be on the principal continuation.
% Otherwise, consider the moves successively (so long as we are
% still in the window), revise the window as necessary, and
% choose the best move.   
%
% I think the timeout check really has to come first here.
% Otherwise we might mistakenly use an incomplete value.

goodenough(_,_,Counts,_End,NodeList,Alpha,Beta,Node,Val,GoodNode,GoodVal,Comp,Comp,_Tables) :-
	seen_enough(NodeList,Alpha,Beta,Node,Val,GoodNode,GoodVal,Comp), !,
	zero_counts(Counts).
goodenough(Depth,Max,Counts,End,NodeList,Alpha,Beta,Node,Val,GoodNode,GoodVal,yes,Complete,Tables) :-
  newbounds_node(Alpha,Beta,Node,Val,NewAlpha,NewBeta), % refine bounds
%  format("Calling bbest from goodenough~n",[]),
  boundedbest(Depth,Max,Counts,End,NodeList,NewAlpha,NewBeta,Node1,Val1,Complete,Tables),
  betterof_node(Complete,Depth,Max,Node,Val,Node1,Val1,GoodNode,GoodVal).


% SEEN_ENOUGH
% We've seen enough if there is nothing left to see, we're out of time,
% or the moves definitely not on the principal continuation.
seen_enough([],_,_,Node,Val,Node,Val,_Comp) :- !.    % no other candidate
seen_enough(_NodeList,_,_,Node,Val,Node,Val,no) :- !. % out of time
seen_enough(_NodeList,Alpha,Beta,Node,Val,Node,Val,yes) :-
  ( min_to_move_node(Node), Val >= Beta   % maximizer attained upper bound
  ; max_to_move_node(Node), Val =< Alpha   % minimizer attained lower bound
  ), !.


newbounds_node(Alpha,Beta,Node,Val,NewAlpha,NewBeta) :- 
	node_state(Node,Pos),
	newbounds(Alpha,Beta,Pos,Val,NewAlpha,NewBeta).

% Could modify here to store those moves whose val was eq 
% to alpha or beta, to get set of moves of = val.
newbounds(Alpha,Beta,Pos,Val,Val,Beta) :-
  min_to_move(Pos), Val > Alpha, !.   % maximizer increased lower bound
newbounds(Alpha,Beta,Pos,Val,Alpha,Val) :-
  max_to_move(Pos), Val < Beta, !.    % minimizer decreased upper bound
newbounds(Alpha,Beta,_,_,Alpha,Beta).



% BETTEROF_NODE(Complete,Depth,Max,Node,Val,Node1,Val1,NodeB,ValB)
% ================================================================
% If we didn't get a useful estimate for the second node, 
% throw it away and just return the first node immediately.
% To be useful, all the node's children must have been completed. 
% This was a new change, and finally got rid of all the bugs
% resulting from misusing or notusing incomplete searches.
%
betterof_node(no,_Depth,_Max,Node,Val,Node1,_Val1,Node,Val) :- 
	node_complete(Node1,no), !.
betterof_node(_Complete,_Depth,_Max,Node,Val,Node1,Val1,NodeB,ValB) :- 
	betterof_node(Node,Val,Node1,Val1,NodeB,ValB).

betterof_node(Node,Val,_Node1,Val1,Node,Val) :-
  min_to_move_node(Node), Val >= Val1, !;
  max_to_move_node(Node), Val =< Val1, !.
betterof_node(_,_,Node1,Val1,Node1,Val1).

max_to_move_node(Node) :- node_state(Node,Pos),	max_to_move(Pos).
min_to_move_node(Node) :- node_state(Node,Pos),	min_to_move(Pos).

max_to_move(Pos) :- control(player,Pos).
min_to_move(Pos) :- control(opponent,Pos).


%================================================================================
% Expanding and Closing Search Nodes
%================================================================================

% Use heuristics to find ordered moves
expand_node(Node,NodeList,Tables) :- 
     tracing_ab(expand,
       ( format("<Expanding> node from move: ",[]),
	 node_move(Node,Move),
         print_move(Move)
       ) ),
  ordered_moves(Node,NodeList,Tables). 

% Instead of returning a NodeList, could attach the pc to
% the Node which will here get ordered first.  Then won't
% have to look in successor positions to see if they're on the 
% pc. 
% Now doing this:  Take the successor node who's move is the move
% on the PC, if any, give it the rest of the PC and order it first.

ordered_moves(Node,NodeList,Tables) :-
  successor_nodes(Node, NodeList1,Tables),        % legal moves in Node produce Nodelist
  node_pc(Node,PC),
  ( ( nonvar(PC),  
      PC = [M|Moves]
    )
  -> 
     tracing_ab(ordering,
       ( format("Ordering PC highest~n",[]),
         print_moves(PC)
       ) ),
     search_node(PCNode),  
     node_pc(PCNode,Moves),
     node_move(PCNode,M),
     select(PCNode,NodeList1,NodeList2),
     NodeList = [PCNode|NodeList2]
  ;  NodeList = NodeList1
  ).


%===================================
% SUCCESSOR_NODES(Node,Nodes,Tables)
%===================================
% Nodes is a list of legal successor nodes (containing possible moves).
% The order is based on the parameter: ORDERING, as follows:
% a. random ==> randomly permute the list order 
% b. fixed  ==> use the list as initially generated.  
%
successor_nodes(Node,Nodes,Tables) :-
	( parameter(ordering,random)
	-> random_findall(Node2,successor_node(Node,Node2,Tables),Nodes)
	; parameter(ordering,fixed)
	-> findall(Node2,successor_node(Node,Node2,Tables),Nodes)
	; otherwise
	-> format("Error in successor_nodes/3: Invalid ordering parameter!!~n"),
	   fail
	).

successor_node(Node,Node2,Tables) :-
	node_state(Node,State),
	successor_pos(Move,State,State2,Tables),
	search_node(Node2),
	node_state(Node2,State2),
	node_move(Node2,Move),
	node_parent(Node2,Node).


% ----------------------------------------
% SUCCESSOR_POS(Move,State,State2,Tables)
% ----------------------------------------
% External predicate, backtracks over all successor
% STATE2 reachable from STATE.
% Uses evaluation tables. 
% Move should be a unique name of this transition. 



% Not used here anymore.
moves(State,States) :- 
	bagof(State2,Move^legal(Move,State,State2),States).
	

close_node(Node,GoodNode,Val,Complete) :- 
     tracing_ab(expand,
       ( format("<Closing> node from move: ",[]),
	 node_move(Node,Move),
         print_move(Move),
	 format("Resulting (~p-complete) value <~p> after move:~n",[Complete,Val]),
	 node_move(GoodNode,GoodMove),
         print_move(GoodMove),
	 nl
       ) ).

%================================================================================
% Static evaluation and terminal position detection
%================================================================================

terminal_node(Node,Val,Tables) :- 
	node_state(Node,Pos),
	terminal_pos_value(Pos,Val,Tables),
	tracing_ab(eval,
	 print_eval_info(Node,Val)).



eval_node(Node,Val,Tables) :-
	node_state(Node,Pos),
        staticval(Pos,Val,Tables),      
	tracing_ab(eval,
	 print_eval_info(Node,Val)),
	tracing_ab(state,
	  print_state(Pos)),
	tracing_ab(advice,
	 show_advices(Pos,Tables)).
	 

print_eval_info(Node,Val) :- 
	node_move(Node,Move),
	format("Evaluation <~p> for move: ",[Val]),
	print_move(Move), nl.



%-----------------------------------
% TERMINAL_POS_VALUE(Pos,Val,Tables)
%-----------------------------------
% External predicate.  Returns a Val for a Pos if it 
% is terminal.  

% This defined in value.pl
%terminal_game_outcome(Pos,Val) :-
%	game_outcome(Outcome,Pos),
%	value_of_outcome(Outcome,Val), !.


% VALUE_OF_OUTCOME(Winner,Value)
% 
value_of_outcome(draw,0).
value_of_outcome(player,100000).
value_of_outcome(opponent,-100000).


% -------------------------
% STATICVAL(Pos,Val,Tables)
% -------------------------
% Must be defined by external file, determines what 
% evaluation procedure will be used! 




%================================================================================
% NODE data structure
%================================================================================


%========================================
% Managing Search Nodes
%========================================

primary_choice_node(Node) :-
	node_parent(Node,Parent),
	node_parent(Parent,[]).

initialized_search_nodes(Move,SIn,NodeIn,SOut,NodeOut) :-
	search_node(NodeIn),
	node_state(NodeIn,SIn),
	node_parent(NodeIn,[]), % Set to be root node of tree
	search_node(NodeOut),
	node_state(NodeOut,SOut),
	node_move(NodeOut,Move).

initialized_start_node(SIn,NodeIn) :-
	search_node(NodeIn),
	node_state(NodeIn,SIn),
	node_parent(NodeIn,[]). % Set to be root node of tree

initialized_choice_node(SOut,NodeOut,Move) :-
	search_node(NodeOut),
	node_state(NodeOut,SOut),
	node_move(NodeOut,Move).


portray_node(node(State,_Parent,Move,_Cont,_PC)) :- 
	format("<Node from parent: ~p to state: ~p",['',State]),
	format(  "by move: ",[]), 
	print_move(Move),
	format(  "end node>~n",[]).

%:- add_portray(portray_node).


% Low-level node implementation.

search_node(node(State,Parent,Move,Cont,PC,Comp),State,Parent,Move,Cont,PC,Comp).
search_node(N) :- functor(N,node,6).
node_state(N,A) :- arg(1,N,A).
node_parent(N,A) :- arg(2,N,A).
node_move(N,A) :-  arg(3,N,A).
node_cont(N,A) :- arg(4,N,A).
node_pc(N,A) :- arg(5,N,A).
node_complete(N,A) :- arg(6,N,A).


%================================================================================
% RESOURCE data structure
%================================================================================

search_resource(N) :- functor(N,resource,3).
resource_expansions(N,A) :- arg(1,N,A).
resource_statics(N,A) :- arg(2,N,A).
resource_terminals(N,A) :- arg(3,N,A).


zero_counts(Counts) :- 
	search_resource(Counts),
	resource_expansions(Counts,0),
	resource_statics(Counts,0),
	resource_terminals(Counts,0).

terminal_counts(Counts) :- 
	search_resource(Counts),
	resource_expansions(Counts,0),
	resource_statics(Counts,0),
	resource_terminals(Counts,1).

static_counts(Counts) :- 
	search_resource(Counts),
	resource_expansions(Counts,0),
	resource_statics(Counts,1),
	resource_terminals(Counts,0).

expansion_counts(Counts) :- 
	search_resource(Counts),
	resource_expansions(Counts,1),
	resource_statics(Counts,0),
	resource_terminals(Counts,0).

add_expansion_counts(Counts1,Counts) :- 
	expansion_counts(Counts0),
	sum_resource_counts(Counts0,Counts1,Counts).
	
sum_resource_counts(Counts1,Counts2,Counts) :-
	resource_expansions(Counts1,Exp1),
	resource_statics(Counts1,Stat1),
	resource_terminals(Counts1,Term1),
	resource_expansions(Counts2,Exp2),
	resource_statics(Counts2,Stat2),
	resource_terminals(Counts2,Term2),
	Exp is Exp1+Exp2,
	Stat is Stat1+Stat2,
	Term is Term1+Term2,
	search_resource(Counts),
	resource_expansions(Counts,Exp),
	resource_statics(Counts,Stat),
	resource_terminals(Counts,Term).

print_resource_consumption(Counts) :- 
	resource_expansions(Counts,Exp),
	resource_statics(Counts,Stat),
	resource_terminals(Counts,Term),
	Total is Exp + Stat + Term,
	format("Number of nodes expanded: <~p>~n",[Exp]),
	format("Number of nodes statically evaluated: <~p>~n",[Stat]),
	format("Number of terminal nodes encountered: <~p>~n",[Term]),
	format("Number of terminal node tests:  <~p>~n",[Total]).


%================================================================================
% LIMIT data structure
%================================================================================

search_limit(N) :- functor(N,limit,4).
limit_depth(N,A) :- search_limit(N), arg(1,N,A).
limit_count(N,A) :- search_limit(N), arg(2,N,A).
limit_timeused(N,A) :- search_limit(N), arg(3,N,A).
limit_timeleft(N,A) :- search_limit(N), arg(4,N,A).


%================================================================================
% tracing execution of alphabeta routines
%================================================================================

% The following tracing modules are used in this file:
%	ordering:  info regarding move ordering heuristics
%	value:  info regarding value of moves found during search
%               also traces principal continuations 
%	resources: info regarding resource consumption during search
%       timing: info on timeout checks during search
%	iteration: info on each iteration of the search (currently just how long they took)
% 
% Each module can be set on/off, using set_ab_verbosity (see below), or 
% using trace_ab_<module>. 
%
% All can be turned off with silent_ab.

:- my_ensure_loaded(library(tracing)).

tracing_ab(Type,Call) :- 
	( tracing(ab(Type)) -> call(Call) ; true ).

% Might cause trouble later when want to use streams also.
tracing_ab_format(Type,String,Args) :- 
	( tracing(ab(Type))
	-> format(String,Args)
	; true 
	).

tracing_ab_timing(Type,Call) :- 
	trace_timing(ab(Type),Call).

set_ab_verbosity(Level,Status) :- set_tracing(ab(Level),Status).

silent_ab :- all_ab(off).
loud_ab :- all_ab(on).

all_ab(Status) :- 
	set_ab_verbosity(ordering,Status), 
	set_ab_verbosity(value,Status), 
	set_ab_verbosity(eval,Status), 
	set_ab_verbosity(expand,Status), 
	set_ab_verbosity(resources,Status), 
	set_ab_verbosity(timing,Status), 
	set_ab_verbosity(iteration,Status).


trace_ab_expand :- set_ab_verbosity(expand,on). 
trace_ab_eval :- set_ab_verbosity(eval,on). 
trace_ab_value :- set_ab_verbosity(value,on). 
trace_ab_ordering :- set_ab_verbosity(ordering,on). 
trace_ab_resources :- set_ab_verbosity(resources,on). 
trace_ab_timing :- set_ab_verbosity(timing,on). 
trace_ab_iterations :- set_ab_verbosity(iteration,on).

%:- trace_ab_value.
%:- silent_ab.

%================================================================================
% Interface
%================================================================================

alpha_beta_com(Move,SIn,SOut) :- 
	timing(alpha_beta_move(Move,SIn,SOut)),
	select_move(Move,SIn,SOut).
	
alpha_beta_com(Move,SIn,SOut,Depth) :- 
	timing(alpha_beta_move(Depth,Move,SIn,SOut)), 
	select_move(Move,SIn,SOut).

iterate_com(Move,SIn,SOut) :- 
	timing(iterate_move(Move,SIn,SOut)), 
	select_move(Move,SIn,SOut).

iterate_random_com(Move,SIn,SOut) :- 
	timing(iterate_random_move(Move,SIn,SOut)), 
	select_move(Move,SIn,SOut).

iterate_fixed_com(Move,SIn,SOut) :- 
	timing(iterate_fixed_move(Move,SIn,SOut)), 
	select_move(Move,SIn,SOut).

