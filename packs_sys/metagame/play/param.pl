%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% param.pl -- default parameter values, and documentation
%%   (set for local interface)
%%
%% external routines:
%%   parameter(+Name,-Value)    :- retrieve a parameter value
%%
%% Note: the parameters for the generator are in gen_parameters.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parameters used by the workbench
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% dynamic so that this can be listed and changed
:- dynamic parameter/2.

%% value: 0 = off, >0 = more verbose.
%% description: how verbose to be in tracing system
%% used in: interface.pl, many other files.

parameter(verbosity,0).

%% value: {off, on}
%% description: whether to display timing information
%% used in: interface.pl

parameter(timing,on).


%% value: {ask,offer}
%% description: determines how a human will select moves.
%%  ask: ask human to enter moves in grammatical notation
%%  offer: backtrack through possible moves, until human accepts one.
%% used in: interface.pl

parameter(selection_method,ask).

%% value: {ask,random}
%% description: determines how random positions will be set up.
%%  ask: ask human to enter assignment in grammatical notation.
%%  random: generate a random assignment.
%% used in: local.pl

parameter(assignment_method,random).

%% value: {yes,no}
%% description:  whether to pause between moves when using local interface.
%%  yes: ask human whether to continue after each move
%%  no: continue play without pausing.
%% used in: local.pl

parameter(continuous,yes).


%% values: {human,<other>}
%% description:  move selection method for PLAYER
%% used in: interface.pl

parameter(player_method,human).

%% values: {human,<other>}
%% description:  move selection method for OPPONENT
%% used in: interface.pl

parameter(opponent_method,human).


%% values: {none,<File>}
%% description:  pre-move initialization file for PLAYER
%% used in: controller.pl

parameter(player_file,none).

%% values: {none,<File>}
%% description:  pre-move initialization file for OPPONENT
%% used in: controller.pl

parameter(opponent_file,none).


%% values: {on,off}
%% description:  whether to be verbose when finding interpretation
%% of communicated move strings in remote matches.
%% used in: interface.pl

parameter(verbose_interp,on).

%% values: {parsing,printing}
%% description:  whether parsing pure or for pretty-printing
%%   parsing: pure parsing and generating
%%   printing: just before generating strings which will
%%             then be pretty-printed.
%% used in: grammar.pl, notation.pl, interface.pl

parameter(parsing_mode,parsing).

%% values: {on,off}
%% description:  whether can use (a,1) notation to denote
%%               squares in move and game grammars
%%   on:   use (a,1) notation
%%   off:  use (1,1) notation
%% used in: grammar.pl, notation.pl, interface.pl, tokenizer.pl
%% In general, this param is off, except for humans 
%% entering moves requiring completion.  It could be
%% used to generate pretty initial assignments for games,
%% but CAUTION, it will not parse games or moves in the
%% other mode.

parameter(alpha_squares_mode,off).

%% values: {on,off}
%% description:  whether to ask the user to confirm choices
%%               (generally during move selection).
%%   on:  ask user to accept choice, consider next, or backtrack
%%   off: don't ask, just assume first choice is acceptable.
%% used in: interface.pl

parameter(confirm_choices,on).


%% values: {on,off}
%% description:  whether to try completing entered moves.
%%   on:  try completing
%%   off: only accept completely grammatical moves.
%% note: this can considerably slow down the interface.
%% used in: interface.pl

parameter(completions,on).


%% values: {on,off}
%% description:  whether to check safety of completed moves.
%%   on:  check safety
%%   off: allow any matching move. 
%% note: this can considerably slow down the interface.
%% used in: interface.pl

parameter(safety,off).


%% values: {on,off}
%% description:  whether to ask the user to confirm choices
%%               (generally during move selection).
%%   on:  compile symmetries when loading a game.
%%   off: don't compile symmetries when loading a game.
%% used in: parse1.pl

parameter(compile_symmetries,on).



% Some parameters for remote communications.
% Not used at the moment.
%parameter(player1_name,undefined).
%parameter(player2_name,undefined).
%parameter(player1_info,undefined).
%parameter(player2_info,undefined).
%parameter(server_name,undefined).
%parameter(server_port,undefined).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parameters used by search engines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% When using the RANDOM advisor, these determine
%% the range within which random evaluations are assigned,
%% when the RANDOM advisor is active.  
%% If the values are integers, only integers in the range will be chosen.
%% used in: value.pl

parameter(random_min,-1.0).
parameter(random_max,1.0).

%% value: {Integer}
%% description: For the alpha_beta player, sets fixed depth for search. 
%% used in: alphabeta.pl

parameter(depth,1).

%% value: {Integer}
%% description: Time for each player to play entire game 
%% (in milliseconds). 
%% used in: controller.pl, alphabeta.pl

%parameter(game_time_limit,180000).
parameter(game_time_limit,99999999).


%% value: {Integer}
%% description: Time for each player to play single move
%% (in milliseconds). 
%%
%% This is not an enforced constraint, but is used by players which
%% take note of it (like alpha-beta, iterate). 
%% used in: alphabeta.pl

parameter(move_time_limit,10000).

%% value: {Integer}
%% description: Number of moves a player should think it still has to
%% make in a given game, for time allocation purposes. 
%% Setting value to 1 means: assume this is the only move we have to make,
%% so use as much time as is available.  
%%
%% This is not an enforced constraint, but is used by players which
%% take note of it (like alpha-beta, iterate). 
%% used in: alphabeta.pl

parameter(move_horizon,1).


%% value: {random,fixed}
%% description: How to order successor moves for node expansion. 
%% random:  order them randomly.
%% fixed:   order them as produced by the move generator. 

parameter(ordering,random).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parameters used by strategic evaluation function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% See also the advisors defined in value.pl

% How to discount goals as a function of number of abstract 
% moves needed to achieve them. 
% inverse:  Val(Dist) = 1/(1+Dist)
% exponent: Val(Dist) = 1/(2^Dist)

parameter(discount,exponent).

% Max number of eradicate targets left before we consider each
% remaining one to be vital.  
% integer

parameter(vital_number,2).  

% Predict a piece would be N times as valuable when possessed as it would be
% on the board. 
% For example, in shogi we capture by possession, this must be more valuable
% than just removing the piece, as in chess. 

parameter(possess_offset,2).  

