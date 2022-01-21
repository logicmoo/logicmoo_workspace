%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

% serve_tourney.pl

:- prolog_flag(redefine_warnings,_,off).

%================================================================================
% SERVER
%================================================================================
%
% SERVER prolog process will be called with command like:
%   metagame
%     'file'
%     this file (serve_tourney)
%     player1  NAME INFO1a .. INFO1n
%     player2  NAME INFO2a .. INFO1n
% For now just assume info1 is an atom with all the nec. info (like a filename?).

% Redefine METAGAME here, so automatically start serving upon startup.

metagame :- serve_tourney.

serve_tourney :-
	find_players(Player1,Info1,Player2,Info2),
	serve_tourney(Player1,Info1,Player2,Info2).

find_players(Player1,Info1,Player2,Info2) :-
	parameter(player1_name,Player1),
	parameter(player1_info,Info1),
	parameter(player2_name,Player2),
	parameter(player2_info,Info2).

serve_tourney(Player1,Info1,Player2,Info2) :-
	use_module(library('linda/server')),
%	nl, write('just used server'), nl,
	greet_players(Player1,Player2),
	nl, write(' I am your friendly Linda Server!'), nl,
%	linda((Addr-format('Server address is ~q~n',[Addr]))).
	linda((Addr-(user:create_tourney(Addr,Player1,Info1,Player2,Info2)))).

greet_players(P1,P2) :- 
	format("~nHello, player1: ~w, player2: ~w~n",[P1,P2]).


create_tourney(Addr,Player1,Info1,Player2,Info2) :-
	format("Server address is ~q~n",[Addr]),
	start_player(Addr,Player1,Info1),
	wait_msecs(45000), % wait 45 seconds
	start_ref(Addr,Player1,Player2),
	wait_msecs(45000), % wait 45 seconds
	start_player(Addr,Player2,Info2).


%good_addr(any).
good_addr(shoveller).


start_ref(ServerAddr:Port,Player1,Player2) :-
	good_addr(Slave),
	command_from_args([ref,ServerAddr,':',Player1,vs,Player2],'_',Title),
	remote_metagame(Slave,
	  [server_name, ServerAddr,
	   server_port, Port,
	   player1_name, Player1,
	   player2_name, Player2,
	   file,'~/prolog/play/referee'],
	   Title).

start_player(ServerAddr:Port,Player,MoveFile) :-
	good_addr(Slave),
	command_from_args([ServerAddr,':',Player,MoveFile],'_',Title),
	remote_metagame(Slave,
	 [ server_name, ServerAddr,
	   server_port, Port,
	   player1_name, Player,
	   file, MoveFile,
	   file,'~/prolog/play/player'],
	 Title).





