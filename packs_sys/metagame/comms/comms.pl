%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%% comms.pl
%%% Deals with communications protocols, and starting remote matches. 


:- ensure_loaded(library(shells)).


%============================================================================
% Modifications to linda communications routines
%============================================================================

linda_client_wait(ServerAddr:PortNum) :-
	linda_client(ServerAddr:PortNum), !.
linda_client_wait(ServerAddr:PortNum) :-
	linda_client_wait(ServerAddr:PortNum).


in_wait(Pattern) :-
	format("~nWaiting to receive pattern: ~w~n",[Pattern]),
%	in(Pattern), !,
	in_wait_loop(Pattern), !,
	format("~nReceived pattern: ~w~n",[Pattern]).
	
% Keep waiting for TimeOut seconds, where linda:timeout(TimeOut),
% until get a pattern.
% Could have an ultimate time limit, after which we give up.
% Could also have a waiting period between.
in_wait_loop(Pattern) :-
	in_noblock(Pattern), !.
in_wait_loop(Pattern) :-
	in_wait_loop(Pattern).


rd_wait(Pattern) :-
	format("~nWaiting to observe pattern: ~w~n",[Pattern]),
%	rd(Pattern),
	rd_wait_loop(Pattern), !,
	format("~nObserved pattern: ~w~n",[Pattern]).
	
% Keep waiting for TimeOut seconds, where linda:timeout(TimeOut),
% until get a pattern.
% Could have an ultimate time limit, after which we give up.
% Could also have a waiting period between.
rd_wait_loop(Pattern) :-
	rd_noblock(Pattern), !.
rd_wait_loop(Pattern) :-
	rd_wait_loop(Pattern).

%============================================================================

remote_metagame(Addr,Args,Title) :-
	shell_rsh(Addr,'/homes/bdp/prolog/play/metagame',Args,Title).

%============================================================================


%play_match(any,innes,'/homes/bdp/prolog/play/randomist',barney,'/homes/bdp/prolog/play/instantist').
% play_match(any,rando,'/homes/bdp/prolog/play/randomist',instanto,'/homes/bdp/prolog/play/instantist').
% play_match(shoveller,rando,'/homes/bdp/prolog/play/randomist',instanto,'/homes/bdp/prolog/play/instantist').

human_file('/homes/bdp/prolog/play/humanist').
random_file('/homes/bdp/prolog/play/randomist').
instant_file('/homes/bdp/prolog/play/instantist').


play_human_match(Server,P1,P2) :-
	human_file(F),
	play_match(Server,P1,F,P2,F).

play_match(Server,Player1,Info1,Player2,Info2) :-
	make_server(Server,Player1,Info1,Player2,Info2).

make_server(Server,Player1,Info1,Player2,Info2) :-
	command_from_args([server,Server,':',Player1,vs,Player2],'_',Title),
	remote_metagame(Server,
	  [file,
	   '/homes/bdp/prolog/play/serve_tourney',
	   player1_name,  Player1,
	   player1_info,  Info1,
	   player2_name,  Player2,
	   player2_info,  Info2 ],
	   Title).

port_number(P,N) :- atom_to_number(P,N).

atom_to_number(A,N) :-
	name(A,N1),
	number_chars(N,N1).





