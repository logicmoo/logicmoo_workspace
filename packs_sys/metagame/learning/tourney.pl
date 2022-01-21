%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%% tourney.pl
%================================================================================
% Players for a tournament
%================================================================================

% HOSTTIME_RANDOMIZE
% A hacky and expensive way to use the real time
% and name of the host to initalize the random seed,
% for when experiments are done on multiple machines.   
hosttime_randomize :- 
	hosttime_random_count_mod(X),
	dotimes(X,(\+ (random(_),fail))).

hosttime_random_count(Count) :- 
	realtime_seconds(S),
	shell(['echo $$'],Process),
	Sum is S+Process,
	random(1,Sum,Count).

hosttime_random_count_mod(X) :- 
	hosttime_random_count(Count),
	X is Count mod 200.

	



		      
do_tourney(Name,File) :-
	tourney_setting(Name,Set),
	tourney_to_file(File,Set).


% Start with a different random seed for each process running a tourney
% to avoid duplication.
tourney_to_file(File,GameMatches) :-
	hosttime_randomize,
	with_output_file(File,append,tourney(GameMatches,_LogFile)),
	    format("Tourney Done!~n",[]).

% File 2nd arg not used now. Perhaps for log file later. 
tourney([],_) :- 
	format("Tourney done!~n",[]), 
	current_output(O),
	flush_output(O).
tourney([Game-Matches|Rest],File) :-
	game_matches(Game,Matches,File),
	tourney(Rest,File).
	
game_matches(Game,Matches,_File) :-
	setup_game(Game),
	play_matches(Matches).

setup_game(Game) :- 
	load_game(Game),
	evalfile_top(static),
	build_tables.
	


play_matches([]).
play_matches([M|Matches]) :-
	play_match(M),
	play_matches(Matches).


play_match(match(White,Black,Number)) :-
	load_player(player,White),
	load_player(opponent,Black),
	dotimes(Number,contest(White,Black)).


% Sets the params and search method for a numbered player. 
load_player(Role,Name) :-
	player_setting(Name,Search,Params),
	set_role_file(Role,Params),
	player_color(Role,Color),
	player_top(Color,Search).
	
contest(White,Black) :-
	format("<~p> vs. <~p>~n",[White,Black]),
	record_seed,
	write_old_seed('%%'),
	start,
        process_contest_outcome(White,Black).

process_contest_outcome(White,Black) :- 
	player_current_game(G),
	game_name(G,GameName), 
	( recorded_game_outcome(Role) ->
           process_contest_outcome(Role,White,Black,Outcome),
	   format("Outcome of <~p> -- <~p> vs. <~p>: <~p>~n",[GameName,White,Black,Outcome])
	;  format("Outcome of <~p> -- <~p> vs. <~p>: <~p>~n",[GameName,White,Black,'NOTHING'])
	).

process_contest_outcome(player,White,_Black,White).
process_contest_outcome(opponent,_White,Black,Black).
process_contest_outcome(draw,_,_,draw).
	  
	  



tourney_test :- tourney_setting(test,Set),
	tourney(Set).


tourney_details(g1,game1,thesis).
tourney_details(g2,game2,thesis).
tourney_details(g3,game3,thesis).
tourney_details(g4,game4,thesis).
tourney_details(g5,game5,thesis).

tourney_details(g123,game1,23).
tourney_details(g124,game1,24).
tourney_details(g134,game1,34).


tourney_details(g101,game1,01).
tourney_details(g102,game1,02).
tourney_details(g103,game1,03).
tourney_details(g104,game1,04).

tourney_details(g201,game2,01).
tourney_details(g202,game2,02).
tourney_details(g203,game2,03).
tourney_details(g204,game2,04).

tourney_details(g301,game3,01).
tourney_details(g302,game3,02).
tourney_details(g303,game3,03).
tourney_details(g304,game3,04).

tourney_details(g401,game4,01).
tourney_details(g402,game4,02).
tourney_details(g403,game4,03).
tourney_details(g404,game4,04).


tourney_details(g501,game5,01).
tourney_details(g502,game5,02).
tourney_details(g503,game5,03).
tourney_details(g504,game5,04).



tourney_details(g423,game4,23).
tourney_details(g424,game4,24).
tourney_details(g434,game4,34).



% playing random player against everyone on every game
% separately, 10 games each color. 
tourney_details(g1r0,game1,r0).
tourney_details(g1r1,game1,r1).
tourney_details(g1r2,game1,r2).
tourney_details(g1r3,game1,r3).
tourney_details(g1r4,game1,r4).

tourney_details(g2r0,game2,r0).
tourney_details(g2r1,game2,r1).
tourney_details(g2r2,game2,r2).
tourney_details(g2r3,game2,r3).
tourney_details(g2r4,game2,r4).

tourney_details(g3r0,game3,r0).
tourney_details(g3r1,game3,r1).
tourney_details(g3r2,game3,r2).
tourney_details(g3r3,game3,r3).
tourney_details(g3r4,game3,r4).

tourney_details(g4r0,game4,r0).
tourney_details(g4r1,game4,r1).
tourney_details(g4r2,game4,r2).
tourney_details(g4r3,game4,r3).
tourney_details(g4r4,game4,r4).

tourney_details(g5r0,game5,r0).
tourney_details(g5r1,game5,r1).
tourney_details(g5r2,game5,r2).
tourney_details(g5r3,game5,r3).
tourney_details(g5r4,game5,r4).


tourney_details(g1r0a,game1,r0a).

tourney_details(g1r1a,game1,r1a).

tourney_details(g1r2a,game1,r2a).

tourney_details(g1r3a,game1,r3a).

tourney_details(g1r4a,game1,r4a).

tourney_details(g2r02,game1,r02).

tourney_details(g3r4a,game3,r4a).

tourney_details(g5a,game5,thesis2).



matches_for_setting(r02,
	[ match(r,0,2)]).


matches_for_setting(r0a,
	[ match(r,0,10)]).

matches_for_setting(r1a,
	[ match(r,1,10)]).

matches_for_setting(01,
	[ match(0,1,10)]).
matches_for_setting(02,
	[ match(0,2,10)]).
matches_for_setting(03,
	[ match(0,3,10)]).
matches_for_setting(04,
	[ match(0,4,10)]).


matches_for_setting(r2a,
	[ match(r,2,10)]).


matches_for_setting(r3a,
	[ match(r,3,10)]).


matches_for_setting(r4a,
	[ match(r,4,10)]).


matches_for_setting(thesis,
	[ match(0,1,1),
	  match(1,0,1),
	  match(0,2,1),
	  match(2,0,1),
	  match(0,3,1),
	  match(3,0,1),
	  match(0,4,1),
	  match(4,0,1),
	  match(1,2,1),
	  match(2,1,1),
	  match(1,3,1),
	  match(3,1,1),
	  match(1,4,1),
	  match(4,1,1),
	  match(2,3,1),
	  match(3,2,1),
	  match(2,4,1),
	  match(4,2,1),
	  match(3,4,1),
	  match(4,3,1),
	  match(0,0,2)
	]).


matches_for_setting(thesis2,
	[ 
	  match(4,3,1),
	  match(3,4,1),
	  match(4,2,1),
	  match(2,4,1),
	  match(2,3,1),
	  match(3,2,1),
	  match(4,1,1),
	  match(1,4,1)
	]).


matches_for_setting(23,
	[ match(2,3,2),
	  match(3,2,2)]).

matches_for_setting(24,
	[ match(2,4,2),
	  match(4,2,2)]).


matches_for_setting(34,
	[ match(3,4,2),
	  match(4,3,2)]).


matches_for_setting(r0,
	[ match(0,r,10),
	  match(r,0,10)]).

matches_for_setting(r1,
	[ match(1,r,10),
	  match(r,1,10)]).

matches_for_setting(r2,
	[ match(2,r,10),
	  match(r,2,10)]).

matches_for_setting(r3,
	[ match(3,r,10),
	  match(r,3,10)]).

matches_for_setting(r4,
	[ match(4,r,10),
	  match(r,4,10)]).



tourney_setting(Tourney,[Game-Matches]) :-
	tourney_details(Tourney,Game,MatchName), !,
	matches_for_setting(MatchName,Matches).

tourney_setting(test,
	[checkers-[   match(1,0,1),
		      match(0,0,1)],
         turncoat_chess-[ match(0,0,1)]]
		   ).



% Just a random player. 
player_setting(r,random,
	[]).


% Just a random_aggressive player. 
player_setting(0,random_aggressive,
	[]).


% Everything, and pthreat.
player_setting(1,iterate,
	[gmovmob-1,
	 gcapmob-1,
	 pthreat-1,
	 gthreat-0,
	 initprom-1,
	 possess-1,
	 arrive_distance-100,
	 promote_distance-1,
	 eventual_mobility-1,
	 static-1,
	 vital-1,
	 %% statics
	 max_static_mob-1,
	 max_eventual_mob-1,
	 eradicate-1,
	 victims-1,
	 immunity-1,
	 giveaway-1,
	 eradicate-1,
	 stalemate-1,
	 arrive-1
	]).



% Just emob, promdist, arrivedist, and static.
player_setting(2,iterate,
	[gmovmob-0,
	 gcapmob-0,
	 pthreat-0,
	 gthreat-0,
	 lthreat-0,
	 vital-0,
	 initprom-1,
	 possess-1,
	 arrive_distance-100,
	 promote_distance-1,
	 eventual_mobility-1,
	 static-1,
	 %% statics
	 max_static_mob-1,
	 max_eventual_mob-1,
	 eradicate-1,
	 victims-1,
	 immunity-1,
	 giveaway-1,
	 eradicate-1,
	 stalemate-1,
	 arrive-1
	]).



% Just emob and static.
player_setting(3,iterate,
	[gmovmob-0,
	 gcapmob-0,
	 pthreat-0,
	 gthreat-0,
	 lthreat-0,
	 vital-0,
	 initprom-0,
	 possess-0,
	 arrive_distance-0,
	 promote_distance-0,
	 eventual_mobility-1,
	 static-1,
	 %% statics
	 max_static_mob-1,
	 max_eventual_mob-1,
	 eradicate-1,
	 victims-1,
	 immunity-1,
	 giveaway-1,
	 eradicate-1,
	 stalemate-1,
	 arrive-1
	]).



% Just promdist and arrivedist
player_setting(4,iterate,
	[gmovmob-0,
	 gcapmob-0,
	 pthreat-0,
	 gthreat-0,
	 lthreat-0,
	 vital-0,
	 initprom-0,
	 possess-0,
	 arrive_distance-100,
	 promote_distance-1,
	 eventual_mobility-0,
	 static-0,
	 %% statics
	 max_static_mob-1,
	 max_eventual_mob-1,
	 eradicate-1,
	 victims-1,
	 immunity-1,
	 giveaway-1,
	 eradicate-1,
	 stalemate-1,
	 arrive-1
	]).



% Just dynamic mobility.
player_setting(5,iterate,
	[gmovmob-1,
	 gcapmob-0,
	 pthreat-0,
	 gthreat-0,
	 lthreat-0,
	 vital-0,
	 initprom-0,
	 possess-0,
	 arrive_distance-0,
	 promote_distance-0,
	 eventual_mobility-0,
	 static-0,
	 %% statics
	 max_static_mob-1,
	 max_eventual_mob-1,
	 eradicate-1,
	 victims-1,
	 immunity-1,
	 giveaway-1,
	 eradicate-1,
	 stalemate-1,
	 arrive-1
	]).




% Everything, and gthreat.
player_setting(6,iterate,
	[gmovmob-1,
	 gcapmob-1,
	 pthreat-0,
	 gthreat-1,
	 initprom-1,
	 possess-1,
	 arrive_distance-100,
	 promote_distance-1,
	 eventual_mobility-1,
	 static-1,
	 vital-1,
	 %% statics
	 max_static_mob-1,
	 max_eventual_mob-1,
	 eradicate-1,
	 victims-1,
	 immunity-1,
	 giveaway-1,
	 eradicate-1,
	 stalemate-1,
	 arrive-1
	]).


% Everything, and lthreat.
player_setting(7,iterate,
	[gmovmob-1,
	 gcapmob-1,
	 pthreat-0,
	 gthreat-0,
	 lthreat-1,
	 initprom-1,
	 possess-1,
	 arrive_distance-100,
	 promote_distance-1,
	 eventual_mobility-1,
	 static-1,
	 vital-1,
	 %% statics
	 max_static_mob-1,
	 max_eventual_mob-1,
	 eradicate-1,
	 victims-1,
	 immunity-1,
	 giveaway-1,
	 eradicate-1,
	 stalemate-1,
	 arrive-1
	]).


%==============================================================================
% Interface
%==============================================================================

playernum_top(Color,Name) :- 
	player_color(Role,Color),
	load_player(Role,Name).

playernums_top(Player,Opp) :- 
	playernum_top(white,Player),
	playernum_top(black,Opp).	
