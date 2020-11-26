
:- assertz(user:file_search_path(oaa,'$OAA_HOME/src/oaalib/prolog')).
:- assertz(user:library_directory(oaa(''))).

:- use_module( library(com_tcp) ).
:- use_module(library(oaa)).
:-use_module(library(system)).


solvable(setTTSPid(_)).
solvable(killAllTTS).
init:-
	com_Connect( parent, [], _, _),
	oaa_Register( parent, tts_killer_agent, [], []),
	oaa_RegisterCallback(app_do_event, solve_local),
	( setof(S,solvable(S),Ss) ; Ss = []),
	oaa_Declare(Ss, [], [], [if_exists(overwrite)], _),
	oaa_MainLoop( true ).

solve_local( setTTSPid(Pid),_):-
	assert(process_id(Pid)).

solve_local(killAllTTS,_):-
	setof(Pid,process_id(Pid),Pids),
	killall(Pids),
	retractall(process_id(_)).

killall([]).
killall([Pid|Pids]):-
	on_exception(_,
		     kill(Pid,9),
		     true),
	killall(Pids).
	