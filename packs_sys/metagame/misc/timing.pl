%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% timing.pl
%% ---------
%% basic timing utilities
%%
%% runtime_once :- print the time to call a prolog goal the first time
%% realtime_once :- same for real time.
%% runtime :- print the time to solve a prolog goal (new times on backtracking)
%% realtime :- same for real time.
%% runtime_success :- returns the time to solve a prolog goal the first time.
%%                    fails if doesn't solve it.
%% realtime_success :- same for real time.
%% cumulative_time(+Tasks) :- print the cumulative time used to do the tasks
%%                            (in runtime).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded(library(aux)).
:- ensure_loaded(library(shells)).

% -----------------------------
% TIMING(G,Method,Time,Success)
% -----------------------------
timing(G,Method,Time,Success) :-
	time_stat(Method,T0), 
	( (call(G), Success = yes) ; 
          Success = no ),
	time_stat(Method,T1), 
        Time is T1-T0. 


% ------------------------
% TIMING(Goal,Method,Time)
% ------------------------
% Succeeds only when Success=yes.
% Formats result whether succeeds or not.
timing(Goal,Method,Time) :- 
	timing(Goal,Method,Time,Success),
	format("Success: ~w~n",[Success]),
	format('Time is ~3d sec.~n',[Time]),
	Success=yes.

realtime(G) :-
	timing(G,realtime,_).

runtime(G) :-
	timing(G,runtime,_).

runtimes(N,G) :- 
	runtime(dotimes(N,G)).

% --------------------------------
% TIMING_SUCCESS(Goal,Method,Time)
% --------------------------------
timing_success(Goal,Method,Time) :- 
	time_stat(Method,T0), 
	call(Goal), !,
	time_stat(Method,T1), 
        Time is T1-T0. 

runtime_success(Goal,Time) :- 
	timing_success(Goal,runtime,Time).

realtime_success(Goal,Time) :- 
	timing_success(Goal,realtime,Time).


% TIMING_ONCE(Goal,Method,Time)
% -----------------------------
% Times first call to goal and prints result.
% Always succeeds.
%
timing_once(Goal,Method,Time) :- 
	timing(Goal,Method,Time,Success), !,
	format("Success: ~w~n",[Success]),
	format('Time is ~3d sec.~n',[Time]).

runtime_once(G) :-
	timing_once(G,runtime,_).
realtime_once(G) :-
	timing_once(G,realtime,_).


% TIME_STAT(Type,Time)
% --------------------
% Type is either realtime or runtime.
% Result is in millisecs. 
time_stat(realtime,Time) :- 
	realtime_msec(Time). 
time_stat(runtime,Time) :- 
	statistics(runtime,[Time|_]). 

%----------------------------------------
% Getting real time
%----------------------------------------
% Returns current world Time in the format:
% Hour-Minute-Second
%
% REAL_TIME(Time)
% ---------------
% Returns the current time in real time.
real_time(Time) :- 
	shell([date,'+%H-%M-%S'],Time).

time_seconds(H-M-S,Sec) :- 
	Sec is S+60*M+3600*H.

time_msec(T,MSec) :- 
	time_seconds(T,Sec),
	MSec is 1000*Sec.

realtime_seconds(Sec) :- 
	real_time(T), time_seconds(T,Sec).

realtime_msec(Sec) :- 
	real_time(T), time_msec(T,Sec).



% REALTIME_RANDOMIZE
% A hacky and expensive way to use the real time
% to initalize the random seed.  
realtime_randomize :- 
	realtime_seconds(S),
	random(10,S,R),
	X is R mod 100,
	( dotimes(X,(random(_),fail)) ; true).
	


%=============================================================================
% Cumulative timing
%=============================================================================

cumulative_time(Tasks) :-  
	statistics(runtime,[T0|_]),
	cumulative_time(Tasks,T0,1).

cumulative_time([],_,_).
cumulative_time([Task|Tasks],T0,N) :-
	(Task->[Mark]=" " ; [Mark]="*"),
	statistics(runtime,[T|_]),
	CTime is T-T0,
	format('~p~8|~p~c~n', [N,CTime,Mark]),
	N1 is N+1,
	!,
	cumulative_time(Tasks,T0,N1).


%=============================================================================
% Waiting
%=============================================================================

wait_msecs(MSec) :-
	statistics(runtime,[T0|_]),
	T1 is T0 + MSec,
	wait_till_time(T1).

wait_till_time(T1) :-
	statistics(runtime,[TN|_]),
	(TN >= T1 -> true
	          ;  wait_till_time(T1)).

%==============================================================================
% Turning on and off TIMING parameter.
%==============================================================================
% Some routines call the predicate TIMING(Call) instead of call directly,
% which means call normally when timing mode off, else call runtime(Call).
% These are only useful for routines who measure REAL-TIME.

timing(Call) :- 
	timing, !,
	runtime(Call).
timing(Call) :- call(Call).

timing :- parameter(timing,on).

set_timing :- set_parameter(timing,on).
unset_timing :- set_parameter(timing,off).

