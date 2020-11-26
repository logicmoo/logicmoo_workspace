/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:- module(ctc_time, [
	time/2,
	performance/4,
	performance/3,
	performanceUnique/4,
	ctc_time/3,
	ctc_time/2
]).

:- use_module(logging).
:- use_module(count).

:- module_transparent time/2.    
  
time(Goal0,time(UsedInf, UsedTime, Wall, Lips)) :-
	expand_goal(Goal0, Goal),
	get_time(OldWall),
	statistics(cputime, OldTime), 
	statistics(inferences, OldInferences), 
	(   catch(Goal, E, true)
	->  Result = yes
	;   Result = no
	),
	statistics(inferences, NewInferences), 
	statistics(cputime, NewTime), 
	get_time(NewWall),
	UsedTime is NewTime - OldTime, 
	UsedInf  is NewInferences - OldInferences - 3, 
	Wall     is NewWall - OldWall,
	(   UsedTime =:= 0
	->  Lips = 'Infinite'
	;   Lips is integer(UsedInf / UsedTime)
	),
	%TR: removed: 
	%print_message(informational, time(UsedInf, UsedTime, Wall, Lips)), 
	(   nonvar(E)
	->  throw(E)
	;   Result == yes
	).
	

/*
 * Helper predicates for reporting time spent.
 *   - performance(Goal, Time, Count)
 *   - ctc_time(Goal, Time)
 *   - startStopwatch
 *   - reportRuntime(ForWhat)
 *   - reportRuntime(ForWhat,CPUMilisSinceLast)
 */
   
%% performance(+Goal, ?Time, ?CountAll)
%
%  Measure milliseconds spent finding all results of a Goal and 
%  count the number of results (including duplicates) and the  
%  number of performed inferences.
performance(Goal, Time, CountAll, Inferences) :-
	ctc_time(count(Goal, CountAll), Time, Inferences).
	
%% performance(+Goal, ?Time, ?CountAll)
%
%  Measure milliseconds spent finding all results of a Goal and 
%  count the number of results (including duplicates).
performance(Goal, Time, CountAll) :- 
  ctc_time(count(Goal, CountAll), Time).



/*
 * Measure time to find and count all results of a Goal and
 * and also all unique results. 
 */   
performanceUnique(Goal, Time, CountAll,CountUnique) :- 
  ctc_time(count_all_and_unique(Goal,CountAll,CountUnique), Time).

:- if(pdt_support:pdt_support(count_inferences)).

ctc_time(Call, Time, Inferences) :- 
   startStopwatchWithInfer(InferencesOld), 
     call(Call),
   measureRuntimeWithInfer(InferencesOld, Time, Inferences).

startStopwatchWithInfer(InferencesOld) :-
    statistics(runtime, _CPU),    % Start new CPU timer
	statistics(inferences, InferencesOld).

measureRuntimeWithInfer(InferencesOld, CPUMilisSinceLast, Inferences) :-
	statistics(inferences, InferencesNew),
	Inferences is InferencesNew - InferencesOld - 3,
	statistics(runtime, [_CPUMilisSinceStart, CPUMilisSinceLast]). 

:- else.

ctc_time(Call, Time, unknown) :-
	ctc_time(Call, Time).
   
:- endif.
    
ctc_time(Call, Time) :- 
   startStopwatch, 
     call(Call),
   measureRuntime(Time).

startStopwatch :-
    statistics(runtime, _CPU).    % Start new CPU timer
%    statistics(real_time, _Real). % Start new real timer
 
 
measureRuntime(CPUMilisSinceLast) :- 
    statistics(runtime,    [_CPUMilisSinceStart, CPUMilisSinceLast]). 

    
reportRuntime(ForWhat) :- 
    statistics(runtime,    [_CPUMilisSinceStart, CPUMilisSinceLast]),   
%    statistics(real_time,  [_RealSecsSinceStart, RealSecsSinceLast]), 
%    log_on_stdout('~a: CPU = ~a milliseconds, real time ca. ~a seconds~n',
	 log_on_stdout('~a: CPU = ~a milliseconds~n', 
           [ForWhat,CPUMilisSinceLast]).
           
    
reportRuntime(ForWhat,CPUMilisSinceLast) :- 
    statistics(runtime,    [_CPUMilisSinceStart, CPUMilisSinceLast]),   
    statistics(real_time,  [_RealSecsSinceStart, RealSecsSinceLast]), 
    log_on_stdout('~a: CPU = ~a milliseconds, real time ca. ~a seconds~n', 
           [ForWhat,CPUMilisSinceLast,RealSecsSinceLast]).    



