:- module(weekDay, [dayOfWeek/2, weekDays/1]).

:- use_module(library(system)).
:- use_module(library(lists)).



dayOfWeek(Int, Str):-
	exec('date +\%w', [std, pipe(Out), std], _),
	get0(Out, IntStr),
	name(Int,[IntStr]),
	weekDays(Wd),
	member((Str,Int),Wd).
	
weekDays([('sunday',0),('monday',1),('tuesday',2),('wednesday',3),('thursday',4),('friday',5),('saturday',6)]).
	
