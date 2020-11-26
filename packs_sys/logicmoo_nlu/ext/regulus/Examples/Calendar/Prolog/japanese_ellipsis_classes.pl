
:- module(ellipsis_classes,
	[in_ellipsis_class/3]
    ).

%======================================================================

:- use_module(library(lists)).
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module('$REGULUS/Examples/Calendar/Prolog/dialogue_utils').
:- use_module('$REGULUS/Examples/Calendar/Prolog/calendar_utils').
:- use_module('$REGULUS/Examples/Calendar/Prolog/japanese_calendar_utils').
 
%======================================================================
 
% NPs referring to dates
in_ellipsis_class(when_np,
		  'kyou etc',
		  term(null, DayWord, _)) :-
	japanese_day_word(DayWord).
in_ellipsis_class(when_np,
		  'gogatsu touka',
		  date(_, _)).
in_ellipsis_class(when_np,
		  'gogatsu',
		  date(_, _, _)).
in_ellipsis_class(when_np,
		  'getsuyoubi',
		  term(_Spec, DayOfWeek, _)) :-
	japanese_dayofweek(DayOfWeek, _EngDay).
in_ellipsis_class(when_np,
		  'raisyuu',
		  term(_Spec, TimePeriod, _)) :-
	japanese_timeperiod(TimePeriod).
in_ellipsis_class(when_np,
		  'asa',
		  term(_Spec, TimeOfDay, _)) :-
	japanese_time_of_day(TimeOfDay).

% PPs referring to dates
in_ellipsis_class(when_pp,
		  pp(Doc),
		  [_Prep1, Term1]) :-
	in_ellipsis_class(when_np, Doc, Term1).

% Names of people
in_ellipsis_class(person,
		  person_name,
		  term(name, Name, [])) :-
	name_for_attendee(_Attendee, Name).
in_ellipsis_class(person,
		  i,
		  term(null, watashi, [])).


