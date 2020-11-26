
:- module(ellipsis_classes,
	[in_ellipsis_class/3]
    ).

%======================================================================

:- use_module(library(lists)).
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module('$REGULUS/Examples/Calendar/Prolog/dialogue_utils').
:- use_module('$REGULUS/Examples/Calendar/Prolog/calendar_utils').
 
%======================================================================
 
% NPs referring to dates
in_ellipsis_class(when_np,
		  'today',
		  term(null, today, [])).
in_ellipsis_class(when_np,
		  'yesterday',
		  term(null, yesterday, [])).
in_ellipsis_class(when_np,
		  'tomorrow',
		  term(null, tomorrow, [])).
in_ellipsis_class(when_np,
		  'may tenth',
		  date(_, _)).
in_ellipsis_class(when_np,
		  'may',
		  date(_, _, _)).
in_ellipsis_class(when_np,
		  'next monday',
		  term(_Spec, DayOfWeek, [])) :-
	is_dayofweek(DayOfWeek).
in_ellipsis_class(when_np,
		  'next week',
		  term(_Spec, TimePeriod, [])) :-
	is_timeperiod(TimePeriod).
in_ellipsis_class(when_np,
		  'morning',
		  term(_Spec, TimeOfDay, [])) :-
	is_time_of_day(TimeOfDay).

% PPs referring to dates
in_ellipsis_class(when_pp,
		  pp(Doc),
		  [[Prep1, Term1], [Prep2, Term2]]) :-
	in_ellipsis_class(when_np, Doc, Term1),
	prep(Prep1, temporal),
	in_ellipsis_class(when_np, _OtherDoc, Term2),
	prep(Prep2, temporal).

in_ellipsis_class(when_pp,
		  pp(Doc),
		  [[Prep1, Term1]]) :-
	in_ellipsis_class(when_np, Doc, Term1),
	prep(Prep1, temporal).

in_ellipsis_class(when_pp,
		  pp(Doc),
		  [[Prep, Term]]) :-
	in_ellipsis_class(when_np, Doc, Term),
	prep(Prep, temporal).

% Names of people
in_ellipsis_class(person,
		  person_name,
		  term(name, Name, [])) :-
	name_for_attendee(_Attendee, Name).
in_ellipsis_class(person,
		  i,
		  term(pro,i,[])).

% WH PPs
in_ellipsis_class(wh_pp,
		  'when',
		  [[time, when]]).
in_ellipsis_class(wh_pp,
		  'where',
		  [[loc, where]]).
in_ellipsis_class(wh_pp,
		  'in which room',
		  [[at_loc, term(which, room, [])]]).

