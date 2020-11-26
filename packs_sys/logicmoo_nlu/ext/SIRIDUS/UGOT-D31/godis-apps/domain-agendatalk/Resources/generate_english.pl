/*************************************************************************
 
         name: generate_english.pl 
      version: 
  description: A generation file for agendatalk which output is
               adapted to the generate module generate_agendatalk 
       author: Rebecca Jonson 
 
*************************************************************************/

%:- discontiguous output_form/3, input_form/2, plan/2, postcond/2.

:- multifile output_form/3.
:- use_module(library(random)).
:- use_module( library(lists), [ member/2, select/3, append/3] ).
:- use_module( library(charsio), [ format_to_chars/3 ] ).
:- use_module(calendar, [ampm_disamb/3, day2date/2]).
:- ensure_loaded(digits_english).
:- ensure_loaded(semsort_agendatalk).


/*----------------------------------------------------------------------
     output_form( +Move, Com, -String )
     -- Canned output --Where Com corresponds to Shared Commitments
----------------------------------------------------------------------*/


% ask-moves

output_form( ask(X^(action(X))), _, ['What can I do for you?'] ).
output_form( ask(X^issue(X) ),_, ['Do you want to check the time of an event or check if you are booked?']).

output_form( ask(action(T)), Com, Str ):-
	output_form(action(T), Com, StrT ),
	append( ['Do you want to'], StrT, Str0 ),
	append( Str0, ['?'], Str).

output_form( ask(X^date_to_store(X)),Com, ['What day is the',Ev,'?']):-
		member(event_to_store(Ev),Com),
		member(start_time_to_store(_),Com).
	
output_form( ask(X^date_to_store(X)), Com, ['When is the',Ev,'?']) :-
		member(event_to_store(Ev), Com).
output_form( ask(X^date_to_store(X)), _Com, ['At what day?']).
output_form( ask(X^event_to_store(X)),_,
	     ['What kind of event are we talking about?'] ).
output_form( ask(X^start_time_to_store(X)),Com,
	     ['Repeat the time of the',Ev,'please!']):-
	     	member(event_to_store(Ev), Com),
		member(which_info(time), Com).
output_form( ask(X^start_time_to_store(X)),Com,
	     ['What time is the',Ev,'?']):-
	     	member(event_to_store(Ev), Com).
output_form( ask(X^start_time_to_store(X)),_Com,['At what time?']).
output_form( ask(X^am_or_pm(X)), _, ['A.M. or PM?']).
output_form( ask(X^duration_time_to_store(X)),Com,
	     ['What time does the',Ev,'end?']):-
	     	member(event_to_store(Ev), Com).
output_form( ask(X^add_more_info(X)),Com, ['Do you want to note more things about the',Ev,'?']):-
		member(event_to_store(Ev), Com).
output_form( ask(X^location_to_store(X)),Com,['Can you repeat the location of the',Ev,'please?']):-
	        member(which_info(location), Com),
		member(event_to_store(Ev), Com).
output_form( ask(X^location_to_store(X)),Com,['Where would the',Ev,'take place?']):-
		member(event_to_store(Ev), Com).
output_form( ask(X^take_down_event(X)),Com,Take):-
		member(event_to_store(Ev), Com),
		member(date_to_store(Day), Com),
		date2str(Day, DateStr), 
		member(start_time_to_store(T), Com),
		member(am_or_pm(AMPM), Com),
		time2str(T, AMPM, TimeStr),
		member(location_to_store(L), Com),
		loc2str(L, LStr).
		append(['Do you want to take down:',Ev],DateStr,Half),
		append(Half,['at'],Halfat),
		append(Halfat,TimeStr,End),
		append(End,LStr,TotEnd),
		append(TotEnd,['?'],Take).
	
output_form( ask(X^take_down_event(X)),Com,Take):-
		member(event_to_store(Ev), Com),
		member(date_to_store(Day), Com),
		date2str(Day, DateStr), 
		member(start_time_to_store(T), Com),
		member(am_or_pm(AMPM), Com),
		time2str(T, AMPM, TimeStr),
		append(['Do you want to take down:',Ev],DateStr,Half),
		append(Half,['at'],Halfat),
		append(Halfat,TimeStr,End),
		append(End,['?'],Take).
output_form( ask(X^which_info(X)), _, ['Which information is wrong?']).
% action

output_form( action(top), _, ['restart'] ).
output_form( action(change_language), _,['change language'] ).
output_form( action(add_event),_, ['take down an event']).
output_form( action(A), _, ['check your calendar']):- member(A, [get_info, get_time, get_date]).
output_form(action(change_info), _, ['change some of the information']).
output_form(action(delete_event),_, ['delete an event']).
output_form(action(change_time),_, ['change the time of an event']).
output_form(action(change_date),_, ['change the date of an event']).
output_form(action(more_info),_, ['add some more information']).
output_form(action(delete_current_event),_, ['start all over']).
%user help
output_form(answer(usage), _, ['AgendaTalk is a talking filofax letting you handle your calendar events by voice. You can add events or ask about bookings. To quit: say goodbye. ']).

%confirmation and reports
output_form(confirm(add_event), _, ['The event is scheduled.']).
output_form(report('AddEvent', done),_,['The event has been scheduled.']).
output_form(report('ConsistentDate', done),_,[]).
output_form(report('AddEvent',failed(_)), _, ['I have deleted everything.']).
output_form(confirm(change_info),_,['The information has been changed.']).
output_form(report('InfoChanged',failed(_)),_, ['Sorry, the information cannot be changed.']).
output_form(report('DeleteEvent', done),_,['The event has been deleted']).
output_form(confirm(delete_event), _, ['Event deleted']).
%greetings
output_form(greet, _, [Answer]):- random(1,6,N),greetings(List),getNoflist(N,List,Answer).
output_form(quit,_, [Answer]):-random(1,5,N),goodbyes(List),getNoflist(N,List,Answer).

%For grounding
output_form(date_to_store(W),_,[W]).
output_form(start_time_to_store(T),_,Time):-
	time2string(T,Tid),
	append(['At'],Tid,Time).
%random lists
getNoflist(1,[X|_],X).
getNoflist(N,[_|Xs],Y):- Num is N-1,getNoflist(Num,Xs,Y).

greetings(['Hi! This is your talking filofax.','Hi! This is AgendaTalk, your personal talking agenda.','Hi, this is AgendaTalk.','Hi! Calendar, at your service!','Hi! This is AgendaTalk, your personal talking agenda.']).
goodbyes(['Bye!','Goodbye!','See you!','See you later!']).

%%%%%%SYSTEM ANSWERS%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_form( answer(time(empty)),COM,['Sorry, there is no',Ev,'scheduled that day']):-
	member(event_to_store(Ev),COM).
output_form( answer(time(T)), COM,['The', Ev,'is at', H, M]):-
	time2string(T,[H,M]),
	member(event_to_store(Ev),COM).

output_form( answer(event(empty)), _COM, ['Time slot available.']).
	
output_form( answer(event(EVENT)), _COM, ['Time slot booked. You have a', EVENT]).

output_form( answer(bookings([[empty],[empty]])), COM, ['You have no appointments today.']):- member(date_to_store(today),COM).
%output_form( answer(bookings([empty,empty])), _COM, ['You have no bookings that day.']).
output_form( answer(bookings([[empty],[empty]])), _COM, ['You have no bookings that day.']).
	
output_form( answer(bookings([EVENT,TIME])), _COM, ['You have a', EVENT,'at',H,M]):-
	sem_sort(EVENT,event),
	time2string(TIME,[H,M]).
output_form( answer(bookings([EVENTS,TIMES])), _COM, BOOK):-
	get_events(EVENTS,TIMES,EVTIMESTR),
	append(['You got:'],EVTIMESTR,BOOK).

get_events([],[],['.']).
get_events([Ev|Events],[T|Times],Book):-
	time2string(T,Tid),
	append([Ev],[at|Tid],EvTime),
	get_events(Events,Times,EventsTimesStr),
	append(EvTime,EventsTimesStr,Book).
%lexical semantics (as in input_form).

output_form( Sem,_,Str):-
	lexsem( Str,Sem).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                Term to string
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loc2str(plaza, [at, plaza]).

%date2str(friday, [eighteenth, of, july]).
date2str(WeekDay, DateStr):-
	sem_sort(WeekDay, weekday),
	calendar:day2date(WeekDay, date(_,M,D)),
	monthStr(MonthStr, M),
	dayStr(DayStr, D),
	append([the],DayStr,DayStr1),
	append(DayStr1, [of], DayStr2),
	append(DayStr2, MonthStr, DateStr).

date2str([next,WeekDay], DateStr):-
	sem_sort(WeekDay, weekday),
	calendar:day2date([next,WeekDay], date(_,M,D)),
	monthStr(MonthStr, M),
	dayStr(DayStr, D),
	append([the],DayStr,DayStr1),
	append(DayStr1, [of], DayStr2),
	append(DayStr2, MonthStr, DateStr).
%(thirtyfirst, [thirtyfirst]).
date2str(Day, [the, Day]):-
	sem_sort(Day, day).

date2str([Day,Month],[the, Day, of, Month]):-
	sem_sort(Day,day),
	sem_sort(Month,month).
date2str([WeekDay, Day,Month],[the, Day, of, Month]):-
	sem_sort(WeekDay,weekday),
	sem_sort(Day,day),
	sem_sort(Month,month).
date2str([WeekDay, Day],[the, Day, of, MonthStr]):-
	sem_sort(WeekDay,weekday),
	calendar:day2date(WeekDay, date(_,M,_)),
	monthStr([MonthStr], M),
	sem_sort(Day,day).
date2str(today,[today]).
date2str(tomorrow, [tomorrow]).
date2str(aftertomorrow, [the,day,after,tomorrow]).

%time2str(1030, pm, [twenty, thirty]).
time2str(Time, AMPM, TimeStr):-
	calendar:ampm_disamb(Time, AMPM,CorrTime),
	time2string(CorrTime, TimeStr).

%time2string(1030, [twenty, thirty]).
time2string(CorrTime, TimeStr):-
	%calendar:ampm_disamb(Time, AMPM, CorrTime),
	name(CorrTime, [H1, H2, M1, M2]),
	name(Hour,[H1, H2]),
	name(Minute, [M1, M2]),
	number_phrase(HourStr, Hour),
	%number_phrase(MinStr, Minute),
	Minute = 0, 
	append(HourStr, [oclock], TimeStr).

time2string(CorrTime, TimeStr):-
	%calendar:ampm_disamb(Time, AMPM, CorrTime),
	name(CorrTime, [H1, M1, M2]),
	name(Hour,[H1]),
	name(Minute, [M1, M2]),
	number_phrase(HourStr, Hour),
	%number_phrase(MinStr, Minute),
	Minute = 0, 
	append(HourStr, [oclock], TimeStr).

time2string(CorrTime,TimeStr):-
	%calendar:ampm_disamb(Time, AMPM, CorrTime),
	name(CorrTime, [H1, H2, M1, M2]),
	name(Hour,[H1, H2]),
	name(Minute, [M1, M2]),
	number_phrase(HourStr, Hour),
	number_phrase(MinStr, Minute),	
	append(HourStr, MinStr, TimeStr).

%%time2str digits!!!
time2string(CorrTime, TimeStr):-
	%calendar:ampm_disamb(Time, AMPM, CorrTime),
	name(CorrTime, [H1, M1, M2]),
	name(Hour,[H1]),
	name(Minute, [M1, M2]),
	number_phrase(HourStr, Hour),
	number_phrase(MinStr, Minute),	
	append(HourStr, MinStr, TimeStr).

