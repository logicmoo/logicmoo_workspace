 
/*************************************************************************
 
         name: generate_svenska.pl 
      version: 
  description: A generation file for agendatalk which output is
               adapted to the generate module generate_agensdatalk 
       author: Rebecca Jonson 
 
*************************************************************************/

%:- discontiguous output_form/3, input_form/2, plan/2, postcond/2.

:- multifile output_form/3.
:- use_module(library(random)).
:- use_module( library(lists), [ member/2, select/3, append/3 ] ).
:- use_module( library(charsio), [ format_to_chars/3 ] ).
:- use_module(calendar, [ampm_disamb/3, day2date/2, month2nr/2, day2nr/2 ,today/1, weekday/2]).
:- ensure_loaded(digits_svenska).
:- ensure_loaded(time_svenska).
:- ensure_loaded(semsort_agendatalk).
:- ensure_loaded(interpret_svenska).


/*----------------------------------------------------------------------
     output_form( +Move, Com, -String )
     -- Canned output --Where Com corresponds to Shared Commitments
----------------------------------------------------------------------*/

% ask-moves

output_form( ask(X^(action(X))), _, ['Vad vill du göra?'] ).
output_form( ask(X^issue(X) ),_, ['Vill du kolla tiden för en bokning eller kolla om du är bokad en viss tid?']).

output_form( ask(action(T)), Com, Str ):-
	output_form(action(T), Com, StrT ),
	append( ['Vill du'], StrT, Str0 ),
	append( Str0, ['?'], Str).

output_form( ask(X^date_to_store(X)),Com, When):-
		member(event_to_store(Ev),Com),
		member(start_time_to_store(_),Com),
		event2string(Ev, EvStr),
		append(['Vilken dag är'],[EvStr],When1),
		append(When1, ['?'],When).
output_form( ask(X^date_to_store(X)), Com, When) :-
		member(event_to_store(Ev), Com),
		event2string(Ev, EvStr),
		append(['När är'],[EvStr] , When1),
		append(When1, ['?'],When).
output_form( ask(X^date_to_store(X)), _Com, ['Vilken dag?']).
output_form( ask(X^olddate(X)), _Com, ['Vilken dag ligger bokningen?']).
output_form( ask(X^newdate(X)), _Com, ['Till vilket datum vill du flytta bokningen?']).
output_form( ask(X^event_to_store(X)),_,
	     ['Vilken typ av bokning gäller det?'] ).
output_form( ask(X^start_time_to_store(X)),Com,
	     WhatTime):-
	     	member(event_to_store(Ev), Com),
		event2string(Ev,EvStr),
		member(which_info(time), Com),
		append(['Upprepa tiden för'], [EvStr], What1),
		append(What1, ['tack.'], WhatTime).
output_form( ask(X^start_time_to_store(X)),Com,
	     WhatTime):-
	     	member(event_to_store(Ev), Com),
		event2string(Ev,EvStr),
		append(['Vilken tid börjar'], [EvStr], What1),
		append(What1, ['?'], WhatTime).
output_form( ask(X^start_time_to_store(X)),Com,
	     ['Vid vilken tid?']).
output_form( ask(X^newtime(X)),Com,
	     ['Till vilken tid vill du ändra bokningen?']).
output_form( ask(X^am_or_pm(X)), _, ['på förmiddagen eller eftermiddagen?']).
output_form( ask(X^duration_time_to_store(X)),Com,
	     Out ):-
	     	member(event_to_store(Ev), Com),
		event2string(Ev, EvStr),
		append(['När tar'], [EvStr],Out1),
		append(Out1, ['slut?'], Out).
output_form( ask(X^add_more_info(X)),Com, Out):-
		member(event_to_store(Ev), Com),
		event2string(Ev, EvStr),
	     append(['Vill du anteckna något mer om'], [EvStr],Out1 ),
	     append(Out1, ['?'], Out).
output_form( ask(X^location_to_store(X)),Com,Out):-
	        member(which_info(location), Com),
		member(event_to_store(Ev), Com),
		event2string(Ev,EvStr),
		append(['Kan du upprepa platsen för'], [EvStr], Out1),
		append(Out1, ['tack?'], Out).
output_form( ask(X^location_to_store(X)),Com,Out):-
		member(event_to_store(Ev), Com),
		event2string(Ev,EvStr),
		append(['I vilken lokal ska'], [EvStr], Out1),
		append(Out1, ['hållas'], Out2),
		append(Out2, ['?'], Out).
output_form( ask(X^take_down_event(X)),Com,Out):-
		member(event_to_store(Ev), Com),
		event2stringnondef(Ev, EvStr),
		member(date_to_store(Day), Com),
		date2str(Day, DateStr), 
		member(start_time_to_store(T), Com),
		member(am_or_pm(AMPM), Com),
		time2str(T, AMPM, TimeStr),
		member(location_to_store(L), Com),
		loc2str(L, LStr),
		append(['Vill du anteckna följande:'], [EvStr], Out1),
		append(Out1, DateStr, Out2),
		append(Out2, [klockan], Out3),
		append(Out3, TimeStr, Out4),
		append(Out4, LStr, Out5),
		append(Out5, ['?'],Out).
output_form( ask(X^take_down_event(X)),Com,Out):-
		member(event_to_store(Ev), Com),
		event2stringnondef(Ev, EvStr),
		member(date_to_store(Day), Com),
		date2str(Day, DateStr), 
		member(start_time_to_store(T), Com),
		member(am_or_pm(AMPM), Com),
		time2str(T, AMPM, TimeStr),
		append(['Vill du anteckna följande:'], [EvStr], Out1),
		append(Out1, DateStr, Out2),
		append(Out2, [klockan], Out3),
		append(Out3, TimeStr, Out4),
		append(Out4, ['?'],Out).

output_form( ask(X^which_info(X)), _, ['Vilken information är felaktig?']).

% action

output_form( action(top),_, ['börja om'] ).
output_form( action(change_language), _,['byta språk'] ).
output_form( action(add_event),_, ['anteckna något']).
output_form( action(A), _, ['kolla med din kalender']):- member(A, [get_info, get_time, get_date]).
output_form(action(change_info), _, ['ändra informationen']).
output_form(action(change_date), _, ['flytta en bokning']).
output_form(action(change_time), _, ['ändra tiden för en bokning']).
output_form(action(delete_event),_, ['ta bort en bokning']).
output_form(action(more_info),_, ['lägga till information']).
output_form(action(delete_current_event),_, ['börja om från början']).
%user help

output_form(answer(usage), _, ['Agendatalk är din talande kalendar som antecknar åt dig och svarar på frågor om dina bokningar. För att avsluta, säg hejdå. ']).

%confirmation and reports
output_form(confirm(add_event), _, ['Antecknat']).
output_form(report('AddEvent', done),_,['Anteckning gjord']).
output_form(report('DeleteEvent', done),_,['Bokningen är borttagen']).

output_form(confirm(delete_event), _, ['Borttaget']).
/*output_form(report('GetTime',done),COM,ReportTime):-
	member(start_time_to_store(TIME),COM),
	append(['The time is'], TIME, ReportTime).*/
output_form(report('ChangeTime', done),_,['Tiden för bokningen är ändrad']).
output_form(confirm(change_time), _, ['Tiden har ändrats']).
output_form(report('ChangeDate', done),_,['Bokningen är flyttad']).
output_form(confirm(change_date), _, ['Bokningen är flyttad']).	
output_form(report('ConsistentDate', done),_,[]).
output_form(report('AddEvent',failed(_)), _, ['Anteckning glömd']).
output_form(confirm(change_info),_,['Informationen har ändrats']).
output_form(report('InfoChanged',failed(_)),_, ['Tyvärr kan informationen inte ändras']).
output_form(report('ChangeDate',failed(_)), _, ['Lyckades inte ändra datumet']).
output_form(report('ChangeTime',failed(_)), _, ['Lyckades inte ändra tiden.']).
output_form(report('DeleteEvent', failed),_,['Bokningen gick inte radera']).
%greetings
output_form(greet, _, Ans):- random(1,7,N),greetings(List),getNoflist(N,List,Answer),gettoday(Tod),append([Answer],Tod,Ans).
output_form(quit,_, [Answer]):-random(1,4,N),goodbyes(List),getNoflist(N,List,Answer).

%For grounding
%output_form(date_to_store(W),_,[W]).
%random lists
getNoflist(1,[X|_],X).
getNoflist(N,[_|Xs],Y):- Num is N-1,getNoflist(Num,Xs,Y).

greetings(['Hej! Det här är din talande kalender.','Hej! Det här är din talande kalender.','Hej, det här är en talande filofax!','Hej, jag är din kalenders röst!','Din kalender, till tjänst!','Välkommen till agendatalk!','Välkommen till din agenda']).
goodbyes(['Hejdå!','Tack för samtalet!','Ha en bra dag!']).

gettoday(TodayString):- today(Date),datenr2str(Date,Today), append(['Dagens datum är:'],Today,TodayString).
%For grounding
output_form(date_to_store(W),_,Date):-
	date2str(W,Date).
output_form(start_time_to_store(T),_,Time):-
	time2string(T,Tid),
	append(['Klockan'],Tid,Time).

%%%%%%%%%%%%%%%%%SYSTEM ANSWERS%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
output_form( answer(time(empty)),COM,['Det finns inget',EVENT,'bokat den dagen.']):-
	member(event_to_store(Ev),COM),
	event2stringnondef(Ev,EVENT).
output_form( answer(time(T)), COM,[Ev,'börjar klockan', H, M,'.']):-
	time2string(T,[H,M]),
	member(event_to_store(EVENT),COM),
	event2string(EVENT,Ev).

output_form( answer(event(empty)), _COM, ['Inget bokat den tiden.']).
	
output_form( answer(event(EVENT)), _COM, ['Du är bokad den tiden. Du har', Ev,'.']):-
	member(event_to_store(EVENT),COM),
	event2stringnondef(EVENT,Ev).

output_form( answer(bookings([[empty],[empty]])), _COM, ['Du har inget bokat den dagen.']).


output_form( answer(bookings([EVENT,TIME])), _COM, ['Du har', EV,'klockan',H,M,'.']):-
	event2stringnondef(EVENT,EV),
	time2string(TIME,[H,M]).
output_form( answer(bookings([EVENTS,TIMES])), _COM, BOOK):-
	get_events(EVENTS,TIMES,EVTIMESTR),
	append(['Du har:'],EVTIMESTR,BOOK).
	%event2stringnondef(EVENT,EV),
	%time2string(TIME,[H,M]).

output_form( answer(todaysdate(Date)), _COM, TodayString):-
	datenr2str(Date,Today),
	append(['Dagens datum är:'],Today,TodayString).

get_events([],[],['.']).
get_events([Ev|Events],[T|Times],Book):-
	event2stringnondef(Ev,EVStr),
	time2string(T,Tid),
	append([EVStr],[klockan|Tid],EvTime),
	append(EvTime,[','],EVTimStr),
	get_events(Events,Times,EventsTimesStr),
	append(EVTimStr,EventsTimesStr,Book).
get_events([Ev|Events],[T|Times],Book):-
	event2stringnondef(Ev,EVStr),
	T == empty,
	append([EVStr],[oviss,tid],EvTime),
	append(EvTime,[','],EVTimStr),
	get_events(Events,Times,EventsTimesStr),
	append(EVTimeStr,EventsTimesStr,Book).
%date_output(today,today) :- !.

%date_output(Date,DateOutput) :-
%	atom_chars(Date,Chars),
%	format_to_chars('~c~c/~c~c',Chars,DateOutputCs),
%	atom_chars(DateOutput,DateOutputCs).
%lexical semantics (as in input_form)
output_form(newdate(WeekDay),_Com,['på',WK,'?']):- input_form([på,WK],answer(date(WeekDay))).
output_form(olddate(WeekDay),_Com,['till',WK,'?']):- input_form([till,WK],answer(date(WeekDay))).
output_form( Sem,_,Str):-
	lexsem( Str,Sem).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          Term 2 string
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
event2string(meeting, 'mötet').
event2string(appointment, 'träffen').
event2string(party, 'festen').
event2string(presentation,'presentationen').
event2string(lecture,'föreläsningen').
event2string(movie,'bion').
event2string(coffee,'fikat').
event2string(shopping,'shoppingen').
event2string(training,'träningen').
event2string(trip,'resan').
event2string(conference,'konferensen').
event2string(haircut,'klippningen').
event2string(dentist,'tandläkarbesöket').
event2string(dinner,'middagen').

event2stringnondef(meeting, 'möte').
event2stringnondef(appointment, 'dejt').
event2stringnondef(party, 'fest').
event2stringnondef(presentation,'presentation').
event2stringnondef(lecture,'föreläsning').
event2stringnondef(coffee,'fika').
event2stringnondef(movie,'bio').
event2stringnondef(shopping,'shopping').
event2stringnondef(trip,'resa').
event2stringnondef(training,'träning').
event2stringnondef(conference,'konferens').
event2stringnondef(haircut,'klippning').
event2stringnondef(dentist,'tandläkarbesök').
event2stringnondef(dinner,'middag').

loc2str(plaza, [på, plaza]).

%date2str(friday, [eighteenth, of, july]).
date2str(WeekDay, DateStr):-
	sem_sort(WeekDay, weekday),
	calendar:day2date(WeekDay, date(_,M,D)),
	monthStr(MonthStr, M),
	dayStr(DayStr, D),
	input_form(WK,answer(date(WeekDay))),
	append(WK,DayStr,Day),
	append(Day, MonthStr, DateStr).

date2str([next,WeekDay], DateStr):-
	sem_sort(WeekDay, weekday),
	calendar:day2date([next,WeekDay], date(_,M,D)),
	monthStr(MonthStr, M),
	dayStr(DayStr, D),
	input_form(WK,answer(date(WeekDay))),
	append(WK,DayStr,Day),
	append(Day, MonthStr, DateStr).
%(thirtyfirst, [thirtyfirst]).
date2str(Day, [den, DayStr]):-
	sem_sort(Day, day),
	day2nr([Day],DayNr),
	dayStr([DayStr],DayNr).
date2str([Day,Month],['den', DaySw, MonthSw]):-
	sem_sort(Day,day),
	day2nr([Day],DayNr),
	dayStr([DaySw],DayNr),
	sem_sort(Month,month),
	month2nr([Month],Nr),
	monthStr([MonthSw],Nr).
date2str([WeekDay, Day,Month],['den', DaySw, MonthSw]):-
	sem_sort(WeekDay,weekday),
	sem_sort(Day,day),
	day2nr([Day],DayNr),
	dayStr([DaySw],DayNr),
	sem_sort(Month,month),
	month2nr([Month],Nr),
	monthStr([MonthSw],Nr).
date2str([WeekDay, Day],['den', DaySw, MonthStr]):-
	sem_sort(WeekDay,weekday),
	calendar:day2date(WeekDay, date(_,M,_)),
	monthStr([MonthStr], M),
	sem_sort(Day,day),
	day2nr([Day],DayNr),
	dayStr([DaySw],DayNr).
date2str(today,[i,dag]).
date2str(tomorrow, [i,morgon]).
date2str(aftertomorrow, [i,övermorgon]).

datenr2str(date(Y,M,D),[WeekDaySwe,DayStr,MonthStr]):-
	monthStr([MonthStr],M),
	dayStr([DayStr],D),
	weekday(date(Y,M,D),WeekDay),
	input_form([WeekDaySwe],answer(date(WeekDay))).

time2str(Time, AMPM, TimeStr):-
	calendar:ampm_disamb(Time, AMPM,CorrTime),
	time2string(CorrTime, TimeStr).

%time2str(1030, pm, [twenty, thirty]).
time2string(CorrTime, TimeStr):-
	%calendar:ampm_disamb(Time, AMPM, CorrTime),
	name(CorrTime, [H1, H2, M1, M2]),
	name(Hour,[H1, H2]),
	name(Minute, [M1, M2]),
	number_phrase(HourStr, Hour),
	%number_phrase(MinStr, Minute),
	Minute = 0, 
	append(HourStr, [nollnoll], TimeStr).

time2string(CorrTime, TimeStr):-
	%calendar:ampm_disamb(Time, AMPM, CorrTime),
	name(CorrTime, [H1, M1, M2]),
	name(Hour,[H1]),
	name(Minute, [M1, M2]),
	number_phrase(HourStr, Hour),
	%number_phrase(MinStr, Minute),
	Minute = 0, 
	append(HourStr, [nollnoll], TimeStr).

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

