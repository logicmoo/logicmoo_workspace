/*************************************************************************
 
         name: generate_english.pl 
      version: 
  description: A generation file which output is adapted to the generate
               module generate_agendatalk 
       author: Rebecca Jonson 
 
*************************************************************************/

%:- discontiguous output_form/3, input_form/2, plan/2, postcond/2.

:-multifile output_form/3.
:- use_module(library(random)).
:- use_module( library(lists), [ member/2, select/3, append/3 ] ).
:- use_module( library(charsio), [ format_to_chars/3 ] ).
:- use_module(calendar, [ampm_disamb/3, day2date/2]).
:- ensure_loaded(digits_spanish).
:- ensure_loaded(semsort_agendatalk).


/*----------------------------------------------------------------------
     output_form( +Move, Com, -String )
     -- Canned output --Where Com corresponds to Shared Commitments
----------------------------------------------------------------------*/


% ask-moves

output_form( ask(X^(action(X))), _, ['Como le puedo ayudar?'] ).


output_form( ask(action(T)), Com, Str ):-
	output_form(action(T), Com, StrT ),
	append( ['Quiere'], StrT, Str0 ),
	append( Str0, ['?'], Str).

output_form( ask(X^date_to_store(X)),Com, When):-
		member(event_to_store(Ev),Com),
		member(start_time_to_store(_),Com),
		event2string(Ev, EvStr),
		append(['Qué día es su'],[EvStr],When1),
		append(When1, ['?'],When).
output_form( ask(X^date_to_store(X)), Com, When) :-
		member(event_to_store(Ev), Com),
		event2string(Ev, EvStr),
		append(['Cuando es su'],[EvStr] , When1),
		append(When1, ['?'],When).
output_form( ask(X^event_to_store(X)),_,
	     ['De qué tipo de actividad estamos hablando?'] ).
output_form( ask(X^start_time_to_store(X)),Com,
	     WhatTime):-
	     	member(event_to_store(Ev), Com),
		event2string(Ev,EvStr),
		member(which_info(time), Com),
		append(['Repita la hora de su'], [EvStr], What1),
		append(What1, ['por favor.'], WhatTime).
output_form( ask(X^start_time_to_store(X)),Com,
	     WhatTime):-
	     	member(event_to_store(Ev), Com),
		event2string(Ev,EvStr),
		append(['A qué hora es su'], [EvStr], What1),
		append(What1, ['?'], WhatTime).
output_form( ask(X^am_or_pm(X)), _, ['por la mañana or por la tarde?']).
output_form( ask(X^duration_time_to_store(X)),Com,
	     Out ):-
	     	member(event_to_store(Ev), Com),
		event2string(Ev, EvStr),
		append(['A qué hora termina su'], [EvStr],Out1),
		append(Out1, ['?'], Out).
output_form( ask(X^add_more_info(X)),Com, Out):-
		member(event_to_store(Ev), Com),
		event2string(Ev, EvStr),
	     append(['Quiere apuntar algo más sobre su'], [EvStr],Out1 ),
	     append(Out1, ['?'], Out).
output_form( ask(X^location_to_store(X)),Com,Out):-
	        member(which_info(location), Com),
		member(event_to_store(Ev), Com),
		event2string(Ev,EvStr),
		append(['Puede repetir la localidad de su'], [EvStr], Out1),
		append(Out1, [',por favor?'], Out).
output_form( ask(X^location_to_store(X)),Com,Out):-
		member(event_to_store(Ev), Com),
		event2string(Ev,EvStr),
		append(['Su'], [EvStr], Out1),
		append(Out1, ['donde tiene lugar'], Out2),
		append(Out2, ['?'], Out).
output_form( ask(X^take_down_event(X)),Com,Out):-
		member(event_to_store(Ev), Com),
		event2string(Ev, EvStr),
		member(date_to_store(Day), Com),
		date2str(Day, DateStr), 
		member(start_time_to_store(T), Com),
		member(am_or_pm(AMPM), Com),
		time2str(T, AMPM, TimeStr),
		member(location_to_store(L), Com),
		loc2str(L, LStr),
		append(['Quiere que apunte lo siguiente:'], [EvStr], Out15),
		append(Out15,[','],Out1),
		append(Out1, DateStr, Out2),
		append(Out2, [at], Out3),
		append(Out3, TimeStr, Out4),
		append(Out4, LStr, Out5),
		append(Out5, ['?'],Out).
output_form( ask(X^take_down_event(X)),Com,Out):-
		member(event_to_store(Ev), Com),
		event2string(Ev, EvStr),
		member(date_to_store(Day), Com),
		date2str(Day, DateStr), 
		member(start_time_to_store(T), Com),
		member(am_or_pm(AMPM), Com),
		time2str(T, AMPM, TimeStr),
		append(['Quiere que apunte lo siguiente:'], [EvStr], Out15),
		append(Out15,[','],Out1),
		append(Out1, DateStr, Out2),
		append(Out2, [at], Out3),
		append(Out3, TimeStr, Out4),
		append(Out4, ['?'],Out).
output_form( ask(X^which_info(X)), _, ['Qué información es incorrecta?']).
%%Check calendar
output_form( ask(X^event_to_get(X)),_,
		['Que actividad quiere verificar?']).
output_form( ask(X^date_to_get(X)),_,
		['Qué fecha es?']).
% action

output_form( action(top), _, ['volver a principio'] ).
output_form( action(change_language), _,['cambiar idioma'] ).
output_form( action(add_event),_, ['apuntar algo']).
output_form( action(A), _, ['mirar tu calendario']):- member(A, [get_event, get_time, get_date]).
output_form(action(change_info), _, ['cambiar los datos']).
output_form(action(delete_event),_, ['borrarlo todo']).
output_form(action(more_info),_, ['añadir información']).

%user help
output_form(answer(usage), _, ['AgendaTalk es un prototipo de un calendario hablante que puede apuntar información sobre tus actividades, como por ejemplo reuniones. Tambien puede dar información sobre actividades apuntadas así como la hora de una reunión. Para salir: di adiós. ']).

%confirmation and reports
output_form(confirm(add_event), _, ['El apunte se ha realizado']).
output_form(report('AddEvent', done),_,['El apunte se ha realizado']).

output_form( answer(start_time_to_store(T)), COM,['Su', Ev,'es a las', H, M]):-
	time2string(T,[H,M]),
	member(event_to_store(EVENT),COM),
	event2string(EVENT,Ev).
       
/*output_form(report('GetTime',done),COM,ReportTime):-
	member(start_time_to_store(TIME),COM),
	append(['The time is'], TIME, ReportTime).*/
	
output_form(report('ConsistentDate', done),_,[]).
output_form(report('AddEvent',failed(_)), _, ['El apunte se ha borrado']).
output_form(confirm(change_info),_,['La informacíón ha sido cambiada.']).
output_form(report('InfoChanged',failed(_)),_, ['Lo siento, no se puede cambiar la información.']).

%greetings
output_form(greet, _, [Answer]):- random(1,5,N),greetings(List),getNoflist(N,List,Answer).
output_form(quit,_, [Answer]):-random(1,4,N),goodbyes(List),getNoflist(N,List,Answer).

%For grounding
output_form(date_to_store(W),_,[W]).
%output_form(start_time_to_store(T),_,['At',T])).
%random lists
getNoflist(1,[X|_],X).
getNoflist(N,[_|Xs],Y):- Num is N-1,getNoflist(Num,Xs,Y).

greetings(['Hola!','Hola! Habla AgendaTalk, su calendario hablante.','El calendario a su disposición!', 'Hola! Habla su calendario hablante']).
goodbyes(['Adiós!','Hasta luego!','Hasta la próxima!']).
%lexical semantics (as in input_form).
output_form( Sem,_,Str):-
	lexsem( Str,Sem).

date_output(today,today) :- !.

date_output(Date,DateOutput) :-
	atom_chars(Date,Chars),
	format_to_chars('~c~c/~c~c',Chars,DateOutputCs),
	atom_chars(DateOutput,DateOutputCs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                Term to string
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
event2string(meeting, 'reunión').
event2string(appointment, 'reunión').
event2string(presentation, 'presentación').
event2string(lecture, 'clase').
event2string(party, 'fiesta').
loc2str(plaza, 'en plaza' ).

%date2str(friday, [eighteenth, of, july]).
date2str(WeekDay, DateStr):-
	sem_sort(WeekDay, weekday),
	calendar:day2date(WeekDay, date(_,M,D)),
	monthStr(MonthStr, M),
	dayStr(DayStr, D),
	append([el],DayStr,DayStr1),
	append(DayStr1, [de], DayStr2),
	append(DayStr2, MonthStr, DateStr).
%(thirtyfirst, [thirtyfirst]).
date2str(Day, [el, Day]):-
	sem_sort(Day, day).

date2str([Day,Month],[el, Day, de, Month]):-
	sem_sort(Day,day),
	sem_sort(Month,month).
date2str([WeekDay, Day,Month],[el, Day, de, Month]):-
	sem_sort(WeekDay,weekday),
	sem_sort(Day,day),
	sem_sort(Month,month).
date2str([WeekDay, Day],[el, Day, de, MonthStr]):-
	sem_sort(WeekDay,weekday),
	calendar:day2date(WeekDay, date(_,M,_)),
	monthStr([MonthStr], M),
	sem_sort(Day,day).
date2str(today,[hoy]).
date2str(tomorrow, [mañana]).

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
	append(HourStr, [y,punto], TimeStr).

time2string(CorrTime, TimeStr):-
	%calendar:ampm_disamb(Time, AMPM, CorrTime),
	name(CorrTime, [H1, M1, M2]),
	name(Hour,[H1]),
	name(Minute, [M1, M2]),
	number_phrase(HourStr, Hour),
	%number_phrase(MinStr, Minute),
	Minute = 0, 
	append(HourStr, [y,punto], TimeStr).

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

