:- module(semsort_agendatalk, [sem_sort/2, isa/2] ).

sem_sort( english, language ).
sem_sort( svenska, language ).

% action
sem_sort( top, action ).


sem_sort( add_event, action ).
sem_sort( get_time, action).
sem_sort( change_event, action).
sem_sort( change_time, action).
sem_sort( change_date, action).
sem_sort( delete_event, action).
sem_sort( delete_current_event, action).
sem_sort( more_info, action).
sem_sort( consistent_date, action).

sem_sort( yes, yn_answer).
sem_sort( yeah, yn_answer).
sem_sort( okay, yn_answer).
sem_sort( correct, yn_answer).
sem_sort( no, yn_answer).
%events

sem_sort(meeting, event).
sem_sort(trip,event).
sem_sort(appointment, event).
sem_sort(class, event).
sem_sort(conference, event).
sem_sort(haircut, event).
sem_sort(dentist, event).
sem_sort(party, event).
sem_sort(date, event).
sem_sort(deadline, event).
sem_sort(presentation, event).
sem_sort(lecture,event).
sem_sort(training,event).
sem_sort(shopping,event).
sem_sort(movie,event).
sem_sort(museum,event).
sem_sort(coffee,event).
sem_sort(dinner,event).
sem_sort(lunch,event).
sem_sort(exam,event).
sem_sort(lab,event).
sem_sort(concert,event).
sem_sort(empty,event).




%locations

sem_sort(home, location).
sem_sort(plaza, location).
sem_sort(myroom, location).
sem_sort(fair, location).
sem_sort(cafe, location).
sem_sort(palace, location).
sem_sort(ritz, location).
sem_sort(office, location).
sem_sort(school, location).
sem_sort(dialoglab, location).
sem_sort(aquarium, location).
sem_sort(roomG312, location).
sem_sort(roomF314, location).
sem_sort(maclab,location).
%cities

sem_sort(madrid, city).
sem_sort(london, city).
sem_sort(barcelona, city).
sem_sort(stockholm, city).
sem_sort(gothenburg, city).
sem_sort(paris, city).

%means_of_transport

sem_sort(plane, means_of_transport).
sem_sort(bus, means_of_transport).
sem_sort(train, means_of_transport).
sem_sort(boat, means_of_transport).
sem_sort(car, means_of_transport).

%persons OBS! senare i agendan

sem_sort(rebecca, person).
sem_sort(oscar, person).
sem_sort(robin, person).
sem_sort(staffan, person).
sem_sort(stina, person).
sem_sort(david, person).

%info
sem_sort(date, info).
sem_sort(time, info).
sem_sort(event, info).
sem_sort(location, info).

% number
sem_sort( N, number ) :-
	integer(N).

% date
sem_sort(today, date ).
sem_sort(tomorrow, date ).
sem_sort(aftertomorrow, date ).
sem_sort([christmas,eve],date).
sem_sort([christmas, day],date).
sem_sort([newyear, eve],date).
sem_sort([newyear, day],date).
sem_sort(WD, date):-
	sem_sort(WD, weekday).
sem_sort([next, WD], date):-
	sem_sort(WD, weekday).
sem_sort([WD, Day], date):-
	sem_sort(WD, weekday),
	sem_sort(Day, day).
sem_sort(Day, date):-
	sem_sort(Day,day).
sem_sort([Day, Month], date):-
	sem_sort(Day, day),
	sem_sort(Month, month).
sem_sort([WD, Day, Month], date):-
	sem_sort(WD, weekday),
	sem_sort(Day, day),
	sem_sort(Month, month).
sem_sort(date(_Y,_M,_D),date).

sem_sort(monday,weekday).
sem_sort(tuesday,weekday).
sem_sort(wednesday,weekday).
sem_sort(thursday,weekday).
sem_sort(friday,weekday).
sem_sort(saturday,weekday).
sem_sort(sunday,weekday).

sem_sort(first, day).
sem_sort(second, day).
sem_sort(third, day).
sem_sort(fourth, day).
sem_sort(fifth, day).
sem_sort(sixth, day).
sem_sort(seventh, day).
sem_sort(eigth, day).
sem_sort(ninth, day).
sem_sort(tenth, day).
sem_sort(eleventh, day).
sem_sort(twelfth, day).
sem_sort(thirteenth, day).
sem_sort(fourteenth, day).
sem_sort(fifteenth, day).
sem_sort(sixteenth, day).
sem_sort(eighteenth, day).
sem_sort(nineteenth, day).
sem_sort(twentieth, day).
sem_sort(twentyfirst, day).
sem_sort(twentysecond, day).
sem_sort(twentythird, day).
sem_sort(twentyfourth, day).
sem_sort(twentyfifth, day).
sem_sort(twentysixth, day).
sem_sort(twentyseventh, day).
sem_sort(twentyeigth, day).
sem_sort(twentyninth, day).
sem_sort(thirtieth, day).
sem_sort(thirtyfirst, day).

sem_sort(january, month).
sem_sort(february, month).
sem_sort(march, month).
sem_sort(april, month).
sem_sort(may, month).
sem_sort(june, month).
sem_sort(july, month).
sem_sort(august, month).
sem_sort(september, month).
sem_sort(october, month).
sem_sort(november, month).
sem_sort(december, month).

sem_sort(am, dayhalf).
sem_sort(pm, dayhalf).
% time
sem_sort(empty,time).
sem_sort( T, time ) :-
	integer( T ),
	name( T, [ T1, T2, T3, T4 ] ),
	name( Hour, [ T1, T2 ] ),
	Hour >= 0,
	Hour =< 23,
	name( Min, [ T3, T4 ] ),
	Min >= 0,
	Min =< 59.
sem_sort( T, time ) :-
	integer( T ),
	name( T, [ T2, T3, T4 ] ),
	T1 = 48,
	name( Hour, [ T1, T2 ] ),
	Hour >= 0,
	Hour =< 23,
	name( Min, [ T3, T4 ] ),
	Min >= 0,
	Min =< 59.
	
to_number( Atom, Number ) :-
%	name( Atom, String ),
	number_atom( Atom ),
	atom_chars( Atom,Cs ),
	number_chars( Number, Cs ).

number_atom(A) :-
	atomic(A),
	\+ number(A).



/*--------------------
conceptual hierarchy
--------------------*/
%AgendaTalk
isa( location_to_store, location).
isa( event_to_store, event).
isa( time, number ).
isa( date_to_store, date ).
isa( newdate, date ).
isa( olddate, date_to_store ).
isa( start_time_to_store, time).
isa( stop_time_to_store, time ).
isa( newtime, time ).
isa( person_to_store, person ).
isa( city_to_store, city).
isa( depcity_to_store, city).
isa( destcity_to_store, city).
isa( transport_to_store, means_of_transport).
isa( age_to_store, number).
isa( am_or_pm, dayhalf).
isa( which_info, info).
isa( T0, T2 ):-
	T0 \= T2,
	isa( T0, T1 ),
	isa( T1, T2 ).



