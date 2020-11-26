 
/*************************************************************************
 
         name: interpret_english.pl 
      version: 
  description: An domain-specific interpretation file for AgendaTalk
       author: Rebecca Jonson 
 
*************************************************************************/

%:- discontiguous output_form/3, input_form/2, plan/2, postcond/2.
:-multifile input_form/2.


:- use_module(library(random)).
:- use_module( library(lists), [ member/2, select/3, append/3 ] ).
:- use_module( library(charsio), [ format_to_chars/3 ] ).
:- use_module(calendar, [ampm_disamb/3, day2date/2]).
:- ensure_loaded(digits_english).
:- ensure_loaded(semsort_agendatalk).

/*----------------------------------------------------------------------
     input_form( +Phrase, -Move )
     -- Almost canned input
----------------------------------------------------------------------*/

%AgendaTalk




input_form([take, down, an, event], request(add_event)).
input_form([take, a, note], request(add_event)).
input_form([take, down],request(add_event)).
input_form([i,want,to,book],request(add_event)).
input_form([book],request(add_event)).
input_form([note, down],request(add_event)).
input_form([note],request(add_event)).
input_form([add, an, event, to, the,calendar],request(add_event)).
input_form([add, something, to, the,calendar],request(add_event)).
input_form([add, to, the,calendar],request(add_event)).

input_form([check,calendar], request(get_info)).
input_form([check,the, calendar], request(get_info)).
input_form([check,my, calendar], request(get_info)).

input_form([add, some, more, information], request(more_info)).
input_form([add, some, information], [request(more_info)]).

input_form([delete, all],request(delete_current_event)).
input_form([delete, it, all],request(delete_current_event)).
input_form([erase, it, all],request(delete_current_event)).
input_form([delete,an,event], request(delete_event)).
input_form([delete],request(delete_event)).
input_form([erase,an,event],request(delete_event)).
input_form([erase, a, booking],request(delete_event)).
input_form([erase],request(delete_event)).

input_form([change, some, information], request(change_info)).
input_form([the, date], answer(which_info(date))).
input_form([the, day], answer(which_info(date))).
input_form([the, time], answer(which_info(time))).
input_form([the, hour], answer(which_info(time))).
input_form([the, location], answer(which_info(location))).
input_form([the,place], answer(which_info(location))).
input_form([change, the, date], [request(change_info), answer(which_info(date))]).
input_form([change, the, time], [request(change_info), answer(which_info(time))]).
input_form([change, time], [request(change_info), answer(which_info(time))]).
input_form([change, date], [request(change_info), answer(which_info(date))]).
input_form([change, the, location], [request(change_info), answer(which_info(location))]).
input_form([change, place], [request(change_info), answer(which_info(location))]).
%HELP
input_form( [help], ask(usage) ).
input_form( [what, can, i, do], ask(usage)).
input_form( [i, need, some, help], ask(usage)).

input_form( [top],                 request(top) ).
 
%%%%%%%%%%%%Check Calendar%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

input_form([what, time],ask(X^start_time_to_store(X))).
input_form([when, is],ask(X^start_time_to_store(X))).
input_form([check, the, time],ask(X^start_time_to_store(X))).

input_form([am, i, booked],ask(X^event_to_store(X))).
input_form([am, i, free],ask(X^event_to_store(X))).
input_form([check, if, i, am, booked],ask(X^event_to_store(X))).
input_form([am, i, busy],ask(X^event_to_store(X))).
input_form([what, is, on, my, schedule],ask(X^event_to_store(X))).
input_form([what, is, on, my, calendar],ask(X^bookings(X))).
input_form([what, am, i, up, to],ask(X^bookings(X))).
input_form([do,i,have,anything,scheduled],ask(X^event_to_store(X))).
input_form([what,do,i,have],ask(X^bookings(X))).
input_form([what,about],ask(X^bookings(X))).

%%%%%%%%%%%%%%%%%%EVENTS%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%input_form( E,	answer(event_to_store(C))):- lexsem(E, C), sem_sort(C, event).
input_form( [E],	answer(event_to_store(E))):- sem_sort(E, event).
input_form( L,	[request(more_info),answer(location_to_store(C))] ):-lexsem(L, C), sem_sort(C, location).

%%%DATES%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
input_form( [today],               answer(date_to_store(today)) ).
input_form( [tomorrow],            answer(date_to_store(tomorrow))).

input_form( [the,day,after,tomorrow],            answer(date_to_store(aftertomorrow))).
input_form([next,week], answer(date_to_store([next,week]))).
%%friday the first of july
input_form([Weekday, the, Day, of, Month], answer(date_to_store([Weekday, Day, Month]))):-
	weekDays(WDs),
	member(Weekday, WDs),
	dayStr( [Day], _),
	monthStr([Month],_).
%%friday first of july
input_form([Weekday, Day, of, Month], answer(date_to_store([Weekday, Day, Month]))):-
	weekDays(WDs),
	member(Weekday, WDs),
	dayStr( [Day], _),
	monthStr([Month],_).
%% (the) first of july
input_form([the, Day, of, Month], answer(date_to_store([Day, Month]))):-
	dayStr( [Day], _),
	monthStr([Month],_).
%input_form([thirtyfirst, of, april], answer(incons_date([thirtyfirst, april]))).
input_form([Day, of, Month], answer(date_to_store([Day, Month]))):-
	dayStr( [Day], _),
	monthStr([Month],_).
%%july the first
input_form([Month, the, Day], answer(date_to_store([Day, Month]))):-
	dayStr( [Day], _),
	monthStr([Month],_).
%%the first
input_form([the, Day], answer(date_to_store(Day))):-
	dayStr( [Day], _).
%%friday the second
input_form([Weekday, the, Day], answer(date_to_store([Weekday, Day]))):-
	weekDays(WDs),
	member(Weekday, WDs),
	dayStr( [Day], _).
%%friday thirtyfirst
input_form([Weekday, Day], answer(date_to_store([Weekday, Day]))):-
	weekDays(WDs),
	member(Weekday, WDs),
	dayStr( [Day], _).
%%next friday
input_form( [next, WeekDay],             answer(date_to_store([next, WeekDay])) ):-
	weekDays(WDs),
	member(WeekDay, WDs).

%%friday
input_form( [on | WeekDayStr ], answer( date_to_store(WeekDay))):-
	  input_form( WeekDayStr, answer(date_to_store(WeekDay))).

input_form( [WeekDay],             answer(date_to_store(WeekDay)) ):-
	weekDays(WDs),
	member(WeekDay, WDs).

%%%%%%%%%%%%%%%%%TIME EXPRESSIONS%%%%%%%%%%%%%%%%%%%%%%%%%%%%
input_form( [a,m], answer(am_or_pm(am))).
input_form( [in, the, morning], answer(am_or_pm(am))).
input_form( [before, lunch], answer(am_or_pm(am))).
input_form( [morning], answer(am_or_pm(am))).
input_form( [pm], answer(am_or_pm(pm))).
input_form( [in, the, afternoon], answer(am_or_pm(pm))).
input_form( [afternoon], answer(am_or_pm(pm))).
input_form( [after,lunch], answer(am_or_pm(pm))).
input_form( [night],answer(am_or_pm(pm))).

input_form( [tonight],[answer(date_to_store(today)),answer(am_or_pm(pm))]).
input_form( [this,morning],[answer(date_to_store(today)),answer(am_or_pm(am))]).
input_form( [this,afternoon],[answer(date_to_store(today)),answer(am_or_pm(pm))]).
%%Time with ampm disambiguation RJ
input_form([at, noon],[TIME, answer(am_or_pm(am))]):-input_form([at, twelve], TIME).
input_form([at, midnight],[TIME, answer(am_or_pm(pm))]):-input_form([at, twelve], TIME).

input_form([at,half,past|S1],answer(time(C))):-
	append(S1,[thirty],S1S2),
	input_form(S1S2, answer(time(C))).

input_form([half,past|S1],answer(time(C))):-
	append(S1,[thirty],S1S2),
	input_form(S1S2, answer(time(C))).

input_form([a,quarter,past|S1],answer(time(C))):-	
	append(S1,[fifteen],S1S2),
	input_form(S1S2, answer(time(C))).

input_form([quarter,past|S1],answer(time(C))):-	
	append(S1,[fifteen],S1S2),
	input_form(S1S2, answer(time(C))).

input_form([quarter,to|S1],answer(time(C2))):-
	input_form(S1,answer(number(C))),
	C1 is C - 1,
	input_form(S2,answer(number(C1))),
	append(S2,[fortyfive],S1S2),
	input_form(S1S2, answer(time(C2))).

input_form([ten,past|S1],answer(time(C))):-	
	append(S1,[ten],S1S2),
	input_form(S1S2, answer(time(C))).

input_form([ten,to|S1],answer(time(C2))):-
	input_form(S1,answer(number(C))),
	C1 is C - 1,
	input_form(S2,answer(number(C1))),
	append(S2,[fifty],S1S2),
	input_form(S1S2, answer(time(C2))).

input_form([five,past|S1],answer(time(C))):-	
	append(S1,[five],S1S2),
	input_form(S1S2, answer(time(C))).

input_form([five,to|S1],answer(time(C2))):-
	input_form(S1,answer(number(C))),
	C1 is C - 1,
	input_form(S2,answer(number(C1))),
	append(S2,[fiftyfive],S1S2),
	input_form(S1S2, answer(time(C2))).
input_form([twenty,past|S1],answer(time(C))):-	
	append(S1,[twenty],S1S2),
	input_form(S1S2, answer(time(C))).

input_form([twenty,to|S1],answer(time(C2))):-
	input_form(S1,answer(number(C))),
	C1 is C - 1,
	input_form(S2,answer(number(C1))),
	append(S2,[forty],S1S2),
	input_form(S1S2, answer(time(C2))).

input_form([S1, S2], [answer(time(C)), answer(am_or_pm(pm))]):-
	input_form([S1,S2], answer(time(C))), 
	calendar:dayhalf(C,pm).

input_form([at | [S1,S2]], [answer(time(C)), answer(am_or_pm(pm))]):-
	input_form([S1,S2], answer(time(C))),
	calendar:dayhalf(C, pm).
input_form([at | [S1,S2]], answer(time(C))):-
	input_form([S1,S2], answer(time(C))).
	%sem_sort(C,_).
	
input_form([at| S1], [answer(time(C)),answer(am_or_pm(pm))]):-
	append(S1, [zero], S1S2),
	input_form(S1S2, answer(time(C))),
	calendar:dayhalf(C, pm).

input_form([at| S1], answer(time(C))):-
	append(S1, [zero], S1S2),
	input_form(S1S2 ,answer(time(C))).
%%%Starttime

input_form([at | [S1,S2]], [answer(start_time_to_store(C)), answer(am_or_pm(pm))]):-
	input_form([S1,S2], answer(time(C))),
	calendar:dayhalf(C, pm).
input_form([at | [S1,S2]], answer(start_time_to_store(C))):-
	input_form([S1,S2], answer(time(C))),
	calendar:dayhalf(C,_).
input_form([at| S1], answer(start_time_to_store(C))):-
	append(S1, [zero], S1S2),
	input_form(S1S2 ,answer(time(C))),
	calendar:dayhalf(C,_).
	
input_form([at| S1], [answer(start_time_to_store(C)),answer(am_or_pm(pm))]):-
	append(S1, [zero], S1S2),
	input_form(S1S2, answer(time(C))),
	calendar:dayhalf(C, pm).


% time: two numbers in sequence


input_form( [S1,S2] , answer( time( C ) ) ) :-
	!,
	lexsem( [S1], C1 ),
	sem_sort( C1, number ),
	lexsem( [S2], C2 ),
	sem_sort( C2, number ),
	name( C1, C1S ),
	name( C2, C2S ),
	(C2S=[_,_] ->
	    C3S=C2S
	;
	    append([48],C2S,C3S)
	),
	append( C1S, C3S, CS ),
	name( C, CS ),
	sem_sort( C, time ).


% time: three numbers in sequence
input_form( [S1,S2,S3] , answer( time( C ) ) ) :-
	lexsem( [S1], C1 ),
	sem_sort( C1, number ),
	lexsem( [S2,S3], C2 ),
	sem_sort( C2, number ),
	name( C1, C1S ),
	name( C2, C2S ),
	append( C1S, C2S, CS),
	name( C, CS ),
	sem_sort( C, time ).


% numbers
input_form( S, answer( number( C ) ) ) :-
	lexsem( S, C ),
	sem_sort( C, number ).

/*----------------------------------------------------------------------
     lexsem( ?Word, ?Concept )
     -- Lexical semantics
----------------------------------------------------------------------*/

% use semantics as surface forms (only possible for english)
lexsem( Word, Concept ):-
	synset( Words, Concept ),
	member( Word, Words ).


synset( [NumberPhrase], Number ):- number_phrase( NumberPhrase, Number ).


synset([[plaza]],plaza).
synset([[room, NR]], Room):- number_phrase(NR,Number), append("room", Number,Room).

weekDays([monday,tuesday,wednesday,thursday,friday,saturday,sunday]).

