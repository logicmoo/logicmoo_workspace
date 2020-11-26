
/***********************************************************************

 
         name: device_agenda.pl
  description: Device interface to Mysql calendar database
               Actions: Add Event, Delete Event, Change Date, Change Time
	       Queries: Event, Time, Bookings, Consistent Date, Usage
       author: Rebecca Jonson

***************************************************************************/



:- module(device_agenda, [dev_set/3,
			 dev_get/2,
			 dev_do/2,
			 dev_query/3,
			 valid_parameter/1,
			 interpret_pragmatically/3,
			 resource_of_type/1
			  
			] ).

resource_of_type(upnp_dev).


:- dynamic variable_value/2.
:- use_module( library(lists), [ member/2, select/3, append/3, nth0/3] ).
:- use_module( library(charsio), [ format_to_chars/3 ] ).


:- use_module( calendar, [day2date/2, inconsistent/2,weekday/2,next_weekday/2, today/1, tomorrow/1, ampm_disamb/3] ).
:- use_module( database_agenda, [insertDB/3, consultDB/4, countSQL/4, deleteSQL/3]).
:- use_module(library(system), [datime/1]).
:- ensure_loaded(semsort_agendatalk).

%%% Environment mode (simulation or real)

environment_mode(simulation).

%%% Valid parameter
valid_parameter(add_more_info(X)):-
	yn_answer(X).

valid_parameter(take_down_event(X)):-
	yn_answer(X).

yn_answer(yes).
yn_answer(no).

%%% Actions (action(+Name,+Parameters))

action( 'AddEvent', [event_to_store,
		     date_to_store,
		     start_time_to_store, am_or_pm, location_to_store ] ).

action( 'AddEvent', [event_to_store, date_to_store, start_time_to_store, am_or_pm ]).
action( 'DeleteEvent',[event_to_store, olddate]).
action( 'ChangeTime',[event_to_store, olddate, newtime, am_or_pm]).
action( 'ChangeDate',[event_to_store, olddate, newdate]).

%queries: query(+Query,+Parameters)

query(consistent_date(_YN), [date_to_store]).
query(usage,[]).
query(event(_A), [date_to_store,start_time_to_store,am_or_pm]).
query(time(_A), [olddate,event_to_store]).
query(bookings(_A), [date_to_store]).
query(todaysdate(_A),[]).

%%% Variable default values
default_value(location_to_store, []).
default_value(event,notbooked).

%%%2004
init_agenda :-
	environment_mode(simulation),
	%user:flag( visualize_devices, yes ),
	!.%trace,
	%ensure_loaded(library(visualization)),
	%gfx_set_initial_values(device_agendatalk,agendatalk).

init_agenda.	%see end of file

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dev_query(usage,_,usage).
dev_query( Query, Commitments, Answer):-
	query(Query,Vars),
	set_command_variables(Vars,Commitments,_Values),
	perform_query(Query,Answer).

dev_set(ID,Value1, Value) :-
	%environment_mode(simulation),
	interpret_pragmatically(ID,Value1,Value),
	try(retract(variable_value(ID,_))),
	assert(variable_value(ID,Value)).

dev_get(ID,Value) :-
	%environment_mode(simulation),
	( variable_value(ID,CurrentValue) ->
	    Value = CurrentValue
	;
	    default_value(ID,Value)
	)

	.


dev_do(Command,Commitments) :-
	action(Command,Vars),
	set_command_variables(Vars,Commitments,Values),
	perform_command(Command).

set_command_variables([],_,[]).
set_command_variables([Var|Vars],Commitments,[NewVal|NewVals]) :-
	Com =.. [ Var, Val ],
	lists:member(Com,Commitments),
	dev_set(Var,Val,NewVal),
	set_command_variables(Vars,Commitments,NewVals).

%%%%%%%%%COMMANDS%%%%%%%%%%%%%%%%%%%%%%%%%

perform_command( 'AddEvent' ) :-
	!,
	dev_get( event_to_store, Event ),
	dev_get( date_to_store, Date ),
	dev_get( start_time_to_store, Time ),
	dev_get( am_or_pm, AMPM),
	calendar:ampm_disamb(Time, AMPM, ExactTime),
	date2dbdate(Date,DBDate),
	concDaTime(Date,ExactTime,DateTime),
        database_agenda:countSQL(_Table,appt_num,set([appt_date>>DBDate]),EventNr),
	newappt_num(Date,EventNr,ApptNum),
	database_agenda:insertDB(_Table,set([appt_num=ApptNum,userid=1,appt_date=DateTime,text=Event]),_Ans).
      

perform_command( 'DeleteEvent' ) :-
	!,
	dev_get( event_to_store, Event ),
	dev_get( olddate, Date ),
	date2dbdate(Date,DBDate),
	database_agenda:selectDB(_Table,time,set([text=Event,appt_date>>DBDate]),appt_date=[DateTime]),
	DateTime \= empty,
        database_agenda:deleteSQL(_Table,set([text=Event,appt_date>>DBDate]),_).

perform_command( 'ChangeDate' ) :-
	!,
	dev_get( event_to_store, Event ),
	dev_get( olddate, OldDate ),
	dev_get( newdate, NewDate),
	date2dbdate(OldDate,OldDBDate),
        date2dbdate(NewDate,NewDBDate),
	database_agenda:countSQL(_Table,appt_num,set([appt_date>>NewDBDate]),EventNr),
	EventNr \= 0,
	newappt_num(NewDate,EventNr,ApptNum),
	database_agenda:selectDB(_Table,time,set([text=Event,appt_date>>OldDBDate]),appt_date=[DateTime]),
	DateTime \= empty,
	dbdate2datetime(DateTime,OldDate,Time),
	%%New DAte + time
	concDaTime(NewDate,Time,NewDateTime),
	database_agenda:insertDB(_Table,set([appt_num=ApptNum,userid=1,appt_date=NewDateTime,text=Event]),Ans),
	database_agenda:deleteSQL(_Table,set([text=Event,appt_date>>DateTime]),AnsDel).

perform_command( 'ChangeTime' ) :-
	!,
	dev_get( event_to_store, Event ),
	dev_get( olddate, OldDate ),
	dev_get( newtime, NewTime),
	dev_get( am_or_pm, AMPM),
	calendar:ampm_disamb(NewTime, AMPM, ExactTime),
	date2dbdate(OldDate,OldDBDate),
	%%det appt_num too!!
	database_agenda:selectDB(_Table,time,set([text=Event,appt_date>>OldDBDate]),appt_date=[DateTime]),
	DateTime \= empty,
	database_agenda:selectDB(_Table,appt_num,set([text=Event,appt_date>>OldDBDate]),appt_num=[ApptNum]),
	ApptNum \= 0,
	%dbdate2datetime(DateTime,OldDate,Time),
	concDaTime(OldDate,ExactTime,NewDateTime),
	database_agenda:deleteSQL(_Table,set([text=Event,appt_date>>DateTime]),AnsDel),
	database_agenda:insertDB(_Table,set([appt_num=ApptNum,userid=1,appt_date=NewDateTime,text=Event]),Ans).

		 
perform_command( _ ) :- true.

%%%%%QUERIES%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
perform_query(event(Event), event(Event)) :-
	!,
	dev_get( date_to_store, Date ),
	dev_get( start_time_to_store, Time ),
	dev_get( am_or_pm, AMPM),
	calendar:ampm_disamb(Time, AMPM, ExactTime),
	concDaTime(Date,ExactTime,DaTime),
	database_agenda:selectDB(_Table,event,set([appt_date>>DaTime]),text=[Event]).

perform_query(bookings(_), bookings([Events,Times])) :-
	!,
	dev_get( date_to_store, Date ),
	date2dbdate(Date,DBDate),
	database_agenda:selectDB(_Table,[text,appt_date],set([appt_date>>DBDate]),[text=Events,appt_date=Dates]),
	( Event=empty,
	  Dates=empty
	  ;
	get_times(Dates,Times)
	).
get_times([],[]).
get_times([D|Dates],[T|Times]):-
	dbdate2datetime(D,_,T),
	get_times(Dates,Times).


perform_query(time(Time),time(Time)):-
	!,
	dev_get(event_to_store, E),
	dev_get(olddate, D),
	date2dbdate(D,DBDate),
	database_agenda:selectDB(_Table,time,set([text=E,appt_date>>DBDate]),appt_date=[DateTime]),
	dbdate2datetime(DateTime,D,Time).

%%Ask for which date a specific event is held
/*
perform_query(date(Date),date(Date)):-
	!,
	dev_get(event_to_store, E),
	%%Need to be unique
	database_agenda:selectDB(_Table,date,set([text=E]),appt_date=[DateTime]),
	date2dbdate(Date,DateTime)
	

*/

perform_query( consistent_date(_), Answer) :-
	  dev_get( date_to_store, Date),
	  (calendar:inconsistent(Date, yes),
	  
	  Answer=consistent_date(no)
	;
	 
	 Answer=consistent_date(yes)
	).

%pretty weird hack
%%%Adapt usage dependent on context? (which info in shared com)
%%%no info--> introduction, some info --> more specific
perform_query( usage, usage).

perform_query(todaysdate(Today),todaysdate(Today)):-
	calendar:today(Today).


add(A,D,B) :-
	atom_chars(A,AC),
	number_chars(AN,AC),
	BN is AN + D,
	number_chars(BN,BC),
	atom_chars(B,BC).

try(G) :-
	( G -> true ; true ).


to_number(Atom,Number) :-
	number_atom(Atom),
	atom_chars(Atom,Cs),
	number_chars(Number,Cs).

number_atom(A) :-
	atomic(A),
	\+ number(A).

interpret_pragmatically(newdate,Day,DATE):-interpret_pragmatically(date_to_store,Day,DATE).
interpret_pragmatically(olddate,Day,DATE):-interpret_pragmatically(date_to_store,Day,DATE).

interpret_pragmatically(date_to_store, today, DATE) :-!,
	calendar:today(DATE).


interpret_pragmatically(date_to_store, tomorrow, DATE) :-!,
	calendar:tomorrow(DATE).


interpret_pragmatically(date_to_store, Weekday, DATE):-
	sem_sort(Weekday, weekday),
	calendar:next_weekday(DATE,Weekday).

	
interpret_pragmatically(date_to_store, Day, DATE):-
	calendar:day2date(Day, DATE).


interpret_pragmatically(_,V,V).

date_field(N,[0'0,C]) :-
	N < 10,
	!,
	format_to_chars('~d',[N],[C]).

date_field(N,Cs) :-
	format_to_chars('~d',[N],Cs).

output_upnp(Command,Parameters) :-
	Term =.. [ Command | Parameters ],
	format('\n[UPnP] ~w\n\n',[Term]).



% adds a '0' to one digit time values
zerofy(In,Out):-
	name(In,[A,B]),!,
	atom_chars(Out,[A,B]).
zerofy(In,Out):-
	name(In,[A,B,C,D]),!,
	atom_chars(Out,[A,B,C,D]).
zerofy(In,Out):-
	name(In,[A,B,C]),
	lists:append([48],[A,B,C],Out2),
	atom_chars(Out,Out2).
zerofy(In,Out):-
	name(In,[A,B,C,D,E,F,G,H]),!,
	atom_chars(Out,[A,B,C,D,E,F,G,H]).
zerofy(In,Out):-
	name(In,[A,B,C,D,E,F,G]),
	lists:append([48],[A,B,C,D,E,F,G],Out2),
	atom_chars(Out,Out2).



concAtom(A,B,AB):-
	name(A,AStr),
	name(B,BStr),
	lists:append(AStr,BStr,ABStr),
	atom_chars(AB,ABStr).

concDaTime(date(Y,M,D),B,ABStr):-
	time2dbtime(B,H:Min),
	name(H,Hour),
	name(Min,Mins),
	name(Y,Year),
	name(M,[Month1,Month2]),
	name(D,[Day1,Day2]),
	append(Year,[45],NY),
	append(NY,[Month1,Month2],YM),
	append(YM,[45],NYM),
	append(NYM,[Day1,Day2],YMD),
	append(YMD,[32],YMDSp),
	append(YMDSp,Hour,YMDSpH),
	append(YMDSpH,[58|Mins],AB),
	name(ABStr,AB).
concDaTime(date(Y,M,D),B,ABStr):-
	time2dbtime(B,H:Min),
	name(H,Hour),
	name(Min,Mins),
	name(Y,Year),
	name(M,Month),
	name(D,[Day1,Day2]),
	append(Year,[45,48],NY),
	append(NY,Month,YM),
	append(YM,[45],NYM),
	append(NYM,[Day1,Day2],YMD),
	append(YMD,[32],YMDSp),
	append(YMDSp,Hour,YMDSpH),
	append(YMDSpH,[58|Mins],AB),
	name(ABStr,AB).
concDaTime(date(Y,M,D),B,ABStr):-
	time2dbtime(B,H:Min),
	name(H,Hour),
	name(Min,Mins),
	name(Y,Year),
	name(M,[Month1,Month2]),
	name(D,Day),
	append(Year,[45],NY),
	append(NY,[Month1,Month2],YM),
	append(YM,[45,48],NYM),
	append(NYM,Day,YMD),
	append(YMD,[32],YMDSp),
	append(YMDSp,Hour,YMDSpH),
	append(YMDSpH,[58|Mins],AB),
	name(ABStr,AB).
concDaTime(date(Y,M,D),B,ABStr):-
	time2dbtime(B,H:Min),
	name(H,Hour),
	name(Min,Mins),
	name(Y,Year),
	name(M,Month),
	name(D,Day),
	append(Year,[45,48],NY),
	append(NY,Month,YM),
	append(YM,[45,48],NYM),
	append(NYM,Day,YMD),
	append(YMD,[32],YMDSp),
	append(YMDSp,Hour,YMDSpH),
	append(YMDSpH,[58|Mins],AB),
	name(ABStr,AB).

date2dbdate(date(Y,M,D),NewDate):-
	name(Y,Year),
	name(M,[Month1,Month2]),
	name(D,[Day1,Day2]),
	append(Year,[45],NY),
	append(NY,[Month1,Month2],YM),
	append(YM,[45],NYM),
	append(NYM,[Day1,Day2],YMD),
	name(NewDate,YMD).
date2dbdate(date(Y,M,D),NewDate):-
	name(Y,Year),
	name(M,Month),
	name(D,[Day1,Day2]),
	append(Year,[45,48],NY),
	append(NY,Month,YM),
	append(YM,[45],NYM),
	append(NYM,[Day1,Day2],YMD),
	name(NewDate,YMD).
date2dbdate(date(Y,M,D),NewDate):-
	name(Y,Year),
	name(M,[Month1,Month2]),
	name(D,Day),
	append(Year,[45],NY),
	append(NY,[Month1,Month2],YM),
	append(YM,[45,48],NYM),
	append(NYM,Day,YMD),
	name(NewDate,YMD).
date2dbdate(date(Y,M,D),NewDate):-
	name(Y,Year),
	name(M,Month),
	name(D,Day),
	append(Year,[45,48],NY),
	append(NY,Month,YM),
	append(YM,[45,48],NYM),
	append(NYM,Day,YMD),
	name(NewDate,YMD).


time2dbtime(Time,H:00):-
	name(Time,[H1,H2,48,48]),
	name(H,[H1,H2]).
time2dbtime(Time,H:M):-
	name(Time,[H1,H2,M1,M2]),
	name(H,[H1,H2]),
	name(M,[M1,M2]).
time2dbtime(Time,H:M):-
	name(Time,[H1,M1,M2]),
	name(H,[H1]),
	name(M,[M1,M2]).

dbdate2datetime(empty,_,empty).
dbdate2datetime(DateTime,date(Y,M,D),Time):-
        name(DateTime,[Y1,Y2,Y3,Y4,45,M1,M2,45,D1,D2,32,H1,H2,58,Min1,Min2,58,_,_]),
	append([H1,H2],[Min1,Min2],T),
	name(Time,T),
	name(Y,[Y1,Y2,Y3,Y4]),
	name(M,[M1,M2]),
	name(D,[D1,D2]).


      
newappt_num(date(Y,M,D),EventNr,ApptNum):-
	M < 10,
	D < 10,
	name(Y,[_Mil,O,O,Nr]),
	name(M,M1),
	name(D,D1),
	name(EventNr,Nr1),
	append([49],[O],Start),
	append(Start,[Nr],Year),
	append(Year,[48|M1],Day),
	append(Day,[48|D1],EndDate),
	append(EndDate,[48|Nr1],Appt),
	name(ApptNum,Appt).

newappt_num(date(Y,M,D),EventNr,ApptNum):-
	M > 10,
	D < 10,
	name(Y,[_Mil,O,O,Nr]),
	name(M,M1),
	name(D,D1),
	name(EventNr,Nr1),
	append([49],[O],Start),
	append(Start,[Nr],Year),
	append(Year,M1,Day),
	append(Day,[48|D1],EndDate),
	append(EndDate,[48|Nr1],Appt),
	name(ApptNum,Appt).

newappt_num(date(Y,M,D),EventNr,ApptNum):-
	M < 10,
	D > 10,
	name(Y,[_Mil,O,O,Nr]),
	name(M,M1),
	name(D,D1),
	name(EventNr,Nr1),
	append([49],[O],Start),
	append(Start,[Nr],Year),
	append(Year,[48|M1],Day),
	append(Day,D1,EndDate),
	append(EndDate,[48|Nr1],Appt),
	name(ApptNum,Appt).

newappt_num(date(Y,M,D),EventNr,ApptNum):-
	M > 10,
	D > 10,
	name(Y,[_Mil,O,O,Nr]),
	name(M,M1),
	name(D,D1),
	name(EventNr,Nr1),
	append([49],[O],Start),
	append(Start,[Nr],Year),
	append(Year,M1,Day),
	append(Day,D1,EndDate),
	append(EndDate,[48|Nr1],Appt),
	name(ApptNum,Appt).
	

:- init_agenda.
