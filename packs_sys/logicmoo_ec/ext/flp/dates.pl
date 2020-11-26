%% generatePageFor(dates,UserName,[overdue,OverdueReversed,upcoming,Upcoming,currentdatetime,CurrentDateTime]) :-
%% 	catalystUserNameResolvesToAgent(UserName,UserActualName),
%% 	getEvents(Upcoming,<),
%% 	getEvents(Overdue,>),
%% 	reverse(Overdue,OverdueReversed),
%% 	getCurrentDateTime(TmpCurrentDateTime),
%% 	generateGlossFor(TmpCurrentDateTime,CurrentDateTime).

month('Jan',1,31).
month('Feb',2,29).
month('Mar',3,31).
month('Apr',4,30).
month('May',5,31).
month('Jun',6,30).
month('Jul',7,31).
month('Aug',8,31).
month('Sep',9,30).
month('Oct',10,31).
month('Nov',11,30).
month('Dec',12,31).

dayOfWeek('monday','Mon').
dayOfWeek('tuesday','Tue').
dayOfWeek('wednesday','Wed').
dayOfWeek('thursday','Thu').
dayOfWeek('friday','Fri').
dayOfWeek('saturday','Sat').
dayOfWeek('sunday','Sun').

number_addendum(1,'st').
number_addendum(2,'nd').
number_addendum(3,'rd').
number_addendum(4,'th').
number_addendum(5,'th').
number_addendum(6,'th').
number_addendum(7,'th').
number_addendum(8,'th').
number_addendum(9,'th').
number_addendum(10,'th').
number_addendum(11,'th').
number_addendum(12,'th').
number_addendum(12,'th').
number_addendum(13,'th').
number_addendum(14,'th').
number_addendum(15,'th').
number_addendum(16,'th').
number_addendum(17,'th').
number_addendum(18,'th').
number_addendum(19,'th').
number_addendum(20,'th').
number_addendum(21,'st').
number_addendum(22,'nd').
number_addendum(23,'rd').
number_addendum(24,'th').
number_addendum(25,'th').
number_addendum(26,'th').
number_addendum(27,'th').
number_addendum(28,'th').
number_addendum(29,'th').
number_addendum(30,'th').
number_addendum(31,'st').


long_to_short_day_of_month(L,S) :-
	number_addendum(S,Ext),
	atomic_list_concat([S,Ext],'',L).

getEvents(Events,Comparison) :-
	setof([A,B],Y^M^D^H^Mi^S^Z^(significantDateForEvent(A,[Y-M-D,H:Mi:S]),
		       Z = [Y-M-D,H:Mi:S],
		       julian:form_time(Z,B)),List),
	setof(Date,Key^member([Key,Date],List),Dates),
	predsort(julian:compare_time,Dates,Tmp1),
	setof(Date,Tmp1^member(Date,Tmp1),SortedDates),
	%% hasLocation(User,Location),
	%% hasTimeZone(Location,TimeZone),
	getCurrentDateTime(Now),
	%% view([sortedDates,SortedDates]),
	findall([[PrintedDateTime,EnglishDescription,Gloss],Tasks],
		(
		 member(Date,SortedDates),
		 julian:form_time([Date,[Y-M-D,H:Mi:S]]),
		 printDateTime([Y-M-D,H:Mi:S],PrintedDateTime),
		 julian:compare_time(Comparison,Now,[Y-M-D,H:Mi:S]),
		 DateTime = [Y-M-D,H:Mi:S],
		 findall([task(TaskID2,Desc),MeetingAgenda,MeetingParticipants,MeetingLocations,MeetingPhoneNumbers],
			 (member([TaskID2,Date],List),
			  meetingInfo(TaskID2,_,Desc,MeetingParticipants,MeetingAgenda,MeetingLocations,MeetingPhoneNumbers)),
			 Tasks),
		 englishDescriptionOfTimeUntil(Now,[Y-M-D,H:Mi:S],EnglishDescription),
		 generateGlossFor([Y-M-D,H:Mi:S],Gloss)
		 ),
		Events).

meetingInfo(TaskID,DateTime,Desc,MeetingParticipants,MeetingAgenda,MeetingLocations,MeetingPhoneNumbers) :-
	significantDateForEvent(TaskID,DateTime),
	description(TaskID,Desc),
	meetingAgenda(TaskID,MeetingParticipants,MeetingAgenda),
	meetingLocations(TaskID,MeetingLocations),
	meetingPhoneNumbers(TaskID,MeetingParticipants,MeetingPhoneNumbers).

generateGlossFor([Y-M-D,H:Mi:S],Gloss) :-
	form_time([dow(DayOfWeek),Y-M-D]),
	dayOfWeek(DayOfWeek,DOW),
	month(Month,M,_),
	long_to_short_day_of_month(DayOfMonth,D),
	(H > 12 -> (H12 is H - 12, AmPm = 'PM') ;
	 (H = 12 -> (H12 is 12, AmPm = 'PM') ;
	  (H = 0 -> (H12 is 12, AmPm = 'AM') ;
	   (H < 12 -> (H12 is H, AmPm = 'AM'))))),
	format(atom(Mi0),'~|~`0t~w~2|', Mi),
	atomic_list_concat([DOW,' ',Month,' ',DayOfMonth,' ',Y,' at ',H12,':',Mi0,' ',AmPm],Gloss),!.

%% getDeadlines(Deadlines,Comparison) :-
%% 	findall([X,Y],(deadline(X,Z),julian:form_time(Z,Y)),List),
%% 	setof(Date,Key^member([Key,Date],List),Dates),
%% 	predsort(julian:compare_time,Dates,SortedDates),
%% 	julian:form_time([now,[Yn-Mn-Dn,Hn:Min:Sn]]),
%% 	Now = [Yn-Mn-Dn,Hn:Min:Sn],
%% 	findall([[PrintedDateTime,EnglishDescription],Tasks],
%% 		(
%% 		 member(Date,SortedDates),
%% 		 julian:form_time([Date,[Y-M-D,H:Mi:S]]),
%% 		 printDateTime([Y-M-D,H:Mi:S],PrintedDateTime),
%% 		 julian:compare_time(Comparison,Now,[Y-M-D,H:Mi:S]),
%% 		 englishDescriptionOfTimeUntil(Now,[Y-M-D,H:Mi:S],EnglishDescription),
%% 		 DateTime = [Y-M-D,H:Mi:S],
%% 		 findall(task(TaskID2,Desc),(member([TaskID2,Date2],List),Date=Date2,description(TaskID2,Desc)),Tasks)
%% 		),
%% 		Deadlines).

sortDeadlines(Deadlines) :-
	findall([X,Y],(deadline(X,Z),julian:form_time(Z,Y)),List),
	%% view([list,List]),
	setof(Date,Key^member([Key,Date],List),Dates),
	%% view([dates,Dates]),
	predsort(julian:compare_time,Dates,SortedDates),
	findall([DateTime,Tasks],
		(
		 member(Date,SortedDates),
		 julian:form_time([Date,[Y-M-D,H:Mi:S]]),
		 DateTime = [Y-M-D,H:Mi:S],
		 findall(TaskID2,(member([TaskID2,Date2],List),Date=Date2),Tasks)
		),
		Deadlines).

englishDescriptionOfTimeUntil(From,To,EnglishDescription) :-
	julian_daysUntilDate_precise(From,To,_,DaysEstimated),
	%% view([daysEstimated,DaysEstimated]),
	Abs is abs(DaysEstimated),
	%% view([abs,Abs]),
	((DaysEstimated < 0) ->
	 (%% view(1),
	  FloorDays is floor(Abs),
	  (Abs > 1.0) -> ((Abs < 2.0) -> (Template = ['1 day ago',[]]) ; (Template = ['~d days ago',[FloorDays]])) ;
	  (%% view(2),
	   %% view([abs,Abs]),
	   Hours is Abs * 24.0,
	   %% view(2.5),
	   FloorHours is floor(Hours),
	   %% view([hours,Hours]),
	   %% view(3),
	   ((Hours > 1.0) -> ((Hours < 2.0) -> (Template = ['1 hour ago',[]]) ; (Template = ['~d hours ago',[FloorHours]])) ;
	   (%% view(4),
	    %% view([hours,Hours]),
	    Minutes is Hours * 60.0,
	    %% view(5),
	    FloorMinutes is floor(Minutes),
	    %% view([minutes,Minutes,floorMinutes,FloorMinutes]),
	    (Minutes > 1.0) -> ((Minutes < 2.0) -> (Template = ['1 minute ago',[]]) ; (Template = ['~d minutes ago',[FloorMinutes]])) ;
	    (Template = ['less than a minute ago',[]]))))) ;
	 (%% view(1),
	  FloorDays is floor(Abs),
	  (Abs > 1.0) -> ((Abs < 2.0) -> (Template = ['in 1 day',[]]) ; (Template = ['in ~d days',[FloorDays]])) ;
	  (%% view(2),
	   %% view([hours,Abs]),
	   Hours is Abs * 24.0,
	   %% view(2.5),
	   FloorHours is floor(Hours),
	   %% view([hours,Hours]),
	   %% view(3),
	   ((Hours > 1.0) -> ((Hours < 2.0) -> (Template = ['in 1 hour',[]]) ; (Template = ['in ~d hours',[FloorHours]])) ;
	   (%% view(4),
	    %% view([hours,Hours]),
	    Minutes is Hours * 60.0,
	    %% view(5),
	    FloorMinutes is floor(Minutes),
	    %% view([minutes,Minutes,floorMinutes,FloorMinutes]),
	    (Minutes > 1.0) -> ((Minutes < 2.0) -> (Template = ['in 1 minute',[]]) ; (Template = ['in ~d minutes',[FloorMinutes]])) ;
	    (Template = ['less than in a minute',[]])))))
	),
	append([atom(EnglishDescription)],Template,Appended),
	Call =.. [format|Appended],
	Call.

printDateTime([Y-M-D,H:Mi:S],PrintedDateTime) :-
	%% view([Y,M,D,H,Mi,S]),
	format(atom(PrintedDateTime),'~d-~d-~d,~d:~d:~d',[Y,M,D,H,Mi,S]).

getCurrentDateTime(DateTime) :-
	currentTimeZone(TimeZone),
	getCurrentDateTimeForTimeZone(TimeZone,DateTime).

getCurrentDate([TmpDate]) :-
	currentTimeZone(TimeZone),
	getCurrentDateTimeForTimeZone(TimeZone,[TmpDate,_]).

getCurrentTime([TmpTime]) :-
	currentTimeZone(TimeZone),
	getCurrentDateTimeForTimeZone(TimeZone,[_,TmpTime]).

%% :- julian:form_time(now,Start),julian:form_time([Start,[Y-M-D,H:Mi:S]]),view([Y-M-D,H:Mi:S]),julian:form_time([2016-4-29,5:30:00],Finish),englishDescriptionOfTimeUntil(Start,Finish,EnglishDescription),view(EnglishDescription).

hasUTCOffset(utc,0).
hasUTCOffset(centralDaylightTime,5).
hasUTCOffset(centralStandardTime,6).

convertUTCDateTimeToTimeZoneDateTime(UTCDateTime,TimeZone,[Y-M-D,H:Mi:S]) :-
	hasUTCOffset(TimeZone,Offset),
	DTs is Offset * 60 * 60,
	delta_time([Y-M-D,H:Mi:S], s(DTs), UTCDateTime).

getCurrentDateTimeForTimeZone(TimeZone,[Y-M-D,H:Mi:S]) :-
	getCurrentUTCDateTime(UTCDateTime),
	convertUTCDateTimeToTimeZoneDateTime(UTCDateTime,TimeZone,[Y-M-D,H:Mi:S]).

getCurrentUTCDateTime([Y-M-D,H:Mi:S]) :-
	julian:form_time([now,[Y-M-D,H:Mi:S]]).

render(auroraIllinois,'Aurora').
render(stCharlesIllinois,'St. Charles').
render(napervilleIllinois,'Naperville').

renderObject(Object,Output) :-
	Object = addressFn(StreetAddress,City,State,ZipCode),
	render(City,CityTmp),
	atomic_list_concat([CityTmp,','],'',CityRendered),
	capitalize(State,StateRendered),
	((StreetAddress = houseNumberFn(Number,Street)) ->
	 (atomic_list_concat([Number,Street,CityRendered,StateRendered,ZipCode],' ',Output)) ;
	 (atomic_list_concat([StreetAddress,CityRendered,StateRendered,ZipCode],' ',Output))).

meetingAgenda(TaskID,MeetingParticipants,MeetingAgenda) :-
	eventParticipants(TaskID,Participants),
	Participants =.. [groupFn|[MeetingParticipants]],
	findall(Point,(hasMeetingAgenda(TaskID,Agenda),member(Point,Agenda)),MeetingAgenda).

%% meetingAgenda(TaskID,MeetingParticipants,MeetingAgenda) :-
%% 	eventParticipants(TaskID,Participants),
%% 	Participants =.. [groupFn|[MeetingParticipants]],
%% 	findall(Point,
%% 		((hasMeetingAgenda(TaskID,Agenda),member(Point,Agenda)) ;
%% 		 (member(Agent1,MeetingParticipants),
%% 		  member(Agent2,MeetingParticipants),
%% 		  Agent1 \= Agent2,
%% 		  ask(Agent1Spec,Agent2,Question),
%% 		  term_contains_subterm(Agent1,Agent1Spec),
%% 		  Point = ask(Agent1,Agent2,Question))),MeetingAgenda).

non_empty_list(List) :-
	length(List,Length),
	Length > 0.

meetingLocations(TaskID,MeetingLocations) :-
	setof([Term,Location],
	      TaskID^Description^Address^(description(TaskID,Description),
					  term_contains_subterm(Term,Description),
					  hasAddress(Term,Address),
					  renderObject(Address,Location)),
	      MeetingLocations) ->
	true ;
	MeetingLocations = [].

meetingPhoneNumbers(TaskID,MeetingParticipants,MeetingPhoneNumbers) :-
	setof([Term,MeetingPhoneNumber],
		TaskID^Description^Term^MeetingParticipants^(((description(TaskID,Description),term_contains_subterm(Term,Description)) ;
		  term_contains_subterm(Term,MeetingParticipants)),
		 hasPhoneNumberSequence(Term,MeetingPhoneNumber)),
		MeetingPhoneNumbers).


meetingPhoneNumbersHuh(TaskID,MeetingParticipants,MeetingPhoneNumbers) :-
	findall(MeetingPhoneNumber,
		(((description(TaskID,Description),
		  term_contains_subterm(Term,Description)) ;
		  term_contains_subterm(Term,MeetingParticipants)),
		 hasPhoneNumberSequence(Term,MeetingPhoneNumber)),
		MeetingPhoneNumbers).

listFactsAboutTask(TaskID,Facts) :-
	description(TaskID,Desc),
	meetingAgenda(TaskID,MeetingParticipants,MeetingAgenda),
	meetingLocations(TaskID,MeetingLocations),
	meetingPhoneNumbers(TaskID,MeetingParticipants,MeetingPhoneNumbers),
	Facts = [MeetingParticipants,MeetingAgenda,MeetingPhoneNumbers].

createNewEvent([DateArg,SpecificationArg,ParticipantsArg,AgendaArg],Results) :-
	newId(AppointmentID),

	parseForm(date,DateArg,Date),
	parseForm(specification,SpecificationArg,Specification),
	parseForm(participants,ParticipantsArg,Participants),
	parseForm(agenda,AgendaArg,Agenda),

	assertz(appointment(AppointmentID,Participants,Specification)),
	assertz(deadline(AppointmentID,Date)),
	assertz(hasMeetingAgenda(TaskID,Agenda)).

parseForm(date,DateArg,Date) :-
	Dates = ['next monday','tomorrow','yesterday','this tuesday','easter'],
	Times = ['in 3 hours','15 minutes from now','tonight','first thing in the morning'],
	true.
parseForm(specification,SpecificationArg,Specification) :-
	true.
parseForm(participants,ParticipantsArg,Participants) :-
	split_string(ParticipantsArg, ",", "", Items),
	true.
parseForm(agenda,AgendaArg,Agenda) :-
	true.

dateTimeP([Y-M-D,H:Mi:S]) :-
	true.
